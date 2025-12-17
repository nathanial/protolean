/-
  Protobuf decoder monad for deserializing messages from wire format.

  The decoder combines ExceptT for error handling with StateM for position
  tracking. It supports nested parsing with length limits for embedded messages.
-/
import Protolean.WireFormat
import Protolean.Varint
import Protolean.ByteArray.Basic

namespace Protolean

/-- Decoding error types -/
inductive DecodeError where
  | unexpectedEof : DecodeError
  | varintOverflow : DecodeError
  | invalidWireType (got : Nat) : DecodeError
  | invalidFieldNumber : DecodeError
  | invalidUtf8 : DecodeError
  | truncatedMessage : DecodeError
  | unexpectedWireType (expected : WireType) (got : WireType) : DecodeError
  | negativeLength : DecodeError
  | custom (msg : String) : DecodeError
  deriving Repr

instance : ToString DecodeError where
  toString
    | .unexpectedEof => "unexpected end of input"
    | .varintOverflow => "varint overflow (more than 10 bytes)"
    | .invalidWireType got => s!"invalid wire type: {got}"
    | .invalidFieldNumber => "invalid field number (must be > 0)"
    | .invalidUtf8 => "invalid UTF-8 string"
    | .truncatedMessage => "message was truncated"
    | .unexpectedWireType expected got => s!"expected wire type {repr expected}, got {repr got}"
    | .negativeLength => "negative length in length-delimited field"
    | .custom msg => msg

/-- Decoder state tracking position in input -/
structure DecoderState where
  input : ByteArray
  position : Nat
  limit : Option Nat  -- For length-delimited parsing
  deriving Inhabited

/-- Decoder monad combining state and error handling -/
abbrev Decoder := ExceptT DecodeError (StateM DecoderState)

namespace Decoder

/-- Run decoder on input bytes -/
def execute (dec : Decoder α) (input : ByteArray) : Except DecodeError α :=
  let (result, _) := ExceptT.run dec { input, position := 0, limit := none }
  result

/-- Run decoder and also return final position -/
def executeWithPos (dec : Decoder α) (input : ByteArray) : Except DecodeError (α × Nat) :=
  let (result, state) := ExceptT.run dec { input, position := 0, limit := none }
  match result with
  | .ok a => .ok (a, state.position)
  | .error e => .error e

/-- Get the effective end position (limit or input size) -/
def getEndPos : Decoder Nat := do
  let s ← get
  return s.limit.getD s.input.size

/-- Check if we've reached end of input (or limit) -/
def atEnd : Decoder Bool := do
  let s ← get
  let endPos ← getEndPos
  return s.position >= endPos

/-- Get remaining bytes count -/
def remaining : Decoder Nat := do
  let s ← get
  let endPos ← getEndPos
  return endPos - s.position

/-- Get current position -/
def getPosition : Decoder Nat := do
  let s ← get
  return s.position

/-- Read a single byte -/
def readByte : Decoder UInt8 := do
  let s ← get
  let endPos ← getEndPos
  if s.position >= endPos then
    throw .unexpectedEof
  let byte := s.input.get! s.position
  set { s with position := s.position + 1 }
  return byte

/-- Peek at the next byte without consuming it -/
def peekByte : Decoder (Option UInt8) := do
  let s ← get
  let endPos ← getEndPos
  if s.position >= endPos then
    return none
  return some (s.input.get! s.position)

/-- Read exactly n bytes -/
def readBytes (n : Nat) : Decoder ByteArray := do
  let s ← get
  let endPos ← getEndPos
  if s.position + n > endPos then
    throw .unexpectedEof
  let bytes := s.input.extract s.position (s.position + n)
  set { s with position := s.position + n }
  return bytes

/-- Skip n bytes -/
def skip (n : Nat) : Decoder Unit := do
  let s ← get
  let endPos ← getEndPos
  if s.position + n > endPos then
    throw .unexpectedEof
  set { s with position := s.position + n }

/-- Read a varint as UInt64 -/
def readVarint : Decoder UInt64 := do
  let s ← get
  match Varint.decodeUInt64 s.input s.position with
  | .error .unexpectedEof => throw DecodeError.unexpectedEof
  | .error .overflow => throw DecodeError.varintOverflow
  | .ok result =>
    set { s with position := s.position + result.bytesRead }
    return result.value

/-- Read a varint as UInt32 -/
def readVarint32 : Decoder UInt32 := do
  let v ← readVarint
  return v.toUInt32

/-- Read a varint as Int32 -/
def readInt32 : Decoder Int32 := do
  let v ← readVarint
  return v.toInt64.toInt32

/-- Read a varint as Int64 -/
def readInt64 : Decoder Int64 := do
  let v ← readVarint
  return v.toInt64

/-- Read a ZigZag-encoded sint32 -/
def readSInt32 : Decoder Int32 := do
  let v ← readVarint
  return Varint.zigZagDecode32 v.toUInt32

/-- Read a ZigZag-encoded sint64 -/
def readSInt64 : Decoder Int64 := do
  let v ← readVarint
  return Varint.zigZagDecode64 v

/-- Read a boolean (varint, 0 = false, nonzero = true) -/
def readBool : Decoder Bool := do
  let v ← readVarint
  return v != 0

/-- Read a field tag -/
def readTag : Decoder (Option FieldTag) := do
  if ← atEnd then
    return none
  let tagValue ← readVarint
  match FieldTag.fromUInt64? tagValue with
  | some tag => return some tag
  | none =>
    -- Check if it's an invalid field number vs invalid wire type
    let wireType := (tagValue &&& 0x7).toNat
    let fieldNum := tagValue >>> 3
    if fieldNum == 0 then
      throw .invalidFieldNumber
    else
      throw (.invalidWireType wireType)

/-- Read a 32-bit fixed value (little-endian) -/
def readFixed32 : Decoder UInt32 := do
  let bytes ← readBytes 4
  match ByteArray.getUInt32LE? bytes 0 with
  | some v => return v
  | none => throw .unexpectedEof  -- Should not happen since we already read 4 bytes

/-- Read a 64-bit fixed value (little-endian) -/
def readFixed64 : Decoder UInt64 := do
  let bytes ← readBytes 8
  match ByteArray.getUInt64LE? bytes 0 with
  | some v => return v
  | none => throw .unexpectedEof  -- Should not happen since we already read 8 bytes

/-- Read a signed 32-bit fixed value -/
def readSFixed32 : Decoder Int32 := do
  let v ← readFixed32
  return v.toInt32

/-- Read a signed 64-bit fixed value -/
def readSFixed64 : Decoder Int64 := do
  let v ← readFixed64
  return v.toInt64

/-- Read a 32-bit float -/
def readFloat : Decoder Float := do
  let bits ← readFixed32
  return float32BitsToFloat64 bits
where
  /-- Convert 32-bit IEEE 754 bits to 64-bit Float -/
  float32BitsToFloat64 (bits : UInt32) : Float :=
    let posInf : Float := 1/0
    let negInf : Float := -1/0
    let nan : Float := 0/0
    let sign32 := bits >>> 31
    let exp32 := ((bits >>> 23) &&& 0xFF).toNat
    let mant32 := bits &&& 0x7FFFFF  -- 23-bit mantissa

    if exp32 == 255 then
      -- Special values
      if mant32 == 0 then
        if sign32 == 1 then negInf else posInf
      else
        nan
    else if exp32 == 0 && mant32 == 0 then
      -- Zero (positive or negative)
      if sign32 == 1 then -0.0 else 0.0
    else if exp32 == 0 then
      -- Denormalized: convert to normalized double
      -- Value = (-1)^sign * 2^(-126) * (0.mant)
      let frac := mant32.toFloat / (2^23 : Float)
      let value := frac * Float.pow 2.0 (-126)
      if sign32 == 1 then -value else value
    else
      -- Normalized number: convert exponent
      -- Use Int arithmetic to handle exponents < 127 correctly
      let exp64 : Nat := (exp32 : Int) - 127 + 1023 |>.toNat  -- Float bias 127 -> Double bias 1023
      let mant64 := mant32.toUInt64 <<< 29  -- 23 bits -> 52 bits
      let bits64 := (sign32.toUInt64 <<< 63) ||| (exp64.toUInt64 <<< 52) ||| mant64
      Float.ofBits bits64

/-- Read a 64-bit double -/
def readDouble : Decoder Float := do
  let bits ← readFixed64
  return ByteArray.bitsToFloat bits

/-- Read a length-delimited byte array -/
def readLengthDelimited : Decoder ByteArray := do
  let len ← readVarint
  if len > UInt64.ofNat (2^31 - 1) then  -- Sanity check
    throw .negativeLength
  readBytes len.toNat

/-- Read a UTF-8 string -/
def readString : Decoder String := do
  let bytes ← readLengthDelimited
  match String.fromUTF8? bytes with
  | some s => return s
  | none => throw .invalidUtf8

/-- Run decoder within a length limit (for embedded messages) -/
def withLimit (length : Nat) (dec : Decoder α) : Decoder α := do
  let s ← get
  let oldLimit := s.limit
  let newLimit := s.position + length
  let effectiveLimit := min newLimit (oldLimit.getD s.input.size)
  set { s with limit := some effectiveLimit }
  let result ← dec
  let s' ← get
  if s'.position < effectiveLimit then
    throw .truncatedMessage
  set { s' with limit := oldLimit }
  return result

/-- Skip a field based on its wire type (for unknown fields) -/
def skipField (wireType : WireType) : Decoder Unit := do
  match wireType with
  | .varint => let _ ← readVarint
  | .fixed64 => skip 8
  | .lengthDelimited =>
    let len ← readVarint
    skip len.toNat
  | .fixed32 => skip 4

/-- Expect a specific wire type, throw error if mismatch -/
def expectWireType (expected : WireType) (got : WireType) : Decoder Unit := do
  if expected != got then
    throw (.unexpectedWireType expected got)

end Decoder

end Protolean
