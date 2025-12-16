/-
  Protobuf encoder monad for serializing messages to wire format.

  The encoder uses a simple state monad with ByteBuilder for efficient
  byte accumulation. There are no errors during encoding - validation
  should happen before encoding.
-/
import Protolean.WireFormat
import Protolean.Varint
import Protolean.ByteArray.Basic
import Protolean.ByteArray.Builder

namespace Protolean

/-- Encoder state holding the accumulated bytes -/
structure EncoderState where
  builder : ByteBuilder
  deriving Inhabited

/-- Encoder monad - pure state accumulation -/
abbrev Encoder := StateM EncoderState

namespace Encoder

/-- Run encoder and extract the final ByteArray -/
def execute (enc : Encoder α) : ByteArray :=
  let (_, state) := enc { builder := ByteBuilder.empty }
  state.builder.toByteArray

/-- Run encoder and return both result and ByteArray -/
def executeWith (enc : Encoder α) : α × ByteArray :=
  let (a, state) := enc { builder := ByteBuilder.empty }
  (a, state.builder.toByteArray)

/-- Emit raw bytes -/
def emitBytes (bs : ByteArray) : Encoder Unit :=
  modify fun s => { s with builder := s.builder ++ ByteBuilder.bytes bs }

/-- Emit a single byte -/
def emitByte (b : UInt8) : Encoder Unit :=
  modify fun s => { s with builder := s.builder ++ ByteBuilder.byte b }

/-- Emit a varint-encoded unsigned 64-bit integer -/
def emitVarint (n : UInt64) : Encoder Unit :=
  emitBytes (Varint.encodeUInt64 n)

/-- Emit a varint-encoded unsigned 32-bit integer -/
def emitVarint32 (n : UInt32) : Encoder Unit :=
  emitBytes (Varint.encodeUInt32 n)

/-- Emit a field tag -/
def emitTag (fieldNum : FieldNumber) (wireType : WireType) : Encoder Unit :=
  let tag := FieldTag.mk fieldNum wireType
  emitVarint tag.toUInt64

/-- Emit a 32-bit fixed value (little-endian) -/
def emitFixed32 (v : UInt32) : Encoder Unit :=
  emitBytes (ByteArray.pushUInt32LE ByteArray.empty v)

/-- Emit a 64-bit fixed value (little-endian) -/
def emitFixed64 (v : UInt64) : Encoder Unit :=
  emitBytes (ByteArray.pushUInt64LE ByteArray.empty v)

/-- Emit length-delimited bytes with length prefix -/
def emitLengthDelimited (bs : ByteArray) : Encoder Unit := do
  emitVarint bs.size.toUInt64
  emitBytes bs

/-- Encode nested content with length prefix (for embedded messages) -/
def emitEmbedded (inner : Encoder Unit) : Encoder Unit := do
  let innerBytes := execute inner
  emitLengthDelimited innerBytes

/-- Emit a string as length-delimited UTF-8 bytes -/
def emitString (s : String) : Encoder Unit :=
  emitLengthDelimited s.toUTF8

/-- Emit a boolean as varint (0 or 1) -/
def emitBool (b : Bool) : Encoder Unit :=
  emitVarint (if b then 1 else 0)

/-- Emit a signed 32-bit integer (standard varint, may use 10 bytes for negatives) -/
def emitInt32 (n : Int32) : Encoder Unit :=
  emitBytes (Varint.encodeInt32 n)

/-- Emit a signed 64-bit integer -/
def emitInt64 (n : Int64) : Encoder Unit :=
  emitBytes (Varint.encodeInt64 n)

/-- Emit a ZigZag-encoded signed 32-bit integer -/
def emitSInt32 (n : Int32) : Encoder Unit :=
  emitBytes (Varint.encodeSInt32 n)

/-- Emit a ZigZag-encoded signed 64-bit integer -/
def emitSInt64 (n : Int64) : Encoder Unit :=
  emitBytes (Varint.encodeSInt64 n)

/-- Emit a float (32-bit IEEE 754) -/
def emitFloat (f : Float) : Encoder Unit := do
  -- Lean's Float is 64-bit (double), we convert to 32-bit float representation
  -- using bit manipulation to convert IEEE 754 double to single
  let bits := float64ToFloat32Bits f
  emitFixed32 bits
where
  /-- Convert a 64-bit float to 32-bit IEEE 754 representation -/
  float64ToFloat32Bits (f : Float) : UInt32 :=
    let posInf : Float := 1/0
    let negInf : Float := -1/0
    if f.isNaN then 0x7FC00000  -- NaN
    else if f == posInf then 0x7F800000  -- +Inf
    else if f == negInf then 0xFF800000  -- -Inf
    else if f == 0.0 then
      -- Check for negative zero
      if (f.toBits >>> 63) == 1 then 0x80000000 else 0x00000000
    else
      let bits64 := f.toBits
      let sign64 := bits64 >>> 63
      let exp64 := ((bits64 >>> 52) &&& 0x7FF).toNat
      let mant64 := bits64 &&& 0xFFFFFFFFFFFFF  -- 52-bit mantissa

      -- Convert exponent: double bias is 1023, float bias is 127
      let exp32 : Int := exp64 - 1023 + 127

      if exp32 >= 255 then
        -- Overflow to infinity
        ((sign64.toUInt32 <<< 31) ||| 0x7F800000)
      else if exp32 <= 0 then
        -- Underflow to zero (denormals not fully supported)
        (sign64.toUInt32 <<< 31)
      else
        -- Normal number: truncate 52-bit mantissa to 23 bits
        let mant32 := (mant64 >>> 29).toUInt32  -- Keep top 23 bits
        ((sign64.toUInt32 <<< 31) ||| (exp32.toNat.toUInt32 <<< 23) ||| mant32)

/-- Emit a double (64-bit IEEE 754) -/
def emitDouble (f : Float) : Encoder Unit := do
  let bits := ByteArray.floatToBits f
  emitFixed64 bits

end Encoder

end Protolean
