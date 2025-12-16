/-
  Variable-length integer encoding for Protocol Buffers.

  Varints use 7 bits per byte for data, with the MSB indicating continuation.
  ZigZag encoding maps signed integers to unsigned for efficient varint encoding.
-/
namespace Protolean.Varint

/-- Maximum bytes needed for a 64-bit varint -/
def maxVarintBytes : Nat := 10

/-- Encode an unsigned 64-bit integer as varint bytes -/
def encodeUInt64 (n : UInt64) : ByteArray := Id.run do
  if n == 0 then
    return ByteArray.mk #[0]
  let mut result := ByteArray.empty
  let mut value := n
  while value != 0 do
    let byte := (value &&& 0x7F).toUInt8
    value := value >>> 7
    if value != 0 then
      result := result.push (byte ||| 0x80)  -- Set continuation bit
    else
      result := result.push byte
  return result

/-- Encode an unsigned 32-bit integer as varint bytes -/
def encodeUInt32 (n : UInt32) : ByteArray :=
  encodeUInt64 n.toUInt64

/-- Encode a signed 32-bit integer as varint (negative numbers use 10 bytes) -/
def encodeInt32 (n : Int32) : ByteArray :=
  -- Negative int32 values are sign-extended to 64 bits
  if n < 0 then
    encodeUInt64 n.toInt64.toUInt64
  else
    encodeUInt64 n.toUInt32.toUInt64

/-- Encode a signed 64-bit integer as varint -/
def encodeInt64 (n : Int64) : ByteArray :=
  encodeUInt64 n.toUInt64

/-- ZigZag encode a signed 32-bit integer: (n << 1) ^ (n >> 31) -/
def zigZagEncode32 (n : Int32) : UInt32 :=
  ((n <<< 1) ^^^ (n >>> 31)).toUInt32

/-- ZigZag encode a signed 64-bit integer: (n << 1) ^ (n >> 63) -/
def zigZagEncode64 (n : Int64) : UInt64 :=
  ((n <<< 1) ^^^ (n >>> 63)).toUInt64

/-- ZigZag decode to signed 32-bit: (n >>> 1) ^ -(n & 1) -/
def zigZagDecode32 (n : UInt32) : Int32 :=
  (n >>> 1).toInt32 ^^^ (-(n &&& 1).toInt32)

/-- ZigZag decode to signed 64-bit: (n >>> 1) ^ -(n & 1) -/
def zigZagDecode64 (n : UInt64) : Int64 :=
  (n >>> 1).toInt64 ^^^ (-(n &&& 1).toInt64)

/-- Encode sint32 using ZigZag + varint -/
def encodeSInt32 (n : Int32) : ByteArray :=
  encodeUInt64 (zigZagEncode32 n).toUInt64

/-- Encode sint64 using ZigZag + varint -/
def encodeSInt64 (n : Int64) : ByteArray :=
  encodeUInt64 (zigZagEncode64 n)

/-- Result of varint decoding -/
structure DecodeResult where
  value : UInt64
  bytesRead : Nat
  deriving Repr, BEq

/-- Decode error types for varint parsing -/
inductive DecodeError where
  | unexpectedEof : DecodeError
  | overflow : DecodeError      -- More than 10 bytes
  deriving Repr, DecidableEq

/-- Decode a varint from bytes starting at offset -/
def decodeUInt64 (arr : ByteArray) (offset : Nat := 0) : Except DecodeError DecodeResult := do
  if offset >= arr.size then
    throw .unexpectedEof
  let mut result : UInt64 := 0
  let mut shift : Nat := 0
  let mut pos := offset
  while pos < arr.size do
    if shift >= 64 then
      throw .overflow
    let byte := arr.get! pos
    let value := (byte &&& 0x7F).toUInt64
    result := result ||| (value <<< shift.toUInt64)
    pos := pos + 1
    if byte &&& 0x80 == 0 then
      return ⟨result, pos - offset⟩
    shift := shift + 7
  throw .unexpectedEof

/-- Decode a varint and return as Int32 -/
def decodeInt32 (arr : ByteArray) (offset : Nat := 0) : Except DecodeError (Int32 × Nat) := do
  let ⟨value, bytesRead⟩ ← decodeUInt64 arr offset
  return (value.toInt64.toInt32, bytesRead)

/-- Decode a varint and return as Int64 -/
def decodeInt64 (arr : ByteArray) (offset : Nat := 0) : Except DecodeError (Int64 × Nat) := do
  let ⟨value, bytesRead⟩ ← decodeUInt64 arr offset
  return (value.toInt64, bytesRead)

/-- Decode a ZigZag-encoded sint32 -/
def decodeSInt32 (arr : ByteArray) (offset : Nat := 0) : Except DecodeError (Int32 × Nat) := do
  let ⟨value, bytesRead⟩ ← decodeUInt64 arr offset
  return (zigZagDecode32 value.toUInt32, bytesRead)

/-- Decode a ZigZag-encoded sint64 -/
def decodeSInt64 (arr : ByteArray) (offset : Nat := 0) : Except DecodeError (Int64 × Nat) := do
  let ⟨value, bytesRead⟩ ← decodeUInt64 arr offset
  return (zigZagDecode64 value, bytesRead)

end Protolean.Varint
