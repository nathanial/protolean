/-
  Support for repeated (array) fields in protobuf.

  In Proto3, repeated numeric fields use packed encoding by default:
  a single length-delimited field containing concatenated values.
-/
import Protolean.Codec
import Protolean.Scalar

namespace Protolean

/-- Encode a repeated field (non-packed, each element with its own tag) -/
def encodeRepeated [inst : ProtoEncodable α] (fieldNum : FieldNumber) (values : Array α) : Encoder Unit := do
  for v in values do
    Encoder.emitTag fieldNum inst.wireType
    inst.encode v

/-- Encode a packed repeated varint field -/
def encodePackedVarint (fieldNum : FieldNumber) (values : Array UInt64) : Encoder Unit := do
  if values.isEmpty then
    return ()
  Encoder.emitTag fieldNum .lengthDelimited
  let packedBytes := Encoder.execute do
    for v in values do
      Encoder.emitVarint v
  Encoder.emitLengthDelimited packedBytes

/-- Encode a packed repeated int32 field -/
def encodePackedInt32 (fieldNum : FieldNumber) (values : Array Int32) : Encoder Unit := do
  if values.isEmpty then
    return ()
  Encoder.emitTag fieldNum .lengthDelimited
  let packedBytes := Encoder.execute do
    for v in values do
      Encoder.emitInt32 v
  Encoder.emitLengthDelimited packedBytes

/-- Encode a packed repeated sint32 field (ZigZag) -/
def encodePackedSInt32 (fieldNum : FieldNumber) (values : Array Int32) : Encoder Unit := do
  if values.isEmpty then
    return ()
  Encoder.emitTag fieldNum .lengthDelimited
  let packedBytes := Encoder.execute do
    for v in values do
      Encoder.emitSInt32 v
  Encoder.emitLengthDelimited packedBytes

/-- Encode a packed repeated fixed32 field -/
def encodePackedFixed32 (fieldNum : FieldNumber) (values : Array UInt32) : Encoder Unit := do
  if values.isEmpty then
    return ()
  Encoder.emitTag fieldNum .lengthDelimited
  let packedBytes := Encoder.execute do
    for v in values do
      Encoder.emitFixed32 v
  Encoder.emitLengthDelimited packedBytes

/-- Encode a packed repeated fixed64 field -/
def encodePackedFixed64 (fieldNum : FieldNumber) (values : Array UInt64) : Encoder Unit := do
  if values.isEmpty then
    return ()
  Encoder.emitTag fieldNum .lengthDelimited
  let packedBytes := Encoder.execute do
    for v in values do
      Encoder.emitFixed64 v
  Encoder.emitLengthDelimited packedBytes

/-- Encode a packed repeated bool field -/
def encodePackedBool (fieldNum : FieldNumber) (values : Array Bool) : Encoder Unit := do
  if values.isEmpty then
    return ()
  Encoder.emitTag fieldNum .lengthDelimited
  let packedBytes := Encoder.execute do
    for v in values do
      Encoder.emitBool v
  Encoder.emitLengthDelimited packedBytes

/-- Decode packed varints from a length-delimited field -/
def decodePackedVarint (length : Nat) : Decoder (Array UInt64) := do
  Decoder.withLimit length do
    let mut result := #[]
    while !(← Decoder.atEnd) do
      let v ← Decoder.readVarint
      result := result.push v
    return result

/-- Decode packed int32 values -/
def decodePackedInt32 (length : Nat) : Decoder (Array Int32) := do
  Decoder.withLimit length do
    let mut result := #[]
    while !(← Decoder.atEnd) do
      let v ← Decoder.readInt32
      result := result.push v
    return result

/-- Decode packed sint32 values (ZigZag) -/
def decodePackedSInt32 (length : Nat) : Decoder (Array Int32) := do
  Decoder.withLimit length do
    let mut result := #[]
    while !(← Decoder.atEnd) do
      let v ← Decoder.readSInt32
      result := result.push v
    return result

/-- Decode packed fixed32 values -/
def decodePackedFixed32 (length : Nat) : Decoder (Array UInt32) := do
  Decoder.withLimit length do
    let mut result := #[]
    while !(← Decoder.atEnd) do
      let v ← Decoder.readFixed32
      result := result.push v
    return result

/-- Decode packed fixed64 values -/
def decodePackedFixed64 (length : Nat) : Decoder (Array UInt64) := do
  Decoder.withLimit length do
    let mut result := #[]
    while !(← Decoder.atEnd) do
      let v ← Decoder.readFixed64
      result := result.push v
    return result

/-- Decode packed bool values -/
def decodePackedBool (length : Nat) : Decoder (Array Bool) := do
  Decoder.withLimit length do
    let mut result := #[]
    while !(← Decoder.atEnd) do
      let v ← Decoder.readBool
      result := result.push v
    return result

/-- Decode packed double values -/
def decodePackedDouble (length : Nat) : Decoder (Array Float) := do
  Decoder.withLimit length do
    let mut result := #[]
    while !(← Decoder.atEnd) do
      let v ← Decoder.readDouble
      result := result.push v
    return result

/-- Decode packed float values -/
def decodePackedFloat (length : Nat) : Decoder (Array Float) := do
  Decoder.withLimit length do
    let mut result := #[]
    while !(← Decoder.atEnd) do
      let v ← Decoder.readFloat
      result := result.push v
    return result

end Protolean
