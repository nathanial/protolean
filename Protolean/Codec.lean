/-
  Type classes for generic protobuf encoding and decoding.
-/
import Protolean.WireFormat
import Protolean.Encoder
import Protolean.Decoder

namespace Protolean

/-- Type class for types that can be encoded to protobuf wire format -/
class ProtoEncodable (α : Type) where
  /-- The wire type used for this type -/
  wireType : WireType
  /-- Encode value (without field tag) -/
  encode : α → Encoder Unit
  /-- Check if value is the default (should not be encoded in proto3) -/
  isDefault : α → Bool := fun _ => false

/-- Type class for types that can be decoded from protobuf wire format -/
class ProtoDecodable (α : Type) where
  /-- The expected wire type -/
  wireType : WireType
  /-- Decode value (assumes tag already consumed) -/
  decode : Decoder α
  /-- Default value for missing fields -/
  defaultValue : α

/-- Combined codec for bidirectional conversion -/
class ProtoCodec (α : Type) extends ProtoEncodable α, ProtoDecodable α

/-- Encode a field with tag (skips if default value in proto3) -/
def encodeField [inst : ProtoEncodable α] (fieldNum : FieldNumber) (value : α) : Encoder Unit := do
  if inst.isDefault value then
    pure ()  -- Proto3: don't encode default values
  else
    Encoder.emitTag fieldNum inst.wireType
    inst.encode value

/-- Encode a field that should always be present (for oneofs, required fields) -/
def encodeFieldAlways [inst : ProtoEncodable α] (fieldNum : FieldNumber) (value : α) : Encoder Unit := do
  Encoder.emitTag fieldNum inst.wireType
  inst.encode value

/-- Helper to decode expecting a specific wire type -/
def decodeExpecting [inst : ProtoDecodable α] (gotWireType : WireType) : Decoder α := do
  let expected := inst.wireType
  if gotWireType != expected then
    throw (.unexpectedWireType expected gotWireType)
  inst.decode

/-- Type class for protobuf enums -/
class ProtoEnum (α : Type) where
  /-- Convert enum to int32 value -/
  toInt32 : α → Int32
  /-- Convert int32 to enum (returns default for unknown values in proto3) -/
  fromInt32 : Int32 → α
  /-- Default enum value (the one with value 0) -/
  defaultValue : α

/-- ProtoEncodable instance for enums -/
instance [inst : ProtoEnum α] : ProtoEncodable α where
  wireType := .varint
  encode v := Encoder.emitInt32 (inst.toInt32 v)
  isDefault v := inst.toInt32 v == 0

/-- ProtoDecodable instance for enums -/
instance [inst : ProtoEnum α] : ProtoDecodable α where
  wireType := .varint
  decode := do
    let v ← Decoder.readInt32
    return inst.fromInt32 v
  defaultValue := inst.defaultValue

end Protolean
