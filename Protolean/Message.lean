/-
  Type class and utilities for protobuf messages.
-/
import Protolean.Codec
import Protolean.Repeated

namespace Protolean

/-- Represents an unknown field that was encountered during parsing -/
structure UnknownField where
  fieldNumber : FieldNumber
  wireType : WireType
  data : ByteArray
  deriving BEq

instance : Repr UnknownField where
  reprPrec u _ := s!"UnknownField({u.fieldNumber}, {repr u.wireType}, <{u.data.size} bytes>)"

/-- Collection of unknown fields for forward compatibility -/
abbrev UnknownFields := Array UnknownField

/-- Type class for protobuf messages -/
class ProtoMessage (α : Type) where
  /-- Encode all fields of the message -/
  encodeFields : α → Encoder Unit
  /-- Decode a single field, returning updated message -/
  decodeField : α → FieldTag → Decoder α
  /-- Default/empty message -/
  defaultValue : α

/-- Encode a message to bytes -/
def encodeMessage [inst : ProtoMessage α] (msg : α) : ByteArray :=
  Encoder.execute (inst.encodeFields msg)

/-- Helper to decode message fields in a loop -/
partial def decodeMessageLoop [inst : ProtoMessage α] (msg : α) : Decoder α := do
  match ← Decoder.readTag with
  | none => return msg
  | some tag =>
    let msg' ← inst.decodeField msg tag
    decodeMessageLoop msg'

/-- Decode a message from bytes -/
def decodeMessage [inst : ProtoMessage α] (bytes : ByteArray) : Except DecodeError α :=
  Decoder.execute (decodeMessageLoop inst.defaultValue) bytes

/-- ProtoEncodable instance for messages (as embedded/length-delimited) -/
instance [inst : ProtoMessage α] : ProtoEncodable α where
  wireType := .lengthDelimited
  encode msg := Encoder.emitEmbedded (inst.encodeFields msg)
  isDefault _ := false  -- Messages are never considered "default" for encoding purposes

/-- Helper to decode embedded message fields -/
partial def decodeEmbeddedLoop [inst : ProtoMessage α] (msg : α) : Decoder α := do
  match ← Decoder.readTag with
  | none => return msg
  | some tag =>
    let msg' ← inst.decodeField msg tag
    decodeEmbeddedLoop msg'

/-- ProtoDecodable instance for messages (as embedded/length-delimited) -/
instance [inst : ProtoMessage α] : ProtoDecodable α where
  wireType := .lengthDelimited
  decode := do
    let len ← Decoder.readVarint
    Decoder.withLimit len.toNat (decodeEmbeddedLoop inst.defaultValue)
  defaultValue := inst.defaultValue

/-- Helper to encode an optional field -/
def encodeOptional [ProtoEncodable α] (fieldNum : FieldNumber) (value : Option α) : Encoder Unit := do
  match value with
  | none => pure ()
  | some v => encodeFieldAlways fieldNum v

/-- Helper to encode a repeated field with packed encoding for supported types -/
def encodeRepeatedPacked [ProtoEncodable α] (fieldNum : FieldNumber) (values : Array α)
    (packFn : FieldNumber → Array α → Encoder Unit) : Encoder Unit := do
  if values.isEmpty then
    return ()
  packFn fieldNum values

/-- Helper to decode a repeated field (accumulating into array) -/
def decodeRepeatedField [inst : ProtoDecodable α] (arr : Array α) (wireType : WireType) : Decoder (Array α) := do
  Decoder.expectWireType inst.wireType wireType
  let v ← inst.decode
  return arr.push v

/-- Merge two messages (for when the same field appears multiple times) -/
class ProtoMergeable (α : Type) where
  merge : α → α → α

/-- Default merge just takes the later value -/
instance [ProtoMessage α] : ProtoMergeable α where
  merge _ b := b

end Protolean
