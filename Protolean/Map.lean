/-
  Support for map fields in protobuf.

  Maps are encoded as repeated messages with key=1 and value=2.
  map<KeyType, ValueType> is semantically equivalent to:
    message MapEntry {
      KeyType key = 1;
      ValueType value = 2;
    }
    repeated MapEntry field = N;
-/
import Protolean.Codec
import Protolean.Scalar
import Std.Data.HashMap

namespace Protolean

/-- Encode a single map entry as a length-delimited message -/
def encodeMapEntry [ProtoEncodable K] [ProtoEncodable V]
    (key : K) (value : V) : Encoder Unit := do
  Encoder.emitEmbedded do
    -- Key is always field 1
    encodeFieldAlways 1 key
    -- Value is always field 2
    encodeFieldAlways 2 value

/-- Encode a map as repeated key-value pair messages -/
def encodeMap [ProtoEncodable K] [ProtoEncodable V]
    (fieldNum : FieldNumber) (entries : List (K × V)) : Encoder Unit := do
  for (k, v) in entries do
    Encoder.emitTag fieldNum .lengthDelimited
    encodeMapEntry k v

/-- Encode a HashMap as a map field -/
def encodeHashMap [BEq K] [Hashable K] [ProtoEncodable K] [ProtoEncodable V]
    (fieldNum : FieldNumber) (map : Std.HashMap K V) : Encoder Unit := do
  for (k, v) in map.toList do
    Encoder.emitTag fieldNum .lengthDelimited
    encodeMapEntry k v

/-- Decode a single map entry from a length-delimited field -/
def decodeMapEntry [ProtoDecodable K] [ProtoDecodable V] (length : Nat) : Decoder (K × V) := do
  Decoder.withLimit length do
    let mut key : Option K := none
    let mut value : Option V := none

    while !(← Decoder.atEnd) do
      match ← Decoder.readTag with
      | none => break
      | some tag =>
        if tag.fieldNumber == 1 then
          key := some (← decodeExpecting tag.wireType)
        else if tag.fieldNumber == 2 then
          value := some (← decodeExpecting tag.wireType)
        else
          Decoder.skipField tag.wireType

    -- Use defaults for missing fields (proto3 semantics)
    let k := key.getD ProtoDecodable.defaultValue
    let v := value.getD ProtoDecodable.defaultValue
    return (k, v)

/-- Accumulate a map entry into a HashMap during message decoding -/
def accumulateMapEntry [BEq K] [Hashable K] [ProtoDecodable K] [ProtoDecodable V]
    (map : Std.HashMap K V) (length : Nat) : Decoder (Std.HashMap K V) := do
  let (k, v) ← decodeMapEntry length
  return map.insert k v

end Protolean
