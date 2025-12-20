/-
  Unit tests for scalar type encoding/decoding.
-/
import Crucible
import Protolean

namespace Tests.Scalar

open Crucible
open Protolean

/-- Repr instance for ByteArray for test output -/
instance : Repr ByteArray where
  reprPrec ba _ := s!"ByteArray.mk #[{", ".intercalate (ba.toList.map toString)}]"

/-- Test helper to check encode/decode round-trip for a type -/
def testRoundTrip [ProtoCodec α] [BEq α] [Repr α] (value : α) : IO Bool := do
  let encoded := Encoder.execute (ProtoEncodable.encode value)
  match Decoder.execute ProtoDecodable.decode encoded with
  | .ok decoded => pure (decoded == value)
  | .error _ => pure false

testSuite "Scalar Tests"

-- UInt32 tests
test "UInt32 roundtrip 0" := do
  ensure (← testRoundTrip (0 : UInt32)) "0 should round-trip"

test "UInt32 roundtrip 1" := do
  ensure (← testRoundTrip (1 : UInt32)) "1 should round-trip"

test "UInt32 roundtrip 127" := do
  ensure (← testRoundTrip (127 : UInt32)) "127 should round-trip"

test "UInt32 roundtrip 128" := do
  ensure (← testRoundTrip (128 : UInt32)) "128 should round-trip"

test "UInt32 roundtrip max" := do
  ensure (← testRoundTrip (0xFFFFFFFF : UInt32)) "max UInt32 should round-trip"

-- UInt64 tests
test "UInt64 roundtrip 0" := do
  ensure (← testRoundTrip (0 : UInt64)) "0 should round-trip"

test "UInt64 roundtrip max" := do
  ensure (← testRoundTrip (0xFFFFFFFFFFFFFFFF : UInt64)) "max UInt64 should round-trip"

-- Int32 tests
test "Int32 roundtrip 0" := do
  ensure (← testRoundTrip (0 : Int32)) "0 should round-trip"

test "Int32 roundtrip pos1" := do
  ensure (← testRoundTrip (1 : Int32)) "1 should round-trip"

test "Int32 roundtrip neg1" := do
  ensure (← testRoundTrip ((-1) : Int32)) "-1 should round-trip"

test "Int32 roundtrip max" := do
  ensure (← testRoundTrip (2147483647 : Int32)) "max Int32 should round-trip"

test "Int32 roundtrip min" := do
  ensure (← testRoundTrip ((-2147483648) : Int32)) "min Int32 should round-trip"

-- Int64 tests
test "Int64 roundtrip 0" := do
  ensure (← testRoundTrip (0 : Int64)) "0 should round-trip"

test "Int64 roundtrip neg1" := do
  ensure (← testRoundTrip ((-1) : Int64)) "-1 should round-trip"

-- Bool tests
test "Bool roundtrip false" := do
  ensure (← testRoundTrip false) "false should round-trip"

test "Bool roundtrip true" := do
  ensure (← testRoundTrip true) "true should round-trip"

-- String tests
test "String roundtrip empty" := do
  ensure (← testRoundTrip "") "empty string should round-trip"

test "String roundtrip hello" := do
  ensure (← testRoundTrip "hello") "hello should round-trip"

test "String roundtrip unicode" := do
  ensure (← testRoundTrip "Hello, 世界!") "unicode string should round-trip"

-- ByteArray tests
test "ByteArray roundtrip empty" := do
  ensure (← testRoundTrip ByteArray.empty) "empty ByteArray should round-trip"

test "ByteArray roundtrip data" := do
  ensure (← testRoundTrip (ByteArray.mk #[1, 2, 3, 4, 5])) "ByteArray should round-trip"

-- Default value detection tests
test "UInt32 zero is default" := do
  ensure (ProtoEncodable.isDefault (0 : UInt32)) "0 should be default"

test "Int32 zero is default" := do
  ensure (ProtoEncodable.isDefault (0 : Int32)) "0 should be default"

test "Bool false is default" := do
  ensure (ProtoEncodable.isDefault false) "false should be default"

test "String empty is default" := do
  ensure (ProtoEncodable.isDefault "") "empty string should be default"

test "ByteArray empty is default" := do
  ensure (ProtoEncodable.isDefault ByteArray.empty) "empty ByteArray should be default"

#generate_tests

end Tests.Scalar
