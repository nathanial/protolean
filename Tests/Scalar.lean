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
  (← testRoundTrip (0 : UInt32)) ≡ true

test "UInt32 roundtrip 1" := do
  (← testRoundTrip (1 : UInt32)) ≡ true

test "UInt32 roundtrip 127" := do
  (← testRoundTrip (127 : UInt32)) ≡ true

test "UInt32 roundtrip 128" := do
  (← testRoundTrip (128 : UInt32)) ≡ true

test "UInt32 roundtrip max" := do
  (← testRoundTrip (0xFFFFFFFF : UInt32)) ≡ true

-- UInt64 tests
test "UInt64 roundtrip 0" := do
  (← testRoundTrip (0 : UInt64)) ≡ true

test "UInt64 roundtrip max" := do
  (← testRoundTrip (0xFFFFFFFFFFFFFFFF : UInt64)) ≡ true

-- Int32 tests
test "Int32 roundtrip 0" := do
  (← testRoundTrip (0 : Int32)) ≡ true

test "Int32 roundtrip pos1" := do
  (← testRoundTrip (1 : Int32)) ≡ true

test "Int32 roundtrip neg1" := do
  (← testRoundTrip ((-1) : Int32)) ≡ true

test "Int32 roundtrip max" := do
  (← testRoundTrip (2147483647 : Int32)) ≡ true

test "Int32 roundtrip min" := do
  (← testRoundTrip ((-2147483648) : Int32)) ≡ true

-- Int64 tests
test "Int64 roundtrip 0" := do
  (← testRoundTrip (0 : Int64)) ≡ true

test "Int64 roundtrip neg1" := do
  (← testRoundTrip ((-1) : Int64)) ≡ true

-- Bool tests
test "Bool roundtrip false" := do
  (← testRoundTrip false) ≡ true

test "Bool roundtrip true" := do
  (← testRoundTrip true) ≡ true

-- String tests
test "String roundtrip empty" := do
  (← testRoundTrip "") ≡ true

test "String roundtrip hello" := do
  (← testRoundTrip "hello") ≡ true

test "String roundtrip unicode" := do
  (← testRoundTrip "Hello, 世界!") ≡ true

-- ByteArray tests
test "ByteArray roundtrip empty" := do
  (← testRoundTrip ByteArray.empty) ≡ true

test "ByteArray roundtrip data" := do
  (← testRoundTrip (ByteArray.mk #[1, 2, 3, 4, 5])) ≡ true

-- Default value detection tests
test "UInt32 zero is default" := do
  (ProtoEncodable.isDefault (0 : UInt32)) ≡ true

test "Int32 zero is default" := do
  (ProtoEncodable.isDefault (0 : Int32)) ≡ true

test "Bool false is default" := do
  (ProtoEncodable.isDefault false) ≡ true

test "String empty is default" := do
  (ProtoEncodable.isDefault "") ≡ true

test "ByteArray empty is default" := do
  (ProtoEncodable.isDefault ByteArray.empty) ≡ true



end Tests.Scalar
