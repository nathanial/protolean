/-
  Unit tests for scalar type encoding/decoding.
-/
import Protolean

namespace Tests.Scalar

open Protolean

/-- Repr instance for ByteArray for test output -/
instance : Repr ByteArray where
  reprPrec ba _ := s!"ByteArray.mk #[{", ".intercalate (ba.toList.map toString)}]"

/-- Test helper to check encode/decode round-trip for a type -/
def testRoundTrip [ProtoCodec α] [BEq α] [Repr α] (name : String) (value : α) : IO Bool := do
  let encoded := Encoder.execute (ProtoEncodable.encode value)
  match Decoder.execute ProtoDecodable.decode encoded with
  | .ok decoded =>
    if decoded == value then
      return true
    else
      IO.println s!"  FAIL: {name} {repr value} decoded as {repr decoded}"
      return false
  | .error e =>
    IO.println s!"  FAIL: {name} {repr value} decode error: {e}"
    return false

/-- Test that default values produce empty encoding -/
def testDefaultEmpty [ProtoEncodable α] [Repr α] (name : String) (value : α) : IO Bool := do
  if ProtoEncodable.isDefault value then
    return true
  else
    IO.println s!"  FAIL: {name} {repr value} should be default but isDefault returned false"
    return false

/-- Run all scalar tests -/
def runTests : IO Unit := do
  let mut passed := 0
  let mut failed := 0

  -- Test UInt32
  IO.println "Testing UInt32..."
  if ← testRoundTrip "UInt32" (0 : UInt32) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "UInt32" (1 : UInt32) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "UInt32" (127 : UInt32) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "UInt32" (128 : UInt32) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "UInt32" (0xFFFFFFFF : UInt32) then passed := passed + 1 else failed := failed + 1

  -- Test UInt64
  IO.println "Testing UInt64..."
  if ← testRoundTrip "UInt64" (0 : UInt64) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "UInt64" (0xFFFFFFFFFFFFFFFF : UInt64) then passed := passed + 1 else failed := failed + 1

  -- Test Int32
  IO.println "Testing Int32..."
  if ← testRoundTrip "Int32" (0 : Int32) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "Int32" (1 : Int32) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "Int32" (-1 : Int32) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "Int32" (2147483647 : Int32) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "Int32" (-2147483648 : Int32) then passed := passed + 1 else failed := failed + 1

  -- Test Int64
  IO.println "Testing Int64..."
  if ← testRoundTrip "Int64" (0 : Int64) then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "Int64" (-1 : Int64) then passed := passed + 1 else failed := failed + 1

  -- Test Bool
  IO.println "Testing Bool..."
  if ← testRoundTrip "Bool" false then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "Bool" true then passed := passed + 1 else failed := failed + 1

  -- Test String
  IO.println "Testing String..."
  if ← testRoundTrip "String" "" then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "String" "hello" then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "String" "Hello, 世界!" then passed := passed + 1 else failed := failed + 1

  -- Test ByteArray
  IO.println "Testing ByteArray..."
  if ← testRoundTrip "ByteArray" ByteArray.empty then passed := passed + 1 else failed := failed + 1
  if ← testRoundTrip "ByteArray" (ByteArray.mk #[1, 2, 3, 4, 5]) then passed := passed + 1 else failed := failed + 1

  -- Test default values are detected
  IO.println "Testing default value detection..."
  if ← testDefaultEmpty "UInt32" (0 : UInt32) then passed := passed + 1 else failed := failed + 1
  if ← testDefaultEmpty "Int32" (0 : Int32) then passed := passed + 1 else failed := failed + 1
  if ← testDefaultEmpty "Bool" false then passed := passed + 1 else failed := failed + 1
  if ← testDefaultEmpty "String" "" then passed := passed + 1 else failed := failed + 1
  if ← testDefaultEmpty "ByteArray" ByteArray.empty then passed := passed + 1 else failed := failed + 1

  IO.println s!"Scalar tests: {passed} passed, {failed} failed"

end Tests.Scalar
