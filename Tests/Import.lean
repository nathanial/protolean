/-
  Tests for proto_import command elaboration.

  This test verifies that proto_import generates usable Lean types.
-/
import Crucible
import Protolean

namespace Tests.Import

open Crucible

-- Import the test proto file - this should generate TestMessage and Person types
proto_import "test.proto"

-- Test that we can create instances of generated types
def testMessage : TestMessage := {
  name := "test"
  value := 42
  active := true
}

def testPerson : Person := {
  firstName := "Alice"
  lastName := "Smith"
  age := 30
  emails := #["alice@example.com", "alice@work.com"]
}

testSuite "Import Tests"

test "encode TestMessage produces bytes" := do
  let bytes := Protolean.encodeMessage testMessage
  (bytes.size != 0) ≡ true

test "TestMessage roundtrip" := do
  let bytes := Protolean.encodeMessage testMessage
  match Protolean.decodeMessage bytes with
  | .ok (decoded : TestMessage) => do
    decoded.name ≡ testMessage.name
    decoded.value ≡ testMessage.value
    decoded.active ≡ testMessage.active
  | .error _ => ensure false "Decode failed"



end Tests.Import
