/-
  Tests for proto_import command elaboration.

  This test verifies that proto_import generates usable Lean types.
-/
import Protolean

namespace Tests.Import

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

-- Test encoding
def testEncode : IO Bool := do
  let bytes := Protolean.encodeMessage testMessage
  if bytes.size > 0 then
    IO.println s!"  PASS: Encoded TestMessage to {bytes.size} bytes"
    return true
  else
    IO.println "  FAIL: Encoding produced empty bytes"
    return false

-- Test round-trip
def testRoundTrip : IO Bool := do
  let bytes := Protolean.encodeMessage testMessage
  match Protolean.decodeMessage bytes with
  | .ok (decoded : TestMessage) =>
    if decoded.name == testMessage.name &&
       decoded.value == testMessage.value &&
       decoded.active == testMessage.active then
      IO.println "  PASS: Round-trip successful"
      return true
    else
      IO.println s!"  FAIL: Decoded values don't match"
      return false
  | .error e =>
    IO.println s!"  FAIL: Decode error: {e}"
    return false

/-- Run all import tests -/
def runTests : IO Unit := do
  let mut passed := 0
  let mut failed := 0

  IO.println "Testing proto_import..."

  IO.println "Testing encoding..."
  if ← testEncode then passed := passed + 1 else failed := failed + 1

  IO.println "Testing round-trip..."
  if ← testRoundTrip then passed := passed + 1 else failed := failed + 1

  IO.println s!"Import tests: {passed} passed, {failed} failed"

end Tests.Import
