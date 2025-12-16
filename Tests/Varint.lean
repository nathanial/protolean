/-
  Unit tests for varint encoding/decoding.
-/
import Protolean.Varint

namespace Tests.Varint

open Protolean.Varint

/-- Test helper to check varint round-trip -/
def testVarintRoundTrip (value : UInt64) : IO Bool := do
  let encoded := encodeUInt64 value
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ =>
    if decoded == value then
      return true
    else
      IO.println s!"  FAIL: varint {value} decoded as {decoded}"
      return false
  | .error e =>
    IO.println s!"  FAIL: varint {value} decode error: {repr e}"
    return false

/-- Test helper to check expected encoding -/
def testVarintEncoding (value : UInt64) (expected : List UInt8) : IO Bool := do
  let encoded := encodeUInt64 value
  let encodedList := encoded.toList
  if encodedList == expected then
    return true
  else
    IO.println s!"  FAIL: varint {value} encoded as {encodedList}, expected {expected}"
    return false

/-- Test ZigZag encoding -/
def testZigZag32 (value : Int32) (expected : UInt32) : IO Bool := do
  let encoded := zigZagEncode32 value
  if encoded == expected then
    let decoded := zigZagDecode32 encoded
    if decoded == value then
      return true
    else
      IO.println s!"  FAIL: zigzag32 {value} roundtrip failed: got {decoded}"
      return false
  else
    IO.println s!"  FAIL: zigzag32 {value} encoded as {encoded}, expected {expected}"
    return false

/-- Run all varint tests -/
def runTests : IO Unit := do
  let mut passed := 0
  let mut failed := 0

  -- Test basic varint encoding
  IO.println "Testing varint encoding..."

  -- 0 encodes as single byte 0x00
  if ← testVarintEncoding 0 [0x00] then passed := passed + 1 else failed := failed + 1

  -- 1 encodes as single byte 0x01
  if ← testVarintEncoding 1 [0x01] then passed := passed + 1 else failed := failed + 1

  -- 127 encodes as single byte 0x7F
  if ← testVarintEncoding 127 [0x7F] then passed := passed + 1 else failed := failed + 1

  -- 128 encodes as 0x80 0x01
  if ← testVarintEncoding 128 [0x80, 0x01] then passed := passed + 1 else failed := failed + 1

  -- 300 encodes as 0xAC 0x02
  if ← testVarintEncoding 300 [0xAC, 0x02] then passed := passed + 1 else failed := failed + 1

  -- 16384 encodes as 0x80 0x80 0x01
  if ← testVarintEncoding 16384 [0x80, 0x80, 0x01] then passed := passed + 1 else failed := failed + 1

  -- Test round-trips
  IO.println "Testing varint round-trips..."

  if ← testVarintRoundTrip 0 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 1 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 127 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 128 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 255 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 256 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 16383 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 16384 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 2097151 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 2097152 then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 0xFFFFFFFF then passed := passed + 1 else failed := failed + 1
  if ← testVarintRoundTrip 0xFFFFFFFFFFFFFFFF then passed := passed + 1 else failed := failed + 1

  -- Test ZigZag encoding
  IO.println "Testing ZigZag encoding..."

  -- ZigZag: 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4, ...
  if ← testZigZag32 0 0 then passed := passed + 1 else failed := failed + 1
  if ← testZigZag32 (-1) 1 then passed := passed + 1 else failed := failed + 1
  if ← testZigZag32 1 2 then passed := passed + 1 else failed := failed + 1
  if ← testZigZag32 (-2) 3 then passed := passed + 1 else failed := failed + 1
  if ← testZigZag32 2 4 then passed := passed + 1 else failed := failed + 1
  if ← testZigZag32 (-2147483648) 0xFFFFFFFF then passed := passed + 1 else failed := failed + 1
  if ← testZigZag32 2147483647 0xFFFFFFFE then passed := passed + 1 else failed := failed + 1

  IO.println s!"Varint tests: {passed} passed, {failed} failed"

end Tests.Varint
