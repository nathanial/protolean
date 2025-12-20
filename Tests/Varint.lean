/-
  Unit tests for varint encoding/decoding.
-/
import Crucible
import Protolean.Varint

namespace Tests.Varint

open Crucible
open Protolean.Varint

testSuite "Varint Tests"

-- Varint encoding tests
test "varint 0 encodes as single byte 0x00" := do
  let encoded := encodeUInt64 0
  ensure (encoded.toList == [0x00]) "0 should encode as [0x00]"

test "varint 1 encodes as single byte 0x01" := do
  let encoded := encodeUInt64 1
  ensure (encoded.toList == [0x01]) "1 should encode as [0x01]"

test "varint 127 encodes as single byte 0x7F" := do
  let encoded := encodeUInt64 127
  ensure (encoded.toList == [0x7F]) "127 should encode as [0x7F]"

test "varint 128 encodes as 0x80 0x01" := do
  let encoded := encodeUInt64 128
  ensure (encoded.toList == [0x80, 0x01]) "128 should encode as [0x80, 0x01]"

test "varint 300 encodes as 0xAC 0x02" := do
  let encoded := encodeUInt64 300
  ensure (encoded.toList == [0xAC, 0x02]) "300 should encode as [0xAC, 0x02]"

test "varint 16384 encodes as 0x80 0x80 0x01" := do
  let encoded := encodeUInt64 16384
  ensure (encoded.toList == [0x80, 0x80, 0x01]) "16384 should encode as [0x80, 0x80, 0x01]"

-- Varint round-trip tests
test "varint round-trip for 0" := do
  let encoded := encodeUInt64 0
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 0) "decoded should be 0"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 1" := do
  let encoded := encodeUInt64 1
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 1) "decoded should be 1"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 127" := do
  let encoded := encodeUInt64 127
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 127) "decoded should be 127"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 128" := do
  let encoded := encodeUInt64 128
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 128) "decoded should be 128"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 255" := do
  let encoded := encodeUInt64 255
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 255) "decoded should be 255"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 256" := do
  let encoded := encodeUInt64 256
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 256) "decoded should be 256"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 16383" := do
  let encoded := encodeUInt64 16383
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 16383) "decoded should be 16383"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 16384" := do
  let encoded := encodeUInt64 16384
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 16384) "decoded should be 16384"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 2097151" := do
  let encoded := encodeUInt64 2097151
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 2097151) "decoded should be 2097151"
  | .error _ => ensure false "decode failed"

test "varint round-trip for 2097152" := do
  let encoded := encodeUInt64 2097152
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 2097152) "decoded should be 2097152"
  | .error _ => ensure false "decode failed"

test "varint round-trip for max u32" := do
  let encoded := encodeUInt64 0xFFFFFFFF
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 0xFFFFFFFF) "decoded should be 0xFFFFFFFF"
  | .error _ => ensure false "decode failed"

test "varint round-trip for max u64" := do
  let encoded := encodeUInt64 0xFFFFFFFFFFFFFFFF
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => ensure (decoded == 0xFFFFFFFFFFFFFFFF) "decoded should be max UInt64"
  | .error _ => ensure false "decode failed"

-- ZigZag encoding tests
test "zigzag32 for 0 encodes as 0" := do
  let encoded := zigZagEncode32 0
  ensure (encoded == 0) "zigzag(0) should be 0"
  let decoded := zigZagDecode32 encoded
  ensure (decoded == 0) "roundtrip should work"

test "zigzag32 for neg1 encodes as 1" := do
  let encoded := zigZagEncode32 (-1)
  ensure (encoded == 1) "zigzag(-1) should be 1"
  let decoded := zigZagDecode32 encoded
  ensure (decoded == -1) "roundtrip should work"

test "zigzag32 for 1 encodes as 2" := do
  let encoded := zigZagEncode32 1
  ensure (encoded == 2) "zigzag(1) should be 2"
  let decoded := zigZagDecode32 encoded
  ensure (decoded == 1) "roundtrip should work"

test "zigzag32 for neg2 encodes as 3" := do
  let encoded := zigZagEncode32 (-2)
  ensure (encoded == 3) "zigzag(-2) should be 3"
  let decoded := zigZagDecode32 encoded
  ensure (decoded == -2) "roundtrip should work"

test "zigzag32 for 2 encodes as 4" := do
  let encoded := zigZagEncode32 2
  ensure (encoded == 4) "zigzag(2) should be 4"
  let decoded := zigZagDecode32 encoded
  ensure (decoded == 2) "roundtrip should work"

test "zigzag32 for min i32 encodes correctly" := do
  let encoded := zigZagEncode32 (-2147483648)
  ensure (encoded == 0xFFFFFFFF) "zigzag(Int32.min) should be 0xFFFFFFFF"
  let decoded := zigZagDecode32 encoded
  ensure (decoded == -2147483648) "roundtrip should work"

test "zigzag32 for max i32 encodes correctly" := do
  let encoded := zigZagEncode32 2147483647
  ensure (encoded == 0xFFFFFFFE) "zigzag(Int32.max) should be 0xFFFFFFFE"
  let decoded := zigZagDecode32 encoded
  ensure (decoded == 2147483647) "roundtrip should work"

#generate_tests

end Tests.Varint
