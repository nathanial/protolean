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
  encoded.toList ≡ [0x00]

test "varint 1 encodes as single byte 0x01" := do
  let encoded := encodeUInt64 1
  encoded.toList ≡ [0x01]

test "varint 127 encodes as single byte 0x7F" := do
  let encoded := encodeUInt64 127
  encoded.toList ≡ [0x7F]

test "varint 128 encodes as 0x80 0x01" := do
  let encoded := encodeUInt64 128
  encoded.toList ≡ [0x80, 0x01]

test "varint 300 encodes as 0xAC 0x02" := do
  let encoded := encodeUInt64 300
  encoded.toList ≡ [0xAC, 0x02]

test "varint 16384 encodes as 0x80 0x80 0x01" := do
  let encoded := encodeUInt64 16384
  encoded.toList ≡ [0x80, 0x80, 0x01]

-- Varint round-trip tests
test "varint round-trip for 0" := do
  let encoded := encodeUInt64 0
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 0
  | .error _ => ensure false "decode failed"

test "varint round-trip for 1" := do
  let encoded := encodeUInt64 1
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 1
  | .error _ => ensure false "decode failed"

test "varint round-trip for 127" := do
  let encoded := encodeUInt64 127
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 127
  | .error _ => ensure false "decode failed"

test "varint round-trip for 128" := do
  let encoded := encodeUInt64 128
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 128
  | .error _ => ensure false "decode failed"

test "varint round-trip for 255" := do
  let encoded := encodeUInt64 255
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 255
  | .error _ => ensure false "decode failed"

test "varint round-trip for 256" := do
  let encoded := encodeUInt64 256
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 256
  | .error _ => ensure false "decode failed"

test "varint round-trip for 16383" := do
  let encoded := encodeUInt64 16383
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 16383
  | .error _ => ensure false "decode failed"

test "varint round-trip for 16384" := do
  let encoded := encodeUInt64 16384
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 16384
  | .error _ => ensure false "decode failed"

test "varint round-trip for 2097151" := do
  let encoded := encodeUInt64 2097151
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 2097151
  | .error _ => ensure false "decode failed"

test "varint round-trip for 2097152" := do
  let encoded := encodeUInt64 2097152
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 2097152
  | .error _ => ensure false "decode failed"

test "varint round-trip for max u32" := do
  let encoded := encodeUInt64 0xFFFFFFFF
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 0xFFFFFFFF
  | .error _ => ensure false "decode failed"

test "varint round-trip for max u64" := do
  let encoded := encodeUInt64 0xFFFFFFFFFFFFFFFF
  match decodeUInt64 encoded with
  | .ok ⟨decoded, _⟩ => decoded ≡ 0xFFFFFFFFFFFFFFFF
  | .error _ => ensure false "decode failed"

-- ZigZag encoding tests
test "zigzag32 for 0 encodes as 0" := do
  let encoded := zigZagEncode32 0
  encoded ≡ 0
  let decoded := zigZagDecode32 encoded
  decoded ≡ 0

test "zigzag32 for neg1 encodes as 1" := do
  let encoded := zigZagEncode32 (-1)
  encoded ≡ 1
  let decoded := zigZagDecode32 encoded
  decoded ≡ -1

test "zigzag32 for 1 encodes as 2" := do
  let encoded := zigZagEncode32 1
  encoded ≡ 2
  let decoded := zigZagDecode32 encoded
  decoded ≡ 1

test "zigzag32 for neg2 encodes as 3" := do
  let encoded := zigZagEncode32 (-2)
  encoded ≡ 3
  let decoded := zigZagDecode32 encoded
  decoded ≡ -2

test "zigzag32 for 2 encodes as 4" := do
  let encoded := zigZagEncode32 2
  encoded ≡ 4
  let decoded := zigZagDecode32 encoded
  decoded ≡ 2

test "zigzag32 for min i32 encodes correctly" := do
  let encoded := zigZagEncode32 (-2147483648)
  encoded ≡ 0xFFFFFFFF
  let decoded := zigZagDecode32 encoded
  decoded ≡ -2147483648

test "zigzag32 for max i32 encodes correctly" := do
  let encoded := zigZagEncode32 2147483647
  encoded ≡ 0xFFFFFFFE
  let decoded := zigZagDecode32 encoded
  decoded ≡ 2147483647



end Tests.Varint
