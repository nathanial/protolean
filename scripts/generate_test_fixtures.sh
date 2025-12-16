#!/bin/bash
# Generate test fixtures using protoc for cross-validation testing.
#
# Prerequisites:
#   - protoc installed (brew install protobuf)
#   - Python 3 with protobuf library (pip install protobuf)
#
# This script generates:
#   1. Python protobuf bindings from .proto files
#   2. Binary encoded test data for known inputs
#   3. Expected decoded outputs

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
FIXTURES_DIR="$PROJECT_DIR/Tests/Integration/fixtures"
OUTPUT_DIR="$FIXTURES_DIR/generated"

mkdir -p "$OUTPUT_DIR"

echo "Generating Python bindings from proto files..."
protoc \
  --proto_path="$FIXTURES_DIR" \
  --python_out="$OUTPUT_DIR" \
  "$FIXTURES_DIR/test.proto"

echo "Generating test data..."
python3 << 'EOF'
import sys
sys.path.insert(0, 'Tests/Integration/fixtures/generated')

try:
    import test_pb2
except ImportError:
    print("Error: Could not import test_pb2. Run this script from the project root.")
    sys.exit(1)

import os

output_dir = "Tests/Integration/fixtures/generated"

def write_binary(filename, message):
    """Write a protobuf message to a binary file."""
    path = os.path.join(output_dir, filename)
    with open(path, 'wb') as f:
        f.write(message.SerializeToString())
    print(f"  Generated: {filename} ({len(message.SerializeToString())} bytes)")

# Simple message tests
print("Generating SimpleMessage test cases...")
simple1 = test_pb2.SimpleMessage(name="Alice", id=123, active=True)
write_binary("simple_alice.bin", simple1)

simple2 = test_pb2.SimpleMessage(name="", id=0, active=False)  # All defaults
write_binary("simple_defaults.bin", simple2)

simple3 = test_pb2.SimpleMessage(name="Hello, 世界!", id=-1, active=True)
write_binary("simple_unicode.bin", simple3)

# All scalars
print("Generating AllScalars test cases...")
scalars = test_pb2.AllScalars(
    double_val=3.14159265359,
    float_val=2.71828,
    int32_val=-42,
    int64_val=-9223372036854775808,
    uint32_val=4294967295,
    uint64_val=18446744073709551615,
    sint32_val=-2147483648,
    sint64_val=-9223372036854775808,
    fixed32_val=12345678,
    fixed64_val=123456789012345678,
    sfixed32_val=-12345678,
    sfixed64_val=-123456789012345678,
    bool_val=True,
    string_val="test string",
    bytes_val=b"\x00\x01\x02\xff\xfe"
)
write_binary("all_scalars.bin", scalars)

# Repeated fields
print("Generating RepeatedFields test cases...")
repeated = test_pb2.RepeatedFields(
    names=["Alice", "Bob", "Charlie"],
    numbers=[1, 2, 3, 100, -100],
    flags=[True, False, True, True]
)
write_binary("repeated.bin", repeated)

# Enum
print("Generating WithEnum test cases...")
with_enum = test_pb2.WithEnum(
    status=test_pb2.STATUS_ACTIVE,
    description="Currently active"
)
write_binary("with_enum.bin", with_enum)

# Nested
print("Generating Outer test cases...")
outer = test_pb2.Outer(
    name="outer name",
    inner=test_pb2.Outer.Inner(value="inner value", count=42)
)
write_binary("nested.bin", outer)

# Oneof
print("Generating WithOneof test cases...")
oneof_text = test_pb2.WithOneof(id="msg1", text="hello")
write_binary("oneof_text.bin", oneof_text)

oneof_number = test_pb2.WithOneof(id="msg2", number=12345)
write_binary("oneof_number.bin", oneof_number)

# Map
print("Generating WithMap test cases...")
with_map = test_pb2.WithMap(
    scores={"Alice": 100, "Bob": 85, "Charlie": 92},
    labels={1: "one", 2: "two", 3: "three"}
)
write_binary("with_map.bin", with_map)

# Complex
print("Generating ComplexMessage test cases...")
complex_msg = test_pb2.ComplexMessage(
    id="complex-123",
    tags=["important", "urgent", "review"],
    status=test_pb2.STATUS_PENDING,
    details=test_pb2.Outer.Inner(value="detail", count=5),
    metadata={"author": "Alice", "version": "1.0"},
    text_payload="This is the payload text"
)
write_binary("complex.bin", complex_msg)

print("\nAll test fixtures generated successfully!")
EOF

echo ""
echo "Test fixtures generated in: $OUTPUT_DIR"
echo ""
echo "To use these for cross-validation:"
echo "1. Read the .bin files in Lean tests"
echo "2. Decode using our Protolean decoder"
echo "3. Compare the decoded values with expected values"
