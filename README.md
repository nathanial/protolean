# Protolean

Protocol Buffers for Lean 4.

Protolean is a complete Protocol Buffers implementation for Lean 4 featuring:

- Proto3 parser
- Binary wire format encoder/decoder
- Compile-time `proto_import` command (no separate codegen step)
- Full support for scalar types, repeated fields, and nested messages

## Installation

Add Protolean to your `lakefile.lean`:

```lean
require protolean from git "https://github.com/nathanial/protolean" @ "master"
```

## Quick Start

### Using proto_import

The easiest way to use Protolean is with the `proto_import` command, which generates Lean types from `.proto` files at compile time:

```protobuf
// example.proto
syntax = "proto3";

message Person {
  string name = 1;
  int32 age = 2;
  repeated string emails = 3;
}
```

```lean
import Protolean

-- Import at compile time - generates Person type and ProtoMessage instance
proto_import "example.proto"

def main : IO Unit := do
  -- Create a message
  let person : Person := {
    name := "Alice"
    age := 30
    emails := #["alice@example.com"]
  }

  -- Encode to bytes
  let bytes := Protolean.encodeMessage person
  IO.println s!"Encoded {bytes.size} bytes"

  -- Decode from bytes
  match Protolean.decodeMessage bytes with
  | .ok decoded => IO.println s!"Decoded: {repr decoded}"
  | .error e => IO.println s!"Error: {e}"
```

### Manual Type Definition

You can also define protobuf-compatible types manually:

```lean
import Protolean

structure MyMessage where
  name : String := ""
  value : Int32 := 0
  deriving Repr, BEq, Inhabited

instance : Protolean.ProtoMessage MyMessage where
  encodeFields msg := do
    Protolean.encodeField 1 msg.name
    Protolean.encodeField 2 msg.value
  decodeField msg tag := match tag with
    | ⟨1, .lengthDelimited⟩ => do
      let v ← Protolean.Decoder.readString
      pure { msg with name := v }
    | ⟨2, .varint⟩ => do
      let v ← Protolean.Decoder.readInt32
      pure { msg with value := v }
    | _ => do
      Protolean.Decoder.skipField tag.wireType
      pure msg
  defaultValue := default
```

## Supported Types

| Proto Type | Lean Type | Wire Type |
|------------|-----------|-----------|
| double | Float | fixed64 |
| float | Float | fixed32 |
| int32 | Int32 | varint |
| int64 | Int64 | varint |
| uint32 | UInt32 | varint |
| uint64 | UInt64 | varint |
| sint32 | Int32 | varint (zigzag) |
| sint64 | Int64 | varint (zigzag) |
| fixed32 | UInt32 | fixed32 |
| fixed64 | UInt64 | fixed64 |
| sfixed32 | Int32 | fixed32 |
| sfixed64 | Int64 | fixed64 |
| bool | Bool | varint |
| string | String | length-delimited |
| bytes | ByteArray | length-delimited |
| repeated T | Array T | varies |
| message | Structure | length-delimited |

## API Reference

### Encoding

```lean
-- Encode a message to bytes
def encodeMessage [ProtoMessage α] (msg : α) : ByteArray

-- Encode a single field (skips if default value)
def encodeField [ProtoEncodable α] (fieldNum : FieldNumber) (value : α) : Encoder Unit

-- Encode a field always (even if default value)
def encodeFieldAlways [ProtoEncodable α] (fieldNum : FieldNumber) (value : α) : Encoder Unit
```

### Decoding

```lean
-- Decode a message from bytes
def decodeMessage [ProtoMessage α] (bytes : ByteArray) : Except DecodeError α
```

### Type Classes

```lean
-- For types that can be encoded
class ProtoEncodable (α : Type) where
  wireType : WireType
  encode : α → Encoder Unit
  isDefault : α → Bool

-- For types that can be decoded
class ProtoDecodable (α : Type) where
  wireType : WireType
  decode : Decoder α
  defaultValue : α

-- Combined encode/decode
class ProtoCodec (α : Type) extends ProtoEncodable α, ProtoDecodable α

-- For complete messages
class ProtoMessage (α : Type) where
  encodeFields : α → Encoder Unit
  decodeField : α → FieldTag → Decoder α
  defaultValue : α
```

## Building

```bash
# Build the library
lake build

# Run tests
lake test
```

## Project Structure

```
protolean/
├── Protolean.lean              # Root module
├── Protolean/
│   ├── WireFormat.lean         # Wire types, field tags
│   ├── Varint.lean             # Varint encoding
│   ├── ByteArray/              # ByteArray utilities
│   ├── Encoder.lean            # Encoder monad
│   ├── Decoder.lean            # Decoder monad
│   ├── Codec.lean              # Type classes
│   ├── Scalar.lean             # Scalar type instances
│   ├── Repeated.lean           # Repeated fields
│   ├── Map.lean                # Map fields
│   ├── Message.lean            # ProtoMessage class
│   ├── Syntax/                 # Proto3 AST
│   ├── Parser/                 # Proto3 parser
│   └── Codegen/                # Code generation
└── Tests/                      # Test suite
```

## License

MIT License - see [LICENSE](LICENSE) for details.
