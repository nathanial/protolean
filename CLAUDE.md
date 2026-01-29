# CLAUDE.md

Protocol Buffers implementation for Lean 4.

## Build Commands

```bash
lake build    # Build the library
lake test     # Run tests
```

## Dependencies

- crucible (testing)
- sift (parser combinators)

## Project Structure

```
Protolean/
├── WireFormat.lean      # Wire types, field tags
├── Varint.lean          # Varint encoding
├── ByteArray/           # ByteArray utilities
├── Encoder.lean         # Encoder monad
├── Decoder.lean         # Decoder monad
├── Codec.lean           # ProtoEncodable/ProtoDecodable type classes
├── Scalar.lean          # Scalar type instances
├── Repeated.lean        # Repeated fields (arrays)
├── Map.lean             # Map fields
├── Message.lean         # ProtoMessage class
├── Syntax/              # Proto3 AST
├── Parser/              # Proto3 parser (Lexer, Proto)
├── Import/              # Import resolution (Resolver, Loader)
├── Service/             # gRPC service types
├── WellKnown.lean       # google.protobuf.* types
└── Codegen/             # Code generation (Names, Types, Encode, Decode, Service, Import)
```

## Key Concepts

### proto_import Command

Generates Lean types from `.proto` files at compile time:

```lean
import Protolean
proto_import "example.proto"
```

### Type Classes

- `ProtoEncodable α` - Types that can be encoded (wireType, encode, isDefault)
- `ProtoDecodable α` - Types that can be decoded (wireType, decode, defaultValue)
- `ProtoCodec α` - Combined encode/decode
- `ProtoMessage α` - Complete messages (encodeFields, decodeField, defaultValue)

### Encoding/Decoding API

```lean
Protolean.encodeMessage [ProtoMessage α] (msg : α) : ByteArray
Protolean.decodeMessage [ProtoMessage α] (bytes : ByteArray) : Except DecodeError α
```

## Supported Proto Types

| Proto Type | Lean Type | Wire Type |
|------------|-----------|-----------|
| int32/int64 | Int32/Int64 | varint |
| uint32/uint64 | UInt32/UInt64 | varint |
| sint32/sint64 | Int32/Int64 | varint (zigzag) |
| fixed32/fixed64 | UInt32/UInt64 | fixed |
| sfixed32/sfixed64 | Int32/Int64 | fixed |
| float/double | Float | fixed32/fixed64 |
| bool | Bool | varint |
| string | String | length-delimited |
| bytes | ByteArray | length-delimited |
| repeated T | Array T | varies |
| message | Structure | length-delimited |
