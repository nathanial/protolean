/-
  Protolean: Protocol Buffers for Lean 4

  A complete Protocol Buffers implementation with:
  - Proto3 parser
  - Binary wire format encoder/decoder
  - Compile-time macro for importing .proto files

  Usage:
    import Protolean

    -- Import a proto file at compile time
    proto_import "path/to/file.proto"

    -- The generated types can be used directly
    def main : IO Unit := do
      let msg : MyMessage := { field1 := "hello", field2 := 42 }
      let bytes := Protolean.encodeMessage msg
      match Protolean.decodeMessage bytes with
      | .ok decoded => IO.println s!"Decoded: {repr decoded}"
      | .error e => IO.println s!"Error: {e}"
-/

-- Core wire format
import Protolean.WireFormat
import Protolean.Varint
import Protolean.ByteArray.Basic
import Protolean.ByteArray.Builder

-- Encoder/Decoder monads
import Protolean.Encoder
import Protolean.Decoder

-- Type classes and instances
import Protolean.Codec
import Protolean.Scalar
import Protolean.Repeated
import Protolean.Map
import Protolean.Message

-- Parser
import Protolean.Syntax.Position
import Protolean.Syntax.AST
import Protolean.Parser.Lexer
import Protolean.Parser.Proto

-- Import resolution
import Protolean.Import.Resolver
import Protolean.Import.Loader

-- Service types
import Protolean.Service.Types

-- Well-known types (google.protobuf.*)
import Protolean.WellKnown

-- Code generation
import Protolean.Codegen.Names
import Protolean.Codegen.Types
import Protolean.Codegen.Encode
import Protolean.Codegen.Decode
import Protolean.Codegen.Service
import Protolean.Codegen.Import
