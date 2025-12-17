/-
  Runtime types for gRPC service metadata and reflection.
-/

namespace Protolean.Service

/-- RPC method kind based on streaming configuration -/
inductive RpcKind where
  | unary         -- Request -> Response
  | clientStream  -- Stream Request -> Response
  | serverStream  -- Request -> Stream Response
  | bidiStream    -- Stream Request -> Stream Response
  deriving Repr, BEq, Inhabited, DecidableEq

namespace RpcKind

def toString : RpcKind → String
  | .unary => "unary"
  | .clientStream => "clientStream"
  | .serverStream => "serverStream"
  | .bidiStream => "bidiStream"

instance : ToString RpcKind := ⟨toString⟩

/-- Determine RPC kind from streaming flags -/
def fromStreaming (inputStream outputStream : Bool) : RpcKind :=
  match inputStream, outputStream with
  | false, false => .unary
  | true, false => .clientStream
  | false, true => .serverStream
  | true, true => .bidiStream

end RpcKind

/-- Runtime metadata for an RPC method -/
structure RpcMethodInfo where
  /-- Method name (e.g., "Search") -/
  name : String
  /-- Fully qualified input type name (e.g., "example.SearchRequest") -/
  inputType : String
  /-- Fully qualified output type name (e.g., "example.SearchResponse") -/
  outputType : String
  /-- Whether the input is a stream -/
  inputStream : Bool := false
  /-- Whether the output is a stream -/
  outputStream : Bool := false
  /-- Method options as key-value pairs -/
  options : List (String × String) := []
  deriving Repr, BEq, Inhabited

namespace RpcMethodInfo

/-- Get the RPC kind for this method -/
def kind (m : RpcMethodInfo) : RpcKind :=
  RpcKind.fromStreaming m.inputStream m.outputStream

/-- Check if this is a unary (non-streaming) RPC -/
def isUnary (m : RpcMethodInfo) : Bool :=
  !m.inputStream && !m.outputStream

/-- Check if this RPC has any streaming -/
def isStreaming (m : RpcMethodInfo) : Bool :=
  m.inputStream || m.outputStream

end RpcMethodInfo

/-- Runtime metadata for a service -/
structure ServiceInfo where
  /-- Service name (e.g., "SearchService") -/
  name : String
  /-- Fully qualified name including package (e.g., "example.SearchService") -/
  fullName : String
  /-- List of RPC methods in this service -/
  methods : List RpcMethodInfo := []
  /-- Service-level options as key-value pairs -/
  options : List (String × String) := []
  deriving Repr, BEq, Inhabited

namespace ServiceInfo

/-- Find a method by name -/
def findMethod (s : ServiceInfo) (name : String) : Option RpcMethodInfo :=
  s.methods.find? (·.name == name)

/-- Get all unary methods -/
def unaryMethods (s : ServiceInfo) : List RpcMethodInfo :=
  s.methods.filter (·.isUnary)

/-- Get all streaming methods -/
def streamingMethods (s : ServiceInfo) : List RpcMethodInfo :=
  s.methods.filter (·.isStreaming)

end ServiceInfo

/-- Type class for types that have associated service metadata -/
class HasServiceInfo (α : Type) where
  serviceInfo : ServiceInfo

end Protolean.Service
