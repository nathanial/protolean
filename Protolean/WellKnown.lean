/-
  Well-known Google Protobuf types.

  This module provides hand-written Lean implementations of Google's
  standard protobuf types (google.protobuf.*). These are pre-registered
  so that code generation can reference them instead of trying to generate
  them from .proto files.
-/
import Std.Data.HashMap
import Protolean.WellKnown.Empty
import Protolean.WellKnown.Timestamp
import Protolean.WellKnown.Duration
import Protolean.WellKnown.Wrappers

namespace Protolean.WellKnown

/-- Registry mapping proto type names to their Lean namespace paths.
    The key is the fully-qualified proto type (e.g., "google.protobuf.Timestamp")
    and the value is the Lean namespace path as a string (e.g., "Google.Protobuf.Timestamp"). -/
def registry : Std.HashMap String String :=
  ({} : Std.HashMap String String)
    -- Priority 1: Core types
    |>.insert "google.protobuf.Empty" "Google.Protobuf.Empty"
    |>.insert "google.protobuf.Timestamp" "Google.Protobuf.Timestamp"
    |>.insert "google.protobuf.Duration" "Google.Protobuf.Duration"
    -- Priority 2: Wrapper types
    |>.insert "google.protobuf.DoubleValue" "Google.Protobuf.DoubleValue"
    |>.insert "google.protobuf.FloatValue" "Google.Protobuf.FloatValue"
    |>.insert "google.protobuf.Int64Value" "Google.Protobuf.Int64Value"
    |>.insert "google.protobuf.UInt64Value" "Google.Protobuf.UInt64Value"
    |>.insert "google.protobuf.Int32Value" "Google.Protobuf.Int32Value"
    |>.insert "google.protobuf.UInt32Value" "Google.Protobuf.UInt32Value"
    |>.insert "google.protobuf.BoolValue" "Google.Protobuf.BoolValue"
    |>.insert "google.protobuf.StringValue" "Google.Protobuf.StringValue"
    |>.insert "google.protobuf.BytesValue" "Google.Protobuf.BytesValue"

/-- Check if a type name is a well-known type -/
def isWellKnownType (protoTypeName : String) : Bool :=
  registry.contains protoTypeName

/-- Get the Lean type name for a well-known proto type -/
def getLeanName (protoTypeName : String) : Option String :=
  registry.get? protoTypeName

/-- Check if a proto import path is for well-known types -/
def isWellKnownImport (importPath : String) : Bool :=
  importPath.startsWith "google/protobuf/"

/-- List of all well-known import paths that we support -/
def supportedImports : List String := [
  "google/protobuf/empty.proto",
  "google/protobuf/timestamp.proto",
  "google/protobuf/duration.proto",
  "google/protobuf/wrappers.proto"
]

/-- Check if a specific import is supported -/
def isSupportedImport (importPath : String) : Bool :=
  supportedImports.contains importPath

end Protolean.WellKnown
