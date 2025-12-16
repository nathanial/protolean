/-
  Import path resolution for proto files.
-/
namespace Protolean.Import

/-- Configuration for import resolution -/
structure ImportConfig where
  /-- List of directories to search for imports -/
  searchPaths : List System.FilePath
  /-- Base directory of the current file -/
  baseDir : System.FilePath
  deriving Repr, Inhabited

/-- Resolve an import path to an absolute file path -/
def resolveImport (config : ImportConfig) (importPath : String) : IO (Option System.FilePath) := do
  -- Try base directory first
  let basePath := config.baseDir / importPath
  if ← basePath.pathExists then
    return some basePath

  -- Try search paths
  for searchPath in config.searchPaths do
    let candidatePath := searchPath / importPath
    if ← candidatePath.pathExists then
      return some candidatePath

  return none

/-- Well-known proto locations (e.g., google/protobuf/*) -/
def wellKnownProtos : List String := [
  "google/protobuf/any.proto",
  "google/protobuf/api.proto",
  "google/protobuf/descriptor.proto",
  "google/protobuf/duration.proto",
  "google/protobuf/empty.proto",
  "google/protobuf/field_mask.proto",
  "google/protobuf/source_context.proto",
  "google/protobuf/struct.proto",
  "google/protobuf/timestamp.proto",
  "google/protobuf/type.proto",
  "google/protobuf/wrappers.proto"
]

/-- Check if an import is a well-known proto -/
def isWellKnown (importPath : String) : Bool :=
  wellKnownProtos.contains importPath

end Protolean.Import
