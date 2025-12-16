/-
  Test runner for Protolean.
-/
import Tests.Varint
import Tests.Scalar
import Tests.Parser
import Tests.Import

def main : IO Unit := do
  IO.println "Running Protolean tests..."
  IO.println ""

  -- Varint tests
  IO.println "=== Varint Tests ==="
  Tests.Varint.runTests
  IO.println ""

  -- Scalar tests
  IO.println "=== Scalar Tests ==="
  Tests.Scalar.runTests
  IO.println ""

  -- Parser tests
  IO.println "=== Parser Tests ==="
  Tests.Parser.runTests
  IO.println ""

  -- Import tests
  IO.println "=== Import Tests ==="
  Tests.Import.runTests
  IO.println ""

  IO.println "All tests completed!"
