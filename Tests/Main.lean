/-
  Test runner for Protolean.
-/
import Tests.Varint
import Tests.Scalar
import Tests.Parser
import Tests.Import
import Tests.Service
import Tests.WellKnown

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

  -- Service tests
  IO.println "=== Service Tests ==="
  Tests.Service.runTests
  IO.println ""

  -- Well-known types tests
  IO.println "=== Well-Known Types Tests ==="
  Tests.WellKnown.runTests
  IO.println ""

  IO.println "All tests completed!"
