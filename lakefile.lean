import Lake
open Lake DSL

package protolean where
  version := v!"0.1.0"

require crucible from ".." / "crucible"

@[default_target]
lean_lib Protolean where
  globs := #[.andSubmodules `Protolean]

lean_lib Tests where
  globs := #[.submodules `Tests]

@[test_driver]
lean_exe tests where
  root := `Tests.Main
