import Lake
open Lake DSL

package protolean where
  version := v!"0.1.0"

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.3"
require sift from git "https://github.com/nathanial/sift" @ "v0.0.2"

@[default_target]
lean_lib Protolean where
  globs := #[.andSubmodules `Protolean]

lean_lib Tests where
  globs := #[.submodules `Tests]

@[test_driver]
lean_exe tests where
  root := `Tests.Main
