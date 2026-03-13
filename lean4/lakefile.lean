import Lake
open Lake DSL

package surd where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

@[default_target]
lean_lib Surd where
  roots := #[`Surd]

lean_exe «surd-trig-table» where
  root := `Demo.TrigTable
  supportInterpreter := true

lean_exe «surd-euler-integral» where
  root := `Demo.EulerIntegral
  supportInterpreter := true

lean_exe «surd-elliptic-integral» where
  root := `Demo.EllipticIntegral
  supportInterpreter := true

lean_exe «surd-solvable-quintic» where
  root := `Demo.SolvableQuintic
  supportInterpreter := true
