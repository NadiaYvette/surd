import Lake
open Lake DSL

package surd where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

@[default_target]
lean_lib Surd where
  roots := #[`Surd]
