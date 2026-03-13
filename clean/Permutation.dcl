definition module Permutation

// Permutation groups on finite sets with Schreier-Sims machinery.

from StdOverloaded import class ==, class <, class toString

:: Perm

// Construction
permId :: !Int -> Perm
fromCycles :: !Int ![[Int]] -> Perm
fromMapping :: ![Int] -> Perm

// Operations
permApply :: !Perm !Int -> Int
permCompose :: !Perm !Perm -> Perm
permInverse :: !Perm -> Perm
permOrder :: !Perm -> Int
permCycles :: !Perm -> [[Int]]
permSign :: !Perm -> Int
permIsId :: !Perm -> Bool
permN :: !Perm -> Int

// Schreier-Sims
:: BSGS

schreierSims :: !Int ![Perm] -> BSGS
groupOrder :: !BSGS -> Int
groupContains :: !BSGS !Perm -> Bool

instance == Perm
instance < Perm
instance toString Perm
