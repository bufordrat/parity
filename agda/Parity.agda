open import Data.Nat.Base

data Parity : Set where
  odd : Parity
  even : Parity

data Number (P : Parity) : Set where
  zero : Number P
  osucc : Number P -> Number P
  esucc : Number P -> Number P

_+N_ : {P : Parity} -> Number P -> Number P -> Number P
zero +N zero = zero
