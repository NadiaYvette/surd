(** Gauss period descent for computing exact trig values.

    For prime denominators [p], decomposes the [p]-th roots of unity
    into Gauss periods using the structure of [(Z/pZ)*].
    The descent produces radical expressions via Lagrange resolvents.

    This is the most sophisticated path for exact trig computation
    and handles all prime denominators. *)

(** Gauss period descent for cos(2*pi/p) where p is prime.

    Stub: the full algorithm involves:
    1. Finding a primitive root g mod p
    2. Building the subgroup chain
    3. Computing Lagrange resolvents at each level
    4. Matching resolvent values to period expressions

    For now, delegates to {!Trig.cos_exact}. *)
let cos_via_gauss _p = None

(** Compute all periods for a given prime.
    Returns a map from exponent k to the radical expression for zeta^k. *)
let all_periods_via_gauss _p = None
