type lit = Var of int | Neg of int
type formule = Lit of lit | Et of formule * formule | Ou of formule * formule
val var : int -> formule
val non : int -> formule
val string_of_formule : formule -> string
val print_formule : formule option -> unit
val same_var : lit -> lit -> bool
val size_of_formule : formule -> int
val neg : lit -> lit
