open Formule

type tseitinD = int ref * (formule, int) Hashtbl.t

val tseitin :
  tseitinD ->
  Formule.formule ->
  Formule.formule

type formule' = F of formule | B of bool

val et: formule' -> formule' -> formule'
