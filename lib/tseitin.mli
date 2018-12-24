open Formule

type tseitinD = int ref * (formule, int) Hashtbl.t

val tseitin :
  tseitinD ->
  Formule.formule ->
  Formule.formule
