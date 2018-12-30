(* type littéral *)
type lit = Var of int | Neg of int

(* Type formule *)
type formule =
  | Faux
  | Vrai
  | Lit of lit
  | Et of formule * formule
  | Ou of formule * formule

(* Transfome des littéraux en formule *)
let var x = Lit (Var x)
let non x = Lit (Neg x)

(* et simplifiant les booléens *)
let et a b =
  match a, b with
  | Faux, _ -> Faux
  | Vrai, g -> g
  | _, Faux -> Faux
  | f, Vrai -> f
  | f, g -> Et(f,g)

(* ou simplifiant les booléens *)
let ou a b =
  match a, b with
  | Faux, g -> g
  | Vrai, _ -> Vrai
  | f, Faux -> f
  | _, Vrai -> Vrai
  | f, g -> Ou(f,g)

let rec string_of_formule f =
  match f with
  | Faux -> "Faux"
  | Vrai -> "Vrai"
  | Et (x,y) -> "(" ^ (string_of_formule x) ^ " Et " ^ (string_of_formule y) ^ ")"
  | Ou (x,y) -> "(" ^ (string_of_formule x) ^ " Ou " ^ (string_of_formule y) ^ ")"
  | Lit f ->
     match f with
     | Var x -> string_of_int x
     | Neg x -> "Neg " ^ (string_of_int x)

let print_formule f =
  print_endline (string_of_formule f)

(* Renvoie vrai ssi il x et y sont des Var et si x = y *)
let same_var x y =
  match x, y with
  | Var x, Var y -> x = y
  | _ -> false

let neg lit =
  match lit with
  | Var x -> Neg x
  | Neg x -> Var x

let get_v x =
  match x with
  | Var x -> x
  | Neg x -> x
