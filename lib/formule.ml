(* type littéral *)
type lit = Var of int | Neg of int

(* Type formule *)
type formule =
  | Lit of lit
  | Et of formule * formule
  | Ou of formule * formule

(* Transfome des littéraux en formule *)
let var x = Lit (Var x)
let non x = Lit (Neg x)

let rec string_of_formule f =
  match f with
  | Et (x,y) -> "(" ^ (string_of_formule x) ^ " Et " ^ (string_of_formule y) ^ ")"
  | Ou (x,y) -> "(" ^ (string_of_formule x) ^ " Ou " ^ (string_of_formule y) ^ ")"
  | Lit f ->
     match f with
     | Var x -> string_of_int x
     | Neg x -> "Neg " ^ (string_of_int x)

let print_formule (f : formule option) =
  match f with
  | None -> print_endline "VRAI"
  | Some f -> print_endline (string_of_formule f)

(* Renvoie vrai ssi il x et y sont des Var et si x = y *)
let same_var x y =
  match x, y with
  | Var x, Var y -> x = y
  | _ -> false

let rec size_of_formule (f : formule) =
  match f with
  | Lit _ -> 1
  | Et (a,b) -> size_of_formule a + size_of_formule b
  | Ou (a,b) -> size_of_formule a + size_of_formule b

let neg lit =
  match lit with
  | Var x -> Neg x
  | Neg x -> Var x
