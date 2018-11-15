open Utils
open Bsp
open Formule
open Generate_formule

module Variables = struct
  type t = int
  let compare x y = compare x y
end

module Sat = Sat_solver.Make(Variables)

(* Renvoie une liste de formule correspondant à une découpe de toute les clauses
   disjonctives de la fnc*)
let rec list_of_fnc (fnc : formule) =
  let get_var f =
    match f with
    | Var x -> (false,x)
    | Neg x -> (true,x)
  in
  let rec get_ou f =
    match f with
    | Et _ -> failwith "Was not in fnc"
    | Lit x -> [get_var x]
    | Ou (a,b) -> get_ou a @ get_ou b
  in
  match fnc with
  | Et (a,b) ->  list_of_fnc a @ list_of_fnc b
  | Ou _  -> [get_ou fnc]
  | Lit x -> [[get_var x]]

(* Affiche une solution donnée par le sat solver *)
let print_possible_sol solution =
  let rec pr_rec t =
    match t with
    | [] -> ()
    | x::xs ->
       print_string (("("^string_of_bool (fst x)) ^ ", " ^string_of_int (snd x)^ ") ");
       pr_rec xs in
  match solution with
  | None -> print_endline "None"
  | Some x ->
     pr_rec x;
     print_endline ""

(* Renvoie None si le bsp possède une unique solution et une deuxième solution sinon *)
let sat_solve (prof : int) (bsp : bsp) =
  match get_fnc_of_bsp prof bsp with
  | None -> None
  | Some f -> Sat.solve (list_of_fnc f)

(* Renvoie vrai si le bsp possède une unique solution et faux sinon *)
let is_uniq prof bsp = maybe true (fun _ -> false) (sat_solve prof bsp)

(* Test si le bsp possède une unique solution et affiche le résultat *)
let print_maybe_other_sol prof bsp = print_possible_sol (sat_solve prof bsp)
