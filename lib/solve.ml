open Utils
open Bsp
open Formule
open Generate_formule
open Generate_formule2

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
  | Et (a,b) -> list_of_fnc a @ list_of_fnc b
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
     pr_rec (List.sort (fun x y -> compare (snd x) (snd y)) x);
     print_endline ""

(* Renvoie None si le bsp possède une unique solution et une deuxième solution sinon *)
let sat_solve (f : formule option) =
  match f with
  | None -> None
  | Some f -> Sat.solve (list_of_fnc f)

(* Renvoie vrai si le bsp possède une unique solution et faux sinon *)
let is_uniq prof bsp = maybe true (fun _ -> false) (sat_solve (get_fnc_of_bsp prof bsp))

(* Test si le bsp possède une unique solution et affiche le résultat *)
let print_maybe_other_sol prof bsp = print_possible_sol (sat_solve (get_fnc_of_bsp prof bsp))

(* Test si le bsp possède une solution et affiche le résultat *)
let print_maybe_other_sol_soluce working_bsp linetree point =
  let sol = sat_solve (get_fnc_of_bsp_soluce_for_point working_bsp linetree point) in
  match sol with
  | None -> print_endline "Pas de solution"
  | _ -> print_endline "Solution possible"

let fill_one_rectangle working_bsp linetree =
  let sol = sat_solve (get_fnc_of_bsp_soluce working_bsp linetree) in
  match sol with
  | None ->
     let (b,r) = tryred working_bsp in
     if b
     then r
     else
       begin
         print_endline "Pas de solution : impossible de remplir";
         working_bsp
       end
  | Some l ->
     let lclean = List.filter (fun x -> (snd x) >= 0) (List.sort (fun x y -> compare (snd x) (snd y)) l) in
     match lclean with
     | [] ->
        print_endline "ERREUR : fill_one_rectangle2 -> liste vide";
        working_bsp
     | x::y::_ ->
        let c =
          if fst x && fst y then `Blue
          else if fst x || fst y then `Green
          else `Red in
        change_coul_with_id working_bsp (snd x/2) c;
     | _ -> failwith "ERREUR : fill_one_rectanlgle -> paire de variable incomplète"

(* For 2 colors *)
let is_uniq2 prof bsp = maybe true (fun _ -> false) (sat_solve (get_fnc_of_bsp2 prof bsp))

(* Test si le bsp possède une unique solution et affiche le résultat *)
let print_maybe_other_sol2 prof bsp = print_possible_sol (sat_solve (get_fnc_of_bsp2 prof bsp))

(* Test si le bsp possède une solution et affiche le résultat *)
let print_maybe_other_sol_soluce2 working_bsp linetree =
  let sol = sat_solve (get_fnc_of_bsp_soluce2 working_bsp linetree) in
  match sol with
  | None -> print_endline "Pas de solution"
  | _ -> print_endline "Solution possible"

let fill_one_rectangle2 working_bsp linetree =
  let sol = sat_solve (get_fnc_of_bsp_soluce2 working_bsp linetree) in
  match sol with
  | None ->
     let (b,r) = tryred working_bsp in
     if b
     then r
     else
       begin
         print_endline "Pas de solution : impossible de remplir";
         working_bsp
       end
  | Some l ->
     match l with
     | [] ->
        print_endline "ERREUR : fill_one_rectangle2 -> liste vide";
        working_bsp
     | x::_ -> change_coul_with_id working_bsp (snd x) (if (fst x) then `Blue else `Red)
