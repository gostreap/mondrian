open Bsp
open Bsp_sat
open Utils
open Formule
open Generate_formule
open Generate_formule2

module Sat = Sat_solver.Make(struct type t = int let compare = compare end)

type solution = Antilogie | Tautologie | L_sol of Sat.literal list

(* Renvoie une liste de formule correspondant à une découpe de toute les clauses
   disjonctives de la fnc*)
let rec list_of_fnc (fnc : formule) =
  let get_var f =
    match f with
    | Var x -> (true,x)
    | Neg x -> (false,x)
  in
  let rec get_ou f =
    match f with
    | Vrai | Faux -> []
    | Et _ -> failwith "Was not in fnc"
    | Lit x -> [get_var x]
    | Ou (a,b) -> get_ou a @ get_ou b
  in
  match fnc with
  | Vrai | Faux -> []
  | Et (a,b) -> list_of_fnc a @ list_of_fnc b
  | Ou _  -> [get_ou fnc]
  | Lit x -> [[get_var x]]

(* Affiche une solution donnée par le sat solver *)
let print_solution sol =
  let pr_rec =
    List.iter
      (fun (x,y) -> print_string ("("^string_of_bool x ^ ", " ^string_of_int y ^ ") ") )
  in
  match sol with
  | Antilogie -> print_endline "Antilogie"
  | Tautologie -> print_endline "Tautologie"
  | L_sol x ->
     pr_rec (List.sort (fun x y -> compare (snd x) (snd y)) x);
     print_endline ""

(* Renvoie None si le bsp possède une unique solution et une deuxième solution sinon *)
let sat_solve f =
  match f with
  | Faux -> Antilogie
  | Vrai -> Tautologie
  | form ->
     let s = Sat.solve (list_of_fnc form) in
     match s with
     | None -> Antilogie
     | Some l -> L_sol l
                                                
(*Renvoie vrai si tout les rectangles sécurisés dans bsp_sat sont de la bonne couleur dans working_bsp*)
let rec check_all_secure_rect bsp_sat working_bsp =
  match bsp_sat, working_bsp with
  | R_sat(_,b,csat), R c ->
       not b || csat = c || c = None
  | L_sat(_,_,lsat,rsat), L (_,l,r) -> check_all_secure_rect lsat l && check_all_secure_rect rsat r
  | _ -> failwith ("ERREUR : (solve.ml) check_all_secure_rect -> bsp_sat et working_bsp different")

let rec get_all_secure_rect bsp_sat working_bsp =
  match bsp_sat, working_bsp with
  | R_sat(n,b,csat), R c ->
         if b && c = None then [(n,csat)] else []
  | L_sat(_,_,lsat,rsat), L (_,l,r) -> (get_all_secure_rect lsat l)@(get_all_secure_rect rsat r)
  | _ -> failwith ("ERREUR : (solve.ml) get_all_secure_rect -> bsp_sat et working_bsp different")

(* Prend une liste de couple (bool,n) et colorie dans working bsp le rectangle
   correspondant à la tete de liste  *)
let color_first working_bsp l =
  let lclean = List.filter (fun x -> (snd x) >= 0)
                           (List.sort (fun x y -> compare (snd x) (snd y)) l) in
  match lclean with
  | [] -> failwith "ERREUR : color_first -> liste vide";
  | x::y::q ->
     let c =
       if fst x && fst y then `Red
       else if fst x && not (fst y) then `Green
       else `Blue in
     (L_sol q,change_coul_with_id working_bsp (snd x/2) c);
  | _ -> failwith "ERREUR : color_first -> paire de variable incomplète"

let color_first2 working_bsp l=
  match l with
  | [] -> failwith "ERREUR : color_first2 -> liste vide";
  | x::q -> (L_sol q,change_coul_with_id working_bsp (snd x) (if fst x then `Red else `Blue))

let fill_one_rectangle (get_fnc : int -> 'a bsp -> 'a linetree -> formule) (col_first : 'a bsp -> (bool*int) list -> solution * 'a bsp) (prof : int) (origin_bsp_sat : 'a bsp_sat)  (working_bsp : 'a bsp) (linetree : 'a linetree) (last_sol : solution) : (solution * 'a bsp) =
  match last_sol with
  | Antilogie | Tautologie | L_sol [] ->
     begin
         if not (check_all_secure_rect origin_bsp_sat working_bsp) then
             begin
                 print_message "Secure incorrect : pas de solution";
                 (Antilogie ,working_bsp)
             end
         else
             let sol = sat_solve (get_fnc prof working_bsp linetree) in
             match sol with
             | Antilogie -> print_message "Pas de solution : impossible de remplir (1)"; (Antilogie, working_bsp)
             | Tautologie ->
                let (b,r) = tryred working_bsp in
                if b then begin clean_message(); (Tautologie, r) end
                else
                    begin
                        print_message "Pas de solution : impossible de remplir";
                        (Antilogie, working_bsp)
                    end
             | L_sol l ->
                let secure = get_all_secure_rect origin_bsp_sat working_bsp in
                if List.length secure = 0 then
                    begin
                        clean_message ();
                        col_first working_bsp l
                    end
                else
                    let x = List.hd secure in
                    match snd x with
                    | Some c ->
                       clean_message ();
                       (sol, change_coul_with_id working_bsp (fst x) c)
                    | _ -> failwith "Erreur : fill_one_rectangle2 -> pas de couleur"
     end
  | L_sol l ->
     let secure = get_all_secure_rect origin_bsp_sat working_bsp in
     if List.length secure = 0 then
         begin
             clean_message ();
             col_first working_bsp l
         end
     else
         let x = List.hd secure in
         match snd x with
         | Some c ->
            clean_message ();
            (L_sol l, change_coul_with_id working_bsp (fst x) c)
         | _ -> failwith "Erreur : fill_one_rectangle2 -> pas de couleur"

let print_maybe_other_sol_soluce_gen get_fnc prof origin_bsp_sat working_bsp linetree =
  if not (check_all_secure_rect origin_bsp_sat working_bsp) then
    print_message "Secure incorrect : pas de solution"
  else
    let sol = sat_solve (get_fnc prof working_bsp linetree) in
    match sol with
    | Antilogie -> print_message "Pas de solution"
    | _ -> print_message "Solution possible"

(* Test si le bsp possède une unique solution et affiche le résultat *)
let print_maybe_other_sol prof bsp =
  print_solution (sat_solve (get_fnc_of_bsp prof bsp))

(* Test si le bsp possède une solution et affiche le résultat *)
let print_maybe_other_sol_soluce =
  print_maybe_other_sol_soluce_gen get_fnc_of_bsp_soluce

(* For 2 colors *)
(* Test si le bsp possède une unique solution et affiche le résultat *)

let print_maybe_other_sol2 prof bsp =
  print_solution (sat_solve (get_fnc_of_bsp2 prof bsp))

(* Test si le bsp possède une solution et affiche le résultat *)
let print_maybe_other_sol_soluce2 =
  print_maybe_other_sol_soluce_gen get_fnc_of_bsp_soluce2
