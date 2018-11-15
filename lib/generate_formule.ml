open Couleur
open Utils
open Bsp
open Formule
open Bsp_sat
open Tseitin

(* Renvoie la liste de tout les tuples à n éléments que l'on 
   peut former avec les éléments de list *)
let rec get_n_tuples_in_list (n : int) (list : 'a list) =
  let rec aux x l res =
    match l with
    | [] -> res
    | ll::q -> aux x q ((x::ll)::res)
  in
  if n != 0 then
      match list with
      | [] -> []
      | x::q -> (aux x (get_n_tuples_in_list (n-1) q) [])@(get_n_tuples_in_list n q)
  else [[]]

(* Renvoie une liste de formule où chaque formule est de la forme :
   - Var n si blue est faux
   - Neg n si blue est vrai
   Cette fonction prend en argument une liste de Rectangle et échoue si 
   la liste contient une Ligne
 *)
let rec formule_list_of_bsp_sat_list (blue : bool) (list : bsp_sat list) =
    match list with
    | [] -> []
    | x::q ->
       match x with
       | R_sat (n,_,_) ->
          if blue then
              (Neg n)::(formule_list_of_bsp_sat_list blue q)
          else
              (Var n)::(formule_list_of_bsp_sat_list blue q)
       | _ -> failwith "Get_fnd_of_bsp_sat"

(* Prend en argument une liste red de Var n et une liste list de Rectangle 
   et renvoie le complémentaire de red dans list. C'est à dire une liste 
   de Neg n correspondant au rectangle non contenu dans red *)
let get_compl (red : formule list) (rect : bsp_sat list) =
  let rec filtre (x : formule) (red : formule list) =
    match red with
    | [] -> true
    | y::q ->
       if same_var x y then false
       else filtre x q
  in
  let rec neg ?(res = []) l =
      match l with
      | [] -> res
      | x::q ->
         match x with
         | Var x -> neg ~res:((Neg x)::res) q
         | _ -> failwith "neg"
  in
  let compl = List.filter (fun x -> filtre x red) (formule_list_of_bsp_sat_list false rect) in
  neg compl

let get_all_compl (red : formule list list) (list : bsp_sat list) =
  List.map (fun x -> x@(get_compl x list)) red

(* Renvoie une liste de liste de string correspondant
   à une fnd satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat
   ATTENTION : seulement pour cette ligne, pas pour ces fils *)
let get_list_list_of_bsp_sat (ligne : bsp_sat) : formule list list =
  let r, b, list = get_adja_stat ligne in
  let size = r + b + List.length list in
  let rec aux (blue:bool) (list : bsp_sat list list) : formule list list =
    match list with
    | [] -> []
    | l::q -> (formule_list_of_bsp_sat_list blue l)::(aux blue q)
  in
  match ligne with
  | R_sat (_,_,_) -> []
  | L_sat (c,_,_,_) ->
     match c with
     | None -> []
     | Some c ->
        match c with
     (* noter que, dans le cas Purple, size est pair *)
        | Purple ->
           let red  = aux false (get_n_tuples_in_list (size/2-r) list) in
           get_all_compl red list
        | C co ->
           match co with
           | Red ->
              if r > size/2 then []
              else aux false (get_n_tuples_in_list (size/2+1-r) list)
           | Blue ->
              if b > size/2 then []
              else aux true (get_n_tuples_in_list (size/2+1-b) list)

(* Prend une liste de liste de formule et retourne une formule de la forme
 * (_ et _ et ... et _) ou (_ et _ et ... et _) ou ... ou (_ et _ et ... et _)*)
let get_formule_of_list_list (ll : formule list list) : formule option =
  let rec conj_all (list : formule list) : formule option =
    match list with
    | [] -> None
    | x::q ->
       let f = conj_all q in
       match f with
       | None -> Some x
       | Some a -> Some (Et (x,a))
  in
  let rec disj_all (list : formule list list) : formule option =
    match list with
    | [] -> None
    | x::q ->
       let f = disj_all q in
       let g = conj_all x in
       match f with
       | None -> g
       | Some a ->
          match g with
          | None -> Some a
          | Some b -> Some (Ou (a,b))
  in
  let f = disj_all ll in
  f

(* Renvoie la formule correspondant à la conjonction des contraintes de bsp_sat
 et de tout ses fils*)
let rec get_formule_complete ?(nvar=(-1)) (bsp_sat : bsp_sat) : int * formule option =
  let (nvar2,formfils) =
    match bsp_sat with
    | R_sat (_,_,_) -> (nvar, None)
    | L_sat (_,_,l,r) ->
       let (nvar1,fl) = get_formule_complete ~nvar:nvar l in
       let (nvar2,fr) = get_formule_complete ~nvar:nvar1 r in
       let f = match fl, fr with
         | None, None -> None
         | Some a, None -> Some a
         | None, Some b -> Some b
         | Some a, Some b -> Some (Et (a,b)) in
       (nvar2,f)
  in
  let form = get_formule_of_list_list (get_list_list_of_bsp_sat bsp_sat) in
  (* on met sous FNC form *)
  let fnc_form = maybe None (fun x -> Some (tseitin ~nvar:nvar2 x)) form in
  match fnc_form, formfils with
  | None, None -> (nvar2,None)
  | Some (n,a), None -> (n,Some a)
  | None, Some b -> (nvar2,Some b)
  | Some (n,a), Some b -> (n,Some (Et (a,b)))

(* Renvoie la formule correspondant à la solution encodé dans bsp_sat*)
let rec get_actual_sol (orig : bsp_sat) =
  match orig with
  | R_sat (i,_,x) -> switch_coul (Neg i) (Var i) x
  | L_sat (_,_,l,r) -> Ou (get_actual_sol l, get_actual_sol r)

(* Renvoie une fnc satisfaisable si et seulement si le bsp à plusieurs solution*)
let get_fnc_of_bsp (prof : int) (bsp : bsp) =
  let sat = bsp_sat_of_bsp bsp |> loop_sat prof in
  let (_,f) = get_formule_complete sat in
  let sol = get_actual_sol sat in
  maybe None (fun fnc -> Some (Et (fnc,sol))) f
