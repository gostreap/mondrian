open Couleur

type label =
  { coord : int;
    colored : bool;
  }

(* Commence avec une séparation verticale *)
type bsp = R of couleur option
         | L of label * bsp * bsp

type point = (int * int)

type linetree =
  | Leef
  | Line of point * point * couleur_l option * linetree * linetree

(* Utilitaire *)
let maybe n f m =
  match m with
  | None -> n
  | Some s -> f s

(* Affichage rudimentaire de BSP *)
let rec string_of_bsp (bsp : bsp) =
  match bsp with
  | L (lab,l,r) ->
     "(" ^ (string_of_bsp l) ^ ") " ^ (string_of_int (lab.coord)) ^
       " (" ^ (string_of_bsp r) ^ ")"
  | R x -> maybe "None" string_of_couleur x

(*
 Génère un BSP aléatoire de profondeur 'profondeur'
 NOTE: Pour l'instant, toutes les arrêtes sont visibles
 *)
let rec random_bsp_naive ?(v=true) ?(minsize=20) ?(start_larg=0)
                         ?(start_haut=0) (prof : int) (larg : int) (haut : int) =
  if prof = 0 || larg-start_larg <= minsize*2 || haut-start_haut <= minsize*2
  then R (Some (if Random.bool () then Blue else Red))
  else
    let lab =
      { coord =
          if v
          then start_larg + minsize + (Random.int (larg-start_larg - minsize*2))
          else start_haut + minsize + (Random.int (haut-start_haut - minsize*2));
        colored = true
      } in
    let l =
      random_bsp_naive
        ~v:(not v)
        ~start_larg:start_larg
        ~start_haut:start_haut
        (prof-1)
        (if v then lab.coord else larg)
        (if v then haut else lab.coord) in
    let r =
      random_bsp_naive
        ~v:(not v)
        ~start_larg:(if v then lab.coord else start_larg)
        ~start_haut:(if v then start_haut else lab.coord)
        (prof-1)
        larg
        haut in
    L (lab,l,r)

(* Change la couleur d'un rectangle d'un bsp, dans lequel se situe p *)
let rec change_color ?(v=true) (bsp : bsp) ((x,y) as p : point) =
  match bsp with
  | L (lab, left, right) ->
     if v
     then if x < lab.coord
          then L (lab, change_color ~v:(not v) left p, right)
          else L (lab, left, change_color ~v:(not v) right p)
     else if y < lab.coord
     then L (lab, change_color ~v:(not v) left p, right)
     else L (lab, left, change_color ~v:(not v) right p)
  | R c -> R (Some (maybe Blue neg_coul c))

(*
Retourne un couple (r,b) où:
 * r est le nombre de rectangle rouge adjacents
 * y est le nombre de rectangle bleu adjacents
 *)
let get_coul_sum (bsp : bsp) =
  let rec get_coul ?(v=true) (is_l : bool) (bsp : bsp) =
    match bsp with
    | L (_,x,y) ->
       let rx,bx as x' = get_coul ~v:(not v) is_l x in
       let ry,by as y' = get_coul ~v:(not v) is_l y in
       if not v
       then if is_l then y' else x'
       else (rx+ry,bx+by)
    | R x -> maybe (0,0) (switch_coul (1,0) (0,1)) x
  in
  match bsp with
  | R _ -> (0,0)
  | L (_,l,r) ->
     let (lr,lb) = get_coul true l in
     let (rr,rb) = get_coul false r in
     (lr+rr,lb+rb)

(* Renvoie la couleur naturel de la ligne correspondant à la racine de bsp *)
let get_color_line (bsp : bsp) =
  let (r,b) = get_coul_sum bsp in
  if r = 0 && b = 0
  then None
  else
    if r = b then
      Some Purple
    else if r < b then
      Some (C Blue)
    else
      Some (C Red)

(* Vérifie si deux colorations sont équivalentes *)
let rec check_current (bsp1 : bsp) (bsp2 : bsp) =
  match bsp1 with
  | L (_,x,y) ->
     begin
       match bsp2 with
         L (_,x',y') ->
          let cond =
            let r ,b =  get_coul_sum bsp1 in
            let r',b' = get_coul_sum bsp2 in
            if r' = b'
            then r = b
            else
              if r' < b'
              then r < b
              else r > b
          in check_current x x' && check_current y y' && cond
       | _ -> false
     end
  | R _ ->
     match bsp2 with
       R _ -> true
     | _ -> false

let rec linetree_of_bsp ?(v=true) ?(infx = 0) ?(infy = 0)
                        (bsp : bsp) (supx:int) (supy:int) =
  match bsp with
  | R _ -> Leef
  | L (lab, left, right) ->
     let color = get_color_line bsp in
     if v then
       let
         left_linetree = linetree_of_bsp ~v:(not v) ~infx:infx ~infy:infy
                           left lab.coord supy
       and
         right_linetree = linetree_of_bsp ~v:(not v) ~infx:lab.coord ~infy:infy
                            right supx supy
       in
       Line ((lab.coord, infy), (lab.coord, supy), color, left_linetree, right_linetree)
     else
       let
         left_linetree = linetree_of_bsp ~v:(not v) ~infx:infx ~infy:infy
                           left supx lab.coord
       and
         right_linetree = linetree_of_bsp ~v:(not v) ~infx:infx ~infy:lab.coord
                            right supx supy
       in
       Line ((infx, lab.coord), (supx, lab.coord), color, left_linetree, right_linetree)

(* Renvoie une copie de bsp avec toute les régions non coloriées *)
let rec empty_copy_of_bsp (bsp : bsp) =
  match bsp with
  | R _ -> R None
  | L (lab,left,right) -> L (lab, empty_copy_of_bsp left, empty_copy_of_bsp right)


(* ------------------------------------------------------------------------------------
   PARTIE SAT 
   ------------------------------------------------------------------------------------ *)
                       
(* bsp for SAT *)
type bsp_sat =
  | R_sat of string * int * couleur
  | L_sat of couleur_l option * bsp_sat * bsp_sat

let rec string_of_bsp_sat (bsp : bsp_sat) =
  match bsp with
  | L_sat (lab,l,r) ->
     "(" ^ (string_of_bsp_sat l) ^ " " ^ (maybe "black" string_of_couleur_l lab) ^
         " " ^ (string_of_bsp_sat r) ^ ")"
  | R_sat (n,x,c) -> n ^ "*" ^ string_of_int x ^ "*" ^ (switch_coul "b" "r" c)

let bsp_sat_of_bsp (bsp : bsp) =
  let rec aux v bsp =
    match bsp with
    | R x ->
       let c =
         match x with
         | None -> failwith "translate_bsp"
         | Some x -> x in (v+1,R_sat (string_of_int v,1,c))
    | L (lab,l,r) ->
       let (n,ll) = aux v l in
       let (m,rr) = aux n r in
       let c =
         if lab.colored
         then get_color_line bsp
         else None in
       (m,L_sat (c,ll,rr))
  in snd (aux 0 bsp)

let rec reduce_bsp_sat (bsp : bsp_sat) =
  match bsp with
  | L_sat (c,l,r) ->
     let lr =
       match l, r with
       | R_sat (n,x,co), R_sat (_,y,_) -> Some (n,(x+y),co)
       | _ -> None in
     if maybe false (switch_coul_l false (fun _ -> true)) c
     then
       match lr with
       | Some (n,x,y) -> R_sat (n,x,y)
       | _ -> L_sat (c,reduce_bsp_sat l,reduce_bsp_sat r)
     else L_sat (c,reduce_bsp_sat l,reduce_bsp_sat r)
  | i -> i

(* Renvoie un bsp ou les feuilles ont un indice différent de 1 si leur couleur est fixé *)
let rec secure_bsp_sat (bsp : bsp_sat) =
    match bsp with
  | L_sat (c,l,r) ->
     let lr =
       match l, r with
       | R_sat (n,x,co), R_sat (_,y,_) -> Some (n,(x+y),co)
       | _ -> None in
     if maybe false (switch_coul_l false (fun _ -> true)) c
     then
       match lr with
       | Some (n,x,co) -> L_sat (c, R_sat (n,x,co), R_sat(n,x,co))
       | _ -> L_sat (c,secure_bsp_sat l,secure_bsp_sat r)
     else L_sat (c,secure_bsp_sat l,secure_bsp_sat r)
  | i -> i

let rec loop_sat (n : int) (b : bsp_sat) =
  if n <= 0
  then b
  else loop_sat (n-1) (secure_bsp_sat b)

(* Renvoie un couple (r, b, list) où:
 * r est le nombre de rectangle rouge adjacents sécurisés
 * b est le nombre de rectangle bleu adjacents sécurisés
 * list est la liste des rectangles non sécurisés*)
let get_adja_stat (bsp_sat : bsp_sat) =
  let rec get_stat ?(v=true) (is_l : bool) (bsp_sat : bsp_sat) =
    match bsp_sat with
    | L_sat (_,x,y) ->
       let rx,bx,ll as x' = get_stat ~v:(not v) is_l x in
       let ry,by,lr as y' = get_stat ~v:(not v) is_l y in
       if not v
       then if is_l then y' else x'
       else (rx+ry,bx+by,ll@lr)
    | R_sat (_,s,x) ->
       if s != 1 then
           let (r,b) = maybe (0,0) (switch_coul (1,0) (0,1)) (Some x) in
           (r, b, [])
       else (0,0,[bsp_sat])
  in
  match bsp_sat with
  | R_sat _ -> (0,0,[])
  | L_sat (_,l,r) ->
     let (lr,lb,llist) = get_stat true l in
     let (rr,rb,rlist) = get_stat false r in
     (lr+rr,lb+rb,llist@rlist)

(* ------------------------------------------------------------------------------------
   FORMULE
   ------------------------------------------------------------------------------------ *)
 
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

let rec string_list_of_bsp_sat_list (blue : bool) (list : bsp_sat list) =
    match list with
    | [] -> []
    | x::q ->
       match x with
       | R_sat (n,_,_) ->
          if blue then 
              ("-" ^ n)::(string_list_of_bsp_sat_list blue q)
          else
              n::(string_list_of_bsp_sat_list blue q)
       | _ -> failwith "Get_fnd_of_bsp_sat"
  
let get_compl (t : string list) (list  : bsp_sat list) =
  let rec filtre (x : string) (l : string list) =
    match l with
    | [] -> true
    | y::q ->
       if x = y then false
       else filtre x q
  in
  let rec neg ?(res = []) l =
      match l with
      | [] -> res
      | x::q -> neg ~res:(("-"^x)::res) q 
  in
  let compl = List.filter (fun x -> filtre x t) (string_list_of_bsp_sat_list false list) in
  neg compl

let get_all_compl (red : string list list) (list : bsp_sat list) =
  List.map (fun x -> x@(get_compl x list)) red

(* Renvoie une liste de liste de string correspondant
   à une fnd satisfaisable ssi il existe un choix de
   coloration possible pour la ligne bsp_sat
   ATTENTION : seulement pour cette ligne, pas pour ces fils
*)
let get_fnd_of_bsp_sat (bsp_sat : bsp_sat) : string list list =
  let r, b, list = get_adja_stat bsp_sat in
  let size = r + b + List.length list in
  let rec aux (blue:bool) (list : bsp_sat list list) : string list list =
    match list with
    | [] -> []
    | l::q -> (string_list_of_bsp_sat_list blue l)::(aux blue q)
  in
  match bsp_sat with
  | R_sat (_,_,_) -> []
  | L_sat (c,_,_) ->
     begin
         match c with
         (* noter que, dans le cas Purple, size est pair *)
         | Some Purple ->
            let red  = aux false (get_n_tuples_in_list (size/2) list) in
            get_all_compl red list
         | Some C co ->
            begin
                match co with
                | Red ->
                   if r > size/2 then []
                   else aux false (get_n_tuples_in_list (size/2+1-r) list)
                | Blue ->
                   if b > size/2 then []
                   else aux true (get_n_tuples_in_list (size/2+1-b) list)
            end
         | None -> []
     end

let rec get_all_fnd (bsp_sat : bsp_sat) =
  let list =
    match bsp_sat with
    | R_sat (_,_,_) -> []
    | L_sat (_,l,r) ->
       get_all_fnd l @ get_all_fnd r in
  get_fnd_of_bsp_sat bsp_sat @ list

type formule =
  | Var of string
  | Neg of formule
  | Et of formule * formule
  | Ou of formule * formule

let rec translate_fnd (fnd : string list list) =
  match fnd with
    [] -> failwith "empty"
  | x::xs ->
     match x with
       [] ->
        begin
          match xs with
            [] -> failwith "empty"
          | _ -> translate_fnd xs
        end
     | (x'::xs') ->
       let c1 = List.fold_left (fun acc e -> Et (acc,Var e)) (Var x') xs'
     in match xs with
          [] -> c1
        | _ -> Ou (c1,translate_fnd xs)

let rec translate_fnc (fnc : formule) =
  let get_v v =
    match v with
    | Var x -> x
    | _ -> failwith "v" in
  let rec get_ou f =
    match f with
    | Ou (a,b) ->
       begin
         match a with
         | Ou _ -> get_ou a @ get_ou b
         | _ -> get_v a :: get_ou b
       end
    | _ -> [] in
  match fnc with
  | Et (a,b) ->
     begin
         match a with
         | Et _ -> translate_fnc a @ translate_fnc b
         | _ -> get_ou a :: translate_fnc b
     end
  | _ -> []

let rec desc_neg f =
  let neg x = match x with
    | Neg p -> desc_neg p
    | Et (a,b) -> desc_neg (Ou ((Neg a),(Neg b)))
    | Ou (a,b) -> desc_neg (Et ((Neg a),(Neg b)))
    | _ -> Neg x
  in match f with
  | Var x -> Var x
  | Neg x -> neg x
  | Et (a,b) -> Et ((desc_neg a),(desc_neg b))
  | Ou (a,b) -> Ou ((desc_neg a),(desc_neg b))

let rec desc_ou f = match f with
  | Var _ -> f
  | Neg _ -> f
  | Et (a,b) -> Et ((desc_ou a),(desc_ou b))
  | Ou (a,b) -> match desc_ou a with
    | Et (x,y) -> desc_ou (Et ((Ou (x,b)),(Ou (y,b))))
    | _ -> match desc_ou b with
      | Et (x,y) -> desc_ou (Et ((Ou (x,a)),(Ou (y,a))))
      | _ -> Ou (a,b)

let fnc f = desc_ou (desc_neg f)

let get_fnc_of_bsp (bsp : bsp) =
  bsp_sat_of_bsp bsp |> loop_sat 20 |> get_all_fnd |> translate_fnd |> fnc |> translate_fnc
    
let rec print_fnc (l : string list list) =
  let rec print_ou l =
    match l with
    | [] -> print_string "FIN"
    | x::xs ->
       print_string x;
       print_string " OU ";
       print_ou xs in
  match l with
    [] -> ()
  | x::xs ->
     print_ou x;
     print_string " ET ";
     print_fnc xs
