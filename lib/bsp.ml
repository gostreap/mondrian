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

let rec empty_copy_of_bsp (bsp : bsp) =
  match bsp with
  | R _ -> R None
  | L (lab,left,right) -> L (lab, empty_copy_of_bsp left, empty_copy_of_bsp right)

(* bsp for SAT *)
type bsp_sat =
  | R_sat of string * int * couleur
  | L_sat of couleur_l option * bsp_sat * bsp_sat

let rec string_of_bsp_sat (bsp : bsp_sat) =
  match bsp with
  | L_sat (lab,l,r) ->
     "(" ^ (string_of_bsp_sat l) ^ " " ^ (maybe "black" string_of_couleur_l lab) ^ " " ^ (string_of_bsp_sat r) ^ ")"
  | R_sat (n,x,c) -> n ^ "*" ^ string_of_int x ^ "*" ^ (switch_coul "b" "r" c)

let translate_bsp (bsp : bsp) =
  let rec aux v bsp =
    match bsp with
    | R x ->
       let c =
         match x with
         | None -> failwith "translate_bsp"
         | Some x -> x in
       (v+1,R_sat (string_of_int v,1,c))
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
       match l,r with
       | R_sat (n,x,co),R_sat (_,y,_) -> Some (n,(x+y),co)
       | _ -> None in
     if maybe false (switch_coul_l false (fun _ -> true)) c
     then
       match lr with
       | Some (n,x,y) -> R_sat (n,x,y)
       | _ -> L_sat (c,reduce_bsp_sat l,reduce_bsp_sat r)
     else L_sat (c,reduce_bsp_sat l,reduce_bsp_sat r)
  | i -> i

let rec loop_sat (n : int) (b : bsp_sat) =
  if n <= 0
  then b
  else loop_sat (n-1) (reduce_bsp_sat b)
