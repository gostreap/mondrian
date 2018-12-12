open Utils
open Couleur

type label =
  { coord : int;
    colored : bool;
  }

(* Commence avec une séparation verticale *)
type 'a bsp =
  R of 'a option
| L of label * 'a bsp * 'a bsp

type point = (int * int)

type 'a linetree =
  | Leef
  | Line of point * point * 'a couleur_l option * 'a linetree * 'a linetree

let rec machinestring_of_bsp (bsp : [< `Red | `Green | `Blue] bsp) =
  let machinestring_of_label lab = "{ coord=" ^ string_of_int (lab.coord) ^ "; colored="^ (string_of_bool lab.colored) ^ " }" in
  match bsp with
  | L (lab,l,r) ->
     "L ("^ (machinestring_of_label lab) ^", " ^ machinestring_of_bsp l ^ ", " ^ machinestring_of_bsp r ^")"
  | R x -> "R (" ^ (maybe "None" (fun x -> "Some " ^ string_of_couleur x ^ ")") x)

let rand_two_coul () =
  if Random.bool ()
  then `Red
  else `Blue

let rand_three_coul () =
  let x = Random.int 3 in
  if x = 0 then `Red
  else if x = 1 then `Green
  else `Blue

(*
 Génère un BSP aléatoire de profondeur 'profondeur'
 NOTE: Pour l'instant, toutes les arrêtes sont visibles
 *)
let rec random_bsp_naive
          ?(v=true) ?(minsize=20) ?(start_larg=0)
          ?(start_haut=0) (prof : int) (larg : int) (haut : int) (rand : unit -> ([< `Red | `Green | `Blue] as 'a)) : ('a bsp) =
  if prof = 0 || larg-start_larg <= minsize*2 || haut-start_haut <= minsize*2
  then R (Some (rand ()))
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
        (if v then haut else lab.coord) rand in
    let r =
      random_bsp_naive
        ~v:(not v)
        ~start_larg:(if v then lab.coord else start_larg)
        ~start_haut:(if v then start_haut else lab.coord)
        (prof-1)
        larg
        haut rand in
    L (lab,l,r)

(* Change la couleur d'un rectangle d'un bsp, dans lequel se situe p *)
let change_color (getnext : 'a option -> 'a) (bsp : 'a bsp) ((x,y) : point) =
  let rec traverse v bsp =
    match bsp with
    | L (lab, left, right) ->
       if v
       then if x < lab.coord
            then L (lab, traverse (not v) left, right)
            else L (lab, left, traverse (not v) right)
       else if y < lab.coord
       then L (lab, traverse (not v) left, right)
       else L (lab, left, traverse (not v) right)
    | R c -> R (Some (getnext c))
  in traverse true bsp

(*
Retourne un couple (r,b) où:
 * r est le nombre de rectangle rouge adjacents
 * y est le nombre de rectangle bleu adjacents
 *)
let get_coul_sum (bsp : [< `Red | `Green | `Blue] bsp) =
  let rec get_coul (v : bool) (is_l : bool) bsp =
    match bsp with
    | L (_,x,y) ->
       let rx,gx,bx as x' = get_coul (not v) is_l x in
       let ry,gy,by as y' = get_coul (not v) is_l y in
       if not v
       then if is_l then y' else x'
       else (rx+ry,gx+gy,bx+by)
    | R x -> maybe (0,0,0) (switch_coul (1,0,0) (0,1,0) (0,0,1)) x
  in
  match bsp with
  | R _ -> (0,0,0)
  | L (_,l,r) ->
     let (lr,lg,lb) = get_coul true true l in
     let (rr,rg,rb) = get_coul true false r in
     (lr+rr,lg+rg,lb+rb)

(* Renvoie la couleur naturel de la ligne correspondant à la racine de bsp *)
let get_color_line (bsp : couleur bsp) : (couleur couleur_l option) =
  let (r,g,b) = get_coul_sum bsp in
  if r = 0 && b = 0 && g=0
  then None
  else
    if r = b && b = g then Some White
    else if r > b && r > g then Some (C `Red)
    else if g > r && g > b then Some (C `Green)
    else if b > r && b > g then Some (C `Blue)
    else if r = b then Some Purple
    else if r = g then Some Yellow
    else Some Cyan

let get_color_line2 (bsp : ([`Red | `Blue] as 'a) bsp) : ('a couleur_l option) =
  let (r,_,b) = get_coul_sum bsp in
  if r = 0 && b = 0
  then None
  else
    if r = b then Some Purple
    else if r > b then Some (C `Red)
    else Some (C `Blue)

(* Vérifie si bsp2 est une solution par rapport à bsp1 *)
let rec check_current (bsp1 : ([< `Red | `Green | `Blue] as 'a) bsp) (bsp2 : 'a bsp) =
  match bsp1 with
  | L (_,x,y) ->
     begin
       match bsp2 with
         L (_,x',y') ->
          let cond =
            let r ,g , b  = get_coul_sum bsp1 in
            let r',g', b' = get_coul_sum bsp2 in
            if r = b && b = g then r' = b' && b' = g'
            else if r > b && r > g then r' > b' && r' > g'
            else if g > r && g > b then g' > r' && g' > b'
            else if b > r && b > g then b' > r' && b' > g'
            else if r = b then r'=b'
            else if r = g then r'=g'
            else b'=g'
          in cond && check_current x x' && check_current y y'
       | _ -> false
     end
  | R _ ->
     match bsp2 with
       R x -> maybe false (fun _ -> true) x
     | _ -> false

let linetree_of_bsp
          (get_col : (([< `Blue | `Green | `Red ] as 'a) bsp -> 'a couleur_l option))
          (bsp : 'a bsp) (supx:int) (supy:int) : ('a linetree) =
  let rec aux v infx infy supx supy bsp =
    match bsp with
    | R _ -> Leef
    | L (lab, left, right) ->
       let color = get_col bsp in
       if v then
         let left_linetree  = aux (not v) infx infy lab.coord supy left
         and right_linetree = aux (not v) lab.coord infy supx supy right
         in
         Line ((lab.coord, infy), (lab.coord, supy), color, left_linetree, right_linetree)
       else
         let left_linetree  = aux (not v) infx infy supx lab.coord left
         and right_linetree = aux (not v) infx lab.coord supx supy right
         in
         Line ((infx, lab.coord), (supx, lab.coord), color, left_linetree, right_linetree)
  in aux true 0 0 supx supy bsp

(* Renvoie une copie de bsp avec toute les régions non coloriées *)
let rec empty_copy_of_bsp (bsp : 'a bsp) : 'b bsp =
  match bsp with
  | R _ -> R None
  | L (lab,left,right) -> L (lab, empty_copy_of_bsp left, empty_copy_of_bsp right)

let change_coul_with_id (bsp : 'a bsp) (num : int) (c : 'a) =
  let rec aux start num bsp =
    match bsp with
    | R _ as p -> (start+1,if start = num then R (Some c) else p)
    | L (a,l,r) ->
       let (n,l')  = aux start num l in
       let (n',r') = aux n num r in
       (n',L (a,l',r'))
  in snd (aux 0 num bsp)
