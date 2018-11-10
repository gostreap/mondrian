open Graphics

type label =
  { coord : int;
    colored : bool;
  }

(* Commence avec une séparation verticale *)
type bsp = R of color option
         | L of label * bsp * bsp

type point = (int * int)

type linetree =
  | Leef
  | Line of point * point * color option * linetree * linetree

(* Affichage rudimentaire de BSP *)
let rec string_of_bsp (bsp : bsp) =
  match bsp with
  | R x ->
     begin
       match x with
         None -> "None"
       | Some x -> if x = red then "red" else "blue"
     end
  | L (lab,l,r) ->
     "(" ^ (string_of_bsp l) ^ ") " ^ (string_of_int (lab.coord)) ^
         " (" ^ (string_of_bsp r) ^ ")"

(* Génère un BSP aléatoire de profondeur 'profondeur'

NOTE: Pour l'instant, toutes les arrêtes sont visibles

 *)
let rec random_bsp_naive ?(v=true) ?(minsize=20) ?(start_larg=0)
                         ?(start_haut=0) (prof : int) (larg : int) (haut : int) =
  let red2 = rgb 211 19 2 in
  if prof = 0 || larg-start_larg <= minsize*2 || haut-start_haut <= minsize*2
  then R (Some (if Random.bool () then blue else red2))
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
    | R c ->
       begin
         match c with
         | None ->   R (Some blue)
         | Some c -> R (Some (if c = blue then red else blue))
       end
    | L (lab, left, right) ->
       if v
       then if x < lab.coord
            then L (lab, change_color ~v:(not v) left p, right)
            else L (lab, left, change_color ~v:(not v) right p)
       else if y < lab.coord
            then L (lab, change_color ~v:(not v) left p, right)
            else L (lab, left, change_color ~v:(not v) right p)

let rec compare_bsp (bsp1 : bsp) (bsp2 : bsp) =
  match bsp1 with
  | R x ->
     begin
       match bsp2 with
         R y -> x = y
       | _ -> false
     end
  | L (_,x,y) ->
     begin
       match bsp2 with
         L (_,x',y') -> compare_bsp x x' && compare_bsp y y'
       | _ -> false
     end

(*
Retourne un couple (r,b) où:
 * r est le nombre de rectangle rouge adjacents
 * y est le nombre de rectangle bleu adjacents
 *)
let get_coul_sum (bsp : bsp) =
  let rec get_coul ?(v=true) (is_l : bool) (bsp : bsp) =
    match bsp with
    | R x ->
       begin
         match x with
           None -> (0,0)
         | Some x ->
            if x = blue then (0,1) else (1,0)
       end
    | L (_,x,y) ->
       let rx,bx as x' = get_coul ~v:(not v) is_l x in
       let ry,by as y' = get_coul ~v:(not v) is_l y in
       if not v
       then if is_l then y' else x'
       else (rx+ry,bx+by)
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
      Some yellow
    else if r < b then
      Some blue
    else
      Some red

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
