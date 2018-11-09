open Graphics

type label =
  { coord : int;
    colored : bool;
  }

(* Commence avec une séparation verticale *)
type bsp = R of color option
         | L of label * bsp * bsp

type point = (int * int)

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
  if prof = 0 || larg-start_larg <= minsize*2 || haut-start_haut <= minsize*2
  then R (Some (if Random.bool () then blue else red))
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
Retourn un couple (r,b) où:

r est le nombre de rectangle rouge sur la partie gauche (resp droite)
b est le nombre de rectangle bleue sur la partie gauche (resp droite)

resp si is_l = false
 *)
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
     if v
     then if is_l then y' else x'
     else (rx+ry,bx+by)

(*
Vérifie si deux colorations sont équivalentes
 *)
let rec check_current (bsp1 : bsp) (bsp2 : bsp) =
  match bsp1 with
  | L (_,x,y) ->
     begin
       match bsp2 with
         L (_,x',y') ->
          let cond =
            let lx', ly' = get_coul true x', get_coul false y' in
            let lx, ly = get_coul true x, get_coul false y in
            let r',b' = fst lx' + fst ly' , snd lx' + snd ly' in
            let r ,b =  fst lx + fst ly , snd lx + snd ly in
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
