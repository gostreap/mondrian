open Bsp
open Couleur
open Utils

(* bsp for SAT *)
type 'a bsp_sat =
  | R_sat of int * bool * 'a option (* id * secure * coul *)
  | L_sat of 'a couleur_l option * bool * 'a bsp_sat * 'a bsp_sat (* coul * secure * left * right *)

let rec string_of_bsp_sat (bsp : [< `Red | `Green | `Blue] bsp_sat) =
  match bsp with
  | L_sat (coul,b,l,r) ->
     "(" ^ (string_of_bsp_sat l) ^ " " ^ string_of_bool b  ^ "*" ^
         (maybe "black" string_of_couleur_l coul) ^ " " ^ (string_of_bsp_sat r) ^ ")"
  | R_sat (n,x,c) -> string_of_int n ^ "*" ^ string_of_bool x ^ "*" ^
                        (maybe "None" (switch_coul "r" "g" "b") c)

let bsp_sat_of_bsp (get_col_line : 'a bsp -> 'a couleur_l option) (bsp : 'a bsp) : ('a bsp_sat) =
  let rec aux v bsp  =
    match bsp with
    | R x ->
       (v+1,R_sat (v,false, x))
    | L (lab,l,r) ->
       let (n,ll) = aux v l in
       let (m,rr) = aux n r in
       let c =
         if lab.colored
         then get_col_line bsp
         else None in
       (m,L_sat (c,false,ll,rr))
  in snd (aux 0 bsp)

let rec color_bsp_sat_line (bsp_sat : [< `Blue | `Red | `Green] bsp_sat) (linetree : [< `Blue | `Red | `Green] linetree) =
  match bsp_sat, linetree with
  | R_sat (_,_,_) , _ -> bsp_sat
  | L_sat (_,s,lbs,rbs), Line (_,_,c, ll,rl) ->
     let lbs_sat = color_bsp_sat_line lbs ll in
     let rbs_sat = color_bsp_sat_line rbs rl in
     L_sat(c,s,lbs_sat,rbs_sat)
  | _ -> failwith "Error color_bsp_sat_line"

let bsp_sat_of_working_bsp (working_bsp : [< `Blue | `Red | `Green] bsp) (linetree :  [< `Blue | `Red | `Green] linetree) =
  let rec aux v bsp =
    match bsp with
    | L (_,l,r) ->
       let (n,ll) = aux v l in
       let (m,rr) = aux n r in
       (m,L_sat (None, false, ll, rr))
    | R x ->
       match x with
         | None -> (v+1, R_sat(v, false, None))
         | Some x -> (v+1, R_sat(v, true, Some x))
  in  color_bsp_sat_line (snd (aux 0 working_bsp)) linetree

(* Renvoie un bsp ou les feuilles ont un indice différent de 1 si leur couleur est fixé *)
let rec secure_bsp_sat (bsp :  [< `Red | `Green | `Blue] bsp_sat) =
  match bsp with
  | R_sat _ as r -> r
  | L_sat (c,_,l,r) ->
      let isnone = L_sat (c,false,secure_bsp_sat l,secure_bsp_sat r) in
      if maybe false (switch_coul_l false false false false (fun _ -> true)) c
      then
        match l, r with
        | R_sat (n,_,co), R_sat (m,_,_) -> L_sat (c,true,R_sat (n,true,co),R_sat (m,true,co))
        | L_sat (_,b,_,_) as l, (L_sat (_,b',_,_) as r) ->
           if b && b'
           then L_sat (c,true,l,r)
           else isnone
        | _ -> isnone
      else isnone

let rec loop_sat (n : int) (b : [< `Red | `Green | `Blue] bsp_sat) =
  if n <= 0 then b
  else loop_sat (n-1) (secure_bsp_sat b)

(* Renvoie un couple (r, g, b, list) où:
 * r est le nombre de rectangle rouge adjacents sécurisés
 * g est le nombre de rectangle vert adjacents sécurisés
 * b est le nombre de rectangle bleu adjacents sécurisés
 * list est la liste des rectangles non sécurisés*)
let get_adja_stat (bsp_sat :  [< `Red | `Green | `Blue] bsp_sat) =
  (* print_endline "début adja stat"; *)
  let rec get_stat v (is_l : bool) (bsp_sat :  [< `Red | `Green | `Blue] bsp_sat)
          : (int * int * int * int list) =
    match bsp_sat with
    | L_sat (_,_,x,y) ->
       let rx,gx,bx,ll as x' = get_stat (not v) is_l x in
       let ry,gy,by,lr as y' = get_stat (not v) is_l y in
       if not v
       then if is_l then y' else x'
       else (rx+ry,gx+gy,bx+by,ll@lr)
    | R_sat (n,s,c) ->
       if s then
         let (r,g,b) = maybe (0,0,0) (switch_coul (1,0,0) (0,1,0) (0,0,1)) c in
         (r,g, b, [])
       else (0,0,0,[n])
  in
  match bsp_sat with
  | R_sat _ -> (0,0,0,[])
  | L_sat (_,_,l,r) ->
     let (lr,lg,lb,llist) = get_stat true true l in
     let (rr,rg,rb,rlist) = get_stat true false r in
     (lr+rr,lg+rg,lb+rb,llist@rlist)

let check_line (line :  [< `Red | `Green | `Blue] bsp_sat) =
  let r,g,b,list = get_adja_stat line in
  let size = r + g + b + List.length list in
  match line with
  | R_sat _ -> true
  | L_sat (c,_,_,_) ->
     match c with
     | None -> true
     | Some Purple -> r <= size/2 && (g < size/3 || g = 0) && b <= size/2
     | Some Yellow -> r <= size/2 && g <= size/2 && (b < size/3 || b = 0)
     | Some Cyan -> (r < size/3 || r = 0) && g <= size/2 && b <= size/2
     | Some White -> r <= size/3 && g <= size/3 && b <= size/3
     | Some (C co) ->
        let borne = if size mod 2 = 1 then size/2+1 else size/2 in
        match co with
        | `Red -> g < borne && b < borne
        | `Green -> r < borne && b < borne
        | `Blue -> r < borne && g < borne

let rec check_all_lines (bsp_sat : [< `Red | `Green | `Blue] bsp_sat) : bool =
  if not (check_line bsp_sat) then false
  else
    match bsp_sat with
    | R_sat _ -> true
    | L_sat (_,_,l,r) -> check_all_lines l && check_all_lines r
