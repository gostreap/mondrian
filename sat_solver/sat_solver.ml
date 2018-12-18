(* Code extrait de:
   SAT-MICRO: petit mais costaud !
   par Sylvain Conchon, Johannes Kanig, Stéphane Lescuyer

   MODIFIÉ
*)

module type VARIABLES = sig
  type t
  val compare : t -> t -> int
end

module Make (V : VARIABLES) = struct

  type literal = bool * V.t

  module L = struct
    type t = literal
    let compare (b1,v1) (b2,v2) =
      let r = compare b1 b2 in
      if r = 0 then compare v1 v2
      else r

    let mk_not (b,v) = (not b, v)
  end

  module S = Set.Make(L)

  exception Unsat
  exception Sat of S.t

  type t = { gamma : S.t ; cl2 : L.t list list ; delta : L.t list list }

  let rec assume env f =
    if S.mem f env.gamma then env
    else bcp { env with gamma = S.add f env.gamma}

  and bcp env =
    (* TODO facto + décider dans quel ordre: start doit travailler sur les 2 cl ou sur delta ? *)
    let start =
      List.fold_left
        (fun env l ->
          try
            let l =
              List.filter
                (fun f ->
                  if S.mem f env.gamma then raise Exit;
                  not (S.mem (L.mk_not f) env.gamma)
                ) l
            in
            match l with
            | [] -> raise Unsat (* conflict *)
            | [f] -> assume env f
            | _ -> { env with cl2 = l :: env.cl2 } (* Ne peut pas être plus qu'une 2-clause *)
          with Exit -> env)
        {env with cl2 = []}
        env.cl2
    in
    List.fold_left
      (fun env l ->
        try
          let l = List.filter
              (fun f ->
                 if S.mem f env.gamma then raise Exit;
                 not (S.mem (L.mk_not f) env.gamma)
              ) l
          in
          match l with
          | [] -> raise Unsat (* conflict *)
          | [f] -> assume env f
          | [_;_] as p -> { env with cl2 = l :: env.cl2 }
          | _ -> { env with delta = l :: env.delta }
        with Exit -> env)
      { start with delta = [] }
      env.delta

  let rec unsat env = try
      (* 3 clauses *)
      match env.delta with
      | [] -> failwith "solve2 not made" (* raise (Sat env.gamma) *)
      | ([_] | []) :: _ -> assert false
      | (a :: xs) :: ys ->
         begin
           try
             unsat (assume {env with delta = ys} a)
           with Unsat -> ()
         end ;
        unsat (assume {env with cl2 = xs::env.cl2} (L.mk_not a))
    with Unsat -> ()

  let solve delta = try
      print_endline "SAT";
      print_endline (string_of_int (List.length delta));
      let b = bcp { gamma = S.empty ; cl2 = [];  delta } in
      print_endline (string_of_int (List.length b.delta));
      let c2,c3p = List.partition (fun x -> List.length x = 2) b.delta in
      let b =
        {b with
          cl2 = b.cl2 @ c2;
          delta = c3p
        } in
      print_endline "cc";
      unsat b ;
      None
    with
    | Sat g -> Some (S.elements g)
    | Unsat -> None

end
