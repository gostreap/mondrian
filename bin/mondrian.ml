open Graphics

let rec loop () =
  let e = wait_next_event  [ Mouse_motion ; Key_pressed ] in
  if e.keypressed
  then
    match e.key with
    | 'q' -> ()
    | _ -> loop ()
  else loop ()

let main () =
  open_graph " 500x500" ;
  loop ()

let _ = main ()
