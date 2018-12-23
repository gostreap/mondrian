val maybe : 'b -> ('a -> 'b) -> 'a option -> 'b
val fmap : ('a -> 'b) -> 'a option -> 'b option
val genl : int -> int -> int list
val maybe2 : ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option
val print_message : string -> unit
val clean_message : unit -> unit
