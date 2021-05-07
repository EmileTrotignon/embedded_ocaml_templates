
(* Compile a single template and outputs a string containing *)
val compile_to_string : Template.t -> string

val compile_folder : ?continuation_mode:bool -> string -> unit
