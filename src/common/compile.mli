
(** [compile_to_string template] compile a single template and outputs a string
    containing the compiled OCaml code for [template] *)
val compile_to_string : Template.t -> string


(** [compile_folder ~continuation_mode:cm filename] compiles a whole folder of
    templates and write to the corresponding OCaml file. 2*)
val compile_folder : ?continuation_mode:bool -> string -> unit
