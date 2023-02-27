type file = File of string | Directory of (string * file array)

val sort_by_int : 'a array -> to_int:('a -> 'b) -> unit

val print_file : file -> unit

val path_readdir : string -> string array

val read_file_or_directory :
  ?filter:(string -> bool) -> ?sorted:bool -> string -> file
