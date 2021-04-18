let continuation_mode = ref false
let filename = ref None

let args =
  Arg.align
    [ ("-continuation", Arg.Set continuation_mode, " Enable continuation mode")
    ; ( "-build-info"
      , Arg.Unit (fun () -> print_endline "RWO" ; exit 0)
      , " Print info about this build and exit" )
    ; ( "-version"
      , Arg.Unit (fun () -> print_endline "0.2" ; exit 0)
      , " Print the version of this build and exit" ) ]

let usage =
  {|Generate an OCaml source file from a template

  eml_compiler FILENAME

More detailed information

=== flags ===
|}

let () =
  Arg.parse args (fun s -> filename := Some s) usage ;
  let filename =
    match !filename with
    | Some s -> s
    | None ->
        prerr_endline "Missing required argument FILENAME" ;
        prerr_endline "For usage, run eml_compiler -help" ;
        exit 1 in
  let continuation_mode = !continuation_mode in
  Common_eml.Compile.compile_folder ~continuation_mode filename
