open Core

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let continuation_param =
  let open Command.Param in
  flag "continuation" no_arg ~doc:"Enable continuation mode"

let command =
  let open Command.Let_syntax in
  Command.basic ~summary:"Generate an OCaml source file from a template"
    ~readme:(fun () -> "More detailed information")
    [%map_open
      let continuation = continuation_param and filename = filename_param in
      fun () ->
        Common_eml.Compile.compile_folder ~continuation_mode:continuation
          filename]

let () = Command.run ~version:"0.2" ~build_info:"RWO" command
