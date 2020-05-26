open Core

let compile_and_output_single_file filename =
  let content = In_channel.read_all filename in
    let t = Template_builder.of_string content in
    match t with
    | Some template ->
        let output = Compile.compile_to_module template in
        
        Out_channel.write_all (fst (Filename.split_extension filename) ^ ".ml") ~data:output
    | None -> ()

let rec compile_and_output filename =
  match Sys.is_directory filename with
  | `Yes -> Array.iter ~f:compile_and_output (Sys.readdir filename)
  | `Unknown -> failwith (sprintf "Unknown file %s" filename)
  | `No -> compile_and_output_single_file filename
  
let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"Generate an OCaml source file from a template"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map filename_param ~f:(fun filename () ->
         compile_and_output filename))

let () = Command.run ~version:"1.0" ~build_info:"RWO" command
