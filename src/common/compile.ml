open Core
open Template

type file = File of string | Directory of (string * file array)

let rec print_file file =
  match file with
  | File f -> printf "File %s\n" f
  | Directory (s, fa) ->
      printf "File %s (\n" s;
      Array.iter fa ~f:print_file;
      print_endline ")"

let path_readdir dirname =
  Array.map ~f:(Filename.concat dirname) (Sys.readdir dirname)

let rec read_file_or_directory ?(filter = fun _ -> true) filename =
  match Sys.is_directory filename with
  | `Yes ->
      Directory
        ( filename,
          Array.map
            ~f:(fun file ->
              match file with
              | File name -> File name
              | Directory (name, files) -> Directory (name, files))
            (Array.filter
               ~f:(fun file ->
                 match file with File s -> filter s | Directory _ -> true)
               (Array.map
                  ~f:(read_file_or_directory ~filter)
                  (Array.map ~f:(Filename.concat filename)
                     (Sys.readdir filename)))) )
  | `No -> (
      match Sys.file_exists filename with
      | `Yes -> File filename
      | `No -> failwith (sprintf "Unknown file %s" filename)
      | `Unknown -> failwith (sprintf "Unknown file %s" filename) )
  | `Unknown -> failwith (sprintf "Unknown file %s" filename)

let compile_to_expr (args, elements) =
  let codes = ref [] in
  let append e = codes := e :: !codes in

  if not (String.is_empty args) then
    append
      (sprintf
         {|
        Core.(fun %s ->
          let ___elements = ref [] in
          let ___append e =
            ___elements := e :: !___elements
          in
        |}
         args)
  else
    append
      {| Core.(let ___elements = ref [] in
          let ___append e =
            ___elements := e :: !___elements
          in |};
  List.iter elements ~f:(fun ele ->
      match ele with
      | Text s -> append (sprintf {| ___append {___|%s|___} ;|} s)
      | Code s -> append s
      | Output_code s -> append (sprintf {| ___append (%s) ;|} s));
  append {|
  String.concat (List.rev !___elements) )
  |};
  String.concat (List.rev !codes)

let compile name header (args, elements) =
  sprintf {|%s
            let %s = |} header name
  ^ compile_to_expr (args, elements)

let compile_to_module template = compile "render" "open Core" template

let compile_to_function name template = compile name "" template

let compile_folder folder_name =
  let directory =
    read_file_or_directory
      ~filter:(fun filename -> Filename.check_suffix filename ".eml")
      folder_name
  in
  let rec aux current_file =
    match current_file with
    | File filename -> (
        let name = Filename.chop_extension filename in
        let function_name = List.last_exn (Filename.parts name) in
        match Template_builder.of_filename filename with
        | Some template -> compile_to_function function_name template
        | None -> failwith "Syntax error" )
    | Directory (name, files) ->
        let module_name =
          String.capitalize (List.last_exn (Filename.parts name))
        in
        sprintf " module %s = struct\n" module_name
        ^ String.concat_array (Array.map ~f:aux files)
        ^ "\nend\n"
  in
  match directory with
  | File _ ->
      if Filename.check_suffix folder_name ".eml" then
        let name = Filename.chop_extension folder_name ^ ".ml" in
        match Template_builder.of_filename folder_name with
        | Some template ->
            Out_channel.write_all name ~data:(compile_to_module template)
        | None -> ()
      else ()
  | Directory (_, files) ->
      let content = String.concat_array (Array.map ~f:aux files) in
      Out_channel.write_all (folder_name ^ ".ml") ~data:content
