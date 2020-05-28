open Core
open Template

let path_readdir dirname =
  Array.map ~f:(Filename.concat dirname) (Sys.readdir dirname)

let compile name header (args, elements) =
  let codes =
    ref
      [
        sprintf
          {|
        %s
        let %s %s =
          let ___elements = ref [] in
          let ___append e =
            ___elements := e :: !___elements
          in
        |}
          header name args;
      ]
  in
  let append e = codes := e :: !codes in
  List.iter elements ~f:(fun ele ->
      match ele with
      | Text s -> append (sprintf {|___append {___|%s|___} ;|} s)
      | Code s -> append s
      | Output_code s -> append (sprintf {|___append (%s) ;|} s));
  append {|
  String.concat (List.rev !___elements)
  |};
  String.concat (List.rev !codes)

let compile_to_module template = compile "render" "open Core" template

let compile_to_function name template = compile name "" template

let compile_folder folder_name =
  let rec aux current_file =
    match Sys.is_directory current_file with
    | `Yes ->
        let module_name_bytes =
          Bytes.of_string (List.last_exn (Filename.parts current_file))
        in
        Bytes.set module_name_bytes 0
          (Char.uppercase (Bytes.get module_name_bytes 0));
        let module_name = Bytes.to_string module_name_bytes in
        sprintf "module %s = struct\n" module_name
        ^ String.concat_array (Array.map ~f:aux (path_readdir current_file))
        ^ "\nend\n"
    | `No ->
        if Filename.check_suffix current_file ".eml" then
          let name = Filename.chop_extension current_file in
          let function_name = List.last_exn (Filename.parts name) in
          match Template_builder.of_filename current_file with
          | Some template -> compile_to_function function_name template
          | None -> ""
        else ""
    | `Unknown -> failwith "Unknown file"
  in
  match Sys.is_directory folder_name with
  | `Yes ->
      let content =
        String.concat_array (Array.map ~f:aux (path_readdir folder_name))
      in
      Out_channel.write_all (folder_name ^ ".ml") ~data:("open Core" ^ content)
  | `No ->
      if Filename.check_suffix folder_name ".eml" then
        let name = Filename.chop_extension folder_name ^ ".ml" in
        match Template_builder.of_filename folder_name with
        | Some template ->
            Out_channel.write_all name ~data:(compile_to_module template)
        | None -> ()
      else ()
  | `Unknown -> failwith "Unknown file"
