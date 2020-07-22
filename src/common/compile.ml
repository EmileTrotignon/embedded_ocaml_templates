open Core
open Template
open File_handling

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

let compile_to_expr_continuation (args, elements) =
  let codes = ref [] in
  let append e = codes := e :: !codes in
  append (sprintf {|Core.(fun %s ___continuation ->|} args);
  List.iter elements ~f:(fun ele ->
      match ele with
      | Text s -> append (sprintf {| ___continuation {___|%s|___} ; |} s)
      | Code s -> append s
      | Output_code s -> append (sprintf {| ___continuation (%s) ; |} s));
  append {| ) |};
  String.concat (List.rev !codes)

let compile ?(continuation_mode = false) ?(and_instead_of_let = false) name
    header (args, elements) =
  sprintf {|%s
            %s %s = |} header
    (if and_instead_of_let then "and" else {|let [@warning "-39"] rec|})
    name
  ^ (if continuation_mode then compile_to_expr_continuation else compile_to_expr)
      (args, elements)

let compile_to_module ?(continuation_mode = false) template =
  compile ~continuation_mode "render" "open Core" template

let compile_to_function ?(continuation_mode = false)
    ?(and_instead_of_let = false) name template =
  compile ~continuation_mode ~and_instead_of_let name "" template

let compile_folder ?(continuation_mode = false) folder_name =
  let directory =
    read_file_or_directory
      ~filter:(fun filename -> Filename.check_suffix filename ".eml")
      ~sorted:true folder_name
  in
  let rec aux first_file_seen_ref current_file =
    match current_file with
    | File filename -> (
        let name = Filename.chop_extension filename in
        let function_name = List.last_exn (Filename.parts name) in
        match Template_builder.of_filename filename with
        | Some template ->
            compile_to_function ~continuation_mode
              ~and_instead_of_let:
                ( if not !first_file_seen_ref then (
                  first_file_seen_ref := true;
                  false )
                else true )
              function_name template
        | None -> failwith "Syntax error" )
    | Directory (name, files) ->
        let module_name =
          String.capitalize (List.last_exn (Filename.parts name))
        in
        sprintf " module %s = struct\n" module_name
        ^ (let first_file_seen = ref false in
           String.concat_array (Array.map ~f:(aux first_file_seen) files))
        ^ "\nend\n"
  in
  match directory with
  | File _ ->
      if Filename.check_suffix folder_name ".eml" then
        let name = Filename.chop_extension folder_name ^ ".ml" in
        match Template_builder.of_filename folder_name with
        | Some template ->
            Out_channel.write_all name
              ~data:(compile_to_module ~continuation_mode template)
        | None -> ()
      else ()
  | Directory (_, files) ->
      let first_file_seen = ref false in

      let content =
        String.concat_array (Array.map ~f:(aux first_file_seen) files)
      in
      Out_channel.write_all (folder_name ^ ".ml") ~data:content
