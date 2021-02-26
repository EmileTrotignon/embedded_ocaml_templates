open Template
open File_handling

let compile_to_expr ((args, elements) : Template.t) =
  let codes = ref [] in
  let append e = codes := e :: !codes in

  if not (CCString.is_empty args) then
    append
      (Printf.sprintf
         {|
        Stdlib.(fun %s ->
          let ___elements = ref [] in
          let ___append e =
            ___elements := e :: !___elements
          in
        |}
         args)
  else
    append
      {| Stdlib.(let ___elements = ref [] in
          let ___append e =
            ___elements := e :: !___elements
          in |};
  ListLabels.iter elements ~f:(fun (ele : Template.elt) ->
      match ele with
      | Text s -> append (Printf.sprintf {| ___append {___|%s|___} ;|} s)
      | Code s -> append s
      | Output_code s -> append (Printf.sprintf {| ___append (%s) ;|} s)
      | Output_format (format, code) ->
          append
            (Printf.sprintf {| ___append (Printf.sprintf {___|%%%s|___} %s) ; |}
               format code));
  append {|
  String.concat "" (List.rev !___elements) )
  |};
  String.concat "" (List.rev !codes)

let compile_to_expr_continuation ((args, elements) : Template.t) =
  let codes = ref [] in
  let append e = codes := e :: !codes in
  append (Printf.sprintf {|Stdlib.(fun %s ___continuation ->|} args);
  ListLabels.iter elements ~f:(fun (ele : Template.elt) ->
      match ele with
      | Text s -> append (Printf.sprintf {| ___continuation {___|%s|___} ; |} s)
      | Code s -> append s
      | Output_code s -> append (Printf.sprintf {| ___continuation (%s) ; |} s)
      | Output_format (format, code) ->
          append
            (Printf.sprintf
               {| ___continuation (Printf.sprintf {___|%%%s|___} %s) ; |}
               format code));
  append {| ) |};
  String.concat "" (List.rev !codes)

let compile ?(continuation_mode = false) ?(and_instead_of_let = false) name
    header (args, elements) =
  Printf.sprintf
    {|%s
            %s %s = |}
    header
    (if and_instead_of_let then "and" else {|let [@warning "-39"] rec|})
    name
  ^ (if continuation_mode then compile_to_expr_continuation else compile_to_expr)
      (args, elements)

let compile_to_module ?(continuation_mode = false) template =
  compile ~continuation_mode "render" "" template

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
        let function_name = Filename.basename name in
        match Template_builder.of_filename filename with
        | Template template ->
            compile_to_function ~continuation_mode
              ~and_instead_of_let:
                ( if not !first_file_seen_ref then (
                  first_file_seen_ref := true;
                  false )
                else true )
              function_name template
        | Error lexbuf ->
            Template_builder.handle_syntax_error lexbuf;
            exit 1 )
    | Directory (name, files) ->
        let module_name = String.capitalize_ascii (Filename.basename name) in
        Printf.sprintf " module %s = struct\n" module_name
        ^ (let first_file_seen = ref false in
           String.concat "" (Array.to_list
            (ArrayLabels.map ~f:(aux first_file_seen) files)))
        ^ "\nend\n"
  in
  match directory with
  | File _ ->
      if Filename.check_suffix folder_name ".eml" then
        let name = Filename.chop_extension folder_name ^ ".ml" in
        match Template_builder.of_filename folder_name with
        | Template template ->
            CCIO.with_out name (fun chan -> output_string chan
              (compile_to_module ~continuation_mode template))
        | Error lexbuf -> Template_builder.handle_syntax_error lexbuf
      else ()
  | Directory (_, files) ->
      let first_file_seen = ref false in
      let content =
        String.concat "" (Array.to_list
          (ArrayLabels.map ~f:(aux first_file_seen) files))
      in
      CCIO.with_out (folder_name ^ ".ml")
        (fun chan -> output_string chan content)
