open Template
open File_handling
open Ocaml

let prefix s = "__eml_" ^ s
let nbuffer = prefix "buffer"
let npush = prefix "push"
let ncontinuation = prefix "continuation"
let eapp_continuation e = EApp (EVar ncontinuation, [e])
let epush e = EApp (EVar npush, [e])
let elist_rev e = EApp (EModuleField ["List"; "rev"], [e])
let estring_concat sep li = EApp (EModuleField ["String"; "concat"], [sep; li])

let esprintf format args =
  EApp (EModuleField ["Printf"; "sprintf"], format :: args)

let compile_to_expr ((args, elements) : Template.t) : Ocaml.expr =
  let header e =
    let defs =
      [ (PVar nbuffer, ERef (ELitList []))
      ; ( PVar npush
        , EFun
            ( [PVar "e"]
            , EAssignToRef
                (EVar nbuffer, ECons ("(::)", [EVar "e"; EDeRef (EVar nbuffer)]))
            ) ) ] in
    EOpenModule
      ( "Stdlib"
      , if not (CCString.is_empty args) then
          EFun ([PPrimitive args], ELet (defs, e))
        else ELet (defs, e) ) in
  let footer =
    estring_concat (ELitString "") (elist_rev (EDeRef (EVar nbuffer))) in
  let ele_to_expr : elt -> mixed = function
    | Text s -> MiUnit (epush (ELitString s))
    | Code s -> MiPrimitive s
    | Output {format; code; escape= _} ->
        let format = Option.value ~default:"%s" format in
        MiUnit (epush (esprintf (ELitString format) [EPrimitive code])) in
  let body = EMixedSequence (List.map ele_to_expr elements, footer) in
  header body

let compile_to_string template =
  Ocaml_printer.expr_to_string (compile_to_expr template)

let compile_to_expr_continuation ((args, elements) : Template.t) : expr =
  let header e =
    EOpenModule ("Stdlib", EFun ([PPrimitive args; PVar ncontinuation], e))
  in
  let ele_to_expr : elt -> mixed = function
    | Text s -> MiUnit (eapp_continuation (ELitString s))
    | Code s -> MiPrimitive s
    | Output {format; code; escape= _} ->
        let format = Option.value ~default:"%s" format in
        MiUnit
          (eapp_continuation (esprintf (ELitString format) [EPrimitive code]))
  in
  header @@ EMixedSequence (List.map ele_to_expr elements, EUnit)

let compile ?(continuation_mode = false) name t =
  let compile =
    if continuation_mode then compile_to_expr_continuation else compile_to_expr
  in
  (PVar name, compile t)

let compile_folder ?(continuation_mode = false) folder_name =
  let directory =
    read_file_or_directory
      ~filter:(fun filename -> Filename.check_suffix filename ".eml")
      ~sorted:true folder_name in
  let rec aux current_file =
    match current_file with
    | File filename -> (
        let name = Filename.chop_extension filename in
        let function_name = Filename.basename name in
        match Template_builder.of_filename filename with
        | Template template ->
            let pat, expr = compile ~continuation_mode function_name template in
            SIDef (pat, expr)
        | Error lexbuf ->
            Template_builder.handle_syntax_error lexbuf ;
            exit 1 )
    | Directory (name, files) ->
        let module_name = String.capitalize_ascii (Filename.basename name) in
        let struct_items = Array.to_list (Array.map aux files) in
        let struct_items = force_mutual_recursion struct_items in
        SIModule (module_name, MStruct struct_items) in
  match directory with
  | File _ ->
      if Filename.check_suffix folder_name ".eml" then
        let name = Filename.chop_extension folder_name ^ ".ml" in
        match Template_builder.of_filename folder_name with
        | Template template ->
            let pattern, value = compile ~continuation_mode "render" template in
            let defs = [SIDef (pattern, value)] in
            CCIO.with_out name (fun chan ->
                Ocaml_printer.print_program chan defs )
        | Error lexbuf -> Template_builder.handle_syntax_error lexbuf
      else assert false
  | Directory (_, files) ->
      let program = Array.to_list @@ Array.map aux files in
      CCIO.with_out (folder_name ^ ".ml") (fun chan ->
          Ocaml_printer.print_program chan program )
