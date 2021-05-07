open Ppxlib

let name = "eml"

let expand ~loc ~path:_ (s : string) =
  let startpos = loc.loc_start in
  (* let startpos = {loc.loc_start with pos_lnum= loc.loc_start.pos_lnum - 1} in *)
  match Common.Template_builder.of_string ~startpos s with
  | Error lexbuf ->
      Common.Template_builder.handle_syntax_error lexbuf ;
      exit 1
  | Template template ->
      let buffer =
        Lexing.from_string (Common.Compile.compile_to_string template) in
      Lexing.set_position buffer loc.loc_start ;
      Parser.parse_expression Lexer.token buffer

let ext =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let () = Ppxlib.Driver.register_transformation name ~extensions:[ext]
