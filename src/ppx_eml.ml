open Ppxlib

let name = "eml"

let expand ~loc ~path:_ (s : string) =
  match Common_eml.Template_builder.of_string s with
  | Error lexbuf ->
      Common_eml.Template_builder.handle_syntax_error lexbuf ;
      exit 1
  | Template template ->
      let buffer =
        Lexing.from_string ~with_positions:false
          (Common_eml.Compile.compile_to_string template) in
      buffer.lex_curr_p <- loc.loc_start ;
      buffer.lex_start_p <- loc.loc_start ;
      Parser.parse_expression Lexer.token buffer

let ext =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let () = Ppxlib.Driver.register_transformation name ~extensions:[ext]
