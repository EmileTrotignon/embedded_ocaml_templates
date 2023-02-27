module Parser_ = Parser
open Ppxlib

(* Not present before 4.11 *)
let set_position lexbuf position =
  Lexing.(
    lexbuf.lex_curr_p <- {position with pos_fname= lexbuf.lex_curr_p.pos_fname} ;
    lexbuf.lex_abs_pos <- position.pos_cnum )

let name = "eml"

let expand ~loc:_ ~path:_ (s : string) loc _delim =
  let startpos = loc.loc_start in
  (* Printf.printf "Position ppx : line %i char %i\n" startpos.pos_lnum
     (startpos.pos_cnum - startpos.pos_bol) ; *)
  (* let startpos = {loc.loc_start with pos_lnum= loc.loc_start.pos_lnum - 1} in *)
  match Common.Template_builder.of_string ~startpos s with
  | Error e ->
      Common.Lexer.pp_error Format.err_formatter e ;
      exit 1
  | Ok template ->
      let code = Common.Compile.compile_to_string template in
      (* print_endline code ; *)
      let buffer = Lexing.from_string code in
      set_position buffer loc.loc_start ;
      Parser_.parse_expression Lexer.token buffer
      |> Selected_ast.Of_ocaml.copy_expression

let ext =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ __)))
    expand

let () = Ppxlib.Driver.register_transformation name ~extensions:[ext]
