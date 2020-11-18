open Parser.MenhirInterpreter
open Core
module S = MenhirLib.General

type error_or_template = Error of Sedlexing.lexbuf | Template of Template.t


let pp_pos out (pos_start, pos_end) =
  Ppxlib.(
    Format.fprintf out "from line %d:%d to line %d:%d in file %s"
      pos_start.pos_lnum
      (pos_start.pos_cnum - pos_start.pos_bol)
      pos_end.pos_lnum
      (pos_end.pos_cnum - pos_end.pos_bol)
      pos_start.pos_fname)

let handle_syntax_error lexbuf =
  let message = "Syntax error" in
  Format.fprintf Format.err_formatter "%s %a\n%!" message pp_pos
    (Sedlexing.lexing_positions lexbuf)

let rec loop next_token lexbuf (checkpoint : Template.t checkpoint) =
  match checkpoint with
  | InputNeeded _env ->
      let token = next_token () in
      let checkpoint = offer checkpoint token in
      loop next_token lexbuf checkpoint
  | Shifting _ | AboutToReduce _ ->
      let checkpoint = resume checkpoint in
      loop next_token lexbuf checkpoint
  | HandlingError _ -> Error lexbuf
  | Accepted template -> Template template
  | Rejected ->
      (* Cannot happen as we stop at syntax error immediatly *)
      assert false

let of_lexing_buffer lexbuf =
  let lexer = Lexer.lexer lexbuf in
  loop lexer lexbuf
    (Parser.Incremental.template (fst @@ Sedlexing.lexing_positions lexbuf))

let of_ustring ?(filename = "") ustring =
  of_lexing_buffer
    (let buffer = Sedlexing.from_uchar_array ustring in
     Sedlexing.set_filename buffer filename;
     buffer)

let of_string ?(filename = "") string =
  of_ustring ~filename (Ustring.of_string string)

let of_filename filename =
  let gen = Gen.of_array (Ustring.of_string @@ In_channel.read_all filename) in
  let buffer = Sedlexing.from_gen gen in
  Sedlexing.set_filename buffer filename;
  
  of_lexing_buffer buffer
