open Template
open File_handling
open Ocaml.Builder
open Ocaml.Primitive

let prefix s = "__eml_" ^ s
let n_escape = prefix "escape"
let n_buffer = prefix "buffer"
let n_push = prefix "push"
let n_continuation = prefix "continuation"
let e_app_continuation e = e_app (e_var n_continuation) [e]
let e_push e = e_app (e_var n_push) [e]
let e_list_rev e = e_app (e_module_field ["List"; "rev"]) [e]

let e_string_concat sep li =
  e_app (e_module_field ["String"; "concat"]) [sep; li]

let e_string_length e = e_app (e_module_field ["String"; "length"]) [e]
let e_string_iter f s = e_app (e_module_field ["String"; "iter"]) [f; s]
let e_buffer_create e = e_app (e_module_field ["Buffer"; "create"]) [e]

let e_buffer_add_string buf s =
  e_app (e_module_field ["Buffer"; "add_string"]) [buf; s]

let e_buffer_add_char buf c =
  e_app (e_module_field ["Buffer"; "add_char"]) [buf; c]

let e_buffer_contents buf = e_app (e_module_field ["Buffer"; "contents"]) [buf]
let e_app_escape e = e_app (e_var n_escape) [e]

(* d, i: convert an integer argument to signed decimal.
   u, n, l, L, or N: convert an integer argument to unsigned decimal. Warning: n, l, L, and N are used for scanf, and should not be used for printf.
   x: convert an integer argument to unsigned hexadecimal, using lowercase letters.
   X: convert an integer argument to unsigned hexadecimal, using uppercase letters.
   o: convert an integer argument to unsigned octal.
   d, i: convert an integer argument to signed decimal.
   u, n, l, L, or N: convert an integer argument to unsigned decimal. Warning: n, l, L, and N are used for scanf, and should not be used for printf.
   x: convert an integer argument to unsigned hexadecimal, using lowercase letters.
   X: convert an integer argument to unsigned hexadecimal, using uppercase letters.

   o: convert an integer argument to unsigned octal.
   s: insert a string argument.
   S: convert a string argument to OCaml syntax (double quotes, escapes).
   c: insert a character argument.
   C: convert a character argument to OCaml syntax (single quotes, escapes).
   f: convert a floating-point argument to decimal notation, in the style dddd.ddd.
   F: convert a floating-point argument to OCaml syntax (dddd. or dddd.ddd or d.ddd e+-dd).
   e or E: convert a floating-point argument to decimal notation, in the style d.ddd e+-dd (mantissa and exponent).
   g or G: convert a floating-point argument to decimal notation, in style f or e, E (whichever is more compact). Moreover, any trailing zeros are removed from the fractional part of the result and the decimal-point character is removed if there is no fractional part remaining.
   h or H: convert a floating-point argument to hexadecimal notation, in the style 0xh.hhhh e+-dd (hexadecimal mantissa, exponent in decimal and denotes a power of 2).
   B: convert a boolean argument to the string true or false
   b: convert a boolean argument (deprecated; do not use in new programs).
   ld, li, lu, lx, lX, lo: convert an int32 argument to the format specified by the second letter (decimal, hexadecimal, etc).
   nd, ni, nu, nx, nX, no: convert a nativeint argument to the format specified by the second letter.
   Ld, Li, Lu, Lx, LX, Lo: convert an int64 argument to the format specified by the second letter.
   a: user-defined printer. Take two arguments and apply the first one to outchan (the current output channel) and to the second argument. The first argument must therefore have type out_channel -> 'b -> unit and the second 'b. The output produced by the function is inserted in the output of fprintf at the current point.
   t: same as %a, but take only one argument (with type out_channel -> unit) and apply it to outchan.
   { fmt %}: convert a format string argument to its type digest. The argument must have the same type as the internal format string fmt.
   ( fmt %): format string substitution. Take a format string argument and substitute it to the internal format string fmt to print following arguments. The argument must have the same type as the internal format string fmt.
   !: take no argument and flush the output.
   %: take no argument and output one % character.
   @: take no argument and output one @ character.
   ,: take no argument and output nothing: a no-op delimiter for conversion specifications
*)

let type_of_format = function
  | "d" | "i" | "u" | "n" | "L" | "N" | "x" | "X" | "o" -> Some (t_name "int")
  | "s" | "S" -> Some (t_name "string")
  | "c" | "C" -> Some (t_name "char")
  | "f" | "F" | "e" | "E" | "g" | "G" | "h" | "H" -> Some (t_name "float")
  | "B" | "b" -> Some (t_name "bool")
  | "nd" | "ni" | "nu" | "nx" | "nX" | "no" -> Some (t_name "nativeint")
  | "Ld" | "Li" | "Lu" | "Lx" | "LX" | "Lo" -> Some (t_name "int64")
  | "t" -> Some (t_arrow ([t_name "unit"] ^-> t_name "string"))
  | _ -> None

(* for reference *)
let _escape s =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#x27;"
      | c -> Buffer.add_char buffer c)
    s ;
  Buffer.contents buffer

let eescape =
  let ns = prefix "s" in
  let nc = prefix "c" in
  let e_add_string s = e_buffer_add_string (e_var n_buffer) (e_lit_string s) in
  e_fun @@ [p_var ns]
  ^-> e_let [p_var n_buffer ^= e_buffer_create (e_string_length (e_var ns))]
  @@ e_sequence
       [ e_string_iter
           (e_function
              [ p_char '&' ^-> e_add_string "&amp"
              ; p_char '<' ^-> e_add_string "&lt"
              ; p_char '>' ^-> e_add_string "&gt"
              ; p_char '"' ^-> e_add_string "&quot"
              ; p_char '\'' ^-> e_add_string "&#x27"
              ; p_var nc ^-> e_buffer_add_char (e_var n_buffer) (e_var nc) ])
           (e_var ns) ]
  @@ e_buffer_contents (e_var n_buffer)

let esprintf format args =
  e_app (e_module_field ["Printf"; "sprintf"]) (format :: args)

let compile_to_expr ((args, elements) : Template.t) : Ocaml.expr =
  let header e =
    let defs =
      [ (p_var n_buffer, e_ref e_empty_list)
      ; ( p_var n_push
        , e_fun @@ [p_var "e"]
          ^-> e_assign_to_ref (e_var n_buffer)
                (e_li_cons (e_var "e") (e_deref (e_var n_buffer))) ) ] in
    e_open_module "Stdlib"
    @@
    if not (CCString.is_empty args.code) then
      e_fun ([p_prim args] ^-> e_let defs e)
    else e_let defs e in
  let footer =
    e_string_concat e_empty_string (e_list_rev (e_deref (e_var n_buffer))) in
  let ele_to_expr : elt -> Ocaml.mixed = function
    | Text s -> mix_unit (e_push (e_lit_string s))
    | Code s -> mix_prim s
    | Output {format; code; escape} ->
        let eescape = if escape then e_app_escape else Fun.id in
        let format = Option.value ~default:"s" format in
        let type_ = type_of_format format in
        let format = "%" ^ format in
        mix_unit
          ( match type_ with
          | None ->
              e_push (eescape @@ esprintf (e_lit_string format) [e_prim code])
          | Some type_ ->
              e_push
                ( eescape
                @@ esprintf (e_lit_string format) [e_prim code ^: type_] ) )
  in
  let body = e_mixed_seq (List.map ele_to_expr elements) footer in
  header body

let compile_to_string template =
  Ocaml.Printer.expr_to_string
    (e_let [p_var n_escape ^= eescape] @@ compile_to_expr template)

let compile_to_expr_continuation ((args, elements) : Template.t) : Ocaml.expr =
  let header e =
    e_open_module "Stdlib" (e_fun @@ [p_prim args; p_var n_continuation] ^-> e)
  in
  let ele_to_expr : elt -> Ocaml.mixed = function
    | Text s -> mix_unit (e_app_continuation (e_lit_string s))
    | Code s -> mix_prim s
    | Output {format; code; escape} ->
        let eescape = if escape then e_app_escape else Fun.id in
        let format = Option.value ~default:"s" format in
        let type_ = type_of_format format in
        let format = "%" ^ format in
        mix_unit
          ( match type_ with
          | None ->
              e_app_continuation @@ eescape
              @@ esprintf (e_lit_string format) [e_prim code]
          | Some type_ ->
              e_app_continuation @@ eescape
              @@ esprintf (e_lit_string format) [e_prim code ^: type_] ) in
  header @@ e_mixed_seq (List.map ele_to_expr elements) e_unit

let compile ?(continuation_mode = false) name t =
  let compile =
    if continuation_mode then compile_to_expr_continuation else compile_to_expr
  in
  (p_var name, compile t)

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
            si_def (pat ^= expr)
        | Error lexbuf ->
            Template_builder.handle_syntax_error lexbuf ;
            exit 1 )
    | Directory (name, files) ->
        let module_name = String.capitalize_ascii (Filename.basename name) in
        let struct_items = Array.to_list (Array.map aux files) in
        let struct_items = Ocaml.Transform.force_mutual_recursion struct_items in
        si_module @@ module_name ^= m_struct struct_items in
  match directory with
  | File _ ->
      if Filename.check_suffix folder_name ".eml" then
        let name = Filename.chop_extension folder_name ^ ".ml" in
        match Template_builder.of_filename folder_name with
        | Template template ->
            let pattern, value = compile ~continuation_mode "render" template in
            let defs =
              [si_def (p_var n_escape ^= eescape); si_def (pattern ^= value)]
            in
            CCIO.with_out name (fun chan ->
                Ocaml.Printer.print_program chan defs)
        | Error lexbuf -> Template_builder.handle_syntax_error lexbuf
      else assert false
  | Directory (_, files) ->
      let program =
        si_def (p_var n_escape ^= eescape)
        :: (Array.to_list @@ Array.map aux files) in
      CCIO.with_out (folder_name ^ ".ml") (fun chan ->
          Ocaml.Printer.print_program chan program)
