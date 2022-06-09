open Template
open File_handling
open Mocaml.Builder
module Prim = Mocaml.Primitive

let prefix s = "__eml_" ^ s

let e_escape = E.module_field ["EML_runtime"; "escape"]

let n_buffer = prefix "buffer"

let n_continuation = prefix "continuation"

let e_app_continuation e = E.(apply (var n_continuation) [e])

let stdlib_module_field fi = E.module_field ("Stdlib" :: fi)

(** Takes an expression representing an integer [n] and allocate a buffer of size [n] *)
let e_buffer_create e = E.apply (stdlib_module_field ["Buffer"; "create"]) [e]

(** add_string b s appends the string s at the end of buffer b. *)
let e_buffer_add_string buf s =
  E.apply (stdlib_module_field ["Buffer"; "add_string"]) [buf; s]

let e_buffer_contents buf =
  E.apply (stdlib_module_field ["Buffer"; "contents"]) [buf]

let e_app_escape e = E.apply e_escape [e]

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

let type_of_format =
  T.(
    function
    | "d" | "i" | "u" | "n" | "L" | "N" | "x" | "X" | "o" ->
        Some int
    | "s" | "S" ->
        Some string
    | "c" | "C" ->
        Some char
    | "f" | "F" | "e" | "E" | "g" | "G" | "h" | "H" ->
        Some float
    | "B" | "b" ->
        Some bool
    | "nd" | "ni" | "nu" | "nx" | "nX" | "no" ->
        Some nativeint
    | "Ld" | "Li" | "Lu" | "Lx" | "LX" | "Lo" ->
        Some int64
    | "t" ->
        Some (unit ^-> string)
    | _ ->
        None)

(* for reference *)
let _escape s =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' ->
          Buffer.add_string buffer "&amp;"
      | '<' ->
          Buffer.add_string buffer "&lt;"
      | '>' ->
          Buffer.add_string buffer "&gt;"
      | '"' ->
          Buffer.add_string buffer "&quot;"
      | '\'' ->
          Buffer.add_string buffer "&#x27;"
      | c ->
          Buffer.add_char buffer c )
    s ;
  Buffer.contents buffer

let esprintf format args =
  E.apply (stdlib_module_field ["Printf"; "sprintf"]) (format :: args)

let compile_to_expr ((args, elements) : Template.t) : Mocaml.expr =
  let header e =
    let defs = [(P.var n_buffer, e_buffer_create (E.lit_int 16))] in
    if not (Prim.is_empty args) then E.fun_ ([P.prim args] ^^-> E.let_ defs e)
    else E.let_ defs e
  in
  let footer = e_buffer_contents (E.var n_buffer) in
  let ele_to_expr : elt -> Mocaml.mixed = function
    | Text s ->
        Mixed.unit E.(e_buffer_add_string (var n_buffer) (lit_string s))
    | Code s ->
        Mixed.prim s
    | Output {format; code; escape} ->
        let eescape = if escape then e_app_escape else Fun.id in
        let format = Option.value ~default:"s" format in
        let type_ = type_of_format format in
        let format = "%" ^ format in
        Mixed.unit
          E.(
            match type_ with
            | None ->
                e_buffer_add_string (var n_buffer)
                  (eescape @@ esprintf (lit_string format) [prim code])
            | Some type_ ->
                e_buffer_add_string (var n_buffer)
                  (eescape @@ esprintf (lit_string format) [prim code ^: type_]))
  in
  let body = E.(mixed_seq (List.map ele_to_expr elements) footer) in
  header body

let compile_to_string template =
  Mocaml.Printer.expr_to_string (compile_to_expr template)

let compile_to_expr_continuation ((args, elements) : Template.t) : Mocaml.expr =
  let header e = E.fun_ @@ P.[prim args; var n_continuation] ^^-> e in
  let ele_to_expr : elt -> Mocaml.mixed = function
    | Text s ->
        Mixed.unit E.(e_app_continuation (lit_string s))
    | Code s ->
        Mixed.prim s
    | Output {format; code; escape} ->
        let eescape = if escape then e_app_escape else Fun.id in
        let format = Option.value ~default:"s" format in
        let type_ = type_of_format format in
        let format = "%" ^ format in
        Mixed.unit
          E.(
            match type_ with
            | None ->
                e_app_continuation @@ eescape
                @@ esprintf (lit_string format) [prim code]
            | Some type_ ->
                e_app_continuation @@ eescape
                @@ esprintf (lit_string format) [prim code ^: type_])
  in
  header @@ E.(mixed_seq (List.map ele_to_expr elements) unit)

let compile ?(continuation_mode = false) name t =
  let compile =
    if continuation_mode then compile_to_expr_continuation else compile_to_expr
  in
  (P.var name, compile t)

let is_eml_file filename =
  let extensions = filename |> String.split_on_char '.' |> List.rev in
  match extensions with
  | [] ->
      false
  | "eml" :: _ ->
      true
  | _ :: "eml" :: _ ->
      true
  | _ ->
      false

let eml_basename filename =
  filename |> String.split_on_char '.' |> List.rev
  |> (function
       | [] ->
           assert false
       | "eml" :: li ->
           li
       | _ :: "eml" :: li ->
           li
       | _ ->
           assert false )
  |> List.rev |> String.concat "."

let compile_folder ?(continuation_mode = false) folder_name =
  let directory =
    read_file_or_directory ~filter:is_eml_file ~sorted:true folder_name
  in
  let rec aux current_file =
    match current_file with
    | File filename -> (
        let name = eml_basename filename in
        let function_name = Filename.basename name in
        match Template_builder.of_filename filename with
        | Template template ->
            let pat, expr = compile ~continuation_mode function_name template in
            Some (SI.def (pat ^= expr))
        | Error lexbuf ->
            Template_builder.handle_syntax_error lexbuf ;
            exit 1 )
    | Directory (name, files) -> (
        let module_name = String.capitalize_ascii (Filename.basename name) in
        files |> Array.to_list |> List.filter_map aux
        |> function
        | [] ->
            None
        | _ :: _ as struct_items ->
            let struct_items =
              Mocaml.Transform.force_mutual_recursion struct_items
            in
            Some (SI.module_ @@ module_name ^= M.struct_ struct_items) )
  in
  match directory with
  | File _ ->
      if is_eml_file folder_name then
        let name = eml_basename folder_name ^ ".ml" in
        match Template_builder.of_filename folder_name with
        | Template template ->
            let pattern, value = compile ~continuation_mode "render" template in
            let defs = [SI.def (pattern ^= value)] in
            CCIO.with_out name (fun chan ->
                Mocaml.Printer.print_program chan defs )
        | Error lexbuf ->
            Template_builder.handle_syntax_error lexbuf
      else assert false
  | Directory (name, files) ->
      if files = [||] then
        Error.fail "Error : directory `%s` does not contain eml files" name ;
      let program = files |> Array.to_list |> List.filter_map aux in
      CCIO.with_out (folder_name ^ ".ml") (fun chan ->
          Mocaml.Printer.print_program chan program )
