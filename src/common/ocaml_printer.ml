open Ocaml
open PPrint

let ( ^-^ ) a b = a ^^ space ^^ b
let ( ^/^ ) a b = a ^^ hardline ^^ b
let ( ^|^ ) a b = a ^^ break 1 ^^ b
let nest = nest 2
let nest_break i doc = nest (break i ^^ doc)
let nl = hardline
let semicolon = string ";"
let rarrow = string "->"
let equal = string "="
let comma_list printer = separate_map (break 0 ^^ comma ^^ space) printer
let dot_list printer = separate_map dot printer

let semicolon_list printer =
  separate_map (break 0 ^^ semicolon ^^ space) printer

let space_list printer = separate_map (break 1) printer
let nl_list printer = separate_map hardline printer
let in_pars doc = lparen ^-^ doc ^-^ rparen
let in_brackets doc = lbracket ^-^ doc ^-^ rbracket
let precede_list prefix = concat_map (precede prefix)
let ( @> ) f g x = f (g x)

let rec pattern p =
  group
    ( match p with
    | PWildcard -> underscore
    | PTuple li -> in_pars (semicolon_list pattern li)
    | PVar name -> string name
    | PPrimitive prim -> string prim )

and def (p, e) =
  nl ^^ string "let" ^-^ pattern p ^-^ equal ^|^ expr e ^|^ string "in"

and mixed = function MiUnit e -> expr e ^-^ semi | MiPrimitive p -> string p

and expr e =
  group
    ( match e with
    | EUnit -> string "()"
    | ELet (defs, e) -> space_list def defs ^-^ expr e
    | EApp (f, args) -> expr f ^-^ space_list (in_pars @> expr) args
    | ECons (cons, args) -> string cons ^-^ in_pars (comma_list expr args)
    | ETuple exprs -> in_pars (comma_list expr exprs)
    | ELitList exprs -> in_brackets (semicolon_list expr exprs)
    | EVar name -> string name
    | EFun (args, body) ->
        string "fun" ^-^ space_list pattern args ^-^ rarrow
        ^^ nest_break 1 (expr body)
    | ERef e -> string "ref" ^^ nest_break 1 (expr e)
    | EDeRef e -> string "!" ^^ expr e
    | EAssignToRef (e, v) -> expr e ^-^ string ":=" ^^ nest_break 1 (expr v)
    | ESequence (li, e) -> semicolon_list expr li ^|^ semicolon ^-^ expr e
    | EPrimitive prim -> string prim
    | EOpenModule (m, e) ->
        string m ^^ dot ^^ lparen ^^ nest_break 0 (expr e) ^|^ rparen
    | EModuleField path -> dot_list string path
    | EMixedSequence (li, e) -> space_list mixed li ^|^ expr e
    | ELitString s -> string "{___|" ^^ arbitrary_string s ^^ string "|___}" )

and mutual_def (p, e) = nl ^^ string "and" ^-^ pattern p ^-^ equal ^|^ expr e

and struct_item = function
  | SIDef (p, e) ->
      string "let"
      ^^ nest_break 1 (pattern p)
      ^|^ equal
      ^^ nest_break 1 (expr e)
  | SIModule (name, m) ->
      string "module" ^-^ string name ^-^ equal ^^ nest_break 1 (module_ m)
  | SIRecDefs defs -> (
    match defs with
    | (p, e) :: defs ->
        string {|let[@warning "-39"] rec|}
        ^^ nest_break 1 (pattern p)
        ^|^ equal
        ^^ nest_break 1 (expr e)
        ^|^ concat_map mutual_def defs
    | [] -> assert false )

and module_ = function
  | MStruct s -> struct_ s
  | MAlias a -> string a
  | MField li -> dot_list string li

and struct_ s = string "struct" ^/^ nl_list struct_item s ^/^ string "end"
and program s = nl_list struct_item s

let to_channel f channel args =
  let doc = f args in
  ToChannel.pretty 0.8 80 channel doc

let to_string f arg =
  let doc = f arg in
  let buffer = Buffer.create 10 in
  ToBuffer.pretty 0.8 80 buffer doc ;
  Bytes.to_string @@ Buffer.to_bytes buffer

let expr_to_string = to_string expr
let program_to_string = to_string program
let print_program = to_channel program
