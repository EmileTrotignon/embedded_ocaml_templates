open Core
open Template

let compile name header (args, elements) =
  let codes =
    ref
      [
        sprintf
          {|
        %s
        let %s %s =
          let ___elements = ref [] in
          let ___append e =
            ___elements := e :: !___elements
          in
        |}
          header name args;
      ]
  in
  let append e = codes := e :: !codes in
  List.iter elements ~f:(fun ele ->
      match ele with
      | Text s -> append (sprintf {|___append {___|%s|___} ;|} s)
      | Code s -> append s
      | Output_code s -> append (sprintf {|___append (%s) ;|} s));
  append {|
  String.concat (List.rev !___elements)
  |};
  String.concat (List.rev !codes)

let compile_to_module template = compile "render" "open Core" template

let compile_to_function name template = compile name "" template
