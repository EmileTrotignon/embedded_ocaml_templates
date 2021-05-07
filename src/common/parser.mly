%{

  open Parser_aux

  let string_of_string_option o =
    match o with
    | Some s -> s
    | None -> Ocaml.Primitive.with_dummy_pos ""
  let soso = string_of_string_option


%}

%token <bool> RPar
%token <bool> LPar (* <% *) (* bool is slurp *)
%token <Parser_aux.output_option> LParOutput (* <%- *)
%token LParArgs (* <%# *)
%token <string> Text
%token <string> Whitespaces
%token <bool> LParFormat (* <%[ *) (* bool is slurp *)
%token <bool> RParFormat (* %]- *) (* bool is escape *)
%token EOF

%start <Template.t> template
%%

%inline located(X):
  x=X { Ocaml.Primitive.build x $startpos $endpos }

%inline text_of_lpar:
| slurp=LPar { if slurp then "<_%" else "<%"  }
| options=LParOutput {
    let {slurp; escape; format} = options in
    let slurp = if slurp then "_" else "" in
    let escape = if escape then "=" else "-" in
    let format = match format with None -> "" | Some format -> format in
      "<" ^ slurp ^ "%" ^ format ^ escape }
| LParArgs { "<%#" }
| slurp=LParFormat format=Text escape=RParFormat {
    let slurp = if slurp then "_" else "" in
    let escape = if escape then "=" else "-" in
    "<" ^ slurp ^ "%[" ^ format ^ "%]" ^ escape }

%inline text_of_rpar:
| slurp = RPar { if slurp then "%_>" else "%>"}

code_item :
| code = Text        { code                         }
| code = Whitespaces { code                         }
| lpar=text_of_lpar
  code=code
  rpar=text_of_rpar  { lpar ^ code ^ rpar           }

code:
| list=list(code_item) { String.concat "" list }

%inline template_arguments :
| LParArgs args=located(code) RPar { args }


template_item :
 | slurp_before=LPar
   code=located(code)
   slurp_after=RPar {
     Tag( {slurp_before; slurp_after}, Code code )
   }
 | options=LParOutput
   code=located(code)
   slurp_after=RPar {
     let {slurp; escape; format} = options in
     Tag ({slurp_before=slurp; slurp_after}, Output {code; format; escape})
   }
 | slurp_before=LParFormat
   format=Text
   escape=RParFormat
   code=located(code)
   slurp_after=RPar {
     Tag( {slurp_before; slurp_after}
        , Template.output' ~escape ~format code )
   }
 | text=Text { Template.Text text }
 | text=Whitespaces { Template.Whitespace text }

template:
  | args=ioption(template_arguments) t=list(template_item) EOF {
      Template.t_of_t' (soso args, t) }