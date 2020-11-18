%{
  let string_of_string_option o =
    match o with
    | Some s -> s
    | None -> ""
  let soso = string_of_string_option
%}

%token <bool> RPar
%token <bool> LPar (* <% *)
%token <bool> LParOutput (* <%- *)
%token LParArgs (* <%# *)
%token <string> Text
%token <string> Whitespaces
%token <bool> LParFormat (* <%( *)
%token RParFormat (* %)- *)
%token <bool*string> LFormatOutput (* <%d- *)
%token EOF

%start <Template.t> template
%% 

%inline text_of_lpar:
| slurp=LPar        { if slurp then "<_%" else "<%"  }
| slurp=LParOutput  { if slurp then "<_%-" else "<%-"  }
| LParArgs    { "<%#" }
| slurp=LParFormat format=Text RParFormat  { (if slurp then "<_%(" else "<%(") ^ format ^ "%)-" }
| slurp_format=LFormatOutput { let slurp, format = slurp_format in (if slurp then "<_%" else "<%") ^ format ^ "-" }

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
| LParArgs args=code RPar { args }


template_item :
 | slurp_before=LPar 
   code=code 
   slurp_after=RPar        { Tag( {slurp_before; slurp_after}, 
                                  Code code                    ) }
 | slurp_before=LParOutput 
   code=code 
   slurp_after=RPar        { Tag( {slurp_before; slurp_after}, 
                                  Output_code code             ) }
 | slurp_before=LParFormat 
   format=Text RParFormat 
   code=code 
   slurp_after=RPar        { Tag( {slurp_before; slurp_after}, 
                                  Output_format (format, code) ) }
 | sf=LFormatOutput 
   code=code 
   slurp_after=RPar          { let slurp_before, format  = sf in
                               Tag( {slurp_before; slurp_after}, 
                                  Output_format (format, code) ) }
 | text=Text               { Template.Text text                  }
 | text=Whitespaces        { Template.Whitespace text                  }

template:
  | args=ioption(template_arguments) t=list(template_item) EOF { Template.t_of_t' (soso args, t) }