%{
  let string_of_string_option o =
    match o with
    | Some s -> s
    | None -> ""
  let soso = string_of_string_option
%}

%token RightPar
%token LeftPar (* <% *)
%token LeftParOutput (* <%- *)
%token LeftParArgs (* <%# *)
%token <string> Text
%token EOF

%start <Template.t> template
%% 

%inline text_of_left_par:
| LeftPar       { "<%" }
| LeftParOutput { "<%-" }
| LeftParArgs   { "<%#" }

code_item :
| code = Text                 { code                         }
| left_par=text_of_left_par 
  code=ioption(Text) RightPar { left_par ^ soso code ^ "%>" } 

code:
| list=list(code_item) { String.concat "" list }

template_arguments :
| LeftParArgs args=Text RightPar { args }


template_item :
 | LeftPar code=code RightPar       { Code code          }
 | LeftParOutput code=code RightPar { Output_code code   }
 | text=Text                        { Template.Text text }

template:
  | args=option(template_arguments) t=list(template_item) EOF { (soso args, t) }