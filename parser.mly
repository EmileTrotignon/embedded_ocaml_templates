
%token RightPar
%token LeftPar
%token LeftParOutput
%token LeftParArgs
%token <string> Text
%token EOF

%start <Template.t> template
%%

template_arguments :
| LeftParArgs ; args = Text ; RightPar { args }


rev_template :
 | (* nothing *) {[]}
 | t = rev_template ; LeftPar ; code = Text ; RightPar { Code code :: t}
 | t = rev_template ; LeftParOutput ; code = Text ; RightPar { Output_code code :: t}
 | t = rev_template ; text = Text { Template.Text text :: t}

template:
  | args = template_arguments; t = rev_template ; EOF {(args, List.rev t)}