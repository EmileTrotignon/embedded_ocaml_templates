# Embedded Ocaml Templates

EML is a simple templating language that lets you generate text with plain OCaml.
The syntax is as follow :

First of all, you need to declare the template's arguments at the top of the template :
```
<%# arg1 (arg2:type) (arg3_1, arg3_2) %>
```

The you can use two tags :

```
<% ocaml code here %>
```
This tag expect any ocaml code. If what you put in here is an expression of type unit, you should include the ";" yourself.
You are able to open parenthesis and close them in a subsequent tag.

```
<%- ocaml expression here %>
```
This tag expect an expression of type string and is going to be replaced by the value of the expression. 
If this tag is inside a loop or an if statement, it's going to behave you would expect it to.

There are some identifiers that you cannot use : "___append" and "___elements" are going to be variables in the generated code.
"{___|" and "|___}" are used as string delimiters.
Using them will not necessarily raise an error, however I cannot guarantee what will happen if you do. 

Because OCaml does not have an eval function, the templates have to be compiled. 
What is provided by this package is an executable that will compile either a single .eml file into an OCaml module containing a function that render the template, or take a whole directory containing a function for each .eml file and a submodule for each subdirectory (it's recursive).

Here is an exemple of a dune rule:
```dune
(rule
 (target templates.ml)
 (deps (source_tree templates))
 (action (run eml_compiler templates)))
```

There is also a ppx rewriter provided : 

```
let name = "John"
let john = [%eml "<%-name%>"]
```

The ppx may be a bit slow at compile time, because I actually call the OCaml parser on generated code to build it. 
This has the advantage to be most likely compatible with future versions of OCaml, but if someone would like to help me do that more cleanly I would appreciate the help. 


