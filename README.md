# Embedded Ocaml Templates

EML is a simple templating language that lets you generate text with plain OCaml.
The syntax is as follow :

First of all, you need to declare the template's arguments at the top of the
template :

```eml
<%# arg1 (arg2:type) (arg3_1, arg3_2) %>
```

Then you can use two tags :

```eml
<% ocaml code here %>
```

This tag expect any ocaml code. If what you put in here is an expression of type
unit, you should include the ";" yourself. You are able to open parenthesis and
close them in a subsequent tag.

```eml
<%= ocaml expression here %>
```

This tag expect an expression of type string and is going to be replaced by the
value of the expression, with HTML escaping.
If this tag is inside a loop or an if statement, or any control structure, it's
going to behave the way you would expect it to :
outputting its content every time the branch is executed, with the right context.

This tag has a variant :

```eml
<%i= ocaml expression here %>
```

Here you can use any "simple" printf format specifier, where simple is defined
by the following regex :

```regex
  'd' | 'i' | 'u' | 'n' | 'l' | 'N' | 'L' | 'x' | 'o' | 'X' | 's' | 'c'
| 'S' | 'C' | 'f' | 'e' | 'E' | 'g' | 'G' | 'h' | 'H' | 'b' | 'B'
| ('l' | 'n' | 'L'), ('d' | 'i' | 'u' | 'x' | 'X' | 'o')
| 't'
```

You can notice that `<%s- x %>` is equivalent to `<%- x %>`

You can use more complicated printf format specifiers with format flags, width
and precision using the following syntax :

```eml
<%[i%]= ocaml expression here %>
```

Every time `=` is used to mark an outputting tag, it can be replaced by `-` to
disable HTML escaping.

A slurp marker is also provided :
`<_%` slurps whitespaces before it, and `%_>` after. It can be combined with
output tags this way : `<_%=`.

Identifiers prefixed with `__eml_` are reserved. This includes string delimiters
`{__eml_|` and `|__eml_}`. Using them will not necessarily raise an error, but
there is no guarantee if you do.

Because OCaml does not have an eval function, the templates have to be compiled.
What is provided by this package is an executable that will compile either a
single .eml file into an OCaml module containing a function that render the
template, or take a whole directory containing a function for each .eml file and
a submodule for each subdirectory (recursively).

Here is an exemple of a dune rule:

```dune
(rule
 (target templates.ml)
 (deps (source_tree templates))
 (action (run eml_compiler templates)))
```

There is also a ppx rewriter provided :

```ocaml
let name = "John"
let john = [%eml "<%-name%>"]
```

You can use the argument tag this way :

```ocaml
let user = [%eml "<%# name age %>name:<%-name%>, age:<%i- age%>"]
```

But in my opinion it is more elegant to write :

```ocaml
let user name age = [%eml "name:<%-name%>, age:<%i- age%>"]
```

There is also this nice new syntax that available from OCaml 4.11 onward :

```ocaml
let user name age = {%eml|name:<%-name%>, age:<%i- age%>|}
```

Unfortunately, I have not managed to make the ppx positions correct, and it
seems to me that this not possible without breaking compatibility with older
OCaml versions. This means that errors will show up in weird positions.
