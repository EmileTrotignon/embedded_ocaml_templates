let simple s = [%eml {|prefix<%= s %>suffix|}]
let simple_no_escape s = [%eml {|prefix<%- s %>suffix|}]
let slurp_left s = [%eml {|prefix          <_%= s %>suffix|}]
let slurp_right s = [%eml {|prefix<%= s %_>           suffix|}]
let slurp_left_no_escape s = [%eml {|prefix          <_%- s %>suffix|}]
let slurp_right_no_escape s = [%eml {|prefix<%- s %_>           suffix|}]

let for_loop s n =
  [%eml {|prefix<%for _=1 to n do (%>ipre<%= s %>isuf<% ) done ; %>suffix|}]

let for_loop_slurp s n =
  [%eml
    {|prefix
        <_%for _=1 to n do %_>
          ipre <_%= s %_> isuf
        <_% done ; %_>
        suffix|}]

let simple_format i = [%eml {|<%d=i%>|}]
let simple_format_no_escape i = [%eml {|<%d-i%>|}]
let complex_format i = [%eml {|<%[d%]= i %>|}]
let complex_format_no_escape i = [%eml {|<%[d%]- i %>|}]
let s' printer a = Printf.sprintf "%a" printer a

let%test "simple" = simple "coucou" = "prefixcoucousuffix"

let%test "simple escape" = simple {|&<>"'|} = "prefix&amp;&lt;&gt;&quot;&#x27;suffix"

let%test "simple_no_escape" =
  simple_no_escape "coucou" = "prefixcoucousuffix"
  && simple_no_escape {|&<>"'|} = {|prefix&<>"'suffix|}

let%test "slurp" =
  slurp_left "coucou" = "prefixcoucousuffix"
  && slurp_right "coucou" = "prefixcoucousuffix"

let%test "slurp escape" =
  slurp_left "coucou" = "prefixcoucousuffix"
  && slurp_right "coucou" = "prefixcoucousuffix"

let%test "slurp_no_escape" =
  slurp_left_no_escape "coucou" = "prefixcoucousuffix"
  && slurp_right_no_escape "coucou" = "prefixcoucousuffix"
  && slurp_left_no_escape {|&<>"'|} = {|prefix&<>"'suffix|}
  && slurp_right_no_escape {|&<>"'|} = {|prefix&<>"'suffix|}

let%test "for_loop" =
  for_loop " foo " 3 = {|prefixipre foo isufipre foo isufipre foo isufsuffix|}
  && for_loop " foo " 2 = {|prefixipre foo isufipre foo isufsuffix|}

let%test "for_loop" =
  for_loop_slurp " foo " 3
  = {|prefixipre foo isufipre foo isufipre foo isufsuffix|}
  && for_loop_slurp " foo " 2 = {|prefixipre foo isufipre foo isufsuffix|}

let%test "simple_format" =
  simple_format 1 = "1"
  && simple_format 12353 = "12353"
  && simple_format 92853 = "92853"
  && complex_format_no_escape 1 = "1"
  && simple_format 12353 = "12353"
  && simple_format 92853 = "92853"
