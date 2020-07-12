open Core
open Containers

type t = Uchar.t array

let to_buffer ustring =
  let buffer = Buffer.create (Array.length ustring) in
  Array.iter (Uutf.Buffer.add_utf_8 buffer) ustring;
  buffer

let print ustring = print_string (Buffer.contents (to_buffer ustring))

let of_string string =
  let src = `String string in
  let decoder = Uutf.decoder src in
  let buffer = CCVector.create () in
  let rec aux () =
    match Uutf.decode decoder with
    | `Await -> assert false
    | `Uchar u ->
        CCVector.push buffer u;
        aux ()
    | `End -> ()
    | `Malformed string -> failwith (sprintf "Malformed input : %s" string)
  in
  aux ();
  CCVector.to_array buffer

let to_string ustring = Buffer.contents (to_buffer ustring)
