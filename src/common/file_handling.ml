open Core

type file = File of string | Directory of (string * file array)

let sort_by_int array ~to_int =
  Array.sort array ~compare:(fun a b -> compare (to_int a) (to_int b))

let rec print_file file =
  match file with
  | File f -> printf "File %s\n" f
  | Directory (s, fa) ->
      printf "File %s (\n" s;
      Array.iter fa ~f:print_file;
      print_endline ")"

let path_readdir dirname =
  Array.map ~f:(Filename.concat dirname) (Sys.readdir dirname)

let rec read_file_or_directory ?(filter = fun _ -> true) ?(sorted = false)
    filename =
  let directories_first_sort files =
    sort_by_int files ~to_int:(fun f ->
        match f with Directory _ -> 0 | File _ -> 1)
  in
  match Sys.is_directory filename with
  | `Yes ->
      Directory
        ( filename,
          let files =
            Array.map
              ~f:(fun file ->
                match file with
                | File name -> File name
                | Directory (name, files) -> Directory (name, files))
              (Array.filter
                 ~f:(fun file ->
                   match file with File s -> filter s | Directory _ -> true)
                 (Array.map
                    ~f:(read_file_or_directory ~filter ~sorted)
                    (Array.map ~f:(Filename.concat filename)
                       (Sys.readdir filename))))
          in
          if sorted then directories_first_sort files;
          files )
  | `No -> (
      match Sys.file_exists filename with
      | `Yes -> File filename
      | `No -> failwith (sprintf "Unknown file %s" filename)
      | `Unknown -> failwith (sprintf "Unknown file %s" filename) )
  | `Unknown -> failwith (sprintf "Unknown file %s" filename)
