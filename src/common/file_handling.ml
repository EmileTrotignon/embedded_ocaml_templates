type file = File of string | Directory of (string * file array)

let sort_by_int array ~to_int =
  ArrayLabels.sort array ~cmp:(fun a b -> compare (to_int a) (to_int b))

let rec print_file file =
  match file with
  | File f -> Printf.printf "File %s\n" f
  | Directory (s, fa) ->
      Printf.printf "File %s (\n" s;
      ArrayLabels.iter fa ~f:print_file;
      print_endline ")"

let path_readdir dirname =
  ArrayLabels.map ~f:(Filename.concat dirname) (Sys.readdir dirname)

let rec read_file_or_directory ?(filter = fun _ -> true) ?(sorted = false)
    filename =
  let directories_first_sort files =
    sort_by_int files ~to_int:(fun f ->
        match f with Directory _ -> 0 | File _ -> 1)
  in
  match Sys.is_directory filename with
  | true ->
      Directory
        ( filename,
          let files =
            ArrayLabels.map
              ~f:(fun file ->
                match file with
                | File name -> File name
                | Directory (name, files) -> Directory (name, files))
              (CCArrayLabels.filter
                 ~f:(fun file ->
                   match file with File s -> filter s | Directory _ -> true)
                 (ArrayLabels.map
                    ~f:(read_file_or_directory ~filter ~sorted)
                    (ArrayLabels.map ~f:(Filename.concat filename)
                       (Sys.readdir filename))))
          in
          if sorted then directories_first_sort files;
          files )
  | false -> (
      match Sys.file_exists filename with
      | true -> File filename
      | false ->
          Printf.eprintf "Unknown file %s\n" filename;
          exit 1 )
