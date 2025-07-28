module Backend = Backend

let module_name_of_path path =
  if Fs_utils.is_ml_file path then
    let mname =
      Filename.basename path |> Filename.remove_extension
      |> String.capitalize_ascii
    in
    Some (mname, path)
  else None

let do_walk () =
  Fs_utils.walk_dir
    (fun () dir files ->
      if Fs_utils.non_project_dir dir then (`Dont_descend, ())
      else
        let ml_files = List.filter_map module_name_of_path files in
        if ml_files <> [] then
          Printf.printf "Create a bridge library for %S with modules %s\n%!" dir
            (String.concat ", " @@ List.map (fun (m, _) -> m) ml_files);
        (`Continue, ()))
    () "."

let create ~packages:_ ~units:_ ~backend:_ _dst_dir = do_walk ()
