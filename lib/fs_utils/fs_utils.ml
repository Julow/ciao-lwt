(** Returns an empty array on error. *)
let list_dir p =
  match Sys.readdir p with
  | exception Sys_error msg ->
      Printf.eprintf "%s\n%!" msg;
      [||]
  | files ->
      (* Sorted to ensure reproducibility. *)
      Array.sort String.compare files;
      Array.iteri (fun i fname -> files.(i) <- Filename.concat p fname) files;
      files

(** Do not raise. *)
let is_dir p = try Sys.is_directory p with Sys_error _ -> false

(** Partition files and directories in the array [ar], returned by [list_dir].
*)
let partition_files ar =
  (* Preserve the ordering from the input. *)
  let rec loop dirs files ar i =
    if i < 0 then (dirs, files)
    else
      let p = ar.(i) and i = i - 1 in
      if is_dir p then loop (p :: dirs) files ar i
      else loop dirs (p :: files) ar i
  in
  loop [] [] ar (Array.length ar - 1)

let walk_dir f acc path =
  let rec walk acc path =
    let dirs, files = partition_files (list_dir path) in
    match f acc path files with
    | `Dont_descend, acc -> acc
    | `Continue, acc -> List.fold_left walk acc dirs
  in
  if is_dir path then walk acc path else acc

let scan_dir ?(descend_into = fun _ -> true) f acc path =
  walk_dir
    (fun acc dir files ->
      if descend_into dir then (`Continue, List.fold_left f acc files)
      else (`Dont_descend, acc))
    acc path

let non_project_dir dir =
  match Filename.basename dir with
  | "_build" | "_opam" | ".git" -> true
  | _ -> false

let is_ml_file path =
  match Filename.extension path with
  | ".ml" | ".eliom" | ".mli" | ".eliomi" -> true
  | ext ->
      (* Accept extensions of the form [foo.client.ml] *)
      String.ends_with ~suffix:".ml" ext

let find_ml_files f path =
  let descend_into dir = not (non_project_dir dir) in
  scan_dir ~descend_into (fun () p -> if is_ml_file p then f p) () path
