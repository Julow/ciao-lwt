module Backend = Backend

module Src_library = struct
  type module_ = {
    name : string;
    intf : string option;  (** Path *)
    impl : string option;  (** Path *)
  }

  type t = {
    rel_path : string;
        (** Relative. Valid path to the library source is
            [Filename.concat input_dir rel_path]. *)
    modules : module_ list;
  }

  let module_of_path path =
    match Fs_utils.is_ml_file path with
    | Some kind ->
        let mname =
          Filename.basename path |> Filename.remove_extension
          |> String.capitalize_ascii
        in
        Some (mname, kind, path)
    | None -> None

  let modules_of_files files =
    (* Group interfaces and implementations. *)
    let rec remove_duplicate_module m = function
      | (m', _, _) :: tl when m' = m ->
          (true, snd (remove_duplicate_module m tl))
      | tl -> (false, tl)
    in
    let rec group acc = function
      | (m1, k1, p1) :: (m2, k2, p2) :: tl when m1 = m2 ->
          let dup_module, tl = remove_duplicate_module m1 tl in
          if k1 = k2 || dup_module then (
            Format.eprintf
              "Warning: Too many interface or implementation files for module \
               %s at %s"
              m1 (Filename.basename p1);
            group acc tl (* Ignore the problematic module *))
          else
            let intf, impl = if k1 = `Intf then (p1, p2) else (p2, p1) in
            next acc m1 ~intf ~impl tl
      | (m, `Intf, p) :: tl -> next acc m ~intf:p tl
      | (m, `Impl, p) :: tl -> next acc m ~impl:p tl
      | [] -> acc
    and next acc name ?intf ?impl tl = group ({ name; intf; impl } :: acc) tl in
    List.filter_map module_of_path files |> List.sort compare |> group []

  let scan input_dir =
    Fs_utils.walk_dir
      (fun acc rel_path files ->
        if Fs_utils.non_project_dir rel_path then (`Dont_descend, acc)
        else
          let acc =
            match modules_of_files files with
            | [] -> acc
            | modules -> { rel_path; modules } :: acc
          in
          (`Continue, acc))
      [] input_dir
end

module Bridge = struct
  (** Raises *)
  let read_module (m : Src_library.module_) =
    match (m.intf, m.impl) with
    | Some _intf, _ -> (* TODO *) ()
    | None, Some _ ->
        failwith
          "Generating a bridge for a module without an interface is not \
           supported yet"
    | None, None -> assert false

  let create (ms : Src_library.module_ list) bridge_dir =
    Fs_utils.mkdir bridge_dir;
    List.iter
      (fun m ->
        let _intf = read_module m in
        let m_dst =
          Filename.concat bridge_dir (String.uncapitalize_ascii m.name ^ ".ml")
        in
        Out_channel.with_open_bin m_dst (fun oc ->
            Out_channel.output_string oc "(* TODO *)"))
      ms
end

let create ~packages:_ ~units:_ ~backend:_ input_dir output_dir =
  let libraries = Src_library.scan input_dir in
  List.iter
    (fun (lib : Src_library.t) ->
      let bridge_dir = Filename.concat output_dir lib.rel_path in
      Printf.printf
        "Generating a bridge for library %S in %S with %d modules\n%!"
        (Filename.concat input_dir lib.rel_path)
        bridge_dir (List.length lib.modules);
      Bridge.create lib.modules bridge_dir)
    libraries
