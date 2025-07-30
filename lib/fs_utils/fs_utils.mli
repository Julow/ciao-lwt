val list_dir : string -> string array
(** [list_dir p] returns valid paths to the files in directory at path [p].
    Returns an empty array if [p] doesn't point to a directory or an error
    occurs. *)

val is_dir : string -> bool
(** Check if a path points to an existing directory and do not raise. *)

val walk_dir :
  ('acc -> string -> string list -> [ `Continue | `Dont_descend ] * 'acc) ->
  'acc ->
  string ->
  'acc
(** [walk_dir f acc path] walks a directory tree and calls
    [f acc dir_path files_paths] for every intermediate directories.
    [files_paths] are valid paths to the files contained in the directory at
    [dir_path]. [f] can control how the tree is walked by returning [`Continue]
    to keep walking down the tree or [`Dont_descend] to ignore an entire
    subtree. Return the initial [acc] if the initial [path] is not a directory
    or doesn't exist. *)

val scan_dir :
  ?descend_into:(string -> bool) ->
  ('acc -> string -> 'acc) ->
  'acc ->
  string ->
  'acc
(** [scan_dir ?descend_into f acc path] calls [f acc path] on every
    non-directory files found recursively into [path]. [descend_into] is called
    on every directories. Directories for which [descend_into] is [false] are
    not traversed. *)

val non_project_dir : string -> bool
(** Returns [true] if [dir] is a directory that should be avoided while scanning
    project files. *)

val is_ml_file : string -> bool
(** Whether a file is OCaml source code by looking at its file extension. *)

val find_ml_files : (string -> unit) -> string -> unit
(** Calls [f] on every OCaml implementation files in a directory tree. *)
