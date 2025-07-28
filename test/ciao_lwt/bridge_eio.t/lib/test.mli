type t1 = int Lwt.t
type 'a t2 = 'a Lwt.t

val f0 : int -> int Lwt.t
val f1 : int Lwt.t -> int
val f2 : (unit -> int Lwt.t) -> int
