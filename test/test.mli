type t
type runner = private { directory: string }
type ('a, 'b) str = ('a -> 'b, Format.formatter, unit, string) format4

val check : bool -> unit
val test : title:string -> descr:string -> (unit -> unit) -> t
val runner : ?g:Random.State.t -> ?fmt:(string, string) str -> string -> runner
val run : runner -> t -> unit
