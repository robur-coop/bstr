type 'buf t = private { buf: 'buf; off: int; len: int }

val unsafe_make : off:int -> len:int -> 'buf -> 'buf t
val pp : Format.formatter -> 'a t -> unit
val length : 'a t -> int
val sub : 'a t -> off:int -> len:int -> 'a t
val shift : 'a t -> int -> 'a t
val is_empty : 'a t -> bool

module type R = sig
  type t

  val make : int -> char -> t
  (** [make len chr] is {!type:t} of length [len] with each index holding the
      character [chr]. *)

  val init : int -> (int -> char) -> t
  (** [init len fn] is {!type:t} of length [len] with index [idx] holding the
      character [fn idx] (called in increasing index order). *)

  val empty : t
  (** An empty {!type:t}. *)

  val length : t -> int
  (** [length t] is the length (number of bytes/characters) of [t]. *)

  val is_empty : t -> bool
  (** [is_empty t] is [length t = 0]. *)

  val chop : ?rev:bool -> t -> char option
  (** [chop t] is [Some (get t idx)] with [idx = 0] if [rev = false] (default)
      or [idx = length t - 1] if [rev = true]. [None] is returned if [t] is
      empty. *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val get : t -> int -> char
  val unsafe_get : t -> int -> char

  val get_int8 : t -> int -> int
  (** [get_int8 bstr i] is [bstr]'s signed 8-bit integer starting at byte index
      [i]. *)

  val get_uint8 : t -> int -> int
  (** [get_uint8 bstr i] is [bstr]'s unsigned 8-bit integer starting at byte
      index [i]. *)

  val get_uint16_ne : t -> int -> int
  (** [get_int16_ne bstr i] is [bstr]'s native-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_uint16_le : t -> int -> int
  (** [get_int16_le bstr i] is [bstr]'s little-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_uint16_be : t -> int -> int
  (** [get_int16_be bstr i] is [bstr]'s big-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_int16_ne : t -> int -> int
  (** [get_int16_ne bstr i] is [bstr]'s native-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int16_le : t -> int -> int
  (** [get_int16_le bstr i] is [bstr]'s little-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int16_be : t -> int -> int
  (** [get_int16_be bstr i] is [bstr]'s big-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int32_ne : t -> int -> int32
  (** [get_int32_ne bstr i] is [bstr]'s native-endian 32-bit integer starting at
      byte index [i]. *)

  val get_int32_le : t -> int -> int32
  (** [get_int32_le bstr i] is [bstr]'s little-endian 32-bit integer starting at
      byte index [i]. *)

  val get_int32_be : t -> int -> int32
  (** [get_int32_be bstr i] is [bstr]'s big-endian 32-bit integer starting at
      byte index [i]. *)

  val get_int64_ne : t -> int -> int64
  (** [get_int64_ne bstr i] is [bstr]'s native-endian 64-bit integer starting at
      byte index [i]. *)

  val get_int64_le : t -> int -> int64
  (** [get_int64_le bstr i] is [bstr]'s little-endian 64-bit integer starting at
      byte index [i]. *)

  val get_int64_be : t -> int -> int64
  (** [get_int64_be bstr i] is [bstr]'s big-endian 64-bit integer starting at
      byte index [i]. *)

  val filter : (char -> bool) -> t -> t
  (** [filter sat t] is a new {!type:t} made of the bytes of [t] that satisfy
      [sat], in the same order. *)

  val filter_map : (char -> char option) -> t -> t
  (** [filter_map fn t] is a new {!type:t} made of the bytes of [t] as mapped by
      [fn], in the same order. *)

  val map : (char -> char) -> t -> t
  val mapi : (int -> char -> char) -> t -> t
  val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a
  val fold_right : (char -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (char -> unit) -> t -> unit
  val iteri : (int -> char -> unit) -> t -> unit
  val hex : t -> string
  val overlap : t -> t -> (int * int * int) option
  val append : t -> t -> t
  val starts_with : prefix:string -> t -> bool

  val is_prefix : affix:string -> t -> bool
  (** [is_prefix ~affix bstr] is [true] iff [affix.[idx] = bstr.{idx}] for all
      indices [idx] of [affix]. *)

  val ends_with : suffix:string -> t -> bool

  val is_suffix : affix:string -> t -> bool
  (** [is_suffix ~affix bstr] is [true] iff [affix.[n - idx] = bstr.{m - idx}]
      for all indices [idx] of [affix] with [n = String.length affix - 1] and
      [m = length bstr - 1]. *)

  val is_infix : affix:string -> t -> bool
  (** [is_infix ~affix bstr] is [true] iff there exists an index [j] in [bstr]
      such that for all indices [i] of [affix] we have
      [affix.[i] = bstr.{j + i}]. *)

  val for_all : (char -> bool) -> t -> bool
  val exists : (char -> bool) -> t -> bool
  val trim : ?drop:(char -> bool) -> t -> t

  val span :
    ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t * t

  val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
  val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
  val shift : t -> int -> t
  val sub : t -> off:int -> len:int -> t
  val split_on_char : char -> t -> t list
  val cut : ?rev:bool -> sep:string -> t -> (t * t) option
  val cuts : ?rev:bool -> ?empty:bool -> sep:string -> t -> t list
  val index : t -> ?rev:bool -> ?from:int -> char -> int
  val contains : t -> ?rev:bool -> ?from:int -> char -> bool
  val concat : t -> t list -> t
  val copy : t -> t
  val sub_string : t -> off:int -> len:int -> string
  val to_string : t -> string
end

module type W = sig
  type t

  val set : t -> int -> char -> unit
  val unsafe_set : t -> int -> char -> unit
  val set_int8 : t -> int -> int -> unit
  val set_uint8 : t -> int -> int -> unit
  val set_uint16_ne : t -> int -> int -> unit
  val set_uint16_le : t -> int -> int -> unit
  val set_uint16_be : t -> int -> int -> unit
  val set_int16_ne : t -> int -> int -> unit
  val set_int16_le : t -> int -> int -> unit
  val set_int16_be : t -> int -> int -> unit
  val set_int32_ne : t -> int -> int32 -> unit
  val set_int32_le : t -> int -> int32 -> unit
  val set_int32_be : t -> int -> int32 -> unit
  val set_int64_ne : t -> int -> int64 -> unit
  val set_int64_le : t -> int -> int64 -> unit
  val set_int64_be : t -> int -> int64 -> unit
  val fill : t -> off:int -> len:int -> char -> unit
  val blit : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
end
