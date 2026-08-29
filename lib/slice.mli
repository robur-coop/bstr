type 'buf t = private { buf: 'buf; off: int; len: int }

val unsafe_make : off:int -> len:int -> 'buf -> 'buf t
val unsafe_sub : 'buf t -> int -> int -> 'buf t
val pp : Format.formatter -> 'a t -> unit
val length : 'a t -> int
val sub : 'a t -> off:int -> len:int -> 'a t
val shift : 'a t -> int -> 'a t
val is_empty : 'a t -> bool

module type R = sig
  type buf
  type t

  val create : int -> t
  (** [create len] is a fresh {!type:t} of length [len]. Its contents are reset
      to zero (bytes are set to ['\000']). *)

  val make : ?off:int -> ?len:int -> buf -> t
  (** [make ~off ~len buf] is the view of [buf] which starts at [off] (defaults
      to [0]) and which is [len] bytes long (defaults to [length buf - off]).
      The buffer is {b not} copied.

      @raise Invalid_argument
        if [off] and [len] do not designate a valid range of [buf]. *)

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
  val constant_equal : t -> t -> bool
  val compare : t -> t -> int

  val get : t -> int -> char
  (** [get t i] is the byte of [t] at index [i].

      Bounds are the ones of the {b slice}, not the ones of the underlying
      buffer: a slice never gives access to a byte which is outside of its own
      range.

      @raise Invalid_argument if [i] is not an index of [t]. *)

  val unsafe_get : t -> int -> char
  (** [unsafe_get t i] is {!val:get} without any bound checking. *)

  val get_int8 : t -> int -> int
  (** [get_int8 bstr i] is [bstr]'s signed 8-bit integer starting at byte index
      [i]. *)

  val get_uint8 : t -> int -> int
  (** [get_uint8 bstr i] is [bstr]'s unsigned 8-bit integer starting at byte
      index [i]. *)

  val get_uint16_ne : t -> int -> int
  (** [get_uint16_ne slice i] is [slice]'s native-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_uint16_le : t -> int -> int
  (** [get_uint16_le slice i] is [slice]'s little-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_uint16_be : t -> int -> int
  (** [get_uint16_be slice i] is [slice]'s big-endian unsigned 16-bit integer
      starting at byte index [i]. *)

  val get_int16_ne : t -> int -> int
  (** [get_int16_ne slice i] is [slice]'s native-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int16_le : t -> int -> int
  (** [get_int16_le slice i] is [slice]'s little-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int16_be : t -> int -> int
  (** [get_int16_be slice i] is [slice]'s big-endian signed 16-bit integer
      starting at byte index [i]. *)

  val get_int32_ne : t -> int -> int32
  (** [get_int32_ne slice i] is [slice]'s native-endian 32-bit integer starting
      at byte index [i]. *)

  val get_int32_le : t -> int -> int32
  (** [get_int32_le slice i] is [slice]'s little-endian 32-bit integer starting
      at byte index [i]. *)

  val get_int32_be : t -> int -> int32
  (** [get_int32_be slice i] is [slice]'s big-endian 32-bit integer starting at
      byte index [i]. *)

  val get_int64_ne : t -> int -> int64
  (** [get_int64_ne slice i] is [slice]'s native-endian 64-bit integer starting
      at byte index [i]. *)

  val get_int64_le : t -> int -> int64
  (** [get_int64_le slice i] is [slice]'s little-endian 64-bit integer starting
      at byte index [i]. *)

  val get_int64_be : t -> int -> int64
  (** [get_int64_be slice i] is [slice]'s big-endian 64-bit integer starting at
      byte index [i]. *)

  val filter : (char -> bool) -> t -> t
  (** [filter sat slice] is a fresh slice made of the bytes of [slice] that
      satisfy [sat], in the same order. *)

  val filter_map : (char -> char option) -> t -> t
  (** [filter_map fn t] is a fresh slice made of the bytes of [t] as mapped by
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
  (** [is_prefix ~affix slice] is [true] iff [affix.[idx] = get slice idx] for
      all indices [idx] of [affix]. *)

  val ends_with : suffix:string -> t -> bool

  val is_suffix : affix:string -> t -> bool
  (** [is_suffix ~affix slice] is [true] iff
      [affix.[n - idx] = get slice (m - idx)] for all indices [idx] of [affix]
      with [n = String.length affix - 1] and [m = length slice - 1]. *)

  val is_infix : affix:string -> t -> bool
  (** [is_infix ~affix slice] is [true] iff there exists an index [j] in [slice]
      such that for all indices [i] of [affix] we have
      [affix.[i] = get t (j + i)]. *)

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
  val index : t -> ?off:int -> ?len:int -> char -> int option
  val contains : t -> ?off:int -> ?len:int -> char -> bool
  val concat : string -> t list -> t
  val extend : t -> int -> int -> t
  val copy : t -> t
  val sub_string : t -> off:int -> len:int -> string
  val to_string : t -> string
  val of_string : string -> t
  val string : ?off:int -> ?len:int -> string -> t

  val blit_to_bytes :
    t -> ?src_off:int -> bytes -> dst_off:int -> len:int -> unit

  val with_range : ?first:int -> ?len:int -> t -> t
  val with_index_range : ?first:int -> ?last:int -> t -> t
  val to_seq : t -> char Seq.t
  val to_seqi : t -> (int * char) Seq.t
  val of_seq : char Seq.t -> t
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
  val fill : t -> ?off:int -> ?len:int -> char -> unit
  val blit : t -> t -> unit
  val blit_from_bytes : bytes -> src_off:int -> t -> ?dst_off:int -> int -> unit

  val blit_from_string :
    string -> src_off:int -> t -> ?dst_off:int -> int -> unit
end

module type S = sig
  type buf
  type nonrec t = buf t

  include R with type buf := buf and type t := t
  include W with type t := t
end
