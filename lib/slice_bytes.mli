type t = bytes Slice.t

val make : ?off:int -> ?len:int -> bytes -> t

val empty : t
(** [empty] is an empty slice. *)

val length : t -> int
(** [length slice] is the number of bytes in [slice]. *)

val get : t -> int -> char
(** [get slice i] is the byte of [slice]' at index [i].

    @raise Invalid_argument if [i] is not an index of [slice]. *)

val get_int8 : t -> int -> int
(** [get_int8 slice i] is [slice]'s signed 8-bit integer starting at byte index
    [i]. *)

val get_uint8 : t -> int -> int
(** [get_uint8 slice i] is [slice]'s unsigned 8-bit integer starting at byte
    index [i]. *)

val get_uint16_ne : t -> int -> int
(** [get_int16_ne slice i] is [slice]'s native-endian unsigned 16-bit integer
    starting at byte index [i]. *)

val get_uint16_le : t -> int -> int
(** [get_int16_le slice i] is [slice]'s little-endian unsigned 16-bit integer
    starting at byte index [i]. *)

val get_uint16_be : t -> int -> int
(** [get_int16_be slice i] is [slice]'s big-endian unsigned 16-bit integer
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
(** [get_int32_ne slice i] is [slice]'s native-endian 32-bit integer starting at
    byte index [i]. *)

val get_int32_le : t -> int -> int32
(** [get_int32_le slice i] is [slice]'s little-endian 32-bit integer starting at
    byte index [i]. *)

val get_int32_be : t -> int -> int32
(** [get_int32_be slice i] is [slice]'s big-endian 32-bit integer starting at
    byte index [i]. *)

val get_int64_ne : t -> int -> int64
(** [get_int64_ne slice i] is [slice]'s native-endian 64-bit integer starting at
    byte index [i]. *)

val get_int64_le : t -> int -> int64
(** [get_int64_le slice i] is [slice]'s little-endian 64-bit integer starting at
    byte index [i]. *)

val get_int64_be : t -> int -> int64
(** [get_int64_be slice i] is [slice]'s big-endian 64-bit integer starting at
    byte index [i]. *)

val set : t -> int -> char -> unit
(** [set t i chr] modifies [t] in place, replacing the byte at index [i] with
    [chr].

    @raise Invalid_argument if [i] is not a valid index in [t]. *)

val set_int8 : t -> int -> int -> unit
(** [set_int8 t i v] sets [t]'s signed 8-bit integer starting at byte index [i]
    to [v]. *)

val set_uint8 : t -> int -> int -> unit
(** [set_uint8 t i v] sets [t]'s unsigned 8-bit integer starting at byte index
    [i] to [v]. *)

val set_uint16_ne : t -> int -> int -> unit
(** [set_uint16_ne t i v] sets [t]'s native-endian unsigned 16-bit integer
    starting at byte index [i] to [v]. *)

val set_uint16_le : t -> int -> int -> unit
(** [set_uint16_le t i v] sets [t]'s little-endian unsigned 16-bit integer
    starting at byte index [i] to [v]. *)

val set_uint16_be : t -> int -> int -> unit
(** [set_uint16_le t i v] sets [t]'s big-endian unsigned 16-bit integer starting
    at byte index [i] to [v]. *)

val set_int16_ne : t -> int -> int -> unit
(** [set_uint16_ne t i v] sets [t]'s native-endian signed 16-bit integer
    starting at byte index [i] to [v]. *)

val set_int16_le : t -> int -> int -> unit
(** [set_uint16_le t i v] sets [t]'s little-endian signed 16-bit integer
    starting at byte index [i] to [v]. *)

val set_int16_be : t -> int -> int -> unit
(** [set_uint16_le t i v] sets [t]'s big-endian signed 16-bit integer starting
    at byte index [i] to [v]. *)

val set_int32_ne : t -> int -> int32 -> unit
(** [set_int32_ne t i v] sets [t]'s native-endian 32-bit integer starting at
    byte index [i] to [v]. *)

val set_int32_le : t -> int -> int32 -> unit
(** [set_int32_ne t i v] sets [t]'s little-endian 32-bit integer starting at
    byte index [i] to [v]. *)

val set_int32_be : t -> int -> int32 -> unit
(** [set_int32_ne t i v] sets [t]'s big-endian 32-bit integer starting at byte
    index [i] to [v]. *)

val set_int64_ne : t -> int -> int64 -> unit
(** [set_int32_ne t i v] sets [t]'s native-endian 64-bit integer starting at
    byte index [i] to [v]. *)

val set_int64_le : t -> int -> int64 -> unit
(** [set_int32_ne t i v] sets [t]'s little-endian 64-bit integer starting at
    byte index [i] to [v]. *)

val set_int64_be : t -> int -> int64 -> unit
(** [set_int32_ne t i v] sets [t]'s big-endian 64-bit integer starting at byte
    index [i] to [v]. *)

val blit : t -> t -> unit
(** [blit src dst] copies all bytes of [src] into [dst]. *)

val blit_from_bytes : bytes -> src_off:int -> t -> ?dst_off:int -> int -> unit
(** [blit_from_bytes src ~src_off dst ~dst_off ~len] copies [len] bytes from
    byte sequence [src], starting at index [src_off], to slice [dst], starting
    at index [dst_off].

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val blit_to_bytes : t -> ?src_off:int -> bytes -> dst_off:int -> len:int -> unit
(** Just like {!val:blit_from_bytes}, but the source is a slice and the
    destination is a [byte]s sequence.

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val fill : t -> ?off:int -> ?len:int -> char -> unit
(** [fill t off len chr] modifies [t] in place, replacing [len] characters with
    [chr], starting at [off].

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [t]. *)

val sub : t -> off:int -> len:int -> t
(** [sub slice ~off ~len] does not allocate a new [bytes], but instead returns a
    new view into [t.buf] starting at [off], and with length [len].

    {b Note} that this does not allocate a new buffer, but instead shares the
    buffer of [t.buf] with the newly-returned slice. *)

val shift : t -> int -> t
(** [shift slice n] is [sub slice n (length slice - n)] (see {!val:sub} for more
    details). *)

val sub_string : t -> off:int -> len:int -> string
(** [sub_string slice ~off ~len] returns a string of length [len] containing the
    bytes of [slice] starting at [off]. *)

val to_string : t -> string
(** [to_string slice] is equivalent to
    [sub_string slice ~off:0 ~len:(length slice)]. *)

val is_empty : t -> bool
(** [is_empty bstr] is [length bstr = 0]. *)

val of_string : string -> t
(** [of_string str] returns a new {!type:t} that contains the contents of the
    given string [str]. *)

val string : ?off:int -> ?len:int -> string -> t
(** [string ~off ~len str] is the sub-buffer of [str] that starts at position
    [off] (defaults to [0]) and stops at position [off + len] (defaults to
    [String.length str]). [str] is fully-replaced by a fresh allocated
    {!type:t}. *)

val overlap : t -> t -> (int * int * int) option
(** [overlap x y] returns the size (in bytes) of what is physically common
    between [x] and [y], as well as the position of [y] in [x] and the position
    of [x] in [y]. *)
