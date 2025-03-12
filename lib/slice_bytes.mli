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

val blit : t -> t -> unit
(** [blit src dst] copies all bytes of [src] into [dst]. *)

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
