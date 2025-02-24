(** A small library for manipulating bigstrings.

    To clarify the use of bigstrings in OCaml, we advise you to read the
    overview of bigstrings and the difference with bytes. After this theoretical
    reading, this module offers a whole host of useful functions for
    manipulating bigstrings.

    {1:overview Overview.}

    A bigstring is a special kind of memory area in the OCaml world. Unlike
    bytes, bigstrings are allocated via [malloc()] or are available via
    [Unix.map_file].

    They therefore exist outside the space normally allocated for OCaml with
    regard to all its values. So there are some particularities to the use of
    bigstrings.

    The first thing to understand about bigstrings is that allocating them can
    take time. Since a bigstring is obtained either by [malloc()] or by
    [Unix.map_file], the former is a performance hit on the [malloc()] used
    (which also depends on the fragmentation of the C heap) and the latter is a
    system call that can interact with your file system.

    By way of comparison, a byte of less than 2048 bytes requires only 3
    processor instructions to exist and be available — beyond that, the bytes is
    allocated in the major heap.

    It is therefore advisable to allocate just a few bigstrings and reuse them
    throughout your application. It's even advisable to allocate large
    bigstrings.

    A particularity of bigstrings is that they cannot be moved by the Garbage
    Collector. Existing in a space other than that of OCaml (the C heap), they
    don't move. With this advantage in mind, we can imagine several situations
    where we'd like a memory zone that doesn't move:
    - a bigstring can be manipulated by several threads/domains. Of course,
      parallel accesses must be protected, but you can be sure that the
      bigstring will not move throughout the process. Thus, its location in
      memory can be shared by several threads/domains.

    One example is to "release" the GC lock when performing a calculation such
    as a hash or checksum on a bigarray. Since the latter will not be moved by
    the GC, if the elements required for the calculation are pre-allocated on
    the C stack, it is possible to perform such a calculation on a Thread other
    than the main OCaml thread.

    - it may be necessary, in system programming, to write to a particular
      address in order to interact with a device. In this case, the bigstring
      can be found as an OCaml value bridging a special memory area (such as the
      framebuffer).

    This is somewhat equivalent to [Unix.map_file]. The latter uses [mmap(3P)],
    which asks the kernel for a special memory address. This address can be
    related (via the kernel) to an area on your hard disk that corresponds to a
    file. In the case of unikernels or embedded systems, it's quite common to
    prepare bigstrings according to the devices available.

    A final feature of bigstring is that it can be seen as a slice. You can have
    another view of a bigstring that would be equally smaller. For example, the
    {!val:sub} operation in particular {b doesn't copy} your bigstring, but
    offers you a "proxy" accessing the same memory area as the original
    bigstring.

    This can be useful for decoding packets, extracting information such as
    integers, without copying parts or all of the bigstring. For example, for a
    TCP/IP packet, we'd like to decode certain information but also give a slice
    of the bigstring that corresponds to the packet's payload (so that we can
    process this payload without having to copy).

    Finally, it may be interesting in an encoder of some kind to give bigstrings
    that the user can write to, and check that these bigstrings are part of a
    larger bigstring (in other words, these bigstrings come from a {!val:sub} of
    a larger bigstring) that has been allocated beforehand.

    Bigstrings therefore have certain advantages over bytes, but also some
    disadvantages. Considering the former as elements you should use
    systematically is not a good choice. However, we are sometimes forced to use
    them (especially when communicating with embedded devices) and they can be
    interesting for certain types of applications. This overview presents a few
    cases, but examples exist in the OCaml community where the use of bigstrings
    is justified.

    In short, this library attempts to summarize everything that can be done
    with bigstrings.

    {2 Performance.}

    {1:pkt Encode & Decode packets.}

    In order to encode or decode packets (such as ARP or DNS packets), Bstr
    offers a small API for converting a slice of bytes from a {!val:Bstr.t} to a
    user-defined variant or record. *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val memcpy : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** [memcpy src ~src_off dst ~dst_off ~len] copies [len] bytes from [src] to
    [dst]. [src] {b must not} overlap [dst]. Use {!val:memmove} if [src] & [dst]
    do overlap.

    You can check whether two buffers overlap using {!val:overlap}. If this
    returns [None], the two values do not refer to a common memory area — and it
    is safe to use memcpy.

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val memmove : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** [memmove src ~src_off dst ~dst_off ~len] copies [len] bytes from [src] to
    [dst]. [src] and [dst] may overlap: copying takes place as though the bytes
    in [src] are first copied into a temporary array that does not overlap [src]
    or [dst], and the bytes are then copied from the temporary array to [dst].

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val memcmp : t -> src_off:int -> t -> dst_off:int -> len:int -> int
val memchr : t -> off:int -> len:int -> char -> int
val memset : t -> off:int -> len:int -> char -> unit

val empty : t
(** [empty] is an empty bigstring. *)

val length : t -> int
(** [length bstr] is the number of bytes in [bstr]. *)

val get : t -> int -> char
(** [get bstr i] is the byte of [bstr]' at index [i]. This is equivalent to the
    [bstr.{i}] notation.

    @raise Invalid_argument if [i] is not an index of [bstr]. *)

val set : t -> int -> char -> unit
(** [set t i chr] modifies [t] in place, replacing the byte at index [i] with
    [chr].

    @raise Invalid_argument if [i] is not a valid index in [t]. *)

val unsafe_get : t -> int -> char
(** [unsafe_get t idx] is like {!val:get} except no bounds checking is
    performed. *)

val unsafe_set : t -> int -> char -> unit
(** [unsafe_set t idx chr] is like {!val:set} except no bounds checking is
    performed. *)

val chop : ?rev:bool -> t -> char option

val create : int -> t
(** [create len] returns a new byte sequence of length [len]. The sequence
    {b is unitialized} and contains arbitrary bytes. *)

val make : int -> char -> t
(** [make len chr] is {!type:t} of length [len] with each index holding the
    character [chr]. *)

val of_string : string -> t
(** [of_string str] returns a new {!type:t} that contains the contents of the
    given string [str]. *)

val string : ?off:int -> ?len:int -> string -> t
(** [string ~off ~len str] is the sub-buffer of [str] that starts at position
    [off] (defaults to [0]) and stops at position [off + len] (defaults to
    [String.length str]). [str] is fully-replaced by a fresh allocated
    {!type:t}. *)

val fill : t -> ?off:int -> ?len:int -> char -> unit
(** [fill t off len chr] modifies [t] in place, replacing [len] characters with
    [chr], starting at [off].

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [t]. *)

val init : int -> (int -> char) -> t
(** [init len fn] returns a fresh byte sequence of length [len], with character
    [idx] initialized to the result of [fn idx] (in increasing index order). *)

val copy : t -> t
(** [copy t] returns a new byte sequence that contains the same bytes as the
    argument. *)

(** {2 Copy operation from one byte sequence to another.} *)

val blit : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** [blit src ~src_off dst ~dst_off ~len] copies [len] bytes from byte sequence
    [src], starting at index [src_off], to byte sequence [dst], starting at
    index [dst_off]. It works correctly even if [src] and [dst] are (physically)
    the same byte sequence, and the source and destination intervals overlap.

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val blit_from_string :
  string -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** Just like {!val:blit}, but with a string as source one.

    {b Note}: since it is impossible for [src] to overlap [dst], {!val:memcpy}
    is used to do the copy.

    @raise Invalid_argument
      if [src_pos] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val blit_from_bytes :
  bytes -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** Just like {!val:blit}, but with a bytes as source one.

    {b Note}: since it is impossible for [src] to overlap [dst], {!val:memcpy}
    is used to do the copy.

    @raise Invalid_argument
      if [src_pos] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val blit_to_bytes : t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
(** [blit_to_bytes src ~src_off dst ~dst_off ~len] copies [len] bytes from
    [src], starting at index [src_off], to byte sequence [dst], starting at
    index [dst_off].

    {b Note}: since it is impossible for [src] to overlap [dst], {!val:memcpy}
    is used to do the copy.

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

(*
val extend : t -> int -> int -> t
val concat : t -> t list -> t
val cat : t -> t -> t
val iter : (char -> unit) -> t -> unit
val iteri : (int -> char -> unit) -> t -> unit
val map : (char -> char) -> t -> t
val mapi : (int -> char -> char) -> t -> t
val fold_left : ('acc -> char -> 'acc) -> 'acc -> t -> 'acc
val fold_right : (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
val index : t -> ?rev:bool -> ?from:int -> char -> int
val contains : t -> ?rev:bool -> ?from:int -> char -> bool
val compare : t -> t -> int
val starts_with : prefix:string -> t -> bool
val ends_with : suffix:string -> t -> bool
*)

(** {2 Decode integers from a byte sequence.} *)

val get_int8 : t -> int -> int
(** [get_int8 bstr i] is [bstr]'s signed 8-bit integer starting at byte index
    [i]. *)

val get_uint8 : t -> int -> int
(** [get_uint8 bstr i] is [bstr]'s unsigned 8-bit integer starting at byte index
    [i]. *)

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
(** [get_int16_be bstr i] is [bstr]'s big-endian signed 16-bit integer starting
    at byte index [i]. *)

val get_int32_ne : t -> int -> int32
(** [get_int32_ne bstr i] is [bstr]'s native-endian 32-bit integer starting at
    byte index [i]. *)

val get_int32_le : t -> int -> int32
(** [get_int32_le bstr i] is [bstr]'s little-endian 32-bit integer starting at
    byte index [i]. *)

val get_int32_be : t -> int -> int32
(** [get_int32_be bstr i] is [bstr]'s big-endian 32-bit integer starting at byte
    index [i]. *)

val get_int64_ne : t -> int -> int64
(** [get_int64_ne bstr i] is [bstr]'s native-endian 64-bit integer starting at
    byte index [i]. *)

val get_int64_le : t -> int -> int64
(** [get_int64_le bstr i] is [bstr]'s little-endian 64-bit integer starting at
    byte index [i]. *)

val get_int64_be : t -> int -> int64
(** [get_int64_be bstr i] is [bstr]'s big-endian 64-bit integer starting at byte
    index [i]. *)

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

val sub : t -> off:int -> len:int -> t
(** [sub bstr ~off ~len] does not allocate a bigstring, but instead returns a
    new view into [bstr] starting at [off], and with length [len].

    {b Note} that this does not allocate a new buffer, but instead shares the
    buffer of [bstr] with the newly-returned bigstring.

    {b Note} [sub] is more expensive than a [Slice.sub] (about 8 times slower).
    If you want to focus on performance while avoiding copying, it's best to use
    a [Slice]. *)

val shift : t -> int -> t
(** [shift bstr n] is [sub bstr n (length bstr - n)] (see {!val:sub} for more
    details). *)

val overlap : t -> t -> (int * int * int) option
(** [overlap x y] returns the size (in bytes) of what is physically common
    between [x] and [y], as well as the position of [y] in [x] and the position
    of [x] in [y]. *)

val sub_string : t -> off:int -> len:int -> string
(** [sub_string bstr ~off ~len] returns a string of length [len] containing the
    bytes of [t] starting at [off]. *)

val to_string : t -> string
(** [to_string bstr] is equivalent to
    [sub_string bstr ~off:0 ~len:(length bstr)]. *)

val is_empty : t -> bool
(** [is_empty bstr] is [length bstr = 0]. *)

val is_prefix : affix:string -> t -> bool
(** [is_prefix ~affix bstr] is [true] iff [affix.[idx] = bstr.{idx}] for all
    indices [idx] of [affix]. *)

val starts_with : prefix:t -> t -> bool
(** [starts_with ~prefix t] is like {!val:is_prefix} but the prefix is a
    {!type:t} (instead of a [string]). *)

val is_infix : affix:string -> t -> bool
(** [is_infix ~affix bstr] is [true] iff there exists an index [j] in [bstr]
    such that for all indices [i] of [affix] we have [affix.[i] = bstr.{j + i}].
*)

val is_suffix : affix:string -> t -> bool
(** [is_suffix ~affix bstr] is [true] iff [affix.[n - idx] = bstr.{m - idx}] for
    all indices [idx] of [affix] with [n = String.length affix - 1] and
    [m = length bstr - 1]. *)

val ends_with : suffix:t -> t -> bool
(** [ends_with ~suffix t] is like {!val:is_suffix} but the suffix is a {!type:t}
    (instead of a [string]. *)

val for_all : (char -> bool) -> t -> bool
(** [for_all p bstr] is [true] iff for all indices [idx] of [bstr],
    [p bstr.{idx} = true]. *)

val exists : (char -> bool) -> t -> bool
(** [exists p bstr] is [true] iff there exists an index [idx] of [bstr] with
    [p bstr.{idx} = true]. *)

val equal : t -> t -> bool
(** [equal a b] is [a = b]. *)

val with_range : ?first:int -> ?len:int -> t -> t
(** [with_range ~first ~len bstr] are the consecutive bytes of [bstr] whose
    indices exist in the range \[[first];[first + len - 1]\].

    [first] defaults to [0] and [len] to [max_int]. Note that [first] can be any
    integer and [len] any positive integer. *)

val with_index_range : ?first:int -> ?last:int -> t -> t
(** [with_index_range ~first ~last bstr] are the consecutive bytes of [bstr]
    whose indices exists in the range \[[first];[last]\].

    [first] defaults to [0] and [last] to [length bstr - 1].

    Note that both [first] and [last] can be any integer. If [first > last] the
    interval is empty and the empty bigstring is returned. *)

val trim : ?drop:(char -> bool) -> t -> t
(** [trim ~drop bstr] is [bstr] with prefix and suffix bytes satisfying [drop]
    in [bstr] removed. [drop] defaults to [fun chr -> chr = ' ']. *)

val span :
  ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t * t
(** [span ~rev ~min ~max ~sat bstr] is [(l, r)] where:
    - if [rev] is [false] (default), [l] is at least [min] and at most [max]
      consecutive [sat] satisfying initial bytes of [bstr] or {!empty} if there
      are no such bytes. [r] are the remaining bytes of [bstr].
    - if [rev] is [true], [r] is at least [min] and at most [max] consecutive
      [sat] satisfying final bytes of [bstr] or {!empty} if there are no such
      bytes. [l] are the remaining bytes of [bstr].

    If [max] is unspecified the span is unlimited. If [min] is unspecified it
    defaults to [0]. If [min > max] the condition can't be satisfied and the
    left or right span, depending on [rev], is always empty. [sat] defaults to
    [Fun.const true].

    @raise Invalid_argument if [max] or [min] is negative. *)

val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
(** [take ~rev ~min ~max ~sat bstr] is the matching span of {!span} without the
    remaining one. In other words:

    {[
      (if rev then snd else fst) (span ~rev ~min ~max ~sat bstr)
    ]} *)

val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
(** [drop ~rev ~min ~max ~sat bstr] is the remaining span of {!span} without the
    matching span. In other words:

    {[
      (if rev then fst else snd) (span ~rev ~min ~max ~sat bstr)
    ]} *)

val cut : ?rev:bool -> sep:string -> t -> (t * t) option
(** [cut ~sep bstr] is either the pair [Some (l, r)] of the two (possibly empty)
    sub-buffers of [bstr] that are delimited by the first match of the non empty
    separator string [sep] or [None] if [sep] can't be matched in [bstr].
    Matching starts from the beginning of [bstr] ([rev] is [false], default) or
    the end ([rev] is [true]).

    The invariant [l ^ sep ^ r = s] holds.

    For instance, the {i ABNF} expression:

    {v
    field_name := *PRINT
    field_value := *ASCII
    field := field_name ":" field_value
    v}

    can be translated to:

    {[
      match Bstr.cut ~sep:":" value with
      | Some (field_name, field_value) -> ...
      | None -> invalid_arg "Invalid field"
    ]}

    @raise Invalid_argument if [sep] is the empty buffer. *)

val split_on_char : char -> t -> t list
val to_seq : t -> char Seq.t
val to_seqi : t -> (int * char) Seq.t
val of_seq : char Seq.t -> t
