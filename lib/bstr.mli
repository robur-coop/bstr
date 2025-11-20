(** A small library for manipulating bigstrings.

    A bigstring is a mutable data structure that contains a fixed-length
    sequence of bytes. Each byte can be indexed in constant time for reading and
    writing.

    Given a byte sequence [bstr] of length [len], we can access each of the
    [len] bytes of [bstr] via its index in the sequence. Indexes start at [0],
    and will call an index valid in [bstr] if it falls within the range
    [[0...len-1]] (inclusive). A position is the point between two bytes or at
    the beginning or end of the sequence. We call a position valid in [bstr] if
    it falls within the range [[0...len]] (inclusive). Note that byte at index
    [n] is between positions [n] and [n+1].

    Two parameters [off] and [len] are said to designate a valid range of [bstr]
    if [len >= 0] and [off] and [off+len] are valid positions in [bstr].

    Byte sequences can be modified in place, for instance via the {!val:set} and
    {!val:blit} functions described below.

    {1:bigarray Bigstrings & Bigarrays.}

    Bigstring is a specialised version of {!module:Bigarray} that not only
    handles bytes in the form of {!module:Char}acter but also imposes a "C-like"
    (see {!val:Bigarray.c_layout}) view as described above and allows common
    functions such as [memcpy(3)] or [memmove(3)] to be offered.

    For more details about Bigstrings and Bigarrays, we invite you to read the
    {!module:Bigarray} documentation, which offers more general functions that
    can be applied to Bigstrings.

    {1:bytes Bigstrings & Bytes.}

    Like bytes, a bigstring is a mutable data structure that contains a
    fixed-length sequence of bytes. However, a bigstring has a few special
    features that can make it more interesting to use than bytes.

    {2:location Bigstrings and the Garbage Collector.}

    A bigstring is not allocated in the same way as a standard OCaml value. In
    fact, the byte sequence that the bigstring refers to is found in the
    {i C heap} (rather than the OCaml heap). This means that the byte sequence
    can come from a [malloc(3)] or a function requesting a particular memory
    area from the system such as [Unix.map_file].

    This particularity has an implication with the GC: the byte sequence is
    {b not relocatable}. That is to say that during the cycle of the Garbage
    Collector, this byte sequence does not move — in contrast, a [bytes] can be
    moved by the GC (typically, from the minor heap to the major heap).

    Thus, bigstrings have advantages and disadvantages compared to bytes due to
    this particularity:
    - Creating a bigstring can be expensive. Whether it is with
      [malloc(3)]/{!val:create} or [Unix.map_file], creating a bigstring will
      always be more expensive than creating bytes with OCaml. For small byte
      sequences, it is therefore preferable to use bytes.
    - Since a bigstring cannot be moved, its position can be shared by [Thread]s
      and/or [Domain]s without interacting with the GC. An example is being able
      to perform a complex computation in parallel from the bytes of this
      sequence without {i blocking} the Garbage Collector during this
      computation.

    Depending on these characteristics, it may be more advantageous to use a
    bigstring rather than [bytes]. This basically depends on your usage, and the
    special features of bigstrings can unlock opportunities to outperform byte
    calculations or analysis.

    {2:sub Bigstring and slice.}

    Another advantage of bigstrings is that copying is avoided when extracting
    part of a larger bigstring. This is because the {!val:sub} function returns
    a "proxy" of the original bigstring.

    In this respect, and to be very precise, {!val:sub} avoids copying but the
    creation of this "proxy" {b remains} costly. In addition, this library is
    distributed with a new {!module:Slice_bstr} module. The latter offers a new
    type whose {!val:Slice_bstr.sub} function is much less costly than
    {!val:sub}.

    {1 Bigstrings.} *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** {2 Constructors.} *)

val empty : t
(** [empty] is an empty bigstring. *)

val create : int -> t
(** [create len] returns a new byte sequence of length [len]. The sequence
    {b is unitialized} and contains arbitrary bytes. *)

val make : int -> char -> t
(** [make len chr] is {!type:t} of length [len] with each index holding the
    character [chr]. *)

val copy : t -> t
(** [copy t] returns a new byte sequence that contains the same bytes as the
    argument. *)

val init : int -> (int -> char) -> t
(** [init len fn] returns a fresh byte sequence of length [len], with character
    [idx] initialized to the result of [fn idx] (in increasing index order). *)

(** {2 Memory-safe Operations.} *)

val of_string : string -> t
(** [of_string str] returns a new {!type:t} that contains the contents of the
    given string [str]. *)

val string : ?off:int -> ?len:int -> string -> t
(** [string ~off ~len str] is the sub-buffer of [str] that starts at position
    [off] (defaults to [0]) and stops at position [off + len] (defaults to
    [String.length str]). [str] is fully-replaced by a fresh allocated
    {!type:t}.

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [str]. *)

val sub_string : t -> off:int -> len:int -> string
(** [sub_string bstr ~off ~len] returns a string of length [len] containing the
    bytes of [bstr] starting at [off].

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [t]. *)

val to_string : t -> string
(** [to_string bstr] is equivalent to
    [sub_string bstr ~off:0 ~len:(length bstr)]. *)

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
(** [chop bstr] returns the first element of [bstr] or the last element if
    [rev = true]. If [bstr] is empty, it returns [None]. *)

val concat : string -> t list -> t
(** [concat sep ts] concatenates the list of bigstrings [ts], inserting the
    separator string [sep] between each. *)

val extend : t -> int -> int -> t
(** [extend bstr left right] returns a new bigstring that contains the bytes of
    [bstr], with [left] zero bytes prepended and [right] zero byte appended to
    it. If [left] or [right] is negative, then bytes are removed (instead of
    appended) from the corresponding side of [bstr].

    @raise Invalid_argument if the result length is negative *)

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

val memcpy_mmaped : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** [memcpy_mmaped] is like {!val:memcpy} but [src] and [dst] can be a
    {i mmaped} bigarray (from [Unix.map_file]). In this specific case, copying
    from one to the other can take some time because it involves reading/writing
    to disk. The operation can take longer than if the two bigarrays were
    allocated via [malloc()]/{!val:Bigarray.Array1.create}.

    It may therefore be worthwhile to release the GC lock so that this specific
    operation can be carried out in parallel (in a [Thread]) without
    interruption by the GC.

    Note that the bigarrays do not necessarily need to be {i mmaped}. This
    function also applies to "normal" bigarrays. It may also be worthwhile to
    use this function if you know that you are copying a large area and would
    like to do it in parallel (in a [Thread]). *)

val memmove : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** [memmove src ~src_off dst ~dst_off ~len] copies [len] bytes from [src] to
    [dst]. [src] and [dst] may overlap: copying takes place as though the bytes
    in [src] are first copied into a temporary array that does not overlap [src]
    or [dst], and the bytes are then copied from the temporary array to [dst].

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val memmove_mmaped : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** [memmove_mmaped] is like {!val:memmove} but [src] and [dst] can be a
    {i mmaped} bigarray (from [Unix.map_file]). In this specific case, copying
    from one to the other can take some time because it involves reading/writing
    to disk. The operation can take longer than if the two bigarrays were
    allocated via [malloc()]/{!val:Bigarray.Array1.create}.

    It may therefore be worthwhile to release the GC lock so that this specific
    operation can be carried out in parallel (in a [Thread]) without
    interruption by the GC.

    Note that the bigarrays do not necessarily need to be {i mmaped}. This
    function also applies to "normal" bigarrays. It may also be worthwhile to
    use this function if you know that you are copying a large area and would
    like to do it in parallel (in a [Thread]). *)

val memcmp : t -> src_off:int -> t -> dst_off:int -> len:int -> int
(** [memcmp s1 ~src_off s2 ~dst_off ~len] compares the first [len] bytes of the
    memory areas [s1] (starting at [src_off]) and [s2] (starting at [dst_off]).

    [memcmp] returns [0] is [s1] and [s2] don't match.

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val memset : t -> off:int -> len:int -> char -> unit
(** [memset t ~off ~len chr] fills [len] bytes (starting at [off]) into [t] with
    the constant byte [chr].

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [t]. *)

val fill : t -> ?off:int -> ?len:int -> char -> unit
(** [fill t off len chr] modifies [t] in place, replacing [len] characters with
    [chr], starting at [off].

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [t]. *)

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

    {b Note} [sub] does not allocate a new buffer, but instead shares the memory
    area of [bstr] with the newly-returned bigstring. This means that the
    changes ([set{,_*}] functions) made to the returned bigstring will also be
    reflected in the [bstr] bigstring given.

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

(** {2 Predicates and comparaisons.} *)

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

val contains : t -> ?off:int -> ?len:int -> char -> bool
(** [contains bstr ?off ?len chr] is [true] if and only if [chr] appears in
    [len] byte(s)'s [bstr] after position [off] (defaults to [0]). *)

val equal : t -> t -> bool
(** [equal a b] is [a = b]. *)

val constant_equal : t -> t -> bool
(** [constant_equal] gives the same result as {!val:equal} but the execution
    time of the function, whether or not the two values are equivalent (as long
    as they have the {b same} size) is the same.

    Indeed, the {!val:equal} function ends as soon as a difference exists. This
    function continues even if a difference exists. This function is useful when
    comparing passwords — and avoiding an {i timing attack}. *)

val compare : t -> t -> int
(** [compare bstr0 bstr1] sorts [bstr0] and [bstr1] in lexicographical order. *)

val index : t -> ?off:int -> ?len:int -> char -> int option
(** [index bstr ?off ?len chr] is the index of the first occurrence of [chr] in
    [len] byte(s)'s [bstr] after position [off] (defaults to [0]). If [chr] does
    not occur in given range of [bstr], we return [None].

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [bstr]. *)

val memchr : t -> off:int -> len:int -> char -> int
(** [memchr t ~off ~len chr] scans [len] bytes (starting at [off]) of [t] for
    the first instance of [chr]. It returns the position in [t] where the first
    occurrence of [chr] is found. Otherwise, it returns [-1].

    @raise Invalid_argument
      if [off] and [len] do not designate a valid range of [t]. *)

(** {2 Extracting substrings.} *)

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

val cuts : ?rev:bool -> ?empty:bool -> sep:string -> t -> t list
(** [cuts sep bstr] is the list of all subbigstrings of [bstr] that are
    delimited by matches of the non empty separator string [sep]. Empty
    subbigstrings are omitted in the list if [empty] is [false] (defaults to
    [true]).

    Matching separators in [bstr] starts from the beginning of [bstr] ([rev] is
    [false], default) or the end ([rev] is [true]). Once one is found, the
    separator is skipped and matching starts again, that is separator matches
    can't overlap. If there is no separator match in [bstr], the list [[bstr]]
    is returned.

    The following invariants hold:
    - [equal (concat (of_string sep) (cuts ~empty:true ~sep bstr)) bstr]
    - [cuts ~empty:true ~sep bstr <> []]

    @raise Invalid_argument if [sep] is the empty string. *)

val split_on_char : char -> t -> t list
(** [split_on_char sep t] is the list of all (possibly empty)
    {!val:sub}-bigstrings of [t] that are delimited by the character [sep]. If
    [t] is empty, the result is the singleton list [[empty]].

    The function's result is specified by the following invariant:
    - the list is not empty.
    - concatenating its elements using [sep] as a separator returns a bigstring
      equal to the input.
    - no bigstring in the result contains the [sep] character. *)

(** {2 Traversing strings.} *)

val iter : (char -> unit) -> t -> unit
(** [iter fn t] applies function [fn] in turn to all the characters of [t]. It
    is equivalent to [fn t.{0}; fn t.{1}; ...; fn t.{length t - 1}; ()]. *)

val to_seq : t -> char Seq.t
(** Iterate on the bigstring, in increasing index order. Modifications of the
    bigstring during iteration will be reflected in the sequence. *)

val to_seqi : t -> (int * char) Seq.t
(** Iterate on the bigstring, in increasing order, yielding indices along chars.
*)

val of_seq : char Seq.t -> t
(** Create a bigstring from the generator. *)
