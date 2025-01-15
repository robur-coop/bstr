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
      memory can be shared by several computing units.
    - it may be necessary, in system programming, to write to a particular zone
      in order to interact with a device. In this case, the bigstring can be
      found as an OCaml value bridging a special memory area (such as the
      framebuffer).

    A final feature of bigstring is that it can be seen as a slice. You can have
    another view of a bigstring that would be equally smaller. For example, the
    {!val:sub} operation in particular doesn't copy your bigstring, but offers
    you a "proxy" accessing the same memory area as the original bigstring.

    {1:pkt Encode & Decode packets.}

    In order to encode or decode packets (such as ARP or DNS packets), Bstr
    offers a small API for converting a slice of bytes from a {!val:Bstr.t} to a
    user-defined variant or record. *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val memcpy : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** [memcpy src ~src_off dst ~dst_off ~len] copies [len] bytes from [src] to
    [dst]. [src] must not overlap [dst]. Use {!val:memmove} if [src] & [dst] do
    overlap. *)

val memmove : t -> src_off:int -> t -> dst_off:int -> len:int -> unit
(** [memmove src ~src_off dst ~dst_off ~len] copies [len] bytes from [src] to
    [dst]. [src] and [dst] may overlap: copying takes place as though the bytes
    in [src] are first copied into a temporary array that does not overlap [src]
    or [dst], and the bytes are then copied from the temporary array to [dst].
*)

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

val create : int -> t
val make : int -> char -> t
val of_string : string -> t
val fill : t -> off:int -> len:int -> char -> unit
val blit : t -> src_off:int -> t -> dst_off:int -> len:int -> unit

val blit_from_string :
  string -> src_off:int -> t -> dst_off:int -> len:int -> unit

val blit_from_bytes :
  bytes -> src_off:int -> t -> dst_off:int -> len:int -> unit

(*
val init : int -> (int -> char) -> t
val copy : t -> t
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
val overlap : t -> t -> (int * int * int) option
*)

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

val set : t -> int -> char -> unit
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
val unsafe_set : t -> int -> char -> unit

val sub : t -> off:int -> len:int -> t
(** [sub bstr ~off ~len] does not allocate a bigstring, but instead returns a
    new view into [bstr] starting at [off], and with length [len].

    {b Note} that this does not allocate a new buffer, but instead shares the
    buffer of [bstr] with the newly-returned bigstring. *)

val overlap : t -> t -> (int * int * int) option

val sub_string : t -> off:int -> len:int -> string
(** [sub_string bstr ~off ~len] returns a string of length [len] containing the
    bytes of [t] starting at [off]. *)

val to_string : t -> string
(** [to_string bstr] is equivalent to
    [sub_string bstr ~off:0 ~len:(length bstr)]. *)

val blit_to_bytes : t -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
(** [blit_to_bytes src ~src_off dst ~dst_off ~len] copies [len] bytes from
    [src], starting at index [src_off], to byte sequence [dst], starting at
    index [dst_off].

    @raise Invalid_argument
      if [src_off] and [len] do not designate a valid range of [src], or if
      [dst_off] and [len] do not designate a valid range of [dst]. *)

val is_empty : t -> bool
(** [is_empty bstr] is [length bstr = 0]. *)

val is_prefix : affix:string -> t -> bool
(** [is_prefix ~affix bstr] is [true] iff [affix.[idx] = bstr.{idx}] for all
    indices [idx] of [affix]. *)

val is_infix : affix:string -> t -> bool
(** [is_infix ~affix bstr] is [true] iff there exists an index [j] in [bstr]
    such that for all indices [i] of [affix] we have [affix.[i] = bstr.{j + i}].
*)

val is_suffix : affix:string -> t -> bool
(** [is_suffix ~affix bstr] is [true] iff [affix.[n - idx] = bstr.{m - idx}] for
    all indices [idx] of [affix] with [n = String.length affix - 1] and
    [m = length bstr - 1]. *)

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

val shift : t -> int -> t
(** [shift bstr n] is [sub bstr n (length bstr - n)]. *)

val split_on_char : char -> t -> t list
val to_seq : t -> char Seq.t
val to_seqi : t -> (int * char) Seq.t
val of_seq : char Seq.t -> t

module Pkt : sig
  type bigstring = t
  type 'a t

  val char : char t
  val uint8 : int t
  val int8 : int t
  val beuint16 : int t
  val leuint16 : int t
  val neuint16 : int t
  val beint16 : int t
  val leint16 : int t
  val neint16 : int t
  val beint32 : int32 t
  val leint32 : int32 t
  val neint32 : int32 t
  val beint64 : int64 t
  val leint64 : int64 t
  val neint64 : int64 t
  val varint31 : int t
  val varint63 : int t
  val bytes : int -> string t
  val cstring : string t
  val until : char -> string t

  (* {2:records Records.}

     {[
       type header =
         { version : int32
         ; number : int32 }

       let _PACK = 0x5041434bl

       let header =
         record (fun pack version number ->
           if pack <> _PACK
           then invalid_arg "Invalid PACK file";
           { version; number })
         |+ field beint32 (fun _ -> _PACK)
         |+ field beint32 (fun t -> t.version)
         |+ field beint32 (fun t -> t.number)
         |> sealr
     ]} *)

  type ('a, 'b, 'c) open_record
  (** The type for representing open records of type ['a] with a constructor of
      ['b]. ['c] represents the remaining fields to be described using the
      {!val:(|+)} operator. An open record initially stisfies ['c = 'b] and can
      be {{!val:sealr} sealed} once ['c = 'a]. *)

  val record : 'b -> ('a, 'b, 'b) open_record
  (** [record f] is an incomplete representation of the record of type ['a] with
      constructor [f]. To complete the representation, add fields with
      {!val:(|+)} and then seal the record with {!val:sealr}. *)

  type ('a, 'b) field
  (** The type for fields holding values of type ['b] and belonging to a record
      of type ['a]. *)

  val field : 'a t -> ('b -> 'a) -> ('b, 'a) field
  (** [field n t g] is the representation of the field called [n] of type [t]
      with getter [g]. For instance:

      {[
        type t = { foo: string }

        let foo = field cstring (fun t -> t.foo)
      ]} *)

  val ( |+ ) :
    ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record
  (** [r |+ f] is the open record [r] augmented with the field [f]. *)

  val sealr : ('a, 'b, 'a) open_record -> 'a t
  (** [sealr r] seals the open record [r]. *)

  (** {2:variants Variants.}

      {[
        type t = Foo | Bar of string

        let t =
          variant (fun foo bar -> function Foo -> foo | Bar s -> bar s)
          |~ case0 Foo
          |~ case1 cstring (fun x -> Bar x)
          |> sealv
      ]} *)

  type ('a, 'b, 'c) open_variant
  (** The type for representing open variants of type ['a] with pattern-matching
      of type ['b]. ['c] represents the remaining constructors to be described
      using the {!val:(|~)} operator. An open variant initially satisfies
      ['c = 'b] and can be {{!val:sealv} sealed} once ['c = 'a]. *)

  val variant : 'b -> ('a, 'b, 'b) open_variant
  (** [variant n p] is an incomplete representation of the variant type called
      [n] of type ['a] using [p] to deconstruct values. To complete the
      representation, add cases with {!val:(|~)} and then seal the variant with
      {!val:sealv}. *)

  type ('a, 'b) case
  (** The type for representing variant cases of type ['a] with patterns of type
      ['b]. *)

  type 'a case_p
  (** The type for representing patterns for a variant of type ['a]. *)

  val case0 : 'a -> ('a, 'a case_p) case
  (** [case0 v] is a representation of a variant constructor [v] with no
      arguments. For instance:

      {[
        type t = Foo

        let foo = case0 Foo
      ]} *)

  val case1 : 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
  (** [case1 n t c] is a representation of a variant constructor [c] with an
      argument of type [t]. For instances:

      {[
        type t = Foo of string

        let foo = case1 cstring (fun s -> Foo s)
      ]} *)

  val ( |~ ) :
       ('a, 'b, 'c -> 'd) open_variant
    -> ('a, 'c) case
    -> ('a, 'b, 'd) open_variant
  (** [v |~ c] is the open variant [v] augmented with the case [c]. *)

  val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
  (** [sealv v] seals the open variant [v]. *)

  (* {2:decoder Decoder.} *)

  val decode : 'a t -> bigstring -> int ref -> 'a
end
