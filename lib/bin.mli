(** [Bin] is a small library for encoding and decoding information from a buffer
    (like a [bytes] or a [bigstring]). Unlike a {i parser combinator}, [Bin]
    cannot decode a stream.

    [Bin] can be used to project values coming from a pre-allocated buffer such
    as a "framebuffer" (video, ethernet, etc.) or to inject values into it.
    [Bin] can be seen as a library for describing (fairly basic) "C-like" types
    of values that can be injected/projected into/from a particular memory area:

    {[
      #define PROPTAG_GET_COMMAND_LINE 0x00050001
      #define VALUE_LENGTH_RESPONSE (1 << 31)

      struct __attribute__((packed)) cmdline {
        uint32_t id;
        uint32_t value_len;
        uint32_t param_len;
        uint8    str[2048];
      };

      struct __attribute__((packed)) property_tag {
        uint32_t id;
        uint32_t value_len;
        uint32_t param_len;
      };

      extern char _tags;

      char *get_cmdline() {
        struct cmdline p;
        p.id = PROPTAG_GET_COMMAND_LINE;
        p.value_len = len - sizeof(struct property_tag);
        p.param_len = 2048 & ~VALUE_LENGTH_RESPONSE;
        memcpy(&_tags, &p, sizeof(struct cmdline)); // inject
        ...
      }
    ]}

    [Bin] therefore allows you to describe a representation of a serialized
    value in bytes and to associate with it a function that allows you to obtain
    an OCaml value such as a record or a variant.

    {[
    open Bin

    type cmdline = {
        id: int32
      ; value_len: int32
      ; param_len: int32
      ; cmdline: string
    }

    let cmdline =
      record (fun id value_len param_len -> { id; value_len; param_len })
      |+ field neint32 (Fun.const 0x00050001l)
      |+ field neint32 (fun t -> t.value_len)
      |+ field neint32 (fun t -> t.param_len)
      |+ field cstring (fun t -> t.cmdline)
      |> sealr

    let encode_into tags ?(off = 0) value =
      let off = ref off in
      Bin.encode_bstr cmdline value tags off (* inject *)
    ]}

    Of course, it's not as fast as what we can do in C, but [Bin] has the
    advantage of offering a small DSL that allows us to describe these types and
    go directly to OCaml values, which is generally more pleasant to manipulate
    with OCaml than to make C stubs. *)

(** For performance reasons, it is sometimes preferable to handle functions
    specialising in one or two arguments rather than applying all the function’s
    arguments (and repeating internal calculations whenever that first or second
    argument appears). The Staged module allows you to define a boundary between
    what should be ‘pre-applied’ and what should be fully applied afterwards.

    {[
    let fn x =
      let x = x + 1 in
      Staged.stage (fun y -> x + y)

    let g = fn 20
    let () = print_int ((Staged.unstage g) 21)
    ]}

    In the example above, we prepare the function [g] so that the calculation
    [x+1] is {i already} done and no longer needs to be performed. Furthermore,
    the application of [g] will only involve the calculation [x+y].

    In the case of [Bin], it may be useful to prepare how a format is to be
    decoded or encoded. Furthermore, {!val:decode} and {!val:encode_bstr} return
    a [Staged.t] value in order to force these functions to specialise via the
    description of a binary format.

    In this way, the decoder/encoder for this format is properly specialised and
    optimised. This is the main reason for the existence of this module. *)
module Staged : sig
  type +'a t

  val stage : 'a -> 'a t
  val unstage : 'a t -> 'a
end

(** {2 Positions and lengths.}

    Decoding and encoding walk a buffer with a {b cursor}. The {!type:pos} given
    to {!val:decode_bstr}, {!val:decode} and {!val:encode_bstr} is a reference
    on the index of the next byte to read or to write, and each of these
    functions advances it by what it consumed or produced. The same cursor can
    therefore be used to chain several values:

    {[
    let dec = Bin.Staged.unstage (Bin.decode_bstr Bin.beuint16) in
    let pos = ref Bin.Off.zero in
    let a = dec buf pos in (* reads bytes 0 and 1, [pos] becomes 2 *)
    let b = dec buf pos in (* reads bytes 2 and 3, [pos] becomes 4 *)
    ...
    ]}

    Two distinct notions are involved, and they have distinct types so that they
    cannot be mixed up: an {!type:Off.t} says {b where} a byte is, a
    {!type:Len.t} says {b how many} bytes there are. Both are [private int]:
    reading one back as an [int] is the free coercion [(x :> int)], while going
    the other way is impossible by accident. *)

module Off : sig
  type 'w t = private int
  (** An index into a byte sequence, counted in bytes and never negative. The
      parameter ['w] records {b what the index is counted from}. *)

  type abs
  (** An {b absolute} index, counted from the first byte of the buffer. It
      designates a byte on its own, so it is what one can read or write at. A
      cursor is of this kind. *)

  type rel
  (** A {b relative} index: a displacement, counted from somewhere else: in
      practice from the beginning of the value being decoded. Describing a
      format yields such displacements (the offset of a field within its record)
      long before any buffer exists, so they designate nothing by themselves.
      They become usable once added to an absolute index, and the parameter is
      there to make sure that addition happens. *)

  val zero : 'w t
  (** [zero] is [0]. It is both the first index of a buffer and the null
      displacement, hence its two possible kinds: it is the value to start a
      cursor with. *)

  val at : abs t -> rel t -> abs t
  (** [at base delta] is the absolute index which sits [delta] bytes after
      [base]. This is the only way to turn a displacement into an index one can
      read at. *)

  val v : int -> abs t
  (** [v n] is a new absolute offset. *)
end

module Len : sig
  type t = private int
  (** A number of bytes, never negative: the size of a field, the width of the
      window a decoder is allowed to read within. *)
end

type pos = Off.abs Off.t ref
(** The cursor of a decoder or an encoder: the index of the next byte to read or
    to write. Start it with [ref Off.zero] and read it back with
    [(!pos :> int)]. *)

type 'a t
(** A type to describe a binary format. *)

(** {1:primitives Primitives.} *)

val char : char t
(** [char] is a representation of the character type. *)

val uint8 : int t
(** [uint8] is a representation of unsigned 8-bit integers. *)

val int8 : int t
(** [int8] is a representation of 8-bit integers. *)

val beuint16 : int t
(** [beint16] is a representation of big-endian unsigned 16-bit integers. *)

val leuint16 : int t
(** [leint16] is a representation of little-endian unsigned 16-bit integers. *)

val neuint16 : int t
(** [neint16] is a representation of native-endian unsigned 16-bit integers. *)

val beint16 : int t
(** [beint16] is a representation of big-endian 16-bit integers. *)

val leint16 : int t
(** [leint16] is a representation of little-endian 16-bit integers. *)

val neint16 : int t
(** [neint16] is a representation of native-endian 16-bit integers. *)

val beint32 : int32 t
(** [beint32] is a representation of big-endian 32-bit integers. *)

val leint32 : int32 t
(** [leint32] is a representation of little-endian 32-bit integers. *)

val neint32 : int32 t
(** [neint32] is a representation of native-endian 32-bit integers. *)

val beint64 : int64 t
(** [beint64] is a representation of big-endian 64-bit integers. *)

val leint64 : int64 t
(** [leint64] is a representation of little-endian 64-bit integers. *)

val neint64 : int64 t
(** [neint64] is a representation of native-endian 64-bit integers. *)

val varint : int t
(** [varint] is a representation of an integer value using a sequence of bytes.
    The number of bytes that is needed depends on the value being represented,
    with larger magnitudes using more bytes than smaller ones. *)

type len
(** [len] is a type that represents a size (fixed or dynamic) for certain
    primitives such as {!val:bytes} or {!val:bstr}. *)

val fixed : int -> len
(** [fixed len] is a fixed length of [len] bytes. *)

val delim : char -> len
(** [delim chr] is a length until we reach the [chr] byte. *)

val prefix : int t -> len
(** [prefix t] is a representation of a value which size is specified by a
    {i header}/prefix [t]. *)

val rest : len
(** [rest] is the rest of the payload. *)

val bytes : len -> string t
(** [bytes n] is a representation of a bytes sequence of [n] byte(s). *)

val bstr : len -> Bstr.t t
(** [bstr n] is a representation of a bigstring of [n] byte(s). *)

val cstring : string t
(** [cstring] is [bytes (delim '\000')]. *)

val until : char -> string t
(** [until chr] is [bytes (delim chr)]. *)

val const : 'a -> 'a t
(** [const v] is [v] without a serialization mechanism. *)

val seq : len -> 'a t -> 'a array t
(** [seq len t] is a consecutive sequence of [len] elements represented by [t].
    It returns an array. *)

val list : len -> 'a t -> 'a list t
(** [list len t] is a consecutive sequence of [len] elements represented by [t].
    It returns an list. *)

val map : 'b t -> ('b -> 'a) -> ('a -> 'b) -> 'a t
(** [map t f g] describes a value of type ['a] {i via} the representation [t] of
    another type ['b], by supplying the coercions between them: decoding reads a
    ['b] with [t] and turns it into an ['a] with [f], encoding turns the ['a]
    back into a ['b] with [g] and writes it with [t].

    [f] and [g] must be inverse of each other, otherwise encoding a value then
    decoding it does not give that value back.

    {[
    type kind = Query | Response

    let kind =
      map uint8
        (function 0 -> Query | _ -> Response)
        (function Query -> 0 | Response -> 1)
    ]}

    The shape of the encoding does not depend on the value: [map] is therefore
    transparent for the decoder, which can still fuse it into its {i static}
    path. Use it whenever it is enough — see {!val:bind} otherwise. *)

val bind : 'a t -> ('a -> 'b t) -> ('b -> 'a) -> 'b t
(** [bind t prj inj] is like {!val:map}, except that the representation of what
    remains {b depends on the value which was just read}. It is the combinator
    for the formats where a header decides how the rest must be interpreted: a
    length, a tag, a version number.

    Decoding reads an ['a] with [t], then decodes the rest with [prj a].
    Encoding is the mirror image and this is where [inj] comes in: from the
    value to write, [inj] recovers the ['a] which describes it, [bind] writes
    that ['a] with [t], and then writes the value itself with [prj (inj v)].
    Without [inj], the encoder would have no way to know which header to emit.

    [inj] must therefore agree with what [prj] accepts: for any value [v] that
    [prj a] can decode, [inj v] must be [a]. Otherwise the header and the
    payload disagree and the result cannot be decoded back.

    Before reaching for [bind], check that a simpler combinator does not already
    cover the case. A size in bytes is {!val:prefix} on {!val:bytes}, a number
    of elements is {!val:prefix} on {!val:list} or {!val:seq}, a tag which
    selects a constructor is a {!val:variant} sealed with {!val:sealv}, and a
    format which refers to itself is {!val:fix}. [bind] is for what remains:
    when the value which was read has to be {b computed with} to know what
    follows.

    Here is such a case, a payload whose length is given in 32-bit words (as the
    {i IHL} of an IPv4 header or the {i data offset} of a TCP one).
    {!val:prefix} cannot express it, since it would take the number for a count
    of bytes:

    {[
    let payload =
      bind uint8
        (fun words -> bytes (fixed (words * 4)))
        (fun payload -> String.length payload / 4)
    ]}

    {b Note} [prj] is called for {b every} value encoded or decoded, and the
    representation it returns is interpreted on the fly. Unlike {!val:map},
    [bind] therefore never takes part in the {i static} (fused) path of the
    decoder, and {!val:size_of_value} can only be dynamic on it. Prefer
    {!val:map} when the shape of the encoding does not depend on the value.

    See {!val:fix} for a recursive format, where [bind] is what lets the
    recursion stop on a value read from the input. *)

val ( let+ ) : 'a t * ('a -> 'b) * ('b -> 'a) -> ('b t -> 'c) -> 'c
(** [let+] is the binding operator of {!val:map}: [let+ t = (x, f, g) in expr]
    is [let t = map x f g in expr].

    {b Note} the identifier it binds is the {b representation} returned by
    {!val:map} — {b not} a decoded value. Nothing is read nor written when this
    runs; it only names an intermediate representation. *)

val ( let* ) : 'a t * ('a -> 'b t) * ('b -> 'a) -> ('b t -> 'c) -> 'c
(** [let*] is the binding operator of {!val:bind}: [let* t = (x, f, g) in expr]
    is [let t = bind x f g in expr]. As with {!val:( let+ )}, [t] is the
    representation, not a value.

    It exists to keep a large {!val:bind} readable: name the two functions, then
    assemble them.

    {[
    let payload =
      let prj words = bytes (fixed (words * 4)) in
      let inj payload = String.length payload / 4 in
      let* t = (uint8, prj, inj) in
      t
    ]}

    On a [bind] as small as this one the plain function reads just as well; see
    {!val:fix} for the shape which motivates the operator. *)

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
    {!val:(|+)} operator. An open record initially stisfies ['c = 'b] and can be
    {{!val:sealr} sealed} once ['c = 'a]. *)

val record : ?name:string -> 'b -> ('a, 'b, 'b) open_record
(** [record f] is an incomplete representation of the record of type ['a] with
    constructor [f]. To complete the representation, add fields with {!val:(|+)}
    and then seal the record with {!val:sealr}. *)

type ('a, 'b) field
(** The type for fields holding values of type ['b] and belonging to a record of
    type ['a]. *)

val field : ?name:string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
(** [field n t g] is the representation of the field called [n] of type [t] with
    getter [g]. For instance:

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

val variant : ?name:string -> 'b -> ('a, 'b, 'b) open_variant
(** [variant n p] is an incomplete representation of the variant type called [n]
    of type ['a] using [p] to deconstruct values. To complete the
    representation, add cases with {!val:(|~)} and then seal the variant with
    {!val:sealv}. *)

type ('a, 'b) case
(** The type for representing variant cases of type ['a] with patterns of type
    ['b]. *)

type 'a case_p
(** The type for representing patterns for a variant of type ['a]. *)

val case0 : ?name:string -> ?tag:int -> 'a -> ('a, 'a case_p) case
(** [case0 v] is a representation of a variant constructor [v] with no
    arguments. For instance:

    {[
    type t = Foo

    let foo = case0 Foo
    ]} *)

val case1 :
  ?name:string -> ?tag:int -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
(** [case1 n t c] is a representation of a variant constructor [c] with an
    argument of type [t]. For instances:

    {[
    type t = Foo of string

    let foo = case1 cstring (fun s -> Foo s)
    ]} *)

val ( |~ ) :
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant
(** [v |~ c] is the open variant [v] augmented with the case [c]. *)

val sealv : ?tag:int t -> ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
(** [sealv v] seals the open variant [v]. *)

(** {2:bits Bits.} *)

type ('a, 'b, 'c) open_bits
type endianness = Big_endian | Little_endian | Native_endian
type bit_order = Msb_first | Lsb_first
type bits_base = B8 | B16 of endianness | B32 of endianness
type ('a, 'b) bit_field
type ('a, 'b) bit_fields

val bits :
     ?name:string
  -> ?order:bit_order
  -> 'a
  -> 'b
  -> 'a * bit_order * ('c -> string * 'b * 'c)

val bit : ?name:string -> int -> ('a -> int) -> ('a, int) bit_field
val flag : ?name:string -> ('a -> bool) -> ('a, bool) bit_field

val ( |* ) :
     bits_base * bit_order * (('c, 'd -> 'e) bit_fields -> 'f)
  -> ('c, 'd) bit_field
  -> bits_base * bit_order * (('c, 'e) bit_fields -> 'f)

val sealb :
     bits_base
     * bit_order
     * (('a, 'a) bit_fields -> string * 'b * ('c, 'b) bit_fields)
  -> 'c t

(** {2:fix Fix point.} *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix @@ fun t -> ...] computes the fixpoint of the given function and runs
    the resultant codec. The argument that [fn] receives is the result of
    [fix fn], which [fn] must use, paradoxically, to define [fix fn].

    [fix] is useful when constructing codecs for inductively-defined types such
    as sequences, trees, etc. Consider for example the codec of a {i QNAME} (see
    RFC1035 § 4.1.2). They describe it as:

    > a domain name represented as a sequence of labels, where each label
    consists of a length octet followed by that number of octets. The domain
    name terminates with the zero length octet for the null label of the root.

    Here is the equivalent using [Bin]:

    {[
    type name = string list

    let qname =
      let open Bin in
      fix @@ fun qname ->
      let prj = function
        | 0 -> const []
        | len ->
            record (fun label rest -> label :: rest)
            |+ field (bytes (fixed len)) List.hd
            |+ field qname List.tl
            |> sealr
      in
      let inj = function [] -> 0 | label :: _ -> String.length label in
      bind uint8 prj inj
    ]} *)

(** {2:decoder Decoder.} *)

val decode_bstr : 'a t -> (Bstr.t -> pos -> 'a) Staged.t
(** [decode_bstr repr] is the binary decoder for values of type [repr] on
    {!type:Bstr.t}. *)

val decode : 'a t -> (string -> pos -> 'a) Staged.t
(** [decode repr] is the binary decoder for values of type [repr] on [string].
*)

(** {2:encoder Encoder.} *)

val encode_bstr : 'a t -> ('a -> Bstr.t -> pos -> unit) Staged.t
(** [encode_bstr repr] is the binary encoder for value of type [repr] on
    {!type:Bstr.t}. *)

val to_string : 'a t -> ('a -> string) Staged.t
(** [to_string repr] is a function which returns the string representation of
    values of type [repr]. *)

(** {2:size Size of representations.} *)

module Size : sig
  (** A value representing information known about the length in bytes of
      encodings produced by a particular binary codec:
      - [Static n]: all encodings produced by this codec have length [n];
      - [Dynamic fn]: the length of binary encodings is dependent on the
        specific value, but may be efficiently computed at run-time via the
        function [fn] *)
  type 'a s = private Static of int | Dynamic of ('a -> int)

  type 'a t
end

val size_of_value : 'a t -> 'a -> int
(** [size_of_value encoding value] attempts to calculate the number of bytes
    needed to encode the given [value] according to the given encoding. *)
