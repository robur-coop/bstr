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

type 'a t

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

val bytes : int -> string t
(** [bytes n] is a representation of a bytes sequence of [n] byte(s). *)

val bstr : int -> Bstr.t t
(** [bstr n] is a representation of a bigstring of [n] byte(s). *)

val cstring : string t
val until : char -> string t

val const : 'a -> 'a t
(** [const v] is [v] without a serialization mechanism. *)

val seq : len:int -> 'a t -> 'a array t
(** [seq ~len v] is a representation of fixed-length arrays of values of type
    [v]. *)

val map : 'b t -> ('b -> 'a) -> ('a -> 'b) -> 'a t
(** This combinator allows defining a representative of one type in terms of
    another by supplying coercions between them. *)

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

val record : 'b -> ('a, 'b, 'b) open_record
(** [record f] is an incomplete representation of the record of type ['a] with
    constructor [f]. To complete the representation, add fields with {!val:(|+)}
    and then seal the record with {!val:sealr}. *)

type ('a, 'b) field
(** The type for fields holding values of type ['b] and belonging to a record of
    type ['a]. *)

val field : 'a t -> ('b -> 'a) -> ('b, 'a) field
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

val variant : 'b -> ('a, 'b, 'b) open_variant
(** [variant n p] is an incomplete representation of the variant type called [n]
    of type ['a] using [p] to deconstruct values. To complete the
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
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant
(** [v |~ c] is the open variant [v] augmented with the case [c]. *)

val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
(** [sealv v] seals the open variant [v]. *)

(** {2:decoder Decoder.} *)

val decode_bstr : 'a t -> Bstr.t -> int ref -> 'a
(** [decode_bstr repr] is the binary decoder for values of type [repr]. *)

val decode : 'a t -> string -> int ref -> 'a

(** {2:encoder Encoder.} *)

val encode_bstr : 'a t -> 'a -> Bstr.t -> int ref -> unit
val to_string : 'a t -> 'a -> string

module Size : sig
  type -'a size_of
  (** The type for size function related to binary encoder/decoder. *)

  val size_of : 'a t -> 'a size_of

  type 'a t = private
    | Static of int
    | Dynamic of 'a
    | Unknown
        (** A value representing information known about the length in bytes of
            encodings produced by a particular binary codec:
            - [Static n]: all encodings produced by this codec have length [n];
            - [Dynamic fn]: the length of binary encodings is dependent on the
              specific value, but may be efficiently computed at run-time via
              the function [fn];
            - [Unknown]: this codec may produce encodings that cannot be
              efficiently pre-computed. *)

  val of_encoding : 'a size_of -> (Bstr.t -> int -> int) t
  val of_value : 'a size_of -> ('a -> int) t
end

val size_of_value : 'a t -> 'a -> int option
(** [size_of_value encoding value] attempts to calculate the number of bytes
    needed to encode the given [value] according to the given encoding. *)

val size_of_bstr : ?off:int -> 'a t -> Bstr.t -> int option
(** [size_of_encoding ?off encoding bstr] attempts to calculate the number of
    bytes required to decode a value according to the given [encoding] and
    according to what can be decoded in the given byte sequence [bstr] (at the
    given offset [off], defaults to [0]). *)
