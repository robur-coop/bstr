type 'a t

(** {1:primitives Primitives.} *)

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

(* {2:decoder Decoder.} *)

module Bstr : sig
  val decode : 'a t -> Bstr.t -> int ref -> 'a
end
