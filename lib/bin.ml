module Witness = struct
  type (_, _) eq = Refl : ('a, 'a) eq
  type _ equality = ..

  module type Inst = sig
    type t
    type _ equality += Eq : t equality
  end

  type 'a t = (module Inst with type t = 'a)

  let make : type a. unit -> a t =
   fun () ->
    let module Inst = struct
      type t = a
      type _ equality += Eq : t equality
    end in
    (module Inst)

  let _eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None
end

type endianness = Big_endian | Little_endian | Native_endian

type _ t =
  | Primary : 'a primary -> 'a t
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t

and _ primary =
  | Char : char primary
  | UInt8 : int primary
  | Int8 : int primary
  | UInt16 : endianness -> int primary
  | Int16 : endianness -> int primary
  | Int32 : endianness -> int32 primary
  | Int64 : endianness -> int64 primary
  | Var_int31 : int primary
  | Var_int63 : int primary
  | Bytes : int -> string primary
  | CString : string primary
  | Until : char -> string primary

and _ a_case = C0 : 'a case0 -> 'a a_case | C1 : ('a, 'b) case1 -> 'a a_case

and _ case_v =
  | CV0 : 'a case0 -> 'a case_v
  | CV1 : ('a, 'b) case1 * 'b -> 'a case_v

and 'a case0 = { ctag0: int; c0: 'a }

and ('a, 'b) case1 = {
    ctag1: int
  ; ctype1: 'b t
  ; cwitn1: 'b Witness.t
  ; c1: 'b -> 'a
}

and 'a record = { rwit: 'a Witness.t; rfields: 'a fields_and_constr }

and 'a fields_and_constr =
  | Fields : ('a, 'b) fields * 'b -> 'a fields_and_constr

and ('a, 'b) fields =
  | F0 : ('a, 'a) fields
  | F1 : ('a, 'b) field * ('a, 'c) fields -> ('a, 'b -> 'c) fields

and ('a, 'b) field = { ftype: 'b t; fget: 'a -> 'b }

and 'a variant = {
    vwit: 'a Witness.t
  ; vcases: 'a a_case array
  ; vget: 'a -> 'a case_v
}

module Fields_folder (Acc : sig
  type ('a, 'b) t
end) =
struct
  type 'a t = {
      nil: ('a, 'a) Acc.t
    ; cons: 'b 'c. ('a, 'b) field -> ('a, 'c) Acc.t -> ('a, 'b -> 'c) Acc.t
  }

  let rec fold : type a c. a t -> (a, c) fields -> (a, c) Acc.t =
   fun folder -> function
    | F0 -> folder.nil
    | F1 (f, fs) -> folder.cons f (fold folder fs)
end

(* decoder *)

module Bstr = struct
  module Record_decoder = Fields_folder (struct
    type ('a, 'b) t = Bstr.t -> int ref -> 'b -> 'a
  end)

  type 'a decoder = Bstr.t -> int ref -> 'a

  let decode_char bstr pos =
    let idx = !pos in
    incr pos; Bstr.get bstr idx
  [@@inline always]

  let decode_uint8 bstr pos =
    let idx = !pos in
    incr pos; Bstr.get_uint8 bstr idx
  [@@inline always]

  let decode_int8 bstr pos =
    let idx = !pos in
    incr pos; Bstr.get_int8 bstr idx
  [@@inline always]

  let decode_uint16 e bstr pos =
    let idx = !pos in
    incr pos;
    match e with
    | Big_endian -> Bstr.get_uint16_be bstr idx
    | Little_endian -> Bstr.get_uint16_le bstr idx
    | Native_endian -> Bstr.get_uint16_ne bstr idx
  [@@inline always]

  let decode_int16 e bstr pos =
    let idx = !pos in
    incr pos;
    match e with
    | Big_endian -> Bstr.get_int16_be bstr idx
    | Little_endian -> Bstr.get_int16_le bstr idx
    | Native_endian -> Bstr.get_int16_ne bstr idx
  [@@inline always]

  let decode_int32 e bstr pos =
    let idx = !pos in
    incr pos;
    match e with
    | Big_endian -> Bstr.get_int32_be bstr idx
    | Little_endian -> Bstr.get_int32_le bstr idx
    | Native_endian -> Bstr.get_int32_ne bstr idx
  [@@inline always]

  let decode_int64 e bstr pos =
    let idx = !pos in
    incr pos;
    match e with
    | Big_endian -> Bstr.get_int64_be bstr idx
    | Little_endian -> Bstr.get_int64_le bstr idx
    | Native_endian -> Bstr.get_int64_ne bstr idx
  [@@inline always]

  let decode_bytes len bstr pos =
    let off = !pos in
    pos := !pos + len;
    Bstr.sub_string bstr ~off ~len
  [@@inline always]

  let rec decode : type a. a t -> a decoder = function
    | Primary p -> prim p
    | Record r -> record r
    | Variant _ -> assert false

  and prim : type a. a primary -> a decoder = function
    | Char -> decode_char
    | UInt8 -> decode_uint8
    | Int8 -> decode_int8
    | UInt16 e -> decode_uint16 e
    | Int16 e -> decode_int16 e
    | Int32 e -> decode_int32 e
    | Int64 e -> decode_int64 e
    | Bytes len -> decode_bytes len
    | _ -> assert false

  and record : type a. a record -> a decoder =
   fun { rfields= Fields (fs, constr); _ } ->
    let nil _bstr _pos fn = fn in
    let cons { ftype; _ } k =
      let decode = decode ftype in
      fun bstr pos constr ->
        let x = decode bstr pos in
        let constr = constr x in
        k bstr pos constr
    in
    let fn = Record_decoder.fold { nil; cons } fs in
    fun bstr pos -> fn bstr pos constr
end

(* combinators *)

let char = Primary Char
let uint8 = Primary UInt8
let int8 = Primary Int8
let beuint16 = Primary (UInt16 Big_endian)
let leuint16 = Primary (UInt16 Little_endian)
let neuint16 = Primary (UInt16 Native_endian)
let beint16 = Primary (Int16 Big_endian)
let leint16 = Primary (Int16 Little_endian)
let neint16 = Primary (Int16 Native_endian)
let beint32 = Primary (Int32 Big_endian)
let leint32 = Primary (Int32 Little_endian)
let neint32 = Primary (Int32 Native_endian)
let beint64 = Primary (Int64 Big_endian)
let leint64 = Primary (Int64 Little_endian)
let neint64 = Primary (Int64 Native_endian)
let varint31 = Primary Var_int31
let varint63 = Primary Var_int63
let bytes len = Primary (Bytes len)
let cstring = Primary CString
let until byte = Primary (Until byte)

(* record *)

type ('a, 'b, 'c) open_record = ('a, 'c) fields -> 'b * ('a, 'b) fields

let field ftype fget = { ftype; fget }
let record : 'b -> ('a, 'b, 'b) open_record = fun c fs -> (c, fs)

let app : type a b c d.
    (a, b, c -> d) open_record -> (a, c) field -> (a, b, d) open_record =
 fun r f fs -> r (F1 (f, fs))

let sealr : type a b. (a, b, a) open_record -> a t =
 fun r ->
  let c, fs = r F0 in
  let rwit = Witness.make () in
  let sealed = { rwit; rfields= Fields (fs, c) } in
  Record sealed

let ( |+ ) = app

(* variant *)

type 'a case_p = 'a case_v
type ('a, 'b) case = int -> 'a a_case * 'b

let case0 c0 ctag0 =
  let c = { ctag0; c0 } in
  (C0 c, CV0 c)

let case1 : type a b. b t -> (b -> a) -> (a, b -> a case_p) case =
 fun ctype1 c1 ctag1 ->
  let cwitn1 : b Witness.t = Witness.make () in
  let c = { ctag1; ctype1; cwitn1; c1 } in
  (C1 c, fun v -> CV1 (c, v))

type ('a, 'b, 'c) open_variant = 'a a_case list -> 'c * 'a a_case list

let variant c vs = (c, vs)

let app v c cs =
  let fc, cs = v cs in
  let c, f = c (List.length cs) in
  (fc f, c :: cs)

let sealv v =
  let vget, vcases = v [] in
  let vwit = Witness.make () in
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vwit; vcases; vget }

let ( |~ ) = app
