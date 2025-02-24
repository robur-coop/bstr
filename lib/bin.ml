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
  | Map : ('a, 'b) map -> 'b t

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

and ('a, 'b) map = { x: 'a t; f: 'a -> 'b; g: 'b -> 'a; mwit: 'b Witness.t }
and _ a_field = Field : ('a, 'b) field -> 'a a_field

let fields r =
  let rec go : type a b. (a, b) fields -> a a_field list = function
    | F0 -> []
    | F1 (x, r) -> Field x :: go r
  in
  match r.rfields with Fields (f, _) -> go f

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

(* sizer *)

module Size = struct
  type 'a encoding = 'a t
  type 'a t = Static of int | Dynamic of 'a | Unknown

  let map : type a b. (a -> b) -> a t -> b t =
   fun fn -> function
    | Unknown -> Unknown
    | Static n -> Static n
    | Dynamic a -> Dynamic (fn a)

  module Offset = struct
    type t = Offset of int [@@unboxed]

    let ( +> ) : t -> int -> t = fun (Offset n) m -> Offset (n + m)
  end

  module Sizer = struct
    type 'a size = 'a t

    type 'a t = {
        of_value: ('a -> int) size
      ; of_encoding: (Bstr.t -> Offset.t -> Offset.t) size
    }

    let ( <+> ) : type a. a t -> a t -> a t =
      let add_of_value (a : _ size) (b : _ size) : _ size =
        match (a, b) with
        | Unknown, _ | _, Unknown -> Unknown
        | Static a, Static b -> Static (a + b)
        | Static 0, other | other, Static 0 -> other
        | Static n, Dynamic f | Dynamic f, Static n ->
            Dynamic (fun a -> n + f a)
        | Dynamic f, Dynamic g -> Dynamic (fun a -> f a + g a)
      in
      let add_of_encoding (a : _ size) (b : _ size) : _ size =
        match (a, b) with
        | Unknown, _ | _, Unknown -> Unknown
        | Static a, Static b -> Static (a + b)
        | Static 0, other | other, Static 0 -> other
        | Dynamic f, Dynamic g -> Dynamic (fun bstr off -> g bstr (f bstr off))
        | Static n, Dynamic f ->
            Dynamic (fun bstr off -> f bstr Offset.(off +> n))
        | Dynamic f, Static n ->
            Dynamic (fun bstr off -> Offset.(f bstr off +> n))
      in
      fun a b ->
        {
          of_value= add_of_value a.of_value b.of_value
        ; of_encoding= add_of_encoding a.of_encoding b.of_encoding
        }

    let static n = { of_value= Static n; of_encoding= Static n }

    let dynamic ~of_value ~of_encoding =
      { of_value= Dynamic of_value; of_encoding= Dynamic of_encoding }

    let using fn t =
      let of_value = map (fun size_of x -> size_of (fn x)) t.of_value in
      { t with of_value }

    let _unknown = { of_value= Unknown; of_encoding= Unknown }
  end

  type 'a size_of = 'a Sizer.t

  let of_scanning : type a. (a -> Offset.t -> Offset.t) -> a -> int -> int =
   fun scan_fn bstr off ->
    let (Offset.Offset off') = scan_fn bstr (Offset.Offset off) in
    off' - off

  let of_encoding : 'a size_of -> (Bstr.t -> int -> int) t =
   fun { of_encoding; _ } -> map of_scanning of_encoding

  let of_value : type a. a size_of -> (a -> int) t =
   fun { of_value; _ } -> of_value

  let sizer_varint =
    let of_value =
      let rec go len n =
        if n >= 0 && n < 128 then len else go (len + 1) (n lsr 7)
      in
      fun n -> go 1 n
    in
    let of_encoding bstr (Offset.Offset off) =
      let pos = ref off in
      while
        let cmd = Bstr.get_uint8 bstr !pos in
        incr pos;
        cmd land 0x80 != 0
      do
        ()
      done;
      Offset.Offset !pos
    in
    Sizer.dynamic ~of_value ~of_encoding

  let sizer_cstring =
    let of_value str = String.length str + 1 in
    let of_encoding bstr (Offset.Offset off) =
      let pos = ref off in
      while Bstr.get_uint8 bstr !pos != 0 do
        incr pos
      done;
      Offset.Offset (!pos + 1)
    in
    Sizer.dynamic ~of_value ~of_encoding

  let sizer_until byte =
    let of_value str = String.length str in
    let of_encoding bstr (Offset.Offset off) =
      let pos = ref off in
      while Bstr.get bstr !pos != byte do
        incr pos
      done;
      Offset.Offset !pos
    in
    Sizer.dynamic ~of_value ~of_encoding

  let rec size_of : type a. a encoding -> a Sizer.t = function
    | Primary p -> prim p
    | Record r -> record r
    | Variant _ -> assert false
    | Map m -> map m

  and prim : type a. a primary -> a Sizer.t = function
    | Char -> Sizer.static 1
    | UInt8 -> Sizer.static 1
    | Int8 -> Sizer.static 1
    | UInt16 _ -> Sizer.static 2
    | Int16 _ -> Sizer.static 2
    | Int32 _ -> Sizer.static 4
    | Int64 _ -> Sizer.static 8
    | Bytes len -> Sizer.static len
    | Var_int31 -> sizer_varint
    | Var_int63 -> sizer_varint
    | CString -> sizer_cstring
    | Until p -> sizer_until p

  and record : type a. a record -> a Sizer.t =
   fun r ->
    fields r
    |> List.map (fun (Field f) -> Sizer.using f.fget (size_of f.ftype))
    |> List.fold_left Sizer.( <+> ) (Sizer.static 0)

  and map : type a b. (a, b) map -> b Sizer.t =
   fun { x; g; _ } -> Sizer.using g (size_of x)
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
    pos := !pos + 2;
    match e with
    | Big_endian -> Bstr.get_uint16_be bstr idx
    | Little_endian -> Bstr.get_uint16_le bstr idx
    | Native_endian -> Bstr.get_uint16_ne bstr idx
  [@@inline always]

  let decode_int16 endian bstr pos =
    let idx = !pos in
    pos := !pos + 2;
    match endian with
    | Big_endian -> Bstr.get_int16_be bstr idx
    | Little_endian -> Bstr.get_int16_le bstr idx
    | Native_endian -> Bstr.get_int16_ne bstr idx
  [@@inline always]

  let decode_int32 endian bstr pos =
    let idx = !pos in
    pos := !pos + 4;
    match endian with
    | Big_endian -> Bstr.get_int32_be bstr idx
    | Little_endian -> Bstr.get_int32_le bstr idx
    | Native_endian -> Bstr.get_int32_ne bstr idx
  [@@inline always]

  let decode_int64 endian bstr pos =
    let idx = !pos in
    pos := !pos + 8;
    match endian with
    | Big_endian -> Bstr.get_int64_be bstr idx
    | Little_endian -> Bstr.get_int64_le bstr idx
    | Native_endian -> Bstr.get_int64_ne bstr idx
  [@@inline always]

  let decode_bytes len bstr pos =
    let off = !pos in
    pos := !pos + len;
    Bstr.sub_string bstr ~off ~len
  [@@inline always]

  let decode_varint bstr pos =
    let bits = ref 0 in
    let res = ref 0 in
    while
      let cmd = Bstr.get_uint8 bstr !pos in
      incr pos;
      res := !res lor ((cmd land 0x7f) lsl !bits);
      bits := !bits + 7;
      cmd land 0x80 != 0
    do
      ()
    done;
    !res
  [@@inline always]

  let decode_cstring bstr pos =
    let off = !pos in
    while Bstr.get_uint8 bstr !pos != 0 do
      incr pos
    done;
    let len = !pos - off in
    let str = Bstr.sub_string bstr ~off ~len in
    incr pos; str
  [@@inline always]

  let decode_until byte bstr pos =
    let predicate byte' = byte != byte' in
    let off = !pos in
    while predicate (Bstr.get bstr !pos) = false do
      incr pos
    done;
    let len = !pos - off in
    Bstr.sub_string bstr ~off ~len
  [@@inline always]

  let rec decode : type a. a t -> a decoder = function
    | Primary p -> prim p
    | Record r -> record r
    | Variant v -> variant v
    | Map m -> map m

  and prim : type a. a primary -> a decoder = function
    | Char -> decode_char
    | UInt8 -> decode_uint8
    | Int8 -> decode_int8
    | UInt16 e -> decode_uint16 e
    | Int16 e -> decode_int16 e
    | Int32 e -> decode_int32 e
    | Int64 e -> decode_int64 e
    | Bytes len -> decode_bytes len
    | Var_int31 -> decode_varint
    | Var_int63 -> decode_varint
    | CString -> decode_cstring
    | Until p -> decode_until p

  and map : type a b. (a, b) map -> b decoder =
   fun { x; f; _ } -> fun buf pos -> f (decode x buf pos)

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

  and variant : type a. a variant -> a decoder =
   fun v ->
    let decoders : a decoder array =
      let fn = function
        | C0 c -> fun _ _ -> c.c0
        | C1 c ->
            let decode_arg = decode c.ctype1 in
            fun bstr pos -> c.c1 (decode_arg bstr pos)
      in
      Array.map fn v.vcases
    in
    fun bstr pos ->
      let i = decode_varint bstr pos in
      decoders.(i) bstr pos

  type 'a encoder = 'a -> Bstr.t -> int ref -> unit

  let encode_char chr bstr off =
    let pos = !off in
    incr off; Bstr.set bstr pos chr
  [@@inline always]

  let encode_uint8 byte bstr off =
    let pos = !off in
    incr off;
    Bstr.set_uint8 bstr pos byte
  [@@inline always]

  let encode_int8 byte bstr off =
    let pos = !off in
    incr off;
    Bstr.set_int8 bstr pos byte
  [@@inline always]

  let encode_uint16 endian value bstr off =
    let pos = !off in
    off := !off + 2;
    match endian with
    | Big_endian -> Bstr.set_uint16_be bstr pos value
    | Little_endian -> Bstr.set_uint16_le bstr pos value
    | Native_endian -> Bstr.set_uint16_ne bstr pos value
  [@@inline always]

  let encode_int16 endian value bstr off =
    let pos = !off in
    off := !off + 2;
    match endian with
    | Big_endian -> Bstr.set_int16_be bstr pos value
    | Little_endian -> Bstr.set_int16_le bstr pos value
    | Native_endian -> Bstr.set_int16_ne bstr pos value
  [@@inline always]

  let encode_int32 endian value bstr off =
    let pos = !off in
    off := !off + 4;
    match endian with
    | Big_endian -> Bstr.set_int32_be bstr pos value
    | Little_endian -> Bstr.set_int32_be bstr pos value
    | Native_endian -> Bstr.set_int32_be bstr pos value

  let encode_int64 endian value bstr off =
    let pos = !off in
    off := !off + 8;
    match endian with
    | Big_endian -> Bstr.set_int64_be bstr pos value
    | Little_endian -> Bstr.set_int64_be bstr pos value
    | Native_endian -> Bstr.set_int64_be bstr pos value

  let encode_bytes len src bstr off =
    let pos = !off in
    off := !off + len;
    Bstr.blit_from_string src ~src_off:0 bstr ~dst_off:pos ~len

  let encode_varint value bstr off =
    let num = ref (value lsr 7) in
    let cmd = ref (value land 0x7f) in
    cmd := if !num != 0 then !cmd lor 0x80 else !cmd;
    Bstr.set_uint8 bstr !off !cmd;
    incr off;
    while !num != 0 do
      cmd := !num land 0x7f;
      num := !num lsr 7;
      cmd := if !num != 0 then !cmd lor 0x80 else !cmd;
      Bstr.set_uint8 bstr !off !cmd;
      incr off
    done

  let encode_cstring src bstr off =
    let pos = !off in
    let len = String.length src in
    off := !off + len;
    Bstr.blit_from_string src ~src_off:0 bstr ~dst_off:pos ~len;
    Bstr.set_uint8 bstr !off 0;
    incr off

  let encode_until src bstr off =
    let pos = !off in
    let len = String.length src in
    off := !off + len;
    Bstr.blit_from_string src ~src_off:0 bstr ~dst_off:pos ~len

  let rec encode : type a. a t -> a encoder = function
    | Primary p -> prim p
    | _ -> assert false

  and prim : type a. a primary -> a encoder = function
    | Char -> encode_char
    | UInt8 -> encode_uint8
    | Int8 -> encode_int8
    | UInt16 e -> encode_uint16 e
    | Int16 e -> encode_int16 e
    | Int32 e -> encode_int32 e
    | Int64 e -> encode_int64 e
    | Bytes len -> encode_bytes len
    | Var_int31 -> encode_varint
    | Var_int63 -> encode_varint
    | CString -> encode_cstring
    | Until _ -> encode_until
end

let decode_bstr = Bstr.decode
let encode_bstr = Bstr.encode

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

(* map *)

let map x f g = Map { x; f; g; mwit= Witness.make () }

let size_of_value t value =
  let sizer = Size.size_of t in
  match Size.of_value sizer with
  | Size.Static len -> Some len
  | Size.Dynamic fn -> Some (fn value)
  | Size.Unknown -> None

let size_of_bstr ?(off = 0) t bstr =
  let sizer = Size.size_of t in
  match Size.of_encoding sizer with
  | Size.Static len -> Some len
  | Size.Dynamic fn -> Some (fn bstr off)
  | Size.Unknown -> None
