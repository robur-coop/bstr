(*
 * Copyright (c) 2024 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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

  let eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None

  let cast_exn : type a b. a t -> b t -> a -> b =
   fun awit bwit a ->
    match eq awit bwit with Some Refl -> a | None -> assert false
end

type endianness = Big_endian | Little_endian | Native_endian

type _ t =
  | Primary : 'a primary -> 'a t
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t
  | Map : ('a, 'b) map -> 'b t
  | Seq : 'a len_v -> 'a array t

and _ primary =
  | Char : char primary
  | UInt8 : int primary
  | Int8 : int primary
  | UInt16 : endianness -> int primary
  | Int16 : endianness -> int primary
  | Int32 : endianness -> int32 primary
  | Int64 : endianness -> int64 primary
  | Var_int : int primary
  | Bytes : int -> string primary
  | CString : string primary
  | Until : char -> string primary
  | Bstr : int -> Bstr.t primary
  | Const : 'a -> 'a primary

and 'a len_v = { llen: int; lval: 'a t }
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

let bstr_decode_varint bstr pos =
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

let string_decode_varint str pos =
  let bits = ref 0 in
  let res = ref 0 in
  while
    let cmd = String.get_uint8 str !pos in
    incr pos;
    res := !res lor ((cmd land 0x7f) lsl !bits);
    bits := !bits + 7;
    cmd land 0x80 != 0
  do
    ()
  done;
  !res
[@@inline always]

module Size = struct
  type 'a encoding = 'a t
  type 'a t = Static of int | Dynamic of 'a | Unknown

  let map : type a b. (a -> b) -> a t -> b t =
   fun fn -> function
    | Unknown -> Unknown
    | Static n -> Static n
    | Dynamic a -> Dynamic (fn a)

  let ( let+ ) x f = map f x

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

    let unknown = { of_value= Unknown; of_encoding= Unknown }
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
    | Variant v -> variant v
    | Map m -> map m
    | Seq { llen; lval } -> seq ~llen lval

  and seq : type a. llen:int -> a encoding -> a array Sizer.t =
   fun ~llen lval ->
    match size_of lval with
    | { Sizer.of_value= Static len; _ } -> Sizer.static (llen * len)
    | lsize ->
        let of_value =
          let+ len = lsize.Sizer.of_value in
          Array.fold_left (fun acc x -> acc + len x) 0
        in
        let of_encoding =
          let+ len = lsize.Sizer.of_encoding in
          let rec go buf off = function
            | 0 -> off
            | n -> go buf (len buf off) (n - 1)
          in
          fun buf off -> go buf off llen
        in
        { Sizer.of_value; of_encoding }

  and prim : type a. a primary -> a Sizer.t = function
    | Char -> Sizer.static 1
    | UInt8 -> Sizer.static 1
    | Int8 -> Sizer.static 1
    | UInt16 _ -> Sizer.static 2
    | Int16 _ -> Sizer.static 2
    | Int32 _ -> Sizer.static 4
    | Int64 _ -> Sizer.static 8
    | Bytes len -> Sizer.static len
    | Bstr len -> Sizer.static len
    | Var_int -> sizer_varint
    | CString -> sizer_cstring
    | Until p -> sizer_until p
    | Const _ -> Sizer.static 0

  and record : type a. a record -> a Sizer.t =
   fun r ->
    fields r
    |> List.map (fun (Field f) -> Sizer.using f.fget (size_of f.ftype))
    |> List.fold_left Sizer.( <+> ) (Sizer.static 0)

  and map : type a b. (a, b) map -> b Sizer.t =
   fun { x; g; _ } -> Sizer.using g (size_of x)

  and variant : type a. a variant -> a Sizer.t =
   fun v ->
    let static_varint_size n =
      let[@warning "-8"] (Dynamic fn) = sizer_varint.Sizer.of_value in
      fn n
    in
    let case_lengths : (int * a Sizer.t) array =
      let fn = function
        | C0 { ctag0; _ } -> (static_varint_size ctag0, Sizer.static 0)
        | C1 { ctag1; ctype1; cwitn1= expected; _ } ->
            let tag_length = static_varint_size ctag1 in
            let arg_length =
              match size_of ctype1 with
              | ({ of_value= Static _; _ } | { of_value= Unknown; _ }) as t -> t
              | { of_value= Dynamic of_value; of_encoding } ->
                  let of_value a =
                    match v.vget a with
                    | CV0 _ -> assert false
                    | CV1 ({ cwitn1= received; _ }, args) ->
                        let v = Witness.cast_exn received expected args in
                        of_value v
                  in
                  { of_value= Dynamic of_value; of_encoding }
            in
            (tag_length, arg_length)
      in
      Array.map fn v.vcases
    in
    let non_dynamic_length =
      let rec go static_so_far = function
        | -1 -> Option.map Sizer.static static_so_far
        | i -> begin
            match case_lengths.(i) with
            | _, { of_value= Unknown; _ } -> Some Sizer.unknown
            | _, { of_value= Dynamic _; _ } -> None
            | tag_len, { of_value= Static arg_len; _ } ->
                let len = tag_len + arg_len in
                begin match static_so_far with
                | None -> go (Some len) (i - 1)
                | Some len' when len = len' -> go static_so_far (i - 1)
                | Some _ -> None
                end
          end
      in
      go None (Array.length case_lengths - 1)
    in
    match non_dynamic_length with
    | Some x -> x
    | None ->
        let of_value a =
          let tag =
            match v.vget a with
            | CV0 { ctag0; _ } -> ctag0
            | CV1 ({ ctag1; _ }, _) -> ctag1
          in
          let tag_length, arg_length = case_lengths.(tag) in
          let arg_length =
            match arg_length.of_value with
            | Dynamic fn -> fn a
            | Static n -> n
            | Unknown -> assert false
          in
          tag_length + arg_length
        in
        let of_encoding buf (Offset.Offset off) =
          let off = ref off in
          let tag = bstr_decode_varint buf off in
          match case_lengths.(tag) with
          | _, { of_encoding= Static n; _ } -> Offset.Offset (!off + n)
          | _, { of_encoding= Dynamic fn; _ } -> fn buf (Offset.Offset !off)
          | _, { of_encoding= Unknown; _ } -> assert false
        in
        Sizer.dynamic ~of_value ~of_encoding
end

module Dispatch = struct
  type 'a t =
    | Base : 'a -> 'a t
    | Arrow : { arg_wit: 'b Witness.t; fn: 'b -> 'a } -> 'a t
end

module Case_folder = struct
  type ('a, 'r) t = { c0: 'a case0 -> 'r; c1: 'b. ('a, 'b) case1 -> 'b -> 'r }
end

let fold_variant : type a r. (a, r) Case_folder.t -> a variant -> a -> r =
 fun folder v_typ ->
  let cases =
    let fn = function
      | C0 c0 -> Dispatch.Base (folder.c0 c0)
      | C1 c1 -> Dispatch.Arrow { arg_wit= c1.cwitn1; fn= folder.c1 c1 }
    in
    Array.map fn v_typ.vcases
  in
  fun v ->
    match v_typ.vget v with
    | CV0 { ctag0; _ } -> begin
        match cases.(ctag0) with Dispatch.Base x -> x | _ -> assert false
      end
    | CV1 ({ ctag1; cwitn1; _ }, v) -> begin
        match cases.(ctag1) with
        | Dispatch.Arrow { fn; arg_wit } ->
            let v = Witness.cast_exn cwitn1 arg_wit v in
            fn v
        | _ -> assert false
      end

module Bytes = struct
  type 'a encoder = 'a -> bytes -> int ref -> unit

  let encode_char chr buf off =
    let pos = !off in
    incr off; Bytes.set buf pos chr
  [@@inline always]

  let encode_uint8 byte buf off =
    let pos = !off in
    incr off;
    Bytes.set_uint8 buf pos byte
  [@@inline always]

  let encode_int8 byte buf off =
    let pos = !off in
    incr off;
    Bytes.set_int8 buf pos byte
  [@@inline always]

  let encode_uint16 endian value buf off =
    let pos = !off in
    off := !off + 2;
    match endian with
    | Big_endian -> Bytes.set_uint16_be buf pos value
    | Little_endian -> Bytes.set_uint16_le buf pos value
    | Native_endian -> Bytes.set_uint16_ne buf pos value
  [@@inline always]

  let encode_int16 endian value buf off =
    let pos = !off in
    off := !off + 2;
    match endian with
    | Big_endian -> Bytes.set_int16_be buf pos value
    | Little_endian -> Bytes.set_int16_le buf pos value
    | Native_endian -> Bytes.set_int16_ne buf pos value
  [@@inline always]

  let encode_int32 endian value buf off =
    let pos = !off in
    off := !off + 4;
    match endian with
    | Big_endian -> Bytes.set_int32_be buf pos value
    | Little_endian -> Bytes.set_int32_le buf pos value
    | Native_endian -> Bytes.set_int32_ne buf pos value

  let encode_int64 endian value buf off =
    let pos = !off in
    off := !off + 8;
    match endian with
    | Big_endian -> Bytes.set_int64_be buf pos value
    | Little_endian -> Bytes.set_int64_le buf pos value
    | Native_endian -> Bytes.set_int64_ne buf pos value

  let encode_bytes len src buf off =
    let pos = !off in
    off := !off + len;
    Bytes.blit_string src 0 buf pos len

  let encode_bstr len src buf off =
    let dst_off = !off in
    off := !off + len;
    Bstr.blit_to_bytes src ~src_off:0 buf ~dst_off ~len

  let encode_varint value buf off =
    let num = ref (value lsr 7) in
    let cmd = ref (value land 0x7f) in
    cmd := if !num != 0 then !cmd lor 0x80 else !cmd;
    Bytes.set_uint8 buf !off !cmd;
    incr off;
    while !num != 0 do
      cmd := !num land 0x7f;
      num := !num lsr 7;
      cmd := if !num != 0 then !cmd lor 0x80 else !cmd;
      Bytes.set_uint8 buf !off !cmd;
      incr off
    done

  let encode_cstring src buf off =
    let pos = !off in
    let len = String.length src in
    off := !off + len;
    Bytes.blit_string src 0 buf pos len;
    Bytes.set_uint8 buf !off 0;
    incr off

  let encode_until src buf off =
    let pos = !off in
    let len = String.length src in
    off := !off + len;
    Bytes.blit_string src 0 buf pos len

  let rec encode : type a. a t -> a encoder = function
    | Primary p -> prim p
    | Map m -> map m
    | Record r -> record r
    | Variant v -> variant v
    | Seq { llen; lval } -> seq ~len:llen lval

  and seq : type a. len:int -> a t -> a array encoder =
   fun ~len t arr buf off ->
    if Array.length arr != len then
      invalid_arg "Impossible to encode such sequence: lengths mismatch";
    for i = 0 to len - 1 do
      encode t (Array.unsafe_get arr i) buf off
    done

  and prim : type a. a primary -> a encoder = function
    | Char -> encode_char
    | UInt8 -> encode_uint8
    | Int8 -> encode_int8
    | UInt16 e -> encode_uint16 e
    | Int16 e -> encode_int16 e
    | Int32 e -> encode_int32 e
    | Int64 e -> encode_int64 e
    | Bytes len -> encode_bytes len
    | Var_int -> encode_varint
    | CString -> encode_cstring
    | Until _ -> encode_until
    | Bstr len -> encode_bstr len
    | Const _ -> fun _v _bstr _off -> ()

  and record : type a. a record -> a encoder =
   fun r ->
    let fields_encoders : (a -> bytes -> int ref -> unit) list =
      let fn (Field f) = fun v buf off -> (encode f.ftype) (f.fget v) buf off in
      List.map fn (fields r)
    in
    fun v buf off -> List.iter (fun fn -> fn v buf off) fields_encoders

  and variant : type a. a variant -> a encoder =
    let c0 { ctag0; _ } = encode_varint ctag0 in
    let c1 c =
      let arg = encode c.ctype1 in
      fun v buf off ->
        encode_varint c.ctag1 buf off;
        arg v buf off
    in
    fun v -> fold_variant { c0; c1 } v

  and map : type a b. (a, b) map -> b encoder =
   fun { x; g; _ } -> fun u buf off -> encode x (g u) buf off
end

(* decoder for [string] *)

module String = struct
  module Record_decoder = Fields_folder (struct
    type ('a, 'b) t = string -> int ref -> 'b -> 'a
  end)

  type 'a decoder = string -> int ref -> 'a

  let decode_char str pos =
    let idx = !pos in
    incr pos; String.get str idx
  [@@inline always]

  let decode_uint8 str pos =
    let idx = !pos in
    incr pos; String.get_uint8 str idx
  [@@inline always]

  let decode_int8 str pos =
    let idx = !pos in
    incr pos; String.get_int8 str idx
  [@@inline always]

  let decode_uint16 e str pos =
    let idx = !pos in
    pos := !pos + 2;
    match e with
    | Big_endian -> String.get_uint16_be str idx
    | Little_endian -> String.get_uint16_le str idx
    | Native_endian -> String.get_uint16_ne str idx
  [@@inline always]

  let decode_int16 endian str pos =
    let idx = !pos in
    pos := !pos + 2;
    match endian with
    | Big_endian -> String.get_int16_be str idx
    | Little_endian -> String.get_int16_le str idx
    | Native_endian -> String.get_int16_ne str idx
  [@@inline always]

  let decode_int32 endian str pos =
    let idx = !pos in
    pos := !pos + 4;
    match endian with
    | Big_endian -> String.get_int32_be str idx
    | Little_endian -> String.get_int32_le str idx
    | Native_endian -> String.get_int32_ne str idx
  [@@inline always]

  let decode_int64 endian str pos =
    let idx = !pos in
    pos := !pos + 8;
    match endian with
    | Big_endian -> String.get_int64_be str idx
    | Little_endian -> String.get_int64_le str idx
    | Native_endian -> String.get_int64_ne str idx
  [@@inline always]

  let decode_bytes len str pos =
    let off = !pos in
    pos := !pos + len;
    String.sub str off len
  [@@inline always]

  let decode_bstr len str pos =
    if len == 0 then Bstr.empty
    else begin
      let src_off = !pos in
      pos := !pos + len;
      let bstr = Bstr.create len in
      Bstr.blit_from_string str ~src_off bstr ~dst_off:0 ~len;
      bstr
    end
  [@@inline always]

  let decode_cstring str pos =
    let off = !pos in
    while String.get_uint8 str !pos != 0 do
      incr pos
    done;
    let len = !pos - off in
    let str = String.sub str off len in
    incr pos; str
  [@@inline always]

  let decode_until byte str pos =
    let predicate byte' = byte != byte' in
    let off = !pos in
    while predicate (String.get str !pos) == false do
      incr pos
    done;
    let len = !pos - off in
    String.sub str off len
  [@@inline always]

  let rec decode : type a. a t -> a decoder = function
    | Primary p -> prim p
    | Record r -> record r
    | Variant v -> variant v
    | Map m -> map m
    | Seq { llen; lval } -> seq ~len:llen lval

  and seq : type a. len:int -> a t -> a array decoder =
   fun ~len t bstr pos ->
    let fn _idx = decode t bstr pos in
    Array.init len fn

  and prim : type a. a primary -> a decoder = function
    | Char -> decode_char
    | UInt8 -> decode_uint8
    | Int8 -> decode_int8
    | UInt16 e -> decode_uint16 e
    | Int16 e -> decode_int16 e
    | Int32 e -> decode_int32 e
    | Int64 e -> decode_int64 e
    | Bytes len -> decode_bytes len
    | Var_int -> string_decode_varint
    | CString -> decode_cstring
    | Until p -> decode_until p
    | Bstr len -> decode_bstr len
    | Const v -> fun _bstr _off -> v

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
    fun str pos ->
      let i = string_decode_varint str pos in
      decoders.(i) str pos
end

(* decoder & encoder for [bstr] *)

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

  let decode_bstr len bstr pos =
    if len == 0 then Bstr.empty
    else begin
      let off = !pos in
      pos := !pos + len;
      Bstr.sub bstr ~off ~len
    end
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
    while predicate (Bstr.get bstr !pos) == false do
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
    | Seq { llen; lval } -> seq ~len:llen lval

  and seq : type a. len:int -> a t -> a array decoder =
   fun ~len t bstr pos ->
    let fn _idx = decode t bstr pos in
    Array.init len fn

  and prim : type a. a primary -> a decoder = function
    | Char -> decode_char
    | UInt8 -> decode_uint8
    | Int8 -> decode_int8
    | UInt16 e -> decode_uint16 e
    | Int16 e -> decode_int16 e
    | Int32 e -> decode_int32 e
    | Int64 e -> decode_int64 e
    | Bytes len -> decode_bytes len
    | Var_int -> bstr_decode_varint
    | CString -> decode_cstring
    | Until p -> decode_until p
    | Bstr len -> decode_bstr len
    | Const v -> fun _bstr _off -> v

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
      let i = bstr_decode_varint bstr pos in
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
    | Little_endian -> Bstr.set_int32_le bstr pos value
    | Native_endian -> Bstr.set_int32_ne bstr pos value

  let encode_int64 endian value bstr off =
    let pos = !off in
    off := !off + 8;
    match endian with
    | Big_endian -> Bstr.set_int64_be bstr pos value
    | Little_endian -> Bstr.set_int64_le bstr pos value
    | Native_endian -> Bstr.set_int64_ne bstr pos value

  let encode_bytes len src bstr off =
    let dst_off = !off in
    off := !off + len;
    Bstr.blit_from_string src ~src_off:0 bstr ~dst_off ~len

  let encode_bstr len src bstr off =
    let dst_off = !off in
    off := !off + len;
    Bstr.blit src ~src_off:0 bstr ~dst_off ~len

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
    let len = Stdlib.String.length src in
    off := !off + len;
    Bstr.blit_from_string src ~src_off:0 bstr ~dst_off:pos ~len;
    Bstr.set_uint8 bstr !off 0;
    incr off

  let encode_until src bstr off =
    let pos = !off in
    let len = Stdlib.String.length src in
    off := !off + len;
    Bstr.blit_from_string src ~src_off:0 bstr ~dst_off:pos ~len

  let rec encode : type a. a t -> a encoder = function
    | Primary p -> prim p
    | Map m -> map m
    | Record r -> record r
    | Variant v -> variant v
    | Seq { llen; lval } -> seq ~len:llen lval

  and seq : type a. len:int -> a t -> a array encoder =
   fun ~len t arr buf off ->
    if Array.length arr != len then
      invalid_arg "Impossible to encode such sequence: lengths mismatch";
    for i = 0 to len - 1 do
      encode t (Array.unsafe_get arr i) buf off
    done

  and prim : type a. a primary -> a encoder = function
    | Char -> encode_char
    | UInt8 -> encode_uint8
    | Int8 -> encode_int8
    | UInt16 e -> encode_uint16 e
    | Int16 e -> encode_int16 e
    | Int32 e -> encode_int32 e
    | Int64 e -> encode_int64 e
    | Bytes len -> encode_bytes len
    | Var_int -> encode_varint
    | CString -> encode_cstring
    | Until _ -> encode_until
    | Bstr len -> encode_bstr len
    | Const _ -> fun _v _bstr _off -> ()

  and record : type a. a record -> a encoder =
   fun r ->
    let fields_encoders : (a -> Bstr.t -> int ref -> unit) list =
      let fn (Field f) = fun v buf off -> (encode f.ftype) (f.fget v) buf off in
      List.map fn (fields r)
    in
    fun v buf off -> List.iter (fun fn -> fn v buf off) fields_encoders

  and variant : type a. a variant -> a encoder =
    let c0 { ctag0; _ } = encode_varint ctag0 in
    let c1 c =
      let arg = encode c.ctype1 in
      fun v buf off ->
        encode_varint c.ctag1 buf off;
        arg v buf off
    in
    fun v -> fold_variant { c0; c1 } v

  and map : type a b. (a, b) map -> b encoder =
   fun { x; g; _ } -> fun u buf off -> encode x (g u) buf off
end

let decode_bstr = Bstr.decode
let encode_bstr = Bstr.encode
let decode = String.decode

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

let to_string t value =
  match size_of_value t value with
  | Some len ->
      let buf = Stdlib.Bytes.create len in
      Bytes.encode t value buf (ref 0);
      Stdlib.Bytes.unsafe_to_string buf
  | None -> assert false (* TODO(dinosaure): with [Buffer.t]. *)

(* combinators *)

let const v = Primary (Const v)
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
let varint = Primary Var_int
let bytes len = Primary (Bytes len)
let bstr len = Primary (Bstr len)
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

let seq ~len:llen lval =
  if llen <= 0 then invalid_arg "Bin.seq";
  Seq { llen; lval }
