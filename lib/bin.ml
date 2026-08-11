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

[@@@warning "-unused-field"]

include Bin_type

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

module Size = Bin_size

module Bytes = struct
  type 'a encoder = 'a -> bytes -> pos -> unit

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
    | Seq s -> seq s

  and seq : type a b. (a, b) seq -> b encoder = fun _ -> assert false

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
    let fields_encoders : (a -> bytes -> pos -> unit) list =
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
    type ('a, 'b) t = string -> pos -> 'b -> 'a
  end)

  type 'a decoder = string -> pos -> 'a

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
    | Seq _ -> assert false

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
    type ('a, 'b) t = Bstr.t -> pos -> 'b -> 'a
  end)

  type 'a decoder = Bstr.t -> pos -> 'a

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
    | Seq _ -> assert false

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

  type 'a encoder = 'a -> Bstr.t -> pos -> unit

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
    | Seq _ -> assert false

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
    let fields_encoders : (a -> Bstr.t -> pos -> unit) list =
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
  match Size.size_of t with
  | Size.Static len -> Some len
  | Size.Dynamic fn -> Some (fn value)
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

let sealv ?tag:(vtag = varint) v =
  let vget, vcases = v [] in
  let vwit = Witness.make () in
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vtag; vwit; vcases; vget }

let ( |~ ) = app

(* map *)

let map x f g = Map { x; f; g; mwit= Witness.make () }
let seq ~len:_ _lval = assert false
