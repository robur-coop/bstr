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

module Size = Bin_size
module Bytes = Bin_encoder_bytes
module String = Bin_decoder_string

module Bstr = struct
  include Bin_decoder_bstr
  include Bin_encoder_bstr
end

let decode_bstr = Bstr.decode
let decode = String.decode

let size_of_value t value =
  match (Size.make t).Size.of_value with
  | Size.Static len -> Some len
  | Size.Dynamic fn -> Some (fn value)
  | Size.Unknown -> None

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
