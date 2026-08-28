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

let invalid_argf fmt = Format.kasprintf invalid_arg fmt

include Bin_type
module Size = Bin_size
module Error = Bin_error
module Bytes = Bin_encoder_bytes
module String = Bin_decoder_string

module Bstr = struct
  include Bin_decoder_bstr
  include Bin_encoder_bstr
end

let encode_bstr t = Staged.stage (Bstr.encode t)
let decode_bstr t = Staged.stage (Bstr.decode t)
let decode t = Staged.stage (String.decode t)

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
let cstring = Primary (Bytes (Delim '\000'))
let until byte = Primary (Until byte)

let fixed len =
  if len < 0 then invalid_arg "Bin.fiex: negative length";
  Fixed len

let prefix t = Prefix t
let delim chr = Delim chr
let rest = Rest

let reject fn = function
  | Delim _ -> invalid_argf "%s: a delimited cannot bound s sequence" fn
  | slen -> slen

let seq slen selt = Seq { slen= reject "Bin.seq" slen; selt; skind= Sarray }
let list slen selt = Seq { slen= reject "Bin.list" slen; selt; skind= Slist }

(* record *)

type ('a, 'b, 'c) open_record = ('a, 'c) fields -> string * 'b * ('a, 'b) fields

let fid = Atomic.make 0

let field ?name:(fname = "") ftype fget =
  { fid= Atomic.fetch_and_add fid 1; fname; ftype; fget }

let record : ?name:string -> 'b -> ('a, 'b, 'b) open_record =
 fun ?(name = "") c fs -> (name, c, fs)

let app : type a b c d.
    (a, b, c -> d) open_record -> (a, c) field -> (a, b, d) open_record =
 fun r f fs -> r (F1 (f, fs))

let sealr : type a b. (a, b, a) open_record -> a t =
 fun r ->
  let rname, c, fs = r F0 in
  let rec renumber : type x y. int -> (x, y) fields -> (x, y) fields =
   fun i -> function
     | F0 -> F0
     | F1 (field, fs) ->
         let field =
           if field.fname = "" then { field with fname= "#" ^ string_of_int i }
           else field
         in
         F1 (field, renumber (succ i) fs)
  in
  Record { rname; rfields= Fields (renumber 0 fs, c) }

let ( |+ ) = app

(* variant *)

type 'a case_p = 'a case_v
type ('a, 'b) case = int -> 'a a_case * 'b

let case0 ?name ?tag c0 idx =
  let ctag0 = match tag with Some t -> t | None -> idx in
  let default = "#" ^ string_of_int idx in
  let cname0 = Option.value ~default name in
  let c = { ctag0; cidx0= idx; cname0; c0 } in
  (C0 c, CV0 c)

let case1 : type a b.
    ?name:string -> ?tag:int -> b t -> (b -> a) -> (a, b -> a case_p) case =
 fun ?name ?tag ctype1 c1 idx ->
  let ctag1 = match tag with Some t -> t | None -> idx in
  let default = "#" ^ string_of_int idx in
  let cname1 = Option.value ~default name in
  let cwitn1 : b Witness.t = Witness.make () in
  let c = { ctag1; cidx1= idx; cname1; ctype1; cwitn1; c1 } in
  (C1 c, fun v -> CV1 (c, v))

type ('a, 'b, 'c) open_variant = 'a a_case list -> string * 'c * 'a a_case list

let variant ?(name = "") c vs = (name, c, vs)

let app v c cs =
  let name, fc, cs = v cs in
  let c, f = c (List.length cs) in
  (name, fc f, c :: cs)

let name_of_case = function
  | C0 { cname0; _ } -> cname0
  | C1 { cname1; _ } -> cname1

let sealv ?tag:(vtag = varint) v =
  let vname, vget, vcases = v [] in
  let vcases = Array.of_list (List.rev vcases) in
  let seen = Hashtbl.create 16 in
  let fn c =
    let t = Case.tag c in
    if t < 0 then
      invalid_argf "Bin.sealv: case %s has a negative tag (%d)" (Case.name c) t;
    match Hashtbl.find_opt seen t with
    | None -> Hashtbl.add seen t (name_of_case c)
    | Some other ->
        invalid_argf "Bin.sealv: cases %s and %s sahre the tag %d" other
          (name_of_case c) t
  in
  Array.iter fn vcases;
  Variant { vname; vtag; vcases; vget }

let ( |~ ) = app

(* map *)

let map x f g = Map { x; f; g; mwit= Witness.make () }
let ( let+ ) (x, f, g) fn = fn (map x f g)
let bind x f g = Bind { bx= x; bf= f; bg= g }
let ( let* ) (x, f, g) fn = fn (bind x f g)
