(*
 * Copyright (c) 2015 The astring programmers
 * SPDX-License-Identifier: ISC
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

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external length : t -> int = "%caml_ba_dim_1"

external ptr : t -> (nativeint[@unboxed])
  = "bstr_bytecode_ptr" "bstr_native_ptr"
[@@noalloc]

let overlap a b =
  let src_a = ptr a in
  let src_b = ptr b in
  let len_a = Nativeint.of_int (length a) in
  let len_b = Nativeint.of_int (length b) in
  let len =
    let ( + ) = Nativeint.add in
    let ( - ) = Nativeint.sub in
    Nativeint.max 0n (Nativeint.min (src_a + len_a) (src_b + len_b))
    - Nativeint.max src_a src_b
  in
  let len = Nativeint.to_int len in
  if src_a >= src_b && src_a < Nativeint.add src_b len_b then
    let offset = Nativeint.(to_int (sub src_a src_b)) in
    Some (len, 0, offset)
  else if src_b >= src_a && src_b < Nativeint.add src_a len_a then
    let offset = Nativeint.(to_int (sub src_b src_a)) in
    Some (len, offset, 0)
  else None

external ( < ) : 'a -> 'a -> bool = "%lessthan"

let ( < ) (x : int) y = x < y [@@inline]

external ( <= ) : 'a -> 'a -> bool = "%lessequal"

let ( <= ) (x : int) y = x <= y [@@inline]

external ( > ) : 'a -> 'a -> bool = "%greaterthan"

let ( > ) (x : int) y = x > y [@@inline]

external ( >= ) : 'a -> 'a -> bool = "%greaterequal"

let ( >= ) (x : int) y = x >= y [@@inline]

module Bytes = struct
  include Bytes

  external _unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"
  external _unsafe_set_uint8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
  external _unsafe_get_int32_ne : bytes -> int -> int32 = "%caml_bytes_get32u"

  external _unsafe_set_int32_ne : bytes -> int -> int32 -> unit
    = "%caml_bytes_set32u"
end

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external get_uint8 : t -> int -> int = "%caml_ba_ref_1"
external unsafe_get_uint8 : t -> int -> int = "%caml_ba_unsafe_ref_1"
external set_uint8 : t -> int -> int -> unit = "%caml_ba_set_1"
external get_uint16_ne : t -> int -> int = "%caml_bigstring_get16"
external set_int16_ne : t -> int -> int -> unit = "%caml_bigstring_set16"
external get_int32_ne : t -> int -> int32 = "%caml_bigstring_get32"
external set_int32_ne : t -> int -> int32 -> unit = "%caml_bigstring_set32"
external set_int64_ne : t -> int -> int64 -> unit = "%caml_bigstring_set64"
external unsafe_get_uint16_ne : t -> int -> int = "%caml_bigstring_get16u"

external unsafe_set_uint16_ne : t -> int -> int -> unit
  = "%caml_bigstring_set16u"

external unsafe_get_int64_ne : t -> (int[@untagged]) -> (int64[@unboxed])
  = "bstr_bytecode_get64u" "bstr_native_get64u"
[@@noalloc]

external _unsafe_set_int64_ne : t -> int -> int64 -> unit
  = "%caml_bigstring_set64u"

external unsafe_memcmp :
     t
  -> (int[@untagged])
  -> t
  -> (int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged]) = "bstr_bytecode_memcmp" "bstr_native_memcmp"
[@@noalloc]

external unsafe_memcpy :
  t -> (int[@untagged]) -> t -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "bstr_bytecode_memcpy" "bstr_native_memcpy"
[@@noalloc]

external unsafe_memcpy_mmaped :
  t -> (int[@untagged]) -> t -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "bstr_bytecode_memcpy" "bstr_native_memcpy_mmaped"

external unsafe_memmove :
  t -> (int[@untagged]) -> t -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "bstr_bytecode_memmove" "bstr_native_memmove"
[@@noalloc]

external unsafe_memmove_mmaped :
  t -> (int[@untagged]) -> t -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "bstr_bytecode_memmove" "bstr_native_memmove_mmaped"

external unsafe_memchr :
     t
  -> (int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged]) = "bstr_bytecode_memchr" "bstr_native_memchr"
[@@noalloc]

external unsafe_memset :
     t
  -> (int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged]) = "bstr_bytecode_memset" "bstr_native_memset"
[@@noalloc]

let memcmp src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bigarray.Array1.dim src - len
    || dst_off < 0
    || dst_off > Bigarray.Array1.dim dst - len
  then invalid_arg "Bstr.memcmp";
  unsafe_memcmp src src_off dst dst_off len

let memcpy src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bigarray.Array1.dim src - len
    || dst_off < 0
    || dst_off > Bigarray.Array1.dim dst - len
  then invalid_arg "Bstr.memcpy";
  unsafe_memcpy src src_off dst dst_off len

let memcpy_mmaped src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bigarray.Array1.dim src - len
    || dst_off < 0
    || dst_off > Bigarray.Array1.dim dst - len
  then invalid_arg "Bstr.memcpy";
  unsafe_memcpy_mmaped src src_off dst dst_off len

let memmove src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bigarray.Array1.dim src - len
    || dst_off < 0
    || dst_off > Bigarray.Array1.dim dst - len
  then invalid_arg "Bstr.memmove";
  unsafe_memmove src src_off dst dst_off len

let memmove_mmaped src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bigarray.Array1.dim src - len
    || dst_off < 0
    || dst_off > Bigarray.Array1.dim dst - len
  then invalid_arg "Bstr.memmove";
  unsafe_memmove_mmaped src src_off dst dst_off len

let memchr src ~off ~len value =
  if len < 0 || off < 0 || off > Bigarray.Array1.dim src - len then
    invalid_arg "Bstr.memchr";
  unsafe_memchr src off len (Char.code value)

let memset src ~off ~len value =
  if len < 0 || off < 0 || off > Bigarray.Array1.dim src - len then
    invalid_arg "Bstr.memset";
  ignore (unsafe_memset src off len (Char.code value))

let empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0
let create len = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len

external get : t -> int -> char = "%caml_ba_ref_1"
external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

let fill bstr ?(off = 0) ?len chr =
  let len = match len with Some len -> len | None -> length bstr - off in
  memset bstr ~off ~len chr

let make len chr =
  let bstr = create len in
  ignore (unsafe_memset bstr 0 len (Char.code chr));
  (* [Obj.magic] instead of [Char.code]? *)
  bstr

let init len fn =
  let bstr = create len in
  for i = 0 to len - 1 do
    unsafe_set bstr i (fn i)
  done;
  bstr

let copy src =
  let len = length src in
  let bstr = create len in
  unsafe_memcpy src 0 bstr 0 len;
  bstr

let chop ?(rev = false) bstr =
  if length bstr == 0 then None
  else if not rev then Some (unsafe_get bstr 0)
  else Some (unsafe_get bstr (length bstr - 1))

let get_int64_ne bstr idx =
  if idx < 0 || idx > length bstr - 8 then invalid_arg "Bstr.get_int64_ne";
  unsafe_get_int64_ne bstr idx

let get_int8 bstr i =
  (get_uint8 bstr i lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

let get_uint16_le bstr i =
  if Sys.big_endian then swap16 (get_uint16_ne bstr i) else get_uint16_ne bstr i

let get_uint16_be bstr i =
  if not Sys.big_endian then swap16 (get_uint16_ne bstr i)
  else get_uint16_ne bstr i

let[@coverage off] _unsafe_get_uint16_le bstr i =
  (* TODO(dinosaure): for unicode. *)
  if Sys.big_endian then swap16 (unsafe_get_uint16_ne bstr i)
  else unsafe_get_uint16_ne bstr i

let[@coverage off] _unsafe_get_uint16_be bstr i =
  (* TODO(dinosaure): for unicode. *)
  if not Sys.big_endian then swap16 (unsafe_get_uint16_ne bstr i)
  else unsafe_get_uint16_ne bstr i

let get_int16_ne bstr i =
  (get_uint16_ne bstr i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_le bstr i =
  (get_uint16_le bstr i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_be bstr i =
  (get_uint16_be bstr i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int32_le bstr i =
  if Sys.big_endian then swap32 (get_int32_ne bstr i) else get_int32_ne bstr i

let get_int32_be bstr i =
  if not Sys.big_endian then swap32 (get_int32_ne bstr i)
  else get_int32_ne bstr i

let get_int64_le bstr i =
  if Sys.big_endian then swap64 (get_int64_ne bstr i) else get_int64_ne bstr i

let get_int64_be bstr i =
  if not Sys.big_endian then swap64 (get_int64_ne bstr i)
  else get_int64_ne bstr i

let[@coverage off] _unsafe_set_uint16_le bstr i x =
  (* TODO(dinosaure): for unicode. *)
  if Sys.big_endian then unsafe_set_uint16_ne bstr i (swap16 x)
  else unsafe_set_uint16_ne bstr i x

let[@coverage off] _unsafe_set_uint16_be bstr i x =
  (* TODO(dinosaure): for unicode. *)
  if Sys.big_endian then unsafe_set_uint16_ne bstr i x
  else unsafe_set_uint16_ne bstr i (swap16 x)

let set_int16_le bstr i x =
  if Sys.big_endian then set_int16_ne bstr i (swap16 x)
  else set_int16_ne bstr i x

let set_int16_be bstr i x =
  if not Sys.big_endian then set_int16_ne bstr i (swap16 x)
  else set_int16_ne bstr i x

let set_int32_le bstr i x =
  if Sys.big_endian then set_int32_ne bstr i (swap32 x)
  else set_int32_ne bstr i x

let set_int32_be bstr i x =
  if not Sys.big_endian then set_int32_ne bstr i (swap32 x)
  else set_int32_ne bstr i x

let set_int64_le bstr i x =
  if Sys.big_endian then set_int64_ne bstr i (swap64 x)
  else set_int64_ne bstr i x

let set_int64_be bstr i x =
  if not Sys.big_endian then set_int64_ne bstr i (swap64 x)
  else set_int64_ne bstr i x

let set_int8 = set_uint8
let set_uint16_ne = set_int16_ne
let set_uint16_be = set_int16_be
let set_uint16_le = set_int16_le

external unsafe_sub : t -> (int[@untagged]) -> (int[@untagged]) -> t
  = "bstr_bytecode_unsafe_sub" "bstr_native_unsafe_sub"

let sub bstr ~off ~len =
  if off < 0 || len < 0 || off > length bstr - len then invalid_arg "Bstr.sub";
  unsafe_sub bstr off len

let[@inline always] unsafe_blit src ~src_off dst ~dst_off ~len =
  unsafe_memmove src src_off dst dst_off len

let blit src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > length src - len
    || dst_off < 0
    || dst_off > length dst - len
  then invalid_arg "Bstr.blit";
  unsafe_blit src ~src_off dst ~dst_off ~len

external unsafe_blit_to_bytes :
     t
  -> src_off:(int[@untagged])
  -> bytes
  -> dst_off:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "bstr_bytecode_unsafe_blit_to_bytes" "bstr_native_unsafe_blit_to_bytes"
[@@noalloc]

external unsafe_blit_from_bytes :
     bytes
  -> src_off:(int[@untagged])
  -> t
  -> dst_off:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "bstr_bytecode_unsafe_blit_from_bytes" "bstr_native_unsafe_blit_from_bytes"
[@@noalloc]

let blit_from_bytes src ~src_off bstr ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bytes.length src - len
    || dst_off < 0
    || dst_off > length bstr - len
  then invalid_arg "Bstr.blit_from_bytes";
  unsafe_blit_from_bytes src ~src_off bstr ~dst_off ~len

let blit_from_string src ~src_off bstr ~dst_off ~len =
  blit_from_bytes (Bytes.unsafe_of_string src) ~src_off bstr ~dst_off ~len

let blit_to_bytes bstr ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > length bstr - len
    || dst_off < 0
    || dst_off > Bytes.length dst - len
  then invalid_arg "Bstr.blit_to_bytes";
  unsafe_blit_to_bytes bstr ~src_off dst ~dst_off ~len

let of_string str =
  let len = String.length str in
  let bstr = create len in
  unsafe_blit_from_bytes
    (Bytes.unsafe_of_string str)
    ~src_off:0 bstr ~dst_off:0 ~len;
  bstr

let string ?(off = 0) ?len str =
  let len =
    match len with Some len -> len | None -> String.length str - off
  in
  if off < 0 || len < 0 || off > String.length str - len then
    invalid_arg "Bstr.string";
  let bstr = create len in
  unsafe_blit_from_bytes
    (Bytes.unsafe_of_string str)
    ~src_off:off bstr ~dst_off:0 ~len;
  bstr

let unsafe_sub_string bstr src_off len =
  let buf = Bytes.create len in
  unsafe_blit_to_bytes bstr ~src_off buf ~dst_off:0 ~len;
  Bytes.unsafe_to_string buf

let sub_string bstr ~off ~len =
  if len < 0 || off < 0 || off > length bstr - len then
    invalid_arg "Bstr.sub_string";
  unsafe_sub_string bstr off len

let to_string bstr =
  if length bstr <= 0 then "" else unsafe_sub_string bstr 0 (length bstr)

let is_empty bstr = length bstr == 0

let is_prefix ~affix bstr =
  let len_affix = String.length affix in
  let len_bstr = length bstr in
  if len_affix > len_bstr then false
  else
    let max_idx_affix = len_affix - 1 in
    let rec go idx =
      if idx > max_idx_affix then true
      else if String.unsafe_get affix idx != unsafe_get bstr idx then false
      else go (idx + 1)
    in
    go 0

let starts_with ~prefix bstr =
  let len_prefix = length prefix in
  let len_bstr = length bstr in
  if len_prefix > len_bstr then false
  else
    let max_idx_prefix = len_prefix - 1 in
    let rec go idx =
      if idx > max_idx_prefix then true
      else if unsafe_get prefix idx != unsafe_get bstr idx then false
      else go (idx + 1)
    in
    go 0

let is_infix ~affix bstr =
  let len_affix = String.length affix in
  let len_bstr = length bstr in
  if len_affix > len_bstr then false
  else
    let max_idx_affix = len_affix - 1 in
    let max_idx_bstr = len_bstr - len_affix in
    let rec go idx k =
      if idx > max_idx_bstr then false
      else if k > max_idx_affix then true
      else if k > 0 then
        if affix.[k] == bstr.{idx + k} then go idx (succ k) else go (succ idx) 0
      else if affix.[0] = bstr.{idx} then go idx 1
      else go (idx + 1) 0
    in
    go 0 0

let is_suffix ~affix bstr =
  let max_idx_affix = String.length affix - 1 in
  let max_idx_bstr = length bstr - 1 in
  if max_idx_affix > max_idx_bstr then false
  else
    let rec go idx =
      if idx > max_idx_affix then true
      else if affix.[max_idx_affix - idx] != bstr.{max_idx_bstr - idx} then
        false
      else go (idx + 1)
    in
    go 0

let ends_with ~suffix bstr =
  let max_idx_suffix = length suffix - 1 in
  let max_idx_bstr = length bstr - 1 in
  if max_idx_suffix > max_idx_bstr then false
  else
    let rec go idx =
      if idx > max_idx_suffix then true
      else if
        unsafe_get suffix (max_idx_suffix - idx)
        != unsafe_get bstr (max_idx_bstr - idx)
      then false
      else go (idx + 1)
    in
    go 0

exception Break

let for_all sat bstr =
  try
    for idx = 0 to length bstr - 1 do
      if sat (unsafe_get bstr idx) == false then raise_notrace Break
    done;
    true
  with Break -> false

let contains bstr ?(off = 0) ?len chr =
  let len = match len with Some len -> len | None -> length bstr - off in
  memchr bstr ~off ~len chr != -1

let index bstr ?(off = 0) ?len chr =
  let len = match len with Some len -> len | None -> length bstr - off in
  match memchr bstr ~off ~len chr with -1 -> None | value -> Some value

let compare a b =
  let len_a = length a and len_b = length b in
  if len_a < len_b then -1
  else if len_a > len_b then 1
  else unsafe_memcmp a 0 b 0 len_a

let equal a b = compare a b == 0

let constant_equal ~len a b =
  let len1 = len asr 1 in
  let r = ref 0 in
  for i = 0 to pred len1 do
    r :=
      !r lor (unsafe_get_uint16_ne a (i * 2) lxor unsafe_get_uint16_ne b (i * 2))
  done;
  for _ = 1 to len land 1 do
    r := !r lor (unsafe_get_uint8 a (len - 1) lxor unsafe_get_uint8 b (len - 1))
  done;
  !r == 0

let constant_equal a b =
  let al = length a in
  let bl = length b in
  if al != bl then false else constant_equal ~len:al a b

let with_range ?(first = 0) ?(len = max_int) bstr =
  if len < 0 then invalid_arg "Bstr.with_range";
  if len == 0 then empty
  else
    let bstr_len = length bstr in
    let max_idx = bstr_len - 1 in
    let last =
      match len with
      | len when len = max_int -> max_idx
      | len ->
          let last = first + len - 1 in
          if last > max_idx then max_idx else last
    in
    let first = if first < 0 then 0 else first in
    if first = 0 && last = max_idx then bstr
    else unsafe_sub bstr first (last + 1 - first)

let with_index_range ?(first = 0) ?last bstr =
  let bstr_len = length bstr in
  let max_idx = bstr_len - 1 in
  let last =
    match last with
    | None -> max_idx
    | Some last -> if last > max_idx then max_idx else last
  in
  let first = if first < 0 then 0 else first in
  if first > max_idx || last < 0 || first > last then empty
  else if first == 0 && last == max_idx then bstr
  else unsafe_sub bstr first (last + 1 - first)

let is_white chr = chr == ' ' || chr == '\t'

let trim ?(drop = is_white) bstr =
  let len = length bstr in
  if len == 0 then bstr
  else
    let max_idx = len - 1 in
    let rec left_pos idx =
      if idx > max_idx then len
      else if drop bstr.{idx} then left_pos (succ idx)
      else idx
    in
    let rec right_pos idx =
      if idx < 0 then 0
      else if drop bstr.{idx} then right_pos (pred idx)
      else succ idx
    in
    let left = left_pos 0 in
    if left = len then empty
    else
      let right = right_pos max_idx in
      if left == 0 && right == len then bstr
      else unsafe_sub bstr left (right - left)

let fspan ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
  if min < 0 then invalid_arg "Bstr.fspan";
  if max < 0 then invalid_arg "Bstr.fspan";
  if min > max || max == 0 then (empty, bstr)
  else
    let len = length bstr in
    let max_idx = len - 1 in
    let max_idx =
      let k = max - 1 in
      if k > max_idx then max_idx else k
    in
    let need_idx = min in
    let rec go idx =
      if idx <= max_idx && sat bstr.{idx} then go (succ idx)
      else if idx < need_idx || idx == 0 then (empty, bstr)
      else if idx == len then (bstr, empty)
      else
        let a = unsafe_sub bstr 0 idx in
        let b = unsafe_sub bstr idx (len - idx) in
        (a, b)
    in
    go 0

let rspan ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
  if min < 0 then invalid_arg "Bstr.rspan";
  if max < 0 then invalid_arg "Bstr.rspan";
  if min > max || max == 0 then (bstr, empty)
  else
    let len = length bstr in
    let max_idx = len - 1 in
    let min_idx =
      let k = len - max in
      if k < 0 then 0 else k
    in
    let need_idx = len - min - 1 in
    let rec go idx =
      if idx >= min_idx && sat (unsafe_get bstr idx) then go (idx - 1)
      else if idx > need_idx || idx == max_idx then (bstr, empty)
      else if idx < 0 then (empty, bstr)
      else
        let cut = idx + 1 in
        let a = unsafe_sub bstr 0 cut in
        let b = unsafe_sub bstr cut (len - cut) in
        (a, b)
    in
    go max_idx

let span ?(rev = false) ?min ?max ?sat bstr =
  match rev with
  | true -> rspan ?min ?max ?sat bstr
  | false -> fspan ?min ?max ?sat bstr

let take ?(rev = false) ?min ?max ?sat bstr =
  let a, b = span ~rev ?min ?max ?sat bstr in
  if rev then b else a

let drop ?(rev = false) ?min ?max ?sat bstr =
  let a, b = span ~rev ?min ?max ?sat bstr in
  if rev then a else b

let fcut ~sep bstr =
  let sep_len = String.length sep in
  let len = length bstr in
  if sep_len == 0 then invalid_arg "cut: empty separator";
  let max_sep_zidx = sep_len - 1 in
  let max_s_zidx = len - sep_len in
  let rec check_sep i k =
    if k > max_sep_zidx then
      let a = unsafe_sub bstr 0 i in
      let b = unsafe_sub bstr (i + sep_len) (len - i - sep_len) in
      Some (a, b)
    else if unsafe_get bstr (i + k) == String.unsafe_get sep k then
      check_sep i (k + 1)
    else scan (i + 1)
  and scan i =
    if i > max_s_zidx then None
    else if unsafe_get bstr i == String.unsafe_get sep 0 then check_sep i 1
    else scan (i + 1)
  in
  scan 0

let rcut ~sep bstr =
  let sep_len = String.length sep in
  let len = length bstr in
  if sep_len == 0 then invalid_arg "cut: empty separator";
  let max_sep_zidx = sep_len - 1 in
  let max_s_zidx = len - 1 in
  let rec check_sep i k =
    if k > max_sep_zidx then
      let a = sub ~off:0 ~len:i bstr in
      let b = sub ~off:(i + sep_len) ~len:(len - i - sep_len) bstr in
      Some (a, b)
    else if unsafe_get bstr (i + k) == String.unsafe_get sep k then
      check_sep i (k + 1)
    else rscan (i - 1)
  and rscan i =
    if i < 0 then None
    else if unsafe_get bstr i == String.unsafe_get sep 0 then check_sep i 1
    else rscan (i - 1)
  in
  rscan (max_s_zidx - max_sep_zidx)

let cut ?(rev = false) ~sep bstr =
  match rev with true -> rcut ~sep bstr | false -> fcut ~sep bstr

let add_sub ~no_empty bstr ~start ~stop acc =
  if start = stop then if no_empty then acc else empty :: acc
  else unsafe_sub bstr start (stop - start) :: acc

let fcuts ~no_empty ~sep bstr =
  let sep_len = String.length sep in
  if sep_len = 0 then invalid_arg "cuts: empty separator";
  let bstr_len = length bstr in
  let max_sep_idx = sep_len - 1 in
  let max_bstr_idx = bstr_len - sep_len in
  let rec check_sep start i k acc =
    if k > max_sep_idx then
      let new_start = i + sep_len in
      scan new_start new_start (add_sub ~no_empty bstr ~start ~stop:i acc)
    else if unsafe_get bstr (i + k) = sep.[k] then check_sep start i (k + 1) acc
    else scan start (i + 1) acc
  and scan start i acc =
    if i > max_bstr_idx then
      if start = 0 then if no_empty && bstr_len = 0 then [] else [ bstr ]
      else List.rev (add_sub ~no_empty bstr ~start ~stop:bstr_len acc)
    else if unsafe_get bstr i = sep.[0] then check_sep start i 1 acc
    else scan start (i + 1) acc
  in
  scan 0 0 []

let rcuts ~no_empty ~sep bstr =
  let sep_len = String.length sep in
  if sep_len = 0 then invalid_arg "cuts: empty separtor";
  let bstr_len = length bstr in
  let max_sep_idx = sep_len - 1 in
  let max_bstr_idx = bstr_len - 1 in
  let rec check_sep stop i k acc =
    if k > max_sep_idx then
      let start = i + sep_len in
      rscan i (i - sep_len) (add_sub ~no_empty bstr ~start ~stop acc)
    else if unsafe_get bstr (i + k) = sep.[k] then check_sep stop i (k + 1) acc
    else rscan stop (i - 1) acc
  and rscan stop i acc =
    if i < 0 then
      if stop = bstr_len then if no_empty && bstr_len = 0 then [] else [ bstr ]
      else add_sub ~no_empty bstr ~start:0 ~stop acc
    else if unsafe_get bstr i = sep.[0] then check_sep stop i 1 acc
    else rscan stop (i - 1) acc
  in
  rscan bstr_len (max_bstr_idx - max_sep_idx) []

let cuts ?(rev = false) ?(empty = true) ~sep bstr =
  match rev with
  | true -> rcuts ~no_empty:(not empty) ~sep bstr
  | false -> fcuts ~no_empty:(not empty) ~sep bstr

let shift bstr off =
  if off > length bstr || off < 0 then invalid_arg "Bstr.shift";
  let len = length bstr - off in
  unsafe_sub bstr off len

let split_on_char sep bstr =
  let lst = ref [] in
  let max = ref (length bstr) in
  for idx = length bstr - 1 downto 0 do
    if unsafe_get bstr idx == sep then begin
      lst := sub bstr ~off:(idx + 1) ~len:(!max - idx - 1) :: !lst;
      max := idx
    end
  done;
  sub bstr ~off:0 ~len:!max :: !lst

let concat sep = function
  | [] -> empty
  | x :: r as lst ->
      let sep_len = String.length sep in
      let fn acc bstr = acc + sep_len + length bstr in
      let res_len = List.fold_left fn (length x) r in
      let res = create res_len in
      let first = ref true in
      let dst_off = ref 0 in
      let fn bstr =
        let len = length bstr in
        if !first then begin
          blit bstr ~src_off:0 res ~dst_off:!dst_off ~len;
          first := false;
          dst_off := !dst_off + len
        end
        else begin
          blit_from_string sep ~src_off:0 res ~dst_off:!dst_off ~len:sep_len;
          dst_off := !dst_off + sep_len;
          blit bstr ~src_off:0 res ~dst_off:!dst_off ~len;
          dst_off := !dst_off + len
        end
      in
      List.iter fn lst; res

let ( ++ ) a b =
  let c = a + b in
  match (a < 0, b < 0, c < 0) with
  | true, true, false | false, false, true -> invalid_arg "Bstr.extend"
  | _ -> c

let extend bstr left right =
  let len = length bstr ++ left ++ right in
  let res = make len '\000' in
  let src_off, dst_off = if left < 0 then (-left, 0) else (0, left) in
  let copy = Int.min (length bstr - src_off) (len - dst_off) in
  if copy > 0 then unsafe_blit bstr ~src_off res ~dst_off ~len:copy;
  res

let iter fn t =
  for i = 0 to length t - 1 do
    fn (unsafe_get t i)
  done

let to_seq bstr =
  let rec go idx () =
    if idx == length bstr then Seq.Nil
    else
      let chr = unsafe_get bstr idx in
      Seq.Cons (chr, go (idx + 1))
  in
  go 0

let to_seqi bstr =
  let rec go idx () =
    if idx == length bstr then Seq.Nil
    else
      let chr = unsafe_get bstr idx in
      Seq.Cons ((idx, chr), go (idx + 1))
  in
  go 0

let of_seq seq =
  let n = ref 0 in
  let buf = ref (make 0x7ff '\000') in
  let resize () =
    let new_len = min (2 * length !buf) Sys.max_string_length in
    (* TODO(dinosaure): should we keep this limit? *)
    if length !buf == new_len then failwith "Bstr.of_seq: cannot grow bigstring";
    let new_buf = make new_len '\000' in
    Bigarray.Array1.blit !buf (sub new_buf ~off:0 ~len:(length !buf));
    buf := new_buf
  in
  let fn chr =
    if !n == length !buf then resize ();
    unsafe_set !buf !n chr;
    incr n
  in
  Seq.iter fn seq; sub !buf ~off:0 ~len:!n
