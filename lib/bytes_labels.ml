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

include Bytes

let[@inline always] blit src ~src_off dst ~dst_off ~len =
  Bytes.blit src src_off dst dst_off len

let[@inline always] blit_to_bytes src ~src_off dst ~dst_off ~len =
  Bytes.blit src src_off dst dst_off len

let[@inline always] blit_from_bytes src ~src_off dst ~dst_off ~len =
  Bytes.blit src src_off dst dst_off len

let[@inline always] blit_from_string src ~src_off dst ~dst_off ~len =
  Bytes.blit_string src src_off dst dst_off len

let[@inline always] unsafe_blit src ~src_off dst ~dst_off ~len =
  Bytes.unsafe_blit src src_off dst dst_off len

let[@inline always] unsafe_blit_from_bytes src ~src_off dst ~dst_off ~len =
  Bytes.unsafe_blit src src_off dst dst_off len

let[@inline always] unsafe_blit_to_bytes src ~src_off dst ~dst_off ~len =
  Bytes.unsafe_blit src src_off dst dst_off len

let[@inline always] unsafe_blit_from_string src ~src_off dst ~dst_off ~len =
  Bytes.unsafe_blit_string src src_off dst dst_off len

let[@inline always] unsafe_fill t ~off ~len chr =
  Bytes.unsafe_fill t off len chr

let fill t ?(off = 0) ?len chr =
  let len = match len with Some len -> len | None -> Bytes.length t - off in
  Bytes.fill t off len chr

(* NOTE(dinosaure): according to our test, [string] must do a copy. We should
   not use [Bytes.unsafe_of_string] (even if we would like to have a sub-part
   of the given string) because it can returns an alias of it. *)
let string ?(off = 0) ?len str =
  let len =
    match len with Some len -> len | None -> String.length str - off
  in
  if len < 0 || off < 0 || off > String.length str - len then
    invalid_arg "Bytes.string";
  let buf = Bytes.create len in
  Bytes.blit_string str off buf 0 len;
  buf

let overlap a b = if a == b then Some (Bytes.length a, 0, 0) else None
let sub t ~off ~len = Bytes.sub t off len

let memchr t ~off ~len chr =
  if len < 0 || off < 0 || off > Bytes.length t - len then
    invalid_arg "Bytes.memchr";
  let max_idx = off + len - 1 in
  let rec go idx =
    if idx > max_idx then -1
    else if Bytes.unsafe_get t idx == chr then idx
    else go (idx + 1)
  in
  go off

external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_get_uint16_ne : bytes -> int -> int = "%caml_bytes_get16u"

external unsafe_set_uint16_ne : bytes -> int -> int -> unit
  = "%caml_bytes_set16u"

external unsafe_get_int32_ne : bytes -> int -> int32 = "%caml_bytes_get32u"

external unsafe_set_int32_ne : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32u"

external unsafe_get_int64_ne : bytes -> int -> int64 = "%caml_bytes_get64u"

external unsafe_set_int64_ne : bytes -> int -> int64 -> unit
  = "%caml_bytes_set64u"

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let unsafe_get_uint8 buf i = Char.code (unsafe_get buf i) [@@inline]
let unsafe_set_uint8 buf i v = unsafe_set buf i (Char.unsafe_chr v) [@@inline]

let unsafe_get_int8 buf i =
  (unsafe_get_uint8 buf i lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)
[@@inline]

let unsafe_set_int8 = unsafe_set_uint8

let unsafe_get_uint16_le buf i =
  if Sys.big_endian then swap16 (unsafe_get_uint16_ne buf i)
  else unsafe_get_uint16_ne buf i
[@@inline]

let unsafe_get_uint16_be buf i =
  if not Sys.big_endian then swap16 (unsafe_get_uint16_ne buf i)
  else unsafe_get_uint16_ne buf i
[@@inline]

let unsafe_get_int16_ne buf i =
  (unsafe_get_uint16_ne buf i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)
[@@inline]

let unsafe_get_int16_le buf i =
  (unsafe_get_uint16_le buf i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)
[@@inline]

let unsafe_get_int16_be buf i =
  (unsafe_get_uint16_be buf i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)
[@@inline]

let unsafe_set_int16_ne = unsafe_set_uint16_ne

let unsafe_set_int16_le buf i v =
  if Sys.big_endian then unsafe_set_uint16_ne buf i (swap16 v)
  else unsafe_set_uint16_ne buf i v
[@@inline]

let unsafe_set_int16_be buf i v =
  if not Sys.big_endian then unsafe_set_uint16_ne buf i (swap16 v)
  else unsafe_set_uint16_ne buf i v
[@@inline]

let unsafe_set_uint16_le = unsafe_set_int16_le
let unsafe_set_uint16_be = unsafe_set_int16_be

let unsafe_get_int32_le buf i =
  if Sys.big_endian then swap32 (unsafe_get_int32_ne buf i)
  else unsafe_get_int32_ne buf i
[@@inline]

let unsafe_get_int32_be buf i =
  if not Sys.big_endian then swap32 (unsafe_get_int32_ne buf i)
  else unsafe_get_int32_ne buf i
[@@inline]

let unsafe_set_int32_le buf i v =
  if Sys.big_endian then unsafe_set_int32_ne buf i (swap32 v)
  else unsafe_set_int32_ne buf i v
[@@inline]

let unsafe_set_int32_be buf i v =
  if not Sys.big_endian then unsafe_set_int32_ne buf i (swap32 v)
  else unsafe_set_int32_ne buf i v
[@@inline]

let unsafe_get_int64_le buf i =
  if Sys.big_endian then swap64 (unsafe_get_int64_ne buf i)
  else unsafe_get_int64_ne buf i
[@@inline]

let unsafe_get_int64_be buf i =
  if not Sys.big_endian then swap64 (unsafe_get_int64_ne buf i)
  else unsafe_get_int64_ne buf i
[@@inline]

let unsafe_set_int64_le buf i v =
  if Sys.big_endian then unsafe_set_int64_ne buf i (swap64 v)
  else unsafe_set_int64_ne buf i v
[@@inline]

let unsafe_set_int64_be buf i v =
  if not Sys.big_endian then unsafe_set_int64_ne buf i (swap64 v)
  else unsafe_set_int64_ne buf i v
[@@inline]

(* NOTE(dinosaure): here, we implement a fast equal function where we don't use
   local functions (to avoid closure allocations) and unroll the equality on
   words (8 and 64) to speed-up the computation. *)

let rec equal_bytes a src_off b dst_off idx len =
  idx >= len
  || Bytes.unsafe_get a (src_off + idx) == Bytes.unsafe_get b (dst_off + idx)
     && equal_bytes a src_off b dst_off (idx + 1) len

let[@inline always] eq_word a src_off b dst_off idx =
  Int64.equal
    (unsafe_get_int64_ne a (src_off + idx))
    (unsafe_get_int64_ne b (dst_off + idx))

(* unroll by 8 bytes *)
let rec equal_word a src_off b dst_off idx len =
  if idx + 8 > len then equal_bytes a src_off b dst_off idx len
  else
    eq_word a src_off b dst_off idx
    && equal_word a src_off b dst_off (idx + 8) len

(* unroll by 64 bytes *)
let rec equal_words a src_off b dst_off idx len =
  if idx + 64 > len then equal_word a src_off b dst_off idx len
  else
    eq_word a src_off b dst_off idx
    && eq_word a src_off b dst_off (idx + 8)
    && eq_word a src_off b dst_off (idx + 16)
    && eq_word a src_off b dst_off (idx + 24)
    && eq_word a src_off b dst_off (idx + 32)
    && eq_word a src_off b dst_off (idx + 40)
    && eq_word a src_off b dst_off (idx + 48)
    && eq_word a src_off b dst_off (idx + 56)
    && equal_words a src_off b dst_off (idx + 64) len

let[@inline always] unsafe_equal a ~src_off b ~dst_off ~len =
  equal_words a src_off b dst_off 0 len

let rec compare_bytes a src_off b dst_off idx len =
  if idx >= len then 0
  else
    let x = Bytes.unsafe_get a (src_off + idx)
    and y = Bytes.unsafe_get b (dst_off + idx) in
    if x == y then compare_bytes a src_off b dst_off (idx + 1) len
    else Char.compare x y

let rec compare_words a src_off b dst_off idx len =
  if idx + 8 > len then compare_bytes a src_off b dst_off idx len
  else
    let x = unsafe_get_int64_be a (src_off + idx)
    and y = unsafe_get_int64_be b (dst_off + idx) in
    if Int64.equal x y then compare_words a src_off b dst_off (idx + 8) len
    else Int64.unsigned_compare x y

let[@inline always] unsafe_memcmp a ~src_off b ~dst_off ~len =
  compare_words a src_off b dst_off 0 len

let memcmp a ~src_off b ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bytes.length a - len
    || dst_off < 0
    || dst_off > Bytes.length b - len
  then invalid_arg "Bytes.memcmp";
  unsafe_memcmp a ~src_off b ~dst_off ~len
