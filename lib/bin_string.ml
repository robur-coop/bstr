(*
 * Copyright (c) 2026 Romain Calascibetta <romain.calascibetta@gmail.com>
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

include String

(* NOTE(dinosaure): see our comment on [Bin_bstr] to understand the purpose of
   this module. *)

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_get_uint8 : string -> int -> int = "%string_unsafe_get"
external unsafe_get_uint16_ne : string -> int -> int = "%caml_string_get16u"
external unsafe_get_int32_ne : string -> int -> int32 = "%caml_bytes_get32u"
external unsafe_get_int64_ne : string -> int -> int64 = "%caml_string_get64u"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let unsafe_get_int8 buf i =
  (unsafe_get_uint8 buf i lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)
[@@inline]

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

let unsafe_get_int32_le buf i =
  if Sys.big_endian then swap32 (unsafe_get_int32_ne buf i)
  else unsafe_get_int32_ne buf i
[@@inline]

let unsafe_get_int32_be buf i =
  if not Sys.big_endian then swap32 (unsafe_get_int32_ne buf i)
  else unsafe_get_int32_ne buf i
[@@inline]

let unsafe_get_int64_le buf i =
  if Sys.big_endian then swap64 (unsafe_get_int64_ne buf i)
  else unsafe_get_int64_ne buf i
[@@inline]

let unsafe_get_int64_be buf i =
  if not Sys.big_endian then swap64 (unsafe_get_int64_ne buf i)
  else unsafe_get_int64_ne buf i
[@@inline]

let sub_string t ~off ~len = sub t off len
let sub_bstr t ~off ~len = Bstr.of_string (sub t off len)
let sub t ~off ~len = sub t off len

let index_from buf ~off ~limit chr =
  let i = ref off in
  let res = ref (-1) in
  while !res < 0 && !i < limit do
    if unsafe_get buf !i = chr then res := !i else incr i
  done;
  !res
