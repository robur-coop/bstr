include String

external unsafe_get_uint8 : string -> int -> int = "%string_unsafe_get"
external unsafe_get_uint16_ne : string -> int -> int = "%caml_string_get16u"
external unsafe_get_int64_ne : string -> int -> int64 = "%caml_string_get64u"
external swap16 : int -> int = "%bswap16"
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

let sub_string t ~off ~len = sub t off len
let sub t ~off ~len = sub t off len
