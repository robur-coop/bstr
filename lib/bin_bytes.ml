include Bytes

external unsafe_get_char : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set_char : bytes -> int -> char -> unit = "%bytes_unsafe_set"
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

let unsafe_get_uint8 buf i = Char.code (unsafe_get_char buf i) [@@inline]

let unsafe_set_uint8 buf i v = unsafe_set_char buf i (Char.unsafe_chr v)
[@@inline]

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

let blit_from_string src ~src_off dst ~dst_off ~len =
  Bytes.blit_string src src_off dst dst_off len
[@@inline]

let blit_from_bstr src ~src_off dst ~dst_off ~len =
  Bstr.blit_to_bytes src ~src_off dst ~dst_off ~len
[@@inline]
