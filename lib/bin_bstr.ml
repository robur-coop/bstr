include Bstr

(* NOTE(dinosaure): we have to re-declare unsafe accessors with [external] to
   allow OCaml to perform a minimum level of cross-module optimisation. If we
   were to use [Bstr], these functions would be declared with [val], which would
   prevent them from being inlined and their call sites from being replaced
   directly by the primitives. *)

external unsafe_get_int64_ne : t -> int -> int64 = "%caml_bigstring_get64u"
external swap64 : int64 -> int64 = "%bswap_int64"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap16 : int -> int = "%bswap16"
external unsafe_set_uint8 : t -> int -> int -> unit = "%caml_ba_unsafe_set_1"

external unsafe_set_uint16_ne : t -> int -> int -> unit
  = "%caml_bigstring_set16u"

external unsafe_set_int32_ne : t -> int -> int32 -> unit
  = "%caml_bigstring_set32u"

external unsafe_set_int64_ne : t -> int -> int64 -> unit
  = "%caml_bigstring_set64u"

let unsafe_set_int8 = unsafe_set_uint8

let[@inline] unsafe_set_uint16_be b i x =
  if not Sys.big_endian then unsafe_set_uint16_ne b i (swap16 x)
  else unsafe_set_uint16_ne b i x

let unsafe_set_int16_ne = unsafe_set_uint16_ne
let unsafe_set_int16_le = unsafe_set_uint16_le
let unsafe_set_int16_be = unsafe_set_uint16_be

let[@inline] unsafe_set_int32_le b i x =
  if Sys.big_endian then unsafe_set_int32_ne b i (swap32 x)
  else unsafe_set_int32_ne b i x

let[@inline] unsafe_set_int32_be b i x =
  if not Sys.big_endian then unsafe_set_int32_ne b i (swap32 x)
  else unsafe_set_int32_ne b i x

let[@inline] unsafe_set_int64_le b i x =
  if Sys.big_endian then unsafe_set_int64_ne b i (swap64 x)
  else unsafe_set_int64_ne b i x

let[@inline] unsafe_set_int64_be b i x =
  if not Sys.big_endian then unsafe_set_int64_ne b i (swap64 x)
  else unsafe_set_int64_ne b i x

let sub_string bstr ~off ~len = Bstr.sub_string bstr ~off ~len [@@inline]
let sub_bstr bstr ~off ~len = Bstr.sub bstr ~off ~len [@@inline]

let blit_from_string src ~src_off dst ~dst_off ~len =
  Bstr.blit_from_string src ~src_off dst ~dst_off ~len
[@@inline]

let blit_from_bstr src ~src_off dst ~dst_off ~len =
  Bstr.blit src ~src_off dst ~dst_off ~len
[@@inline]

let blit_string src src_off dst dst_off len =
  blit_from_string src ~src_off dst ~dst_off ~len
[@@inline]

let index_from bstr ~off ~limit chr =
  let i = ref off in
  let res = ref (-1) in
  while !res < 0 && !i < limit do
    if Bstr.unsafe_get bstr !i = chr then res := !i else incr i
  done;
  !res
