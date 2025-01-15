type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Bytes = struct
  include Bytes

  external unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"
  external unsafe_set_uint8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"
  external unsafe_get_int32_ne : bytes -> int -> int32 = "%caml_bytes_get32u"

  external unsafe_set_int32_ne : bytes -> int -> int32 -> unit
    = "%caml_bytes_set32u"
end

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external get_uint8 : t -> int -> int = "%caml_ba_ref_1"
external set_uint8 : t -> int -> int -> unit = "%caml_ba_set_1"
external get_uint16_ne : t -> int -> int = "%caml_bigstring_get16"
external set_int16_ne : t -> int -> int -> unit = "%caml_bigstring_set16"
external get_int32_ne : t -> int -> int32 = "%caml_bigstring_get32"
external set_int32_ne : t -> int -> int32 -> unit = "%caml_bigstring_set32"
external set_int64_ne : t -> int -> int64 -> unit = "%caml_bigstring_set64"
external unsafe_get_uint8 : t -> int -> int = "%caml_ba_unsafe_ref_1"
external unsafe_set_uint8 : t -> int -> int -> unit = "%caml_ba_unsafe_set_1"
external unsafe_get_uint16_ne : t -> int -> int = "%caml_bigstring_get16u"

external unsafe_set_uint16_ne : t -> int -> int -> unit
  = "%caml_bigstring_set16u"

external unsafe_get_int32_ne : t -> int -> int32 = "%caml_bigstring_get32u"

external unsafe_set_int32_ne : t -> int -> int32 -> unit
  = "%caml_bigstring_set32u"

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

external unsafe_memmove :
  t -> (int[@untagged]) -> t -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "bstr_bytecode_memmove" "bstr_native_memmove"
[@@noalloc]

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
  -> (int[@untagged]) = "bstr_bytecode_memchr" "bstr_native_memchr"
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

let memmove src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > Bigarray.Array1.dim src - len
    || dst_off < 0
    || dst_off > Bigarray.Array1.dim dst - len
  then invalid_arg "Bstr.memmove";
  unsafe_memmove src src_off dst dst_off len

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

external length : t -> int = "%caml_ba_dim_1"
external get : t -> int -> char = "%caml_ba_ref_1"
external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

let unsafe_fill bstr ~off ~len v =
  let nv = Nativeint.of_int v in
  let vv = Nativeint.(logor (shift_left nv 8) nv) in
  let vvvv = Nativeint.(logor (shift_left vv 16) vv) in
  let vvvv = Nativeint.to_int32 vvvv in
  let len0 = len land 3 in
  let len1 = len lsr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    unsafe_set_int32_ne bstr (off + i) vvvv
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    unsafe_set_uint8 bstr (off + i) v
  done

let fill bstr ~off ~len chr =
  if len < 0 || off < 0 || off > length bstr - len then invalid_arg "Bstr.fill";
  unsafe_fill bstr ~off ~len (Char.code chr)

let make len chr =
  let bstr = create len in
  unsafe_fill bstr ~off:0 ~len (Char.code chr);
  (* [Obj.magic] instead of [Char.code]? *)
  bstr

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

let _unsafe_get_uint16_le bstr i =
  (* TODO(dinosaure): for unicode. *)
  if Sys.big_endian then swap16 (unsafe_get_uint16_ne bstr i)
  else unsafe_get_uint16_ne bstr i

let _unsafe_get_uint16_be bstr i =
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

let _unsafe_set_uint16_le bstr i x =
  (* TODO(dinosaure): for unicode. *)
  if Sys.big_endian then unsafe_set_uint16_ne bstr i (swap16 x)
  else unsafe_set_uint16_ne bstr i x

let _unsafe_set_uint16_be bstr i x =
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

external sub : t -> off:int -> len:int -> t = "caml_ba_sub"

let unsafe_blit src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len lsr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = unsafe_get_int32_ne src (src_off + i) in
    unsafe_set_int32_ne dst (dst_off + i) v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = unsafe_get_uint8 src (src_off + i) in
    unsafe_set_uint8 dst (dst_off + i) v
  done

let blit src ~src_off dst ~dst_off ~len =
  if
    len < 0
    || src_off < 0
    || src_off > length src - len
    || dst_off < 0
    || dst_off > length dst - len
  then invalid_arg "Bstr.blit";
  unsafe_blit src ~src_off dst ~dst_off ~len

let unsafe_blit_to_bytes bstr ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len lsr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = unsafe_get_int32_ne bstr (src_off + i) in
    Bytes.unsafe_set_int32_ne dst (dst_off + i) v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = unsafe_get_uint8 bstr (src_off + i) in
    Bytes.unsafe_set_uint8 dst (dst_off + i) v
  done

let unsafe_blit_from_bytes src ~src_off bstr ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len lsr 2 in
  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = Bytes.unsafe_get_int32_ne src (src_off + i) in
    unsafe_set_int32_ne bstr (dst_off + i) v
  done;
  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = Bytes.unsafe_get_uint8 src (src_off + i) in
    unsafe_set_uint8 bstr (dst_off + i) v
  done

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

let unsafe_sub_string bstr src_off len =
  let buf = Bytes.create len in
  unsafe_blit_to_bytes bstr ~src_off buf ~dst_off:0 ~len;
  Bytes.unsafe_to_string buf

let sub_string bstr ~off ~len =
  if len < 0 || off < 0 || off > length bstr - len then
    invalid_arg "Bstr.sub_string";
  unsafe_sub_string bstr off len

let to_string bstr = unsafe_sub_string bstr 0 (length bstr)
let is_empty bstr = length bstr == 0

let is_prefix ~affix bstr =
  let len_affix = String.length affix in
  let len_bstr = length bstr in
  if len_affix > len_bstr then false
  else
    let max_idx_affix = len_affix - 1 in
    let rec go idx =
      if idx > max_idx_affix then true
      else if affix.[idx] != bstr.{idx} then false
      else go (succ idx)
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
      else go (succ idx) 0
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
      else go (succ idx)
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

let exists sat bstr =
  try
    for idx = 0 to length bstr - 1 do
      if sat (unsafe_get bstr idx) then raise_notrace Break
    done;
    false
  with Break -> true

let equal a b =
  if length a == length b then
    try
      let len = length a in
      let len0 = len land 3 in
      let len1 = len lsr 2 in
      for i = 0 to len1 - 1 do
        let i = i * 4 in
        if unsafe_get_int32_ne a i <> unsafe_get_int32_ne b i then
          raise_notrace Break
      done;
      for i = 0 to len0 - 1 do
        let i = (len1 * 4) + i in
        if unsafe_get_uint8 a i != unsafe_get_uint8 b i then raise_notrace Break
      done;
      true
    with Break -> false
  else false

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
    else sub bstr ~off:first ~len:(last + 1 - first)

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
  else if first == 0 && last = max_idx then bstr
  else sub bstr ~off:first ~len:(last + 1 - first)

let is_white chr = chr == ' '

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
      else sub bstr ~off:left ~len:(right - left)

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
      else (sub bstr ~off:0 ~len:idx, sub bstr ~off:idx ~len:(len - idx))
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
    let need_idx = max_idx - min in
    let rec go idx =
      if idx >= min_idx && sat bstr.{idx} then go (pred idx)
      else if idx > need_idx || idx == max_idx then (bstr, empty)
      else if idx == -1 then (empty, bstr)
      else
        let cut = idx + 1 in
        (sub bstr ~off:0 ~len:cut, sub bstr ~off:cut ~len:(len - cut))
    in
    go 0

let span ?(rev = false) ?min ?max ?sat bstr =
  match rev with
  | true -> rspan ?min ?max ?sat bstr
  | false -> fspan ?min ?max ?sat bstr

let ftake ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
  if min < 0 then invalid_arg "Bstr.ftake";
  if max < 0 then invalid_arg "Bstr.ftake";
  if min > max || max == 0 then empty
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
      else if idx < need_idx || idx == 0 then empty
      else if idx == len then bstr
      else sub bstr ~off:0 ~len:idx
    in
    go 0

let rtake ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
  if min < 0 then invalid_arg "Bstr.rtake";
  if max < 0 then invalid_arg "Bstr.rtake";
  if min > max || max == 0 then empty
  else
    let len = length bstr in
    let max_idx = len - 1 in
    let min_idx =
      let k = len - max in
      if k < 0 then 0 else k
    in
    let need_idx = max_idx - min in
    let rec go idx =
      if idx >= min_idx && sat bstr.{idx} then go (pred idx)
      else if idx > need_idx || idx == max_idx then empty
      else if idx == -1 then bstr
      else
        let cut = idx + 1 in
        sub bstr ~off:cut ~len:(len - cut)
    in
    go 0

let take ?(rev = false) ?min ?max ?sat bstr =
  match rev with
  | true -> rtake ?min ?max ?sat bstr
  | false -> ftake ?min ?max ?sat bstr

let fdrop ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
  if min < 0 then invalid_arg "Bstr.fdrop";
  if max < 0 then invalid_arg "Bstr.fdrop";
  if min > max || max == 0 then bstr
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
      else if idx < need_idx || idx == 0 then bstr
      else if idx == len then bstr
      else sub bstr ~off:idx ~len:(len - idx)
    in
    go 0

let rdrop ?(min = 0) ?(max = max_int) ?(sat = Fun.const true) bstr =
  if min < 0 then invalid_arg "Bstr.rdrop";
  if max < 0 then invalid_arg "Bstr.rdrop";
  if min > max || max == 0 then bstr
  else
    let len = length bstr in
    let max_idx = len - 1 in
    let min_idx =
      let k = len - max in
      if k < 0 then 0 else k
    in
    let need_idx = max_idx - min in
    let rec go idx =
      if idx >= min_idx && sat bstr.{idx} then go (pred idx)
      else if idx > need_idx || idx == max_idx then bstr
      else if idx == -1 then empty
      else
        let cut = idx + 1 in
        sub bstr ~off:0 ~len:cut
    in
    go 0

let drop ?(rev = false) ?min ?max ?sat bstr =
  match rev with
  | true -> rdrop ?min ?max ?sat bstr
  | false -> fdrop ?min ?max ?sat bstr

let shift bstr off =
  if off > length bstr then invalid_arg "Bstr.shift";
  let len = length bstr - off in
  Bigarray.Array1.sub bstr off len

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
    let new_len = Int.min (2 * length !buf) Sys.max_string_length in
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

  let _eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None
end

module Pkt = struct
  type bigstring = t
  type endianness = Big_endian | Little_endian | Native_endian

  type _ t =
    | Primary : 'a primary -> 'a t
    | Record : 'a record -> 'a t
    | Variant : 'a variant -> 'a t

  and _ primary =
    | Char : char primary
    | UInt8 : int primary
    | Int8 : int primary
    | UInt16 : endianness -> int primary
    | Int16 : endianness -> int primary
    | Int32 : endianness -> int32 primary
    | Int64 : endianness -> int64 primary
    | Var_int31 : int primary
    | Var_int63 : int primary
    | Bytes : int -> string primary
    | CString : string primary
    | Until : char -> string primary

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

  (* decoder *)

  module Record_decoder = Fields_folder (struct
    type ('a, 'b) t = bigstring -> int ref -> 'b -> 'a
  end)

  type 'a decoder = bigstring -> int ref -> 'a

  let decode_char bstr pos =
    let idx = !pos in
    incr pos; get bstr idx
  [@@inline always]

  let decode_uint8 bstr pos =
    let idx = !pos in
    incr pos; get_uint8 bstr idx
  [@@inline always]

  let decode_int8 bstr pos =
    let idx = !pos in
    incr pos; get_int8 bstr idx
  [@@inline always]

  let decode_uint16 e bstr pos =
    let idx = !pos in
    incr pos;
    match e with
    | Big_endian -> get_uint16_be bstr idx
    | Little_endian -> get_uint16_le bstr idx
    | Native_endian -> get_uint16_ne bstr idx
  [@@inline always]

  let decode_int16 e bstr pos =
    let idx = !pos in
    incr pos;
    match e with
    | Big_endian -> get_int16_be bstr idx
    | Little_endian -> get_int16_le bstr idx
    | Native_endian -> get_int16_ne bstr idx
  [@@inline always]

  let decode_int32 e bstr pos =
    let idx = !pos in
    incr pos;
    match e with
    | Big_endian -> get_int32_be bstr idx
    | Little_endian -> get_int32_le bstr idx
    | Native_endian -> get_int32_ne bstr idx
  [@@inline always]

  let decode_int64 e bstr pos =
    let idx = !pos in
    incr pos;
    match e with
    | Big_endian -> get_int64_be bstr idx
    | Little_endian -> get_int64_le bstr idx
    | Native_endian -> get_int64_ne bstr idx
  [@@inline always]

  let decode_bytes len bstr pos =
    let off = !pos in
    pos := !pos + len;
    sub_string bstr ~off ~len

  let rec decode : type a. a t -> a decoder = function
    | Primary p -> prim p
    | Record r -> record r
    | Variant _ -> assert false

  and prim : type a. a primary -> a decoder = function
    | Char -> decode_char
    | UInt8 -> decode_uint8
    | Int8 -> decode_int8
    | UInt16 e -> decode_uint16 e
    | Int16 e -> decode_int16 e
    | Int32 e -> decode_int32 e
    | Int64 e -> decode_int64 e
    | Bytes len -> decode_bytes len
    | _ -> assert false

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

  (* combinators *)

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
  let varint31 = Primary Var_int31
  let varint63 = Primary Var_int63
  let bytes len = Primary (Bytes len)
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
end
