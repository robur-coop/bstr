include Bstr

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
