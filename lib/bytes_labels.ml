include Bytes

let[@inline always] blit src ~src_off dst ~dst_off ~len =
  Bytes.blit src src_off dst dst_off len

let[@inline always] fill t ~off ~len chr = Bytes.fill t off len chr

let[@inline always] blit_to_bytes src ~src_off dst ~dst_off ~len =
  Bytes.blit src src_off dst dst_off len

let string ?(off = 0) ?len str =
  let len =
    match len with Some len -> len | None -> String.length str - off
  in
  if len < 0 || off < 0 || off > String.length str - len then
    invalid_arg "Bytes.string";
  let buf = String.sub str off len in
  Bytes.unsafe_of_string buf

let overlap a b = if a == b then Some (Bytes.length a, 0, 0) else None
