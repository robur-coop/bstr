include Bytes

let[@inline always] blit src ~src_off dst ~dst_off ~len =
  Bytes.blit src src_off dst dst_off len

let[@inline always] fill t ~off ~len chr = Bytes.fill t off len chr

let[@inline always] blit_to_bytes src ~src_off dst ~dst_off ~len =
  Bytes.blit src src_off dst dst_off len
