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
let sub t ~off ~len = Bytes.sub t off len

let blit_from_bytes src ~src_off dst ~dst_off ~len =
  Bytes.blit src src_off dst dst_off len
