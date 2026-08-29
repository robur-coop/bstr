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

type 'a t = { buf: 'a; off: int; len: int }

let unsafe_make ~off ~len buf = { off; len; buf }
let unsafe_sub { off; buf; _ } off' len' = { off= off + off'; len= len'; buf }

let pp ppf { off; len; _ } =
  Format.fprintf ppf "@[<hov>{ off=@ %d;@ len=@ %d;@ }@]" off len

let length { len; _ } = len
let is_empty { len; _ } = len == 0

let sub t ~off ~len =
  let off' = t.off + off in
  let top = off' + len in
  let old = t.off + t.len in
  if off' >= t.off && top <= old && off' <= top then { t with off= off'; len }
  else invalid_arg "Slice.sub"

let shift t off =
  if off > length t || off < 0 then invalid_arg "Slice.shift";
  let len = length t - off in
  unsafe_sub t off len

module type R = sig
  type buf
  type t

  val create : int -> t
  val make : ?off:int -> ?len:int -> buf -> t
  val init : int -> (int -> char) -> t
  val empty : t
  val length : t -> int
  val is_empty : t -> bool
  val chop : ?rev:bool -> t -> char option
  val hash : t -> int
  val equal : t -> t -> bool
  val constant_equal : t -> t -> bool
  val compare : t -> t -> int
  val get : t -> int -> char
  val unsafe_get : t -> int -> char
  val get_int8 : t -> int -> int
  val get_uint8 : t -> int -> int
  val get_uint16_ne : t -> int -> int
  val get_uint16_le : t -> int -> int
  val get_uint16_be : t -> int -> int
  val get_int16_ne : t -> int -> int
  val get_int16_le : t -> int -> int
  val get_int16_be : t -> int -> int
  val get_int32_ne : t -> int -> int32
  val get_int32_le : t -> int -> int32
  val get_int32_be : t -> int -> int32
  val get_int64_ne : t -> int -> int64
  val get_int64_le : t -> int -> int64
  val get_int64_be : t -> int -> int64
  val filter : (char -> bool) -> t -> t
  val filter_map : (char -> char option) -> t -> t
  val map : (char -> char) -> t -> t
  val mapi : (int -> char -> char) -> t -> t
  val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a
  val fold_right : (char -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (char -> unit) -> t -> unit
  val iteri : (int -> char -> unit) -> t -> unit
  val hex : t -> string
  val overlap : t -> t -> (int * int * int) option
  val append : t -> t -> t
  val starts_with : prefix:string -> t -> bool
  val is_prefix : affix:string -> t -> bool
  val ends_with : suffix:string -> t -> bool
  val is_suffix : affix:string -> t -> bool
  val is_infix : affix:string -> t -> bool
  val for_all : (char -> bool) -> t -> bool
  val exists : (char -> bool) -> t -> bool
  val trim : ?drop:(char -> bool) -> t -> t

  val span :
    ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t * t

  val take : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
  val drop : ?rev:bool -> ?min:int -> ?max:int -> ?sat:(char -> bool) -> t -> t
  val shift : t -> int -> t
  val sub : t -> off:int -> len:int -> t
  val split_on_char : char -> t -> t list
  val cut : ?rev:bool -> sep:string -> t -> (t * t) option
  val cuts : ?rev:bool -> ?empty:bool -> sep:string -> t -> t list
  val index : t -> ?off:int -> ?len:int -> char -> int option
  val contains : t -> ?off:int -> ?len:int -> char -> bool
  val concat : string -> t list -> t
  val extend : t -> int -> int -> t
  val copy : t -> t
  val sub_string : t -> off:int -> len:int -> string
  val to_string : t -> string
  val of_string : string -> t
  val string : ?off:int -> ?len:int -> string -> t

  val blit_to_bytes :
    t -> ?src_off:int -> bytes -> dst_off:int -> len:int -> unit

  val with_range : ?first:int -> ?len:int -> t -> t
  val with_index_range : ?first:int -> ?last:int -> t -> t
  val to_seq : t -> char Seq.t
  val to_seqi : t -> (int * char) Seq.t
  val of_seq : char Seq.t -> t
end

module type W = sig
  type t

  val set : t -> int -> char -> unit
  val unsafe_set : t -> int -> char -> unit
  val set_int8 : t -> int -> int -> unit
  val set_uint8 : t -> int -> int -> unit
  val set_uint16_ne : t -> int -> int -> unit
  val set_uint16_le : t -> int -> int -> unit
  val set_uint16_be : t -> int -> int -> unit
  val set_int16_ne : t -> int -> int -> unit
  val set_int16_le : t -> int -> int -> unit
  val set_int16_be : t -> int -> int -> unit
  val set_int32_ne : t -> int -> int32 -> unit
  val set_int32_le : t -> int -> int32 -> unit
  val set_int32_be : t -> int -> int32 -> unit
  val set_int64_ne : t -> int -> int64 -> unit
  val set_int64_le : t -> int -> int64 -> unit
  val set_int64_be : t -> int -> int64 -> unit
  val fill : t -> ?off:int -> ?len:int -> char -> unit
  val blit : t -> t -> unit
  val blit_from_bytes : bytes -> src_off:int -> t -> ?dst_off:int -> int -> unit

  val blit_from_string :
    string -> src_off:int -> t -> ?dst_off:int -> int -> unit
end

module type S = sig
  type buf
  type nonrec t = buf t

  include R with type buf := buf and type t := t
  include W with type t := t
end
