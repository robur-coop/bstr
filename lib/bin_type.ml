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

  let eq : type a b. a t -> b t -> (a, b) eq option =
   fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None

  let cast_exn : type a b. a t -> b t -> a -> b =
   fun awit bwit a ->
    match eq awit bwit with Some Refl -> a | None -> assert false
end

module Staged = struct
  type +'a t

  external stage : 'a -> 'a t = "%identity"
  external unstage : 'a t -> 'a = "%identity"
end

(* NOTE(dinosaure): [Len] and [Off] enable us to avoid making a mess of things
   at little cost (as they do not add any penalty to the encoding and decoding
   process). Let's put our trust (and faith) in OCaml, the grand master. *)

module Len : sig
  type t = private int [@@immediate]

  val zero : t
  val one : t
  val v : int -> t
  val to_int : t -> int
  val ( + ) : t -> t -> t
  val ( * ) : int -> t -> t
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  external unsafe : int -> t = "%identity"
end = struct
  type t = int

  let zero = 0
  let one = 1
  let[@inline] to_int (n : int) = n
  let[@inline] ( + ) (a : int) (b : int) = a + b
  let[@inline] ( * ) (a : int) (b : int) = a * b
  let[@inline] ( = ) (a : int) (b : int) = a = b
  let[@inline] ( < ) (a : int) (b : int) = a < b
  let[@inline] ( <= ) (a : int) (b : int) = a <= b

  external unsafe : int -> t = "%identity"

  let[@inline] v n =
    if n < 0 then invalid_arg "Bin_type.Len.v";
    n
end

module Off : sig
  type _ t = private int [@@immediate]
  type abs
  type rel

  val zero : 'w t
  val v : int -> 'w t
  val to_int : 'w t -> int
  val ( +> ) : 'w t -> Len.t -> 'w t
  val ( -- ) : 'w t -> 'w t -> Len.t
  val ( = ) : 'w t -> 'w t -> bool
  val ( < ) : 'w t -> 'w t -> bool
  val ( <= ) : 'w t -> 'w t -> bool
  val ( > ) : 'w t -> 'w t -> bool
  val at : abs t -> rel t -> abs t
  val from_base : rel t -> Len.t
  val of_len : Len.t -> 'w t
  val incr : 'w t ref -> unit
  external unsafe : int -> _ t = "%identity"
end = struct
  type _ t = int
  type abs
  type rel

  let zero = 0
  let[@inline] to_int (o : int) = o
  let[@inline] ( +> ) (a : int) (b : Len.t) = a + (b :> int)
  let[@inline] ( -- ) (a : int) (b : int) = Len.v (if a > b then a - b else 0)
  let[@inline] ( = ) (a : int) (b : int) = a = b
  let[@inline] ( < ) (a : int) (b : int) = a < b
  let[@inline] ( <= ) (a : int) (b : int) = a <= b
  let[@inline] ( > ) (a : int) (b : int) = a > b
  let[@inline] at (base : int) (delta : int) = base + delta
  let[@inline] from_base (delta : int) = Len.v delta
  let[@inline] of_len (n : Len.t) = (n :> int)
  let[@inline] incr ref = incr ref

  external unsafe : int -> _ t = "%identity"

  let[@inline] v n =
    if n < 0 then invalid_arg "Bin_type.Off.v";
    n
end

let[@inline] have ~limit ~offset = Off.(limit -- offset)

type pos = Off.abs Off.t ref
type endianness = Big_endian | Little_endian | Native_endian
type bit_order = Msb_first | Lsb_first
type bits_base = B8 | B16 of endianness | B32 of endianness

type _ t =
  | Primary : 'a primary -> 'a t
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t
  | Map : ('a, 'b) map -> 'b t
  | Seq : ('a, 'b) seq -> 'b t
  | Bind : ('a, 'b) bind -> 'b t
  | Bits : 'a bits -> 'a t

and _ primary =
  | Char : char primary
  | UInt8 : int primary
  | Int8 : int primary
  | UInt16 : endianness -> int primary
  | Int16 : endianness -> int primary
  | Int32 : endianness -> int32 primary
  | Int64 : endianness -> int64 primary
  | Var_int : int primary
  | Bytes : len -> string primary
  | Bstr : len -> Bstr.t primary
  | Until : char -> string primary
  | Const : 'a -> 'a primary

and len = Fixed of int | Prefix of int t | Delim of char | Rest
and ('a, 'b) seq = { slen: len; selt: 'a t; skind: ('a, 'b) seq_kind }

and (_, _) seq_kind =
  | Sarray : ('a, 'a array) seq_kind
  | Slist : ('a, 'a list) seq_kind

and _ a_case = C0 : 'a case0 -> 'a a_case | C1 : ('a, 'b) case1 -> 'a a_case

and _ case_v =
  | CV0 : 'a case0 -> 'a case_v
  | CV1 : ('a, 'b) case1 * 'b -> 'a case_v

and 'a case0 = { ctag0: int; cidx0: int; cname0: string; c0: 'a }

and ('a, 'b) case1 = {
    ctag1: int
  ; cidx1: int
  ; cname1: string
  ; ctype1: 'b t
  ; cwitn1: 'b Witness.t
  ; c1: 'b -> 'a
}

and 'a record = { rname: string; rfields: 'a fields_and_constr }

and 'a fields_and_constr =
  | Fields : ('a, 'b) fields * 'b -> 'a fields_and_constr

and ('a, 'b) fields =
  | F0 : ('a, 'a) fields
  | F1 : ('a, 'b) field * ('a, 'c) fields -> ('a, 'b -> 'c) fields

and ('a, 'b) field = { fid: int; fname: string; ftype: 'b t; fget: 'a -> 'b }

and 'a variant = {
    vname: string
  ; vcases: 'a a_case array
  ; vget: 'a -> 'a case_v
  ; vtag: int t
}

and ('a, 'b) map = { x: 'a t; f: 'a -> 'b; g: 'b -> 'a; mwit: 'b Witness.t }
and ('a, 'b) bind = { bx: 'a t; bf: 'a -> 'b t; bg: 'b -> 'a }
and _ a_field = Field : ('a, 'b) field -> 'a a_field

and 'a bits = {
    bname: string
  ; bbase: bits_base
  ; border: bit_order
  ; bfields: 'a bits_and_constr
}

and 'a bits_and_constr =
  | BFields : ('a, 'b) bit_fields * 'b -> 'a bits_and_constr

and ('a, 'b) bit_fields =
  | BF0 : ('a, 'a) bit_fields
  | BF1 : ('a, 'b) bit_field * ('a, 'c) bit_fields -> ('a, 'b -> 'c) bit_fields

and ('a, 'b) bit_field = {
    bfname: string
  ; bfwidth: int
  ; bfget: 'a -> 'b
  ; bfkind: 'b bit_kind
}

and _ bit_kind = Bint : int bit_kind | Bbool : bool bit_kind

type _ a_bit_field = Bit_field : ('a, 'b) bit_field -> 'a a_bit_field

let fields r =
  let rec go : type a b. (a, b) fields -> a a_field list = function
    | F0 -> []
    | F1 (x, r) -> Field x :: go r
  in
  match r.rfields with Fields (f, _) -> go f

let rec a_bit_fields : type a b. (a, b) bit_fields -> a a_bit_field list =
  function
  | BF0 -> []
  | BF1 (x, r) -> Bit_field x :: a_bit_fields r

let bit_fields : type a. a bits -> a a_bit_field list =
 fun v -> match v.bfields with BFields (fs, _) -> a_bit_fields fs

let shift ~order ~total ~used ~width =
  match order with Lsb_first -> used | Msb_first -> total - used - width
[@@inline]

let bits_layout : type a. a bits -> (int * int) array =
 fun b ->
  let total = match b.bbase with B8 -> 8 | B16 _ -> 16 | B32 _ -> 32 in
  let used = ref 0 in
  let fn (Bit_field field) =
    let shift = shift ~order:b.border ~total ~used:!used ~width:field.bfwidth in
    used := !used + field.bfwidth;
    (shift, (1 lsl field.bfwidth) - 1)
  in
  let cells = List.map fn (bit_fields b) in
  Array.of_list cells

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

let rec a_fields : type a b. (a, b) fields -> a a_field list = function
  | F0 -> []
  | F1 (x, r) -> Field x :: a_fields r

let record_fields : type a. a record -> a a_field list =
 fun { rfields= Fields (fs, _); _ } -> a_fields fs

module Dispatch = struct
  type 'a t =
    | Base : 'a -> 'a t
    | Arrow : { arg_wit: 'b Witness.t; fn: 'b -> 'a } -> 'a t
end

module Case_folder = struct
  type ('a, 'r) t = { c0: 'a case0 -> 'r; c1: 'b. ('a, 'b) case1 -> 'b -> 'r }
end

let fold_variant : type a r. (a, r) Case_folder.t -> a variant -> a -> r =
 fun folder v_typ ->
  let cases =
    let fn = function
      | C0 c0 -> Dispatch.Base (folder.c0 c0)
      | C1 c1 -> Dispatch.Arrow { arg_wit= c1.cwitn1; fn= folder.c1 c1 }
    in
    Array.map fn v_typ.vcases
  in
  fun v ->
    match v_typ.vget v with
    | CV0 { ctag0; _ } ->
        begin match cases.(ctag0) with
        | Dispatch.Base x -> x
        | _ -> assert false
        end
    | CV1 ({ ctag1; cwitn1; _ }, v) ->
        begin match cases.(ctag1) with
        | Dispatch.Arrow { fn; arg_wit } ->
            let v = Witness.cast_exn cwitn1 arg_wit v in
            fn v
        | _ -> assert false
        end

module Case = struct
  let tag = function C0 { ctag0; _ } -> ctag0 | C1 { ctag1; _ } -> ctag1
  let name = function C0 { cname0; _ } -> cname0 | C1 { cname1; _ } -> cname1
  let expected vcases = Array.to_list (Array.map tag vcases)

  let dense vcases =
    let n = Array.length vcases in
    let rec go i = i >= n || (tag vcases.(i) = i && go (i + 1)) in
    go 0
end

module Bits_folder (Acc : sig
  type ('a, 'b) t
end) =
struct
  type 'a t = {
      nil: ('a, 'a) Acc.t
    ; cons: 'b 'c. ('a, 'b) bit_field -> ('a, 'c) Acc.t -> ('a, 'b -> 'c) Acc.t
  }

  let rec fold : type a c. a t -> (a, c) bit_fields -> (a, c) Acc.t =
   fun folder -> function
    | BF0 -> folder.nil
    | BF1 (field, fs) -> folder.cons field (fold folder fs)
end
