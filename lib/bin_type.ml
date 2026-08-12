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

  let[@inline] v n =
    if n < 0 then invalid_arg "Bin_type.Off.v";
    n
end

let[@inline] have ~limit ~offset = Off.(limit -- offset)

type pos = Off.abs Off.t ref
type endianness = Big_endian | Little_endian | Native_endian

type _ t =
  | Primary : 'a primary -> 'a t
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t
  | Map : ('a, 'b) map -> 'b t
  | Seq : ('a, 'b) seq -> 'b t

and _ primary =
  | Char : char primary
  | UInt8 : int primary
  | Int8 : int primary
  | UInt16 : endianness -> int primary
  | Int16 : endianness -> int primary
  | Int32 : endianness -> int32 primary
  | Int64 : endianness -> int64 primary
  | Var_int : int primary
  | Bytes : int -> string primary
  | CString : string primary
  | Until : char -> string primary
  | Bstr : int -> Bstr.t primary
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

and 'a case0 = { ctag0: int; c0: 'a }

and ('a, 'b) case1 = {
    ctag1: int
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
    vwit: 'a Witness.t
  ; vcases: 'a a_case array
  ; vget: 'a -> 'a case_v
  ; vtag: int t
}

and ('a, 'b) map = { x: 'a t; f: 'a -> 'b; g: 'b -> 'a; mwit: 'b Witness.t }
and _ a_field = Field : ('a, 'b) field -> 'a a_field

let fields r =
  let rec go : type a b. (a, b) fields -> a a_field list = function
    | F0 -> []
    | F1 (x, r) -> Field x :: go r
  in
  match r.rfields with Fields (f, _) -> go f

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
