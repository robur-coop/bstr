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

type pos = int ref
type endianness = Big_endian | Little_endian | Native_endian

type _ t =
  | Primary : 'a primary -> 'a t
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t
  | Map : ('a, 'b) map -> 'b t
  | Seq : 'a len_v -> 'a array t

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

and 'a len_v = { llen: int; lval: 'a t }
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

and ('a, 'b) map = { x: 'a t; f: 'a -> 'b; g: 'b -> 'a; mwit: 'b Witness.t }
and _ a_field = Field : ('a, 'b) field -> 'a a_field
