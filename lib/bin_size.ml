type 'a encoding = 'a Bin_type.t
type 'a t = Static of int | Dynamic of 'a | Unknown

let map : type a b. (a -> b) -> a t -> b t =
 fun fn -> function
  | Unknown -> Unknown
  | Static n -> Static n
  | Dynamic a -> Dynamic (fn a)

let ( let+ ) x f = map f x

module Offset = struct
  type t = Offset of int [@@unboxed]

  let ( +> ) : t -> int -> t = fun (Offset n) m -> Offset (n + m)
end

module Sizer = struct
  type 'a size = 'a t

  type 'a t = {
      of_value: ('a -> int) size
    ; of_encoding: (Bstr.t -> Offset.t -> Offset.t) size
  }

  let ( <+> ) : type a. a t -> a t -> a t =
    let add_of_value (a : _ size) (b : _ size) : _ size =
      match (a, b) with
      | Unknown, _ | _, Unknown -> Unknown
      | Static a, Static b -> Static (a + b)
      | Static 0, other | other, Static 0 -> other
      | Static n, Dynamic f | Dynamic f, Static n -> Dynamic (fun a -> n + f a)
      | Dynamic f, Dynamic g -> Dynamic (fun a -> f a + g a)
    in
    let add_of_encoding (a : _ size) (b : _ size) : _ size =
      match (a, b) with
      | Unknown, _ | _, Unknown -> Unknown
      | Static a, Static b -> Static (a + b)
      | Static 0, other | other, Static 0 -> other
      | Dynamic f, Dynamic g -> Dynamic (fun bstr off -> g bstr (f bstr off))
      | Static n, Dynamic f ->
          Dynamic (fun bstr off -> f bstr Offset.(off +> n))
      | Dynamic f, Static n ->
          Dynamic (fun bstr off -> Offset.(f bstr off +> n))
    in
    fun a b ->
      {
        of_value= add_of_value a.of_value b.of_value
      ; of_encoding= add_of_encoding a.of_encoding b.of_encoding
      }

  let static n = { of_value= Static n; of_encoding= Static n }

  let dynamic ~of_value ~of_encoding =
    { of_value= Dynamic of_value; of_encoding= Dynamic of_encoding }

  let using fn t =
    let of_value = map (fun size_of x -> size_of (fn x)) t.of_value in
    { t with of_value }

  let unknown = { of_value= Unknown; of_encoding= Unknown }
end

type 'a size_of = 'a Sizer.t

let of_scanning : type a. (a -> Offset.t -> Offset.t) -> a -> int -> int =
 fun scan_fn bstr off ->
  let (Offset.Offset off') = scan_fn bstr (Offset.Offset off) in
  off' - off

let of_encoding : 'a size_of -> (Bstr.t -> int -> int) t =
 fun { of_encoding; _ } -> map of_scanning of_encoding

let of_value : type a. a size_of -> (a -> int) t =
 fun { of_value; _ } -> of_value

let sizer_varint =
  let of_value =
    let rec go len n =
      if n >= 0 && n < 128 then len else go (len + 1) (n lsr 7)
    in
    fun n -> go 1 n
  in
  let of_encoding bstr (Offset.Offset off) =
    let pos = ref off in
    while
      let cmd = Bstr.get_uint8 bstr !pos in
      incr pos;
      cmd land 0x80 != 0
    do
      ()
    done;
    Offset.Offset !pos
  in
  Sizer.dynamic ~of_value ~of_encoding

let sizer_cstring =
  let of_value str = String.length str + 1 in
  let of_encoding bstr (Offset.Offset off) =
    let pos = ref off in
    while Bstr.get_uint8 bstr !pos != 0 do
      incr pos
    done;
    Offset.Offset (!pos + 1)
  in
  Sizer.dynamic ~of_value ~of_encoding

let sizer_until byte =
  let of_value str = String.length str in
  let of_encoding bstr (Offset.Offset off) =
    let pos = ref off in
    while Bstr.get bstr !pos != byte do
      incr pos
    done;
    Offset.Offset !pos
  in
  Sizer.dynamic ~of_value ~of_encoding

let rec size_of : type a. a encoding -> a Sizer.t = function
  | Primary p -> prim p
  | Record r -> record r
  | Variant v -> variant v
  | Map m -> map m
  | Seq { llen; lval } -> seq ~llen lval

and seq : type a. llen:int -> a encoding -> a array Sizer.t =
 fun ~llen lval ->
  match size_of lval with
  | { Sizer.of_value= Static len; _ } -> Sizer.static (llen * len)
  | lsize ->
      let of_value =
        let+ len = lsize.Sizer.of_value in
        Array.fold_left (fun acc x -> acc + len x) 0
      in
      let of_encoding =
        let+ len = lsize.Sizer.of_encoding in
        let rec go buf off = function
          | 0 -> off
          | n -> go buf (len buf off) (n - 1)
        in
        fun buf off -> go buf off llen
      in
      { Sizer.of_value; of_encoding }

and prim : type a. a Bin_type.primary -> a Sizer.t = function
  | Char -> Sizer.static 1
  | UInt8 -> Sizer.static 1
  | Int8 -> Sizer.static 1
  | UInt16 _ -> Sizer.static 2
  | Int16 _ -> Sizer.static 2
  | Int32 _ -> Sizer.static 4
  | Int64 _ -> Sizer.static 8
  | Bytes len -> Sizer.static len
  | Bstr len -> Sizer.static len
  | Var_int -> sizer_varint
  | CString -> sizer_cstring
  | Until p -> sizer_until p
  | Const _ -> Sizer.static 0

and record : type a. a Bin_type.record -> a Sizer.t =
 fun r ->
  let open Bin_type in
  fields r
  |> List.map (fun (Field f) -> Sizer.using f.fget (size_of f.ftype))
  |> List.fold_left Sizer.( <+> ) (Sizer.static 0)

and map : type a b. (a, b) Bin_type.map -> b Sizer.t =
 fun { x; g; _ } -> Sizer.using g (size_of x)

and variant : type a. a Bin_type.variant -> a Sizer.t =
 fun v ->
  let static_varint_size n =
    let[@warning "-partial-match"] (Dynamic fn) = sizer_varint.Sizer.of_value in
    fn n
  in
  let case_lengths : (int * a Sizer.t) array =
    let open Bin_type in
    let fn = function
      | C0 { ctag0; _ } -> (static_varint_size ctag0, Sizer.static 0)
      | C1 { ctag1; ctype1; cwitn1= expected; _ } ->
          let tag_length = static_varint_size ctag1 in
          let arg_length =
            match size_of ctype1 with
            | ({ of_value= Static _; _ } | { of_value= Unknown; _ }) as t -> t
            | { of_value= Dynamic of_value; of_encoding } ->
                let of_value a =
                  match v.vget a with
                  | CV0 _ -> assert false
                  | CV1 ({ cwitn1= received; _ }, args) ->
                      let v = Witness.cast_exn received expected args in
                      of_value v
                in
                { of_value= Dynamic of_value; of_encoding }
          in
          (tag_length, arg_length)
    in
    Array.map fn v.vcases
  in
  let non_dynamic_length =
    let rec go static_so_far = function
      | -1 -> Option.map Sizer.static static_so_far
      | i ->
          begin match case_lengths.(i) with
          | _, { of_value= Unknown; _ } -> Some Sizer.unknown
          | _, { of_value= Dynamic _; _ } -> None
          | tag_len, { of_value= Static arg_len; _ } ->
              let len = tag_len + arg_len in
              begin match static_so_far with
              | None -> go (Some len) (i - 1)
              | Some len' when len = len' -> go static_so_far (i - 1)
              | Some _ -> None
              end
          end
    in
    go None (Array.length case_lengths - 1)
  in
  match non_dynamic_length with
  | Some x -> x
  | None ->
      let of_value a =
        let tag =
          match v.vget a with
          | CV0 { ctag0; _ } -> ctag0
          | CV1 ({ ctag1; _ }, _) -> ctag1
        in
        let tag_length, arg_length = case_lengths.(tag) in
        let arg_length =
          match arg_length.of_value with
          | Dynamic fn -> fn a
          | Static n -> n
          | Unknown -> assert false
        in
        tag_length + arg_length
      in
      let of_encoding buf (Offset.Offset off) =
        let off = ref off in
        let tag = Bin_type.bstr_decode_varint buf off in
        match case_lengths.(tag) with
        | _, { of_encoding= Static n; _ } -> Offset.Offset (!off + n)
        | _, { of_encoding= Dynamic fn; _ } -> fn buf (Offset.Offset !off)
        | _, { of_encoding= Unknown; _ } -> assert false
      in
      Sizer.dynamic ~of_value ~of_encoding
