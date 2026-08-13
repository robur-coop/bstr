type 'a s = Static of int | Dynamic of ('a -> int) | Unknown

let size_to_option = function Static n -> Some n | _ -> None

let add_size : type a. a s -> a s -> a s =
 fun a b ->
  match (a, b) with
  | Unknown, _ | _, Unknown -> Unknown
  | Static a, Static b -> Static (a + b)
  | Static 0, x | x, Static 0 -> x
  | Static n, Dynamic fn | Dynamic fn, Static n -> Dynamic (fun v -> n + fn v)
  | Dynamic f0, Dynamic f1 -> Dynamic (fun v -> f0 v + f1 v)

let using : type a b. (b -> a) -> a s -> b s =
 fun fn0 -> function
  | Unknown -> Unknown
  | Static n -> Static n
  | Dynamic fn1 -> Dynamic (fun v -> fn1 (fn0 v))

module Len = Bin_type.Len

type 'a t = { layout: Len.t option; of_value: 'a s }

let static n = { layout= Some (Len.v n); of_value= Static n }
let dynamic fn = { layout= None; of_value= Dynamic fn }
let unknown = { layout= None; of_value= Unknown }

let ( <+> ) : type a. a t -> a t -> a t =
 fun a b ->
  let layout =
    match (a.layout, b.layout) with
    | Some a, Some b -> Some Len.(a + b)
    | _ -> None
  in
  let of_value = add_size a.of_value b.of_value in
  { layout; of_value }

let using : type a b. (b -> a) -> a t -> b t =
 fun fn t -> { t with of_value= using fn t.of_value }

let varint n =
  let rec go len n =
    if n >= 0 && n < 128 then len else go (len + 1) (n lsr 7)
  in
  go 1 n

let rec make : type a. a Bin_type.t -> a t = function
  | Primary p -> prim p
  | Record r ->
      let fn acc (Bin_type.Field field) =
        acc <+> using field.fget (make field.ftype)
      in
      List.fold_left fn (static 0) (Bin_type.record_fields r)
  | Variant v -> variant v
  | Map { x; g; _ } -> using g (make x)
  | Seq s -> seq s

and seq : type a b. (a, b) Bin_type.seq -> b t =
 fun { slen; selt; skind } ->
  let count : b -> int =
    match skind with Sarray -> Array.length | Slist -> List.length
  and iter : (a -> unit) -> b -> unit =
    match skind with Sarray -> Array.iter | Slist -> List.iter
  in
  let elt = make selt in
  let len : b s =
    match (slen, (make selt).of_value) with
    | _, Unknown -> Unknown
    | Fixed k, Static n -> Static (k * n)
    | _, Static n -> Dynamic (fun v -> n * count v)
    | _, Dynamic fn ->
        let fn v =
          let acc = ref 0 in
          iter (fun x -> acc := !acc + fn x) v;
          !acc
        in
        Dynamic fn
  in
  let of_value =
    match slen with
    | Fixed _ | Rest | Delim _ -> len
    | Prefix t ->
        begin match (make t).of_value with
        | Static n -> add_size (Static n) len
        | Dynamic fn -> add_size (Dynamic (fun v -> fn (count v))) len
        | Unknown -> Unknown
        end
  in
  let layout =
    match (slen, elt.layout) with
    | Fixed n, Some w -> Some Len.(n * w)
    | _ -> None
  in
  { layout; of_value }

and prim : type a. a Bin_type.primary -> a t = function
  | Char -> static 1
  | UInt8 -> static 1
  | Int8 -> static 1
  | UInt16 _ -> static 2
  | Int16 _ -> static 2
  | Int32 _ -> static 4
  | Int64 _ -> static 8
  | Bytes len -> payload len String.length
  | Bstr len -> payload len Bstr.length
  | Var_int -> dynamic varint
  | Until _ -> dynamic String.length
  | Const _ -> static 0

and payload : type a. Bin_type.len -> (a -> int) -> a t =
 fun l length ->
  match l with
  | Fixed n -> static n
  | Rest -> dynamic length
  | Delim _ -> dynamic (fun v -> length v + 1)
  | Prefix t ->
      begin match (make t).of_value with
      | Static n -> dynamic (fun v -> n + length v)
      | Dynamic fn ->
          let fn v =
            let n = length v in
            fn n + n
          in
          dynamic fn
      | Unknown -> unknown
      end

and variant : type a. a Bin_type.variant -> a t =
 fun v ->
  let tag = make v.vtag in
  let tag_written v =
    match tag.of_value with
    | Static n -> Some n
    | Dynamic fn -> Some (fn v)
    | Unknown -> None
  in
  let cases =
    let fn = function
      | Bin_type.C0 { ctag0; _ } ->
          let len = tag_written ctag0 in
          (len, tag.layout, Some 0, Some Len.zero)
      | C1 { ctag1; ctype1; _ } ->
          let p = make ctype1 in
          let len0 = tag_written ctag1 in
          let len1 = size_to_option p.of_value in
          (len0, tag.layout, len1, p.layout)
    in
    Array.map fn v.vcases
  in
  let uniform pick =
    let exception Not_uniform in
    try
      let acc = ref None in
      let fn case =
        match pick case with
        | None -> raise_notrace Not_uniform
        | Some total ->
            begin match !acc with
            | None -> acc := Some total
            | Some n when n = total -> ()
            | Some _ -> raise_notrace Not_uniform
            end
      in
      Array.iter fn cases; !acc
    with Not_uniform -> None
  in
  let fn (_, tl, _, pl) =
    match (tl, pl) with
    | Some t, Some p -> Some Len.(t + p)
    | Some _, None | None, Some _ | None, None -> None
  in
  let layout = uniform fn in
  let of_value =
    let fn (tw, _, pw, _) =
      match (tw, pw) with
      | Some t, Some p -> Some (t + p)
      | Some _, None | None, Some _ | None, None -> None
    in
    match uniform fn with
    | Some n -> Static n
    | None ->
        let c0 { Bin_type.ctag0; _ } =
          match tag_written ctag0 with Some n -> n | None -> 0
        in
        let c1 : type b. (a, b) Bin_type.case1 -> b -> int =
         fun c ->
          let tag = match tag_written c.ctag1 with Some n -> n | None -> 0 in
          let len = (make c.ctype1).of_value in
          fun v ->
            let len =
              match len with Static n -> n | Dynamic fn -> fn v | Unknown -> 0
            in
            tag + len
        in
        Dynamic (Bin_type.fold_variant { Bin_type.Case_folder.c0; c1 } v)
  in
  { layout; of_value }
