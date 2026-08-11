type 'a encoding = 'a Bin_type.t
type 'a t = Static of int | Dynamic of ('a -> int) | Unknown

let add_size : type a. a t -> a t -> a t =
 fun a b ->
  match (a, b) with
  | Unknown, _ | _, Unknown -> Unknown
  | Static a, Static b -> Static (a + b)
  | Static 0, x | x, Static 0 -> x
  | Static n, Dynamic fn | Dynamic fn, Static n -> Dynamic (fun v -> n + fn v)
  | Dynamic f0, Dynamic f1 -> Dynamic (fun v -> f0 v + f1 v)

let using : type a b. (b -> a) -> a t -> b t =
 fun fn0 -> function
  | Unknown -> Unknown
  | Static n -> Static n
  | Dynamic fn1 -> Dynamic (fun v -> fn1 (fn0 v))

let varint n =
  let rec go len n =
    if n >= 0 && n < 128 then len else go (len + 1) (n lsr 7)
  in
  go 1 n

let rec size_of : type a. a Bin_type.t -> a t = function
  | Primary p -> prim p
  | Record r ->
      let fn acc (Bin_type.Field field) =
        add_size acc (using field.fget (size_of field.ftype))
      in
      List.fold_left fn (Static 0) (Bin_type.record_fields r)
  | Variant v -> variant v
  | Map { x; g; _ } -> using g (size_of x)
  | Seq _ -> assert false

and prim : type a. a Bin_type.primary -> a t = function
  | Char -> Static 1
  | UInt8 -> Static 1
  | Int8 -> Static 1
  | UInt16 _ -> Static 2
  | Int16 _ -> Static 2
  | Int32 _ -> Static 4
  | Int64 _ -> Static 8
  | Bytes len -> Static len
  | Bstr len -> Static len
  | Var_int -> Dynamic varint
  | CString -> assert false
  | Until _ -> assert false
  | Const _ -> Static 0

and variant : type a. a Bin_type.variant -> a t =
 fun v ->
  let tag_size = size_of v.vtag in
  let tag_of tag =
    match tag_size with
    | Static n -> Some n
    | Dynamic fn -> Some (fn tag)
    | Unknown -> None
  in
  let uniform =
    let exception Not_uniform in
    try
      let acc = ref None in
      let fn case =
        let tag, len =
          match case with
          | Bin_type.C0 { ctag0; _ } -> (tag_of ctag0, Some 0)
          | C1 { ctag1; ctype1; _ } ->
              let tag = tag_of ctag1 in
              let len =
                match size_of ctype1 with
                | Static n -> Some n
                | Dynamic _ | Unknown -> None
              in
              (tag, len)
        in
        match (tag, len) with
        | Some x, Some y ->
            let total = x + y in
            begin match !acc with
            | None -> acc := Some total
            | Some n when n = total -> ()
            | Some _ -> raise_notrace Not_uniform
            end
        | _ -> raise_notrace Not_uniform
      in
      Array.iter fn v.vcases; !acc
    with Not_uniform -> None
  in
  match uniform with
  | Some n -> Static n
  | None ->
      let c0 { Bin_type.ctag0; _ } =
        match tag_of ctag0 with Some n -> n | None -> 0
      in
      let c1 : type b. (a, b) Bin_type.case1 -> b -> int =
       fun c ->
        let tag = match tag_of c.ctag1 with Some n -> n | None -> 0 in
        let len = size_of c.ctype1 in
        fun v ->
          let len =
            match len with Static n -> n | Dynamic fn -> fn v | Unknown -> 0
          in
          tag + len
      in
      Dynamic (Bin_type.fold_variant { Bin_type.Case_folder.c0; c1 } v)
