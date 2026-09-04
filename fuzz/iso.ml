open Crowbar

type t = T : 'a spec -> t

and 'a spec = {
    repr: 'a Bin.t
  ; gen: 'a Crowbar.gen
  ; pp: 'a Crowbar.printer
  ; eq: 'a -> 'a -> bool
}

let spec repr gen pp eq = T { repr; gen; pp; eq }
let pp_char ppf chr = pp ppf "%C" chr
let pp_str ppf str = pp ppf "%S" str
let pp_bstr ppf bstr = pp ppf "%S" (Bstr.to_string bstr)
let pp_unit ppf () = pp ppf "()"
let pp_pair pp_a pp_b ppf (a, b) = pp ppf "@[<1>(%a,@ %a)@]" pp_a a pp_b b

let pp_either pp_a pp_b ppf = function
  | Either.Left a -> pp ppf "@[<1>(Left@ %a)@]" pp_a a
  | Either.Right b -> pp ppf "@[<1>(Right@ %a)@]" pp_b b

let pp_array pp_elt ppf arr =
  pp ppf "@[<1>[|%a|]@]" (pp_list pp_elt) (Array.to_list arr)

let string_without chr =
  map [ bytes ] (fun str -> String.concat "" (String.split_on_char chr str))

let string_up_to len =
  map [ bytes ] (fun str -> String.sub str 0 (min len (String.length str)))

let rec repeat n gen =
  if n <= 0 then const [] else map [ gen; repeat (n - 1) gen ] List.cons

let up_to n lst =
  let rec go n acc = function
    | x :: r when n > 0 -> go (n - 1) (x :: acc) r
    | _ -> List.rev acc
  in
  go n [] lst

let int_leaf repr gen = const (spec repr gen pp_int Int.equal)
let int32_leaf repr = const (spec repr int32 pp_int32 Int32.equal)
let int64_leaf repr = const (spec repr int64 pp_int64 Int64.equal)
let string_leaf repr gen = const (spec repr gen pp_str String.equal)

let bstr_leaf repr gen =
  let eq a b = String.equal (Bstr.to_string a) (Bstr.to_string b) in
  const (spec repr (map [ gen ] Bstr.of_string) pp_bstr eq)

let ( let* ) = dynamic_bind

let leaves =
  [
    const (spec Bin.char char pp_char Char.equal); int_leaf Bin.uint8 uint8
  ; int_leaf Bin.int8 int8; int_leaf Bin.beuint16 uint16
  ; int_leaf Bin.leuint16 uint16; int_leaf Bin.neuint16 uint16
  ; int_leaf Bin.beint16 int16; int_leaf Bin.leint16 int16
  ; int_leaf Bin.neint16 int16; int32_leaf Bin.beint32; int32_leaf Bin.leint32
  ; int32_leaf Bin.neint32; int64_leaf Bin.beint64; int64_leaf Bin.leint64
  ; int64_leaf Bin.neint64
  ; int_leaf Bin.varint (map [ int ] (fun n -> n land max_int))
  ; const (spec (Bin.const ()) (const ()) pp_unit Unit.equal)
  ; string_leaf Bin.cstring (string_without '\000')
  ; string_leaf Bin.(bytes (delim 'x')) (string_without 'x')
  ; string_leaf Bin.(bytes (prefix uint8)) (string_up_to 0xff)
  ; string_leaf Bin.(bytes (prefix beuint16)) bytes
  ; string_leaf Bin.(bytes (prefix varint)) bytes
  ; bstr_leaf Bin.(bstr (prefix beuint16)) bytes
  ; bstr_leaf Bin.(bstr (prefix varint)) bytes
  ; (let* len = range 9 in
     string_leaf Bin.(bytes (fixed len)) (bytes_fixed len))
  ; (let* len = range 9 in
     bstr_leaf Bin.(bstr (fixed len)) (bytes_fixed len))
  ]

let bits_leaf base width order =
  let repr =
    let open Bin in
    bits ~order base (fun a b c -> (a, b, c))
    |* flag (fun (a, _, _) -> a)
    |* bit 3 (fun (_, b, _) -> b)
    |* bit (width - 4) (fun (_, _, c) -> c)
    |> sealb
  in
  let gen =
    map [ bool; range 8; range (1 lsl (width - 4)) ] @@ fun a b c -> (a, b, c)
  in
  let pp ppf (a, b, c) = pp ppf "@[<1>(%b,@ %d,@ %d)@]" a b c in
  const (spec repr gen pp ( = ))

let bits_leaves =
  let bases =
    Bin.
      [
        (B8, 8); (B16 Big_endian, 16); (B16 Little_endian, 16)
      ; (B32 Big_endian, 32); (B32 Little_endian, 32)
      ]
  in
  let fn (base, width) =
    [ bits_leaf base width Bin.Msb_first; bits_leaf base width Bin.Lsb_first ]
  in
  List.concat_map fn bases

let record2 : t -> t -> t =
 fun (T a) (T b) ->
  let repr =
    let open Bin in
    record (fun x y -> (x, y)) |+ field a.repr fst |+ field b.repr snd |> sealr
  in
  let eq (x0, y0) (x1, y1) = a.eq x0 x1 && b.eq y0 y1 in
  spec repr (pair a.gen b.gen) (pp_pair a.pp b.pp) eq

let either : t -> t -> t =
 fun (T a) (T b) ->
  let repr =
    let open Bin in
    variant (fun left right -> function
      | Either.Left x -> left x
      | Either.Right y -> right y)
    |~ case1 a.repr Either.left
    |~ case1 b.repr Either.right
    |> sealv
  in
  let gen = choose [ map [ a.gen ] Either.left; map [ b.gen ] Either.right ] in
  let eq x y =
    match (x, y) with
    | Either.Left x, Either.Left y -> a.eq x y
    | Either.Right x, Either.Right y -> b.eq x y
    | _ -> false
  in
  spec repr gen (pp_either a.pp b.pp) eq

let optional : t -> t =
 fun (T a) ->
  let repr =
    let open Bin in
    variant (fun none some -> function None -> none | Some x -> some x)
    |~ case0 None
    |~ case1 a.repr Option.some
    |> sealv
  in
  let gen = choose [ const None; map [ a.gen ] Option.some ] in
  let eq x y =
    match (x, y) with
    | None, None -> true
    | Some x, Some y -> a.eq x y
    | _ -> false
  in
  spec repr gen (pp_option a.pp) eq

let mapped : t -> t =
 fun (T a) ->
  let repr = Bin.map a.repr Option.some Option.get in
  let gen = map [ a.gen ] Option.some in
  let eq x y = a.eq (Option.get x) (Option.get y) in
  spec repr gen (pp_option a.pp) eq

let list_of : t -> t =
 fun (T a) ->
  let repr = Bin.(list (prefix varint) a.repr) in
  spec repr (list a.gen) (pp_list a.pp) (List.equal a.eq)

let array_of : t -> t =
 fun (T a) ->
  let repr = Bin.(seq (prefix uint8) a.repr) in
  let gen = map [ list a.gen ] (fun lst -> Array.of_list (up_to 0xff lst)) in
  let eq x y = List.equal a.eq (Array.to_list x) (Array.to_list y) in
  spec repr gen (pp_array a.pp) eq

let fixed_list : int -> t -> t =
 fun len (T a) ->
  let repr = Bin.(list (fixed len) a.repr) in
  spec repr (repeat len a.gen) (pp_list a.pp) (List.equal a.eq)

let bound_list : t -> t =
 fun (T a) ->
  let repr =
    let open Bin in
    let prj len = list (fixed len) a.repr in
    let inj lst = List.length lst in
    bind uint8 prj inj
  in
  let gen = map [ list a.gen ] (up_to 0xff) in
  spec repr gen (pp_list a.pp) (List.equal a.eq)

let rec_list : t -> t =
 fun (T a) ->
  let repr =
    let open Bin in
    fix @@ fun self ->
    let prj = function
      | 0 -> const []
      | _ ->
          record (fun x rest -> x :: rest)
          |+ field a.repr List.hd
          |+ field self List.tl
          |> sealr
    in
    let inj = function [] -> 0 | _ -> 1 in
    bind uint8 prj inj
  in
  spec repr (list a.gen) (pp_list a.pp) (List.equal a.eq)

let max_product_depth = 4

let repr : t Crowbar.gen =
  let rec go n =
    let products =
      if n <= 0 then []
      else
        let repr = go (n - 1) in
        [
          map [ repr; repr ] record2
        ; (let* len = range 5 in
           map [ repr ] (fixed_list len))
        ]
    in
    fix @@ fun repr ->
    choose
      (leaves @ bits_leaves @ products
      @ [
          map [ repr; repr ] either; map [ repr ] optional; map [ repr ] mapped
        ; map [ repr ] list_of; map [ repr ] array_of; map [ repr ] bound_list
        ; map [ repr ] rec_list
        ])
  in
  go max_product_depth

type v = V : 'a spec * 'a -> v

let value : v Crowbar.gen =
  let* (T spec) = repr in
  map [ spec.gen ] (fun v -> V (spec, v))

let () =
  add_test ~name:"bin/iso" [ value ] @@ fun (V (spec, value)) ->
  let to_string = Bin.Staged.unstage (Bin.to_string spec.repr) in
  let encode_bstr = Bin.Staged.unstage (Bin.encode_bstr spec.repr) in
  let decode = Bin.Staged.unstage (Bin.decode spec.repr) in
  let decode_bstr = Bin.Staged.unstage (Bin.decode_bstr spec.repr) in
  let str = to_string value in
  let bstr = Bstr.create (Bin.size_of_value spec.repr value) in
  encode_bstr value bstr (ref Bin.Off.zero);
  check_eq ~pp:pp_str ~eq:String.equal str (Bstr.to_string bstr);
  let pos = ref Bin.Off.zero in
  let value' = decode str pos in
  check_eq ~pp:spec.pp ~eq:spec.eq value value';
  check_eq ~pp:pp_int ~eq:Int.equal (String.length str) (!pos :> int);
  let pos = ref Bin.Off.zero in
  let value' = decode_bstr bstr pos in
  check_eq ~pp:spec.pp ~eq:spec.eq value value';
  check_eq ~pp:pp_int ~eq:Int.equal (Bstr.length bstr) (!pos :> int)
