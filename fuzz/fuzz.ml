open Crowbar

let len = 1 + 1 + 2 + 2 + 2 + 2 + 4 + 4 + 8 + 8

[@@@ocamlformat "disable"]
let fields =
  let open Bin in
  fun fn ->
    fn
    |+ field int8     (fun (v, _, _, _, _, _, _, _, _, _) -> v)
    |+ field uint8    (fun (_, v, _, _, _, _, _, _, _, _) -> v)
    |+ field beint16  (fun (_, _, v, _, _, _, _, _, _, _) -> v)
    |+ field leint16  (fun (_, _, _, v, _, _, _, _, _, _) -> v)
    |+ field beuint16 (fun (_, _, _, _, v, _, _, _, _, _) -> v)
    |+ field leuint16 (fun (_, _, _, _, _, v, _, _, _, _) -> v)
    |+ field beint32  (fun (_, _, _, _, _, _, v, _, _, _) -> v)
    |+ field leint32  (fun (_, _, _, _, _, _, _, v, _, _) -> v)
    |+ field beint64  (fun (_, _, _, _, _, _, _, _, v, _) -> v)
    |+ field leint64  (fun (_, _, _, _, _, _, _, _, _, v) -> v)
[@@@ocamlformat "enable"]

let () =
  (* NOTE(dinosaure): here, we assume that the generic path is our oracle and
     we test our fusion against it: we decode via our static plan and with our
     generic decoder (due to [cstring] and should have exactly the same result
     (bit sign, endian). *)
  let fn0 a b c d e f g h i j = (a, b, c, d, e, f, g, h, i, j) in
  let fn1 a b c d e f g h i j _ = (a, b, c, d, e, f, g, h, i, j) in
  let static = fields (Bin.record fn0) |> Bin.sealr in
  let generic =
    Bin.(fields (record fn1) |+ field cstring (Fun.const "") |> sealr)
  in
  let dec0 = Bin.Staged.unstage (Bin.decode static) in
  let dec1 = Bin.Staged.unstage (Bin.decode generic) in
  add_test ~name:"static/generic" [ bytes_fixed len ] @@ fun str ->
  let a = dec0 str (ref Bin.Off.zero) in
  let b = dec1 (str ^ "\000") (ref Bin.Off.zero) in
  check_eq a b
