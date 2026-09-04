open Crowbar

[@@@ocamlformat "disable"]
let fields =
  let open Bin in
  fun fn ->
    fn
    |+ field uint8     (fun (v, _, _, _, _, _, _, _, _, _) -> v)
    |+ field uint8     (fun (_, v, _, _, _, _, _, _, _, _) -> v)
    |+ field beuint16  (fun (_, _, v, _, _, _, _, _, _, _) -> v)
    |+ field beuint16  (fun (_, _, _, v, _, _, _, _, _, _) -> v)
    |+ field beuint16  (fun (_, _, _, _, v, _, _, _, _, _) -> v)
    |+ field uint8     (fun (_, _, _, _, _, v, _, _, _, _) -> v)
    |+ field uint8     (fun (_, _, _, _, _, _, v, _, _, _) -> v)
    |+ field beuint16  (fun (_, _, _, _, _, _, _, v, _, _) -> v)
    |+ field beint32   (fun (_, _, _, _, _, _, _, _, v, _) -> v)
    |+ field beint32   (fun (_, _, _, _, _, _, _, _, _, v) -> v)
[@@@ocamlformat "enable"]

let check name static generic len =
  let dec0 = Bin.Staged.unstage (Bin.decode static) in
  let dec1 = Bin.Staged.unstage (Bin.decode generic) in
  add_test ~name [ bytes_fixed len ] @@ fun str ->
  let a = dec0 str (ref Bin.Off.zero) in
  let b = dec1 (str ^ "\000") (ref Bin.Off.zero) in
  check_eq a b

let () =
  let fn0 a b c d e f g h i j = (a, b, c, d, e, f, g, h, i, j) in
  let fn1 a b c d e f g h i j _ = (a, b, c, d, e, f, g, h, i, j) in
  let static = fields (Bin.record fn0) |> Bin.sealr in
  let generic =
    let open Bin in
    fields (record fn1) |+ field cstring (Fun.const "") |> sealr
  in
  check "sint/ipv4" static generic 20

let () =
  let fields =
    let open Bin in
    fun fn ->
      fn
      |+ field beuint16 (fun (v, _, _, _, _, _) -> v)
      |+ field beuint16 (fun (_, v, _, _, _, _) -> v)
      |+ field beuint16 (fun (_, _, v, _, _, _) -> v)
      |+ field beuint16 (fun (_, _, _, v, _, _) -> v)
      |+ field beuint16 (fun (_, _, _, _, v, _) -> v)
      |+ field beuint16 (fun (_, _, _, _, _, v) -> v)
  in
  let fn0 a b c d e f = (a, b, c, d, e, f) in
  let fn1 a b c d e f _ = (a, b, c, d, e, f) in
  let static = fields (Bin.record fn0) |> Bin.sealr in
  let generic =
    let open Bin in
    fields (record fn1) |+ field cstring (Fun.const "") |> sealr
  in
  check "sint/window" static generic 12

[@@@ocamlformat "disable"]
let fields =
  let open Bin in
  fun fn ->
    fn
    |+ field beuint16  (fun (v, _, _, _, _, _) -> v)
    |+ field beuint16  (fun (_, v, _, _, _, _) -> v)
    |+ field beuint16  (fun (_, _, v, _, _, _) -> v)
    |+ field uint8     (fun (_, _, _, v, _, _) -> v)
    |+ field uint8     (fun (_, _, _, _, v, _) -> v)
    |+ field uint8     (fun (_, _, _, _, _, v) -> v)
[@@@ocamlformat "enable"]

let () =
  let fn0 a b c d e f = (a, b, c, d, e, f) in
  let fn1 a b c d e f _ = (a, b, c, d, e, f) in
  let static = fields (Bin.record fn0) |> Bin.sealr in
  let generic =
    let open Bin in
    fields (record fn1) |+ field cstring (Fun.const "") |> sealr
  in
  check "sint/tail" static generic 9

[@@@ocamlformat "disable"]
let fields =
  let open Bin in
  fun fn ->
    fn
    |+ field uint8     (fun (v, _, _, _, _, _) -> v)
    |+ field beuint16  (fun (_, v, _, _, _, _) -> v)
    |+ field leuint16  (fun (_, _, v, _, _, _) -> v)
    |+ field uint8     (fun (_, _, _, v, _, _) -> v)
    |+ field beuint16  (fun (_, _, _, _, v, _) -> v)
    |+ field beuint16  (fun (_, _, _, _, _, v) -> v)
[@@@ocamlformat "enable"]

let () =
  let fn0 a b c d e f = (a, b, c, d, e, f) in
  let fn1 a b c d e f _ = (a, b, c, d, e, f) in
  let static = fields (Bin.record fn0) |> Bin.sealr in
  let generic =
    let open Bin in
    fields (record fn1) |+ field cstring (Fun.const "") |> sealr
  in
  check "sint/cut" static generic 10

[@@@ocamlformat "disable"]
let fields =
  let open Bin in
  fun fn ->
    fn
    |+ field uint8     (fun (v, _, _) -> v)
    |+ field beuint16  (fun (_, v, _) -> v)
    |+ field beuint16  (fun (_, _, v) -> v)
[@@@ocamlformat "enable"]

let () =
  let fn0 a b c = (a, b, c) in
  let fn1 a b c _ = (a, b, c) in
  let static = fields (Bin.record fn0) |> Bin.sealr in
  let generic =
    let open Bin in
    fields (record fn1) |+ field cstring (Fun.const "") |> sealr
  in
  check "sint/short" static generic 5
