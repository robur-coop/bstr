let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let random length =
  let get _ =
    match Random.int (10 + 26 + 26) with
    | n when n < 10 -> Char.(chr (code '0' + n))
    | n when n < 10 + 26 -> Char.(chr (code 'a' + n - 10))
    | n -> Char.(chr (code 'A' + n - 10 - 26))
  in
  String.init length get

let str = random 4096
let chr_into_str = str.[Random.int 4096]

open Bechamel
open Toolkit

let bstr_contains =
  let bstr = Bstr.of_string str in
  Test.make ~name:"bstr"
  @@ Staged.stage
  @@ fun () -> ignore (Bstr.contains bstr chr_into_str)

let bigstringaf_contains =
  let bstr = Bigstringaf.of_string str ~off:0 ~len:4096 in
  Test.make ~name:"bigstringaf"
  @@ Staged.stage
  @@ fun () -> ignore (Bigstringaf.memchr bstr 0 chr_into_str 4096)

let cstruct_contains =
  let cs = Cstruct.of_string str in
  let fn chr = chr == chr_into_str in
  Test.make ~name:"cstruct"
  @@ Staged.stage
  @@ fun () -> ignore (Cstruct.exists fn cs)

let tests =
  Test.make_grouped ~name:"contains" ~fmt:"%s %s"
    [ bstr_contains; bigstringaf_contains; cstruct_contains ]

let benchmark () =
  let bootstrap = 0 and r_square = true and predictors = Measure.[| run |] in
  let ols = Analyze.ols ~bootstrap ~r_square ~predictors in
  let instances = Instance.[ monotonic_clock ] in
  let limit = 2000
  and stabilize = true
  and quota = Time.second 1.0
  and kde = Some 1000 in
  let cfg = Benchmark.cfg ~limit ~stabilize ~quota ~kde () in
  let raw = Benchmark.all cfg instances tests in
  let res = List.map (fun i -> Analyze.all ols i raw) instances in
  let res = Analyze.merge ols instances res in
  (res, raw)

let nothing _ = Ok ()
let compare = String.compare

let () =
  let res = benchmark () in
  let res =
    let open Bechamel_js in
    let dst = Channel stdout
    and x_label = Measure.run
    and y_label = Measure.label Instance.monotonic_clock in
    emit ~dst nothing ~compare ~x_label ~y_label res
  in
  match res with Ok () -> () | Error (`Msg msg) -> failwith msg
