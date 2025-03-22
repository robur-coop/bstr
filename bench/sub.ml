open Bechamel
open Toolkit

let cs = Cstruct.create 32
let bstr = Bstr.create 32
let slice : Slice_bstr.t = Slice_bstr.make bstr
let cstruct_sub () = Cstruct.sub cs 8 8
let bstr_sub () = Bstr.sub bstr ~off:8 ~len:8
let bigstringaf_sub () = Bigstringaf.sub bstr ~off:8 ~len:8
let slice_sub () = Slice.sub slice ~off:8 ~len:8
let cstruct_sub = Staged.stage cstruct_sub
let bstr_sub = Staged.stage bstr_sub
let bigstringaf_sub = Staged.stage bigstringaf_sub
let slice_sub = Staged.stage slice_sub
let test0 = Test.make ~name:"Cstruct" cstruct_sub
let test1 = Test.make ~name:"Bstr" bstr_sub
let test2 = Test.make ~name:"Bigstringaf" bigstringaf_sub
let test3 = Test.make ~name:"Slice" slice_sub

let benchmark () =
  let bootstrap = 0 and r_square = true and predictors = Measure.[| run |] in
  let ols = Analyze.ols ~bootstrap ~r_square ~predictors in
  let instances = Instance.[ monotonic_clock ] in
  let limit = 2000
  and stabilize = true
  and quota = Time.second 1.0
  and kde = Some 1000 in
  let cfg = Benchmark.cfg ~limit ~stabilize ~quota ~kde () in
  let tests =
    Test.make_grouped ~name:"sub" ~fmt:"%s %s" [ test0; test1; test2; test3 ]
  in
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
