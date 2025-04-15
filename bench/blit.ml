open Bechamel
open Toolkit

let src = String.make 256 '\x10'
let cs = Cstruct.create 512
let bstr = Bstr.create 512
let bigstringaf = Bigstringaf.create 512
let cstruct_blit () = Cstruct.blit_from_string src 0 cs 0 256
let bstr_blit () = Bstr.blit_from_string src ~src_off:0 bstr ~dst_off:0 ~len:256

let bigstringaf_blit () =
  Bigstringaf.blit_from_string src ~src_off:0 bigstringaf ~dst_off:0 ~len:256

let cstruct_blit = Staged.stage cstruct_blit
let bstr_blit = Staged.stage bstr_blit
let bigstringaf_blit = Staged.stage bigstringaf_blit
let test0 = Test.make ~name:"Cstruct" cstruct_blit
let test1 = Test.make ~name:"Bstr" bstr_blit
let test2 = Test.make ~name:"Bigstringaf" bigstringaf_blit

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
    Test.make_grouped ~name:"blit" ~fmt:"%s %s" [ test0; test1; test2 ]
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
