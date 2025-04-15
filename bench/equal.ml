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

let hash_eq_0 = random 4096
let hash_eq_1 = Bytes.to_string (Bytes.of_string hash_eq_0)
let chr_into_hash_eq_0 = hash_eq_0.[Random.int 4096]
let hash_neq_0 = random 4096

let hash_neq_1 =
  let rec go limit =
    if limit <= 0 then failwith "Impossible to generate different hashes.";
    let res = random 4096 in
    if res = hash_neq_0 then go (pred limit) else res
  in
  go 10

let random_chr =
  let rec go limit =
    if limit <= 0 then
      failwith
        "Impossible to generate a byte which does not appear into hash_neq_0.";
    let res = Char.chr (Random.int 256) in
    if not (String.contains hash_neq_0 res) then res else go (pred limit)
  in
  go 10

open Bechamel
open Toolkit

let bstr_equal =
  let hash_eq_0 = Bstr.of_string hash_eq_0 in
  let hash_eq_1 = Bstr.of_string hash_eq_1 in
  Test.make ~name:"bstr"
  @@ Staged.stage
  @@ fun () -> Bstr.equal hash_eq_0 hash_eq_1

let bigstringaf_equal =
  let hash_eq_0 = Bigstringaf.of_string hash_eq_0 ~off:0 ~len:4096 in
  let hash_eq_1 = Bigstringaf.of_string hash_eq_1 ~off:0 ~len:4096 in
  Test.make ~name:"bigstringaf"
  @@ Staged.stage
  @@ fun () -> Bigstringaf.memcmp hash_eq_0 0 hash_eq_1 0 4096

let cstruct_equal =
  let hash_eq_0 = Cstruct.of_string hash_eq_0 in
  let hash_eq_1 = Cstruct.of_string hash_eq_1 in
  Test.make ~name:"cstruct"
  @@ Staged.stage
  @@ fun () -> Cstruct.equal hash_eq_0 hash_eq_1

let tests =
  Test.make_grouped ~name:"equal" ~fmt:"%s %s"
    [ bstr_equal; bigstringaf_equal; cstruct_equal ]

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
