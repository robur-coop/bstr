open Test
module S = Slice_bstr

let test01 =
  let descr = {text|String.Sub misc. base functions|text} in
  Test.test ~title:"misc" ~descr @@ fun () ->
  let eq sbstr str = String.equal (S.to_string sbstr) str |> check in
  let err v =
    match Lazy.force v with
    | exception Invalid_argument _ -> check true
    | _ -> check false
  in
  eq S.empty "";
  eq (S.of_string "abc") "abc";
  eq (S.string "abc" ~off:0 ~len:1) "a";
  eq (S.string "abc" ~off:1 ~len:1) "b";
  eq (S.string "abc" ~off:1 ~len:2) "bc";
  eq (S.string "abc" ~off:2 ~len:1) "c";
  eq (S.string "abc" ~off:2 ~len:0) "";
  let v = S.string "abc" ~off:2 ~len:1 in
  lazy (S.get v 3) |> err;
  lazy (S.get v 2) |> err;
  lazy (S.get v 1) |> err;
  check (S.get v 0 == 'c')

let test02 =
  let descr = {text|overlap|text} in
  Test.test ~title:"overlap" ~descr @@ fun () ->
  let test value expected =
    match (value, expected) with
    | None, None -> check true
    | Some (len, a, b), Some (len', x, y) ->
        Format.eprintf "len:%d, a:%d, b:%d\n%!" len a b;
        check (a == x && b == y && len == len')
    | _ -> check false
  in
  let t = S.string (String.make 10 '\000') in
  let ab = S.sub t ~off:5 ~len:5 in
  let cd = S.sub t ~off:0 ~len:5 in
  test (S.overlap ab cd) None;
  let ab = S.sub t ~off:0 ~len:5 in
  let cd = S.sub t ~off:5 ~len:5 in
  test (S.overlap ab cd) None;
  let ab = S.sub t ~off:0 ~len:6 in
  let cd = S.sub t ~off:5 ~len:5 in
  test (S.overlap ab cd) (Some (1, 5, 0));
  let ab = S.sub t ~off:5 ~len:5 in
  let cd = S.sub t ~off:0 ~len:6 in
  test (S.overlap ab cd) (Some (1, 0, 5));
  let ab = S.sub t ~off:0 ~len:8 in
  let cd = S.sub t ~off:2 ~len:8 in
  test (S.overlap ab cd) (Some (6, 2, 0));
  let ab = S.sub t ~off:0 ~len:10 in
  let cd = S.sub t ~off:2 ~len:8 in
  test (S.overlap ab cd) (Some (8, 2, 0));
  let ab = S.sub t ~off:0 ~len:10 in
  let cd = S.sub t ~off:2 ~len:6 in
  test (S.overlap ab cd) (Some (6, 2, 0));
  let ab = S.sub t ~off:0 ~len:8 in
  let cd = S.sub t ~off:0 ~len:10 in
  test (S.overlap ab cd) (Some (8, 0, 0));
  let ab = S.sub t ~off:2 ~len:6 in
  let cd = S.sub t ~off:0 ~len:10 in
  test (S.overlap ab cd) (Some (6, 0, 2));
  let ab = S.sub t ~off:2 ~len:8 in
  let cd = S.sub t ~off:0 ~len:10 in
  test (S.overlap ab cd) (Some (8, 0, 2));
  let ab = S.sub t ~off:2 ~len:8 in
  let cd = S.sub t ~off:0 ~len:8 in
  test (S.overlap ab cd) (Some (6, 0, 2))

let ( / ) = Filename.concat

let () =
  let tests = [ test01; test02 ] in
  let ({ Test.directory } as runner) = Test.runner (Sys.getcwd () / "_tests") in
  let run idx test =
    Format.printf "test%03d: %!" (succ idx);
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
