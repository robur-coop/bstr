open Test

let test01 =
  let descr = {text|"empty bstr"|text} in
  Test.test ~title:"empty bstr" ~descr @@ fun () ->
  let x = Bstr.create 0 in
  check (0 = Bstr.length x);
  let y = Bstr.to_string x in
  check ("" = y)

let test02 =
  let descr = {text|negative length|text} in
  Test.test ~title:"negative bstr" ~descr @@ fun () ->
  try
    let _ = Bstr.create (-1) in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test03 =
  let descr = {text|positive shift|text} in
  Test.test ~title:"positive shift" ~descr @@ fun () ->
  let x = Bstr.create 1 in
  let y = Bstr.shift x 1 in
  check (0 = Bstr.length y)

let test04 =
  let descr = {text|negative shift|text} in
  Test.test ~title:"negative shift" ~descr @@ fun () ->
  let x = Bstr.create 2 in
  let y = Bstr.sub x ~off:1 ~len:1 in
  begin
    try
      let _ = Bstr.shift x (-1) in
      check false
    with
    | Invalid_argument _ -> check true
    | _exn -> check false
  end;
  begin
    try
      let _ = Bstr.shift y (-1) in
      check false
    with
    | Invalid_argument _ -> check true
    | _exn -> check false
  end

let test05 =
  let descr = {text|bad positive shift|text} in
  Test.test ~title:"bad positive shift" ~descr @@ fun () ->
  let x = Bstr.create 10 in
  try
    let _ = Bstr.shift x 11 in
    check false
  with Invalid_argument _ -> check true

let test06 =
  let descr = {text|sub|text} in
  Test.test ~title:"sub" ~descr @@ fun () ->
  let x = Bstr.create 100 in
  let y = Bstr.sub x ~off:10 ~len:80 in
  begin
    match Bstr.overlap x y with
    | Some (len, x_off, _) ->
        check (len = 80);
        check (x_off = 10)
    | None -> check false
  end;
  let z = Bstr.sub y ~off:20 ~len:60 in
  begin
    match Bstr.overlap x z with
    | Some (len, x_off, _) ->
        check (len = 60);
        check (x_off = 30)
    | None -> check false
  end

let test07 =
  let descr = {text|negative sub|text} in
  Test.test ~title:"negative sub" ~descr @@ fun () ->
  let x = Bstr.create 2 in
  let y = Bstr.sub ~off:1 ~len:1 x in
  begin
    try
      let _ = Bstr.sub x ~off:(-1) ~len:0 in
      check false
    with
    | Invalid_argument _ -> check true
    | _exn -> check false
  end;
  begin
    try
      let _ = Bstr.sub y ~off:(-1) ~len:0 in
      check false
    with
    | Invalid_argument _ -> check true
    | _exn -> check false
  end

let test08 =
  let descr = {text|sub len too big|text} in
  Test.test ~title:"sub len too big" ~descr @@ fun () ->
  let x = Bstr.create 0 in
  try
    let _ = Bstr.sub x ~off:0 ~len:1 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test09 =
  let descr = {text|sub len too small|text} in
  Test.test ~title:"sub len too small" ~descr @@ fun () ->
  let x = Bstr.create 0 in
  try
    let _ = Bstr.sub x ~off:0 ~len:(-1) in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test10 =
  let descr = {text|sub offset too big|text} in
  Test.test ~title:"sub offset too big" ~descr @@ fun () ->
  let x = Bstr.create 10 in
  begin
    try
      let _ = Bstr.sub x ~off:11 ~len:0 in
      check false
    with
    | Invalid_argument _ -> check true
    | _exn -> check false
  end;
  let y = Bstr.sub x ~off:1 ~len:9 in
  begin
    try
      let _ = Bstr.sub y ~off:10 ~len:0 in
      check false
    with
    | Invalid_argument _ -> check true
    | _exn -> check false
  end

let test11 =
  let descr = {text|blit offset too big|text} in
  Test.test ~title:"blit offset too big" ~descr @@ fun () ->
  let x = Bstr.create 1 in
  let y = Bstr.create 1 in
  try
    Bstr.blit x ~src_off:2 y ~dst_off:1 ~len:1;
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test12 =
  let descr = {text|blit offset too small|text} in
  Test.test ~title:"blit offset too small" ~descr @@ fun () ->
  let x = Bstr.create 1 in
  let y = Bstr.create 1 in
  try
    Bstr.blit x ~src_off:(-1) y ~dst_off:1 ~len:1;
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test13 =
  let descr = {text|blit dst offset too big|text} in
  Test.test ~title:"blit dst offset too big" ~descr @@ fun () ->
  let x = Bstr.create 1 in
  let y = Bstr.create 1 in
  try
    Bstr.blit x ~src_off:1 y ~dst_off:2 ~len:1;
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test14 =
  let descr = {text|blit dst offset too small|text} in
  Test.test ~title:"blit dst offset too small" ~descr @@ fun () ->
  let x = Bstr.create 1 in
  let y = Bstr.create 1 in
  try
    Bstr.blit x ~src_off:1 y ~dst_off:(-1) ~len:1;
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test15 =
  let descr = {text|blit dst offset negative|text} in
  Test.test ~title:"blit dst offset negative" ~descr @@ fun () ->
  let x = Bstr.create 1 in
  let y = Bstr.create 1 in
  try
    Bstr.blit x ~src_off:0 y ~dst_off:(-1) ~len:1;
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test16 =
  let descr = {text|blit len too big|text} in
  Test.test ~title:"blit len too big" ~descr @@ fun () ->
  let x = Bstr.create 1 in
  let y = Bstr.create 2 in
  try
    Bstr.blit x ~src_off:0 y ~dst_off:0 ~len:2;
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test17 =
  let descr = {text|blit len too big (2)|text} in
  Test.test ~title:"blit len too big (2)" ~descr @@ fun () ->
  let x = Bstr.create 2 in
  let y = Bstr.create 1 in
  try
    Bstr.blit x ~src_off:0 y ~dst_off:0 ~len:2;
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test18 =
  let descr = {text|blit len too small|text} in
  Test.test ~title:"blit len too small" ~descr @@ fun () ->
  let x = Bstr.create 1 in
  let y = Bstr.create 1 in
  try
    Bstr.blit x ~src_off:0 y ~dst_off:0 ~len:(-1);
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let ( / ) = Filename.concat

let () =
  let tests =
    [
      test01; test02; test03; test04; test05; test06; test07; test08; test09
    ; test10; test11; test12; test13; test14; test15; test16; test17; test18
    ]
  in
  let ({ Test.directory } as runner) = Test.runner (Sys.getcwd () / "_tests") in
  let run idx test =
    Format.printf "test%03d: %!" (succ idx);
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
