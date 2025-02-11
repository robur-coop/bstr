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

let test19 =
  let descr = {text|view bounds too small|text} in
  Test.test ~title:"view bounds too small" ~descr @@ fun () ->
  let x = Bstr.create 4 in
  let y = Bstr.create 4 in
  let z = Bstr.sub y ~off:0 ~len:2 in
  try
    Bstr.blit x ~src_off:0 z ~dst_off:0 ~len:3;
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test20 =
  let descr = {text|view bounds too small get_uint8|text} in
  Test.test ~title:"view bounds too small get_uint8" ~descr @@ fun () ->
  let x = Bstr.create 2 in
  let y = Bstr.sub x ~off:0 ~len:1 in
  try
    let _ = Bstr.get_uint8 y 1 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test21 =
  let descr = {text|view bounds too small get_char|text} in
  Test.test ~title:"view bounds too small get_char" ~descr @@ fun () ->
  let x = Bstr.create 2 in
  let y = Bstr.sub x ~off:0 ~len:1 in
  try
    let _ = Bstr.get y 1 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test22 =
  let descr = {text|view bounds too small get_uint16_be|text} in
  Test.test ~title:"view bounds too small get_uint16_be" ~descr @@ fun () ->
  let x = Bstr.create 4 in
  let y = Bstr.sub x ~off:0 ~len:1 in
  try
    let _ = Bstr.get_uint16_be y 0 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test23 =
  let descr = {text|view bounds too small get_int32_be|text} in
  Test.test ~title:"view bounds too small get_int32_be" ~descr @@ fun () ->
  let x = Bstr.create 8 in
  let y = Bstr.sub x ~off:2 ~len:5 in
  try
    let _ = Bstr.get_int32_be y 2 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test24 =
  let descr = {text|view bounds too small get_int64_be|text} in
  Test.test ~title:"view bounds too small get_int64_be" ~descr @@ fun () ->
  let x = Bstr.create 9 in
  let y = Bstr.sub x ~off:1 ~len:5 in
  try
    let _ = Bstr.get_int64_be y 0 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test25 =
  let descr = {text|view bounds too small get_uint16_le|text} in
  Test.test ~title:"view bounds too small get_uint16_le" ~descr @@ fun () ->
  let x = Bstr.create 4 in
  let y = Bstr.sub x ~off:0 ~len:1 in
  try
    let _ = Bstr.get_uint16_le y 0 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test26 =
  let descr = {text|view bounds too small get_int32_le|text} in
  Test.test ~title:"view bounds too small get_int32_le" ~descr @@ fun () ->
  let x = Bstr.create 8 in
  let y = Bstr.sub x ~off:2 ~len:5 in
  try
    let _ = Bstr.get_int32_le y 2 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test27 =
  let descr = {text|view bounds too small get_int64_le|text} in
  Test.test ~title:"view bounds too small get_int64_le" ~descr @@ fun () ->
  let x = Bstr.create 9 in
  let y = Bstr.sub x ~off:1 ~len:5 in
  try
    let _ = Bstr.get_int64_le y 0 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test28 =
  let descr = {text|view bounds too small get_uint16_ne|text} in
  Test.test ~title:"view bounds too small get_uint16_ne" ~descr @@ fun () ->
  let x = Bstr.create 4 in
  let y = Bstr.sub x ~off:0 ~len:1 in
  try
    let _ = Bstr.get_uint16_ne y 0 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test29 =
  let descr = {text|view bounds too small get_int32_ne|text} in
  Test.test ~title:"view bounds too small get_int32_ne" ~descr @@ fun () ->
  let x = Bstr.create 8 in
  let y = Bstr.sub x ~off:2 ~len:5 in
  try
    let _ = Bstr.get_int32_ne y 2 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let test30 =
  let descr = {text|view bounds too small get_int64_ne|text} in
  Test.test ~title:"view bounds too small get_int64_ne" ~descr @@ fun () ->
  let x = Bstr.create 9 in
  let y = Bstr.sub x ~off:1 ~len:5 in
  try
    let _ = Bstr.get_int64_ne y 0 in
    check false
  with
  | Invalid_argument _ -> check true
  | _exn -> check false

let ( test31
    , test32
    , test33
    , test34
    , test35
    , test36
    , test37
    , test38
    , test39
    , test40
    , test41 ) =
  let test get zero =
    let descr = "subview containment get"
    and title = "subview containment get" in
    Test.test ~title ~descr @@ fun () ->
    let x = Bstr.create 32 in
    let y = Bstr.sub x ~off:8 ~len:16 in
    for i = 0 to Bstr.length x - 1 do
      Bstr.set_uint8 x i 0xff
    done;
    for i = 0 to Bstr.length y - 1 do
      Bstr.set_uint8 y i 0x00
    done;
    for i = -8 to 7 do
      try
        let v = get y i in
        if v <> zero then check false;
        check (i >= 0)
      with
      | Invalid_argument _ -> check (i < 0)
      | _exn -> check false
    done
  in
  ( test Bstr.get '\000'
  , test Bstr.get_uint8 0
  , test Bstr.get_uint16_be 0
  , test Bstr.get_uint16_le 0
  , test Bstr.get_uint16_ne 0
  , test Bstr.get_int32_be 0l
  , test Bstr.get_int32_le 0l
  , test Bstr.get_int32_ne 0l
  , test Bstr.get_int64_be 0L
  , test Bstr.get_int64_le 0L
  , test Bstr.get_int64_ne 0L )

let ( test42
    , test43
    , test44
    , test45
    , test46
    , test47
    , test48
    , test49
    , test50
    , test51
    , test52 ) =
  let test set ff =
    let descr = "subview containment set"
    and title = "subview containment set" in
    Test.test ~title ~descr @@ fun () ->
    let x = Bstr.create 32 in
    let y = Bstr.sub x ~off:8 ~len:16 in
    for i = 0 to Bstr.length x - 1 do
      Bstr.set_uint8 x i 0x00
    done;
    for i = -8 to 7 do
      try
        set y i ff;
        check (i >= 0)
      with
      | Invalid_argument _ -> check (i < 0)
      | _exn -> check false
    done;
    let acc = ref 0 in
    for i = 0 to Bstr.length x - 1 do
      acc := !acc + Bstr.get_uint8 x i
    done;
    check (!acc >= 8 * 0xff)
  in
  ( test Bstr.set '\xff'
  , test Bstr.set_uint8 0xff
  , test Bstr.set_uint16_be 0xffff
  , test Bstr.set_uint16_le 0xffff
  , test Bstr.set_uint16_ne 0xffff
  , test Bstr.set_int32_be 0xffffffffl
  , test Bstr.set_int32_le 0xffffffffl
  , test Bstr.set_int32_ne 0xffffffffl
  , test Bstr.set_int64_be 0xffffffffffffffffL
  , test Bstr.set_int64_le 0xffffffffffffffffL
  , test Bstr.set_int64_ne 0xffffffffffffffffL )

let check_bstr a b = check (Bstr.equal a b)
let check_raise exn fn = try fn (); check false with exn' -> check (exn = exn')

let test53 =
  let descr = {text|Miscellaneous tests|text} in
  Test.test ~title:"Miscellaneous tests" ~descr @@ fun () ->
  let x = Bstr.create 0 in
  check_bstr x Bstr.empty;
  check_bstr (Bstr.string "abc") (Bstr.of_string "abc");
  check_bstr (Bstr.string ~off:0 ~len:1 "abc") (Bstr.of_string "a");
  check_bstr (Bstr.string ~off:1 ~len:1 "abc") (Bstr.of_string "b");
  check_bstr (Bstr.string ~off:1 ~len:2 "abc") (Bstr.of_string "bc");
  check_bstr (Bstr.string ~off:3 ~len:0 "abc") (Bstr.of_string "");
  let x = Bstr.string ~off:2 ~len:1 "abc" in
  check (Bstr.length x = 1);
  let exn = Invalid_argument "index out of bounds" in
  check_raise exn (fun () -> ignore (Bstr.get x 3));
  check_raise exn (fun () -> ignore (Bstr.get x 2));
  check_raise exn (fun () -> ignore (Bstr.get x 1));
  check (Bstr.get x 0 = 'c');
  check (Bstr.get_uint8 x 0 = 0x63)

let test54 =
  let descr = {text|chop|text} in
  Test.test ~title:"chop" ~descr @@ fun () ->
  let x = Bstr.of_string "abc" in
  let y = Bstr.sub ~off:2 ~len:0 x in
  let z = Bstr.sub ~off:1 ~len:2 x in
  check (Bstr.chop y = None);
  check (Bstr.chop ~rev:true y = None);
  check (Bstr.chop z = Some 'b');
  check (Bstr.chop ~rev:true z = Some 'c')

let test55 =
  let descr = {text|is_empty|text} in
  Test.test ~title:"is_empty" ~descr @@ fun () ->
  let abcd = Bstr.of_string "abcd" in
  let huyi = Bstr.of_string "huyi" in
  check (Bstr.is_empty (Bstr.sub ~off:4 ~len:0 abcd));
  check (Bstr.is_empty (Bstr.sub ~off:0 ~len:0 huyi));
  check (Bstr.is_empty (Bstr.sub ~off:0 ~len:1 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:0 ~len:2 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:0 ~len:3 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:0 ~len:4 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:1 ~len:0 abcd));
  check (Bstr.is_empty (Bstr.sub ~off:1 ~len:1 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:1 ~len:2 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:1 ~len:3 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:2 ~len:0 abcd));
  check (Bstr.is_empty (Bstr.sub ~off:2 ~len:1 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:2 ~len:2 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:3 ~len:0 abcd));
  check (Bstr.is_empty (Bstr.sub ~off:3 ~len:1 huyi) == false);
  check (Bstr.is_empty (Bstr.sub ~off:4 ~len:0 huyi))

let test56 =
  let descr = {text|is_prefix & starts_with|text} in
  Test.test ~title:"is_prefix" ~descr @@ fun () ->
  let ugoadfj = Bstr.of_string "ugoadfj" in
  let dfkdjf = Bstr.of_string "dfkdjf" in
  let abhablablu = Bstr.of_string "abhablablu" in
  let hadfdffdf = Bstr.of_string "hadfdffdf" in
  let hadhabfdffdf = Bstr.of_string "hadhabfdffdf" in
  let iabla = Bstr.of_string "iabla" in
  let empty0 = Bstr.sub ~off:3 ~len:0 ugoadfj in
  let empty1 = Bstr.sub ~off:4 ~len:0 dfkdjf in
  let habla = Bstr.sub ~off:2 ~len:5 abhablablu in
  let h = Bstr.sub ~off:0 ~len:1 hadfdffdf in
  let ha = Bstr.sub ~off:0 ~len:2 hadfdffdf in
  let hab = Bstr.sub ~off:3 ~len:3 hadhabfdffdf in
  let abla = Bstr.sub ~off:1 ~len:4 iabla in
  check (Bstr.is_empty empty0);
  check (Bstr.is_empty empty1);
  check (Bstr.equal habla (Bstr.of_string "habla"));
  check (Bstr.equal h (Bstr.of_string "h"));
  check (Bstr.equal ha (Bstr.of_string "ha"));
  check (Bstr.equal hab (Bstr.of_string "hab"));
  check (Bstr.equal abla (Bstr.of_string "abla"));
  check (Bstr.is_prefix ~affix:"" empty1);
  check (Bstr.starts_with ~prefix:empty0 empty1);
  check (Bstr.is_prefix ~affix:"" habla);
  check (Bstr.starts_with ~prefix:empty0 habla);
  check (Bstr.is_prefix ~affix:"ha" empty1 == false);
  check (Bstr.starts_with ~prefix:ha empty1 == false);
  check (Bstr.is_prefix ~affix:"ha" h == false);
  check (Bstr.starts_with ~prefix:ha h == false);
  check (Bstr.is_prefix ~affix:"ha" hab);
  check (Bstr.starts_with ~prefix:ha hab);
  check (Bstr.is_prefix ~affix:"ha" habla);
  check (Bstr.starts_with ~prefix:ha habla);
  check (Bstr.is_prefix ~affix:"ha" abla == false);
  check (Bstr.starts_with ~prefix:ha abla == false)

let test57 =
  let descr = {text|is_infix|text} in
  Test.test ~title:"is_infix" ~descr @@ fun () ->
  let dfkdjf = Bstr.of_string "dfkdjf" in
  let aasdflablu = Bstr.of_string "aasdflablu" in
  let cda = Bstr.of_string "cda" in
  let h = Bstr.of_string "h" in
  let uhadfdffdf = Bstr.of_string "uhadfdffdf" in
  let ah = Bstr.of_string "ah" in
  let aaaha = Bstr.of_string "aaaha" in
  let ahaha = Bstr.of_string "ahaha" in
  let hahbdfdf = Bstr.of_string "hahbdfdf" in
  let blhahbdfdf = Bstr.of_string "blhahbdfdf" in
  let fblhahbdfdfl = Bstr.of_string "fblhahbdfdfl" in
  let empty = Bstr.sub dfkdjf ~off:2 ~len:0 in
  let asdf = Bstr.sub aasdflablu ~off:1 ~len:4 in
  let a = Bstr.sub cda ~off:2 ~len:1 in
  let h = Bstr.sub h ~off:0 ~len:1 in
  let ha = Bstr.sub uhadfdffdf ~off:1 ~len:2 in
  let ah = Bstr.sub ah ~off:0 ~len:2 in
  let aha = Bstr.sub aaaha ~off:2 ~len:3 in
  let haha = Bstr.sub ahaha ~off:1 ~len:4 in
  let hahb = Bstr.sub hahbdfdf ~off:0 ~len:4 in
  let blhahb = Bstr.sub blhahbdfdf ~off:0 ~len:6 in
  let blha = Bstr.sub fblhahbdfdfl ~off:1 ~len:4 in
  let blh = Bstr.sub fblhahbdfdfl ~off:1 ~len:3 in
  check (Bstr.to_string asdf = "asdf");
  check (Bstr.to_string ha = "ha");
  check (Bstr.to_string h = "h");
  check (Bstr.to_string a = "a");
  check (Bstr.to_string aha = "aha");
  check (Bstr.to_string haha = "haha");
  check (Bstr.to_string hahb = "hahb");
  check (Bstr.to_string blhahb = "blhahb");
  check (Bstr.to_string blha = "blha");
  check (Bstr.to_string blh = "blh");
  check (Bstr.is_infix ~affix:"" empty);
  check (Bstr.is_infix ~affix:"" asdf);
  check (Bstr.is_infix ~affix:"" ha);
  check (Bstr.is_infix ~affix:"ha" empty == false);
  check (Bstr.is_infix ~affix:"ha" a == false);
  check (Bstr.is_infix ~affix:"ha" h == false);
  check (Bstr.is_infix ~affix:"ha" ah == false);
  check (Bstr.is_infix ~affix:"ha" ha);
  check (Bstr.is_infix ~affix:"ha" aha);
  check (Bstr.is_infix ~affix:"ha" haha);
  check (Bstr.is_infix ~affix:"ha" hahb);
  check (Bstr.is_infix ~affix:"ha" blhahb);
  check (Bstr.is_infix ~affix:"ha" blha);
  check (Bstr.is_infix ~affix:"ha" blh == false)

let test58 =
  let descr = {text|is_suffix & ends_with|text} in
  Test.test ~title:"is_suffix" ~descr @@ fun () ->
  let ugoadfj = Bstr.of_string "ugoadfj" in
  let dfkdjf = Bstr.of_string "dfkdjf" in
  let aasdflablu = Bstr.of_string "aasdflablu" in
  let cda = Bstr.of_string "cda" in
  let h = Bstr.of_string "h" in
  let uhadfdffdf = Bstr.of_string "uhadfdffdf" in
  let ah = Bstr.of_string "ah" in
  let aaaha = Bstr.of_string "aaaha" in
  let ahaha = Bstr.of_string "ahaha" in
  let hahbdfdf = Bstr.of_string "hahbdfdf" in
  let empty0 = Bstr.sub ugoadfj ~off:1 ~len:0 in
  let empty1 = Bstr.sub dfkdjf ~off:2 ~len:0 in
  let asdf = Bstr.sub aasdflablu ~off:1 ~len:4 in
  let a = Bstr.sub cda ~off:2 ~len:1 in
  let h = Bstr.sub h ~off:0 ~len:1 in
  let ha = Bstr.sub uhadfdffdf ~off:1 ~len:2 in
  let ah = Bstr.sub ah ~off:0 ~len:2 in
  let aha = Bstr.sub aaaha ~off:2 ~len:3 in
  let haha = Bstr.sub ahaha ~off:1 ~len:4 in
  let hahb = Bstr.sub hahbdfdf ~off:0 ~len:4 in
  check (Bstr.to_string asdf = "asdf");
  check (Bstr.to_string ha = "ha");
  check (Bstr.to_string h = "h");
  check (Bstr.to_string a = "a");
  check (Bstr.to_string aha = "aha");
  check (Bstr.to_string haha = "haha");
  check (Bstr.to_string hahb = "hahb");
  check (Bstr.is_suffix ~affix:"" empty1);
  check (Bstr.ends_with ~suffix:empty0 empty1);
  check (Bstr.is_suffix ~affix:"" asdf);
  check (Bstr.ends_with ~suffix:empty0 asdf);
  check (Bstr.is_suffix ~affix:"ha" empty1 == false);
  check (Bstr.ends_with ~suffix:ha empty1 == false);
  check (Bstr.is_suffix ~affix:"ha" a == false);
  check (Bstr.ends_with ~suffix:ha a == false);
  check (Bstr.is_suffix ~affix:"ha" h == false);
  check (Bstr.ends_with ~suffix:ha h == false);
  check (Bstr.is_suffix ~affix:"ha" ah == false);
  check (Bstr.ends_with ~suffix:ha ah == false);
  check (Bstr.is_suffix ~affix:"ha" ha);
  check (Bstr.ends_with ~suffix:ha ha);
  check (Bstr.is_suffix ~affix:"ha" aha);
  check (Bstr.ends_with ~suffix:ha aha);
  check (Bstr.is_suffix ~affix:"ha" haha);
  check (Bstr.ends_with ~suffix:ha haha);
  check (Bstr.is_suffix ~affix:"ha" hahb == false);
  check (Bstr.ends_with ~suffix:ha hahb == false)

let test59 =
  let descr = {text|for_all|text} in
  Test.test ~title:"for_all" ~descr @@ fun () ->
  let asldfksaf = Bstr.of_string "asldfksaf" in
  let sf123df = Bstr.of_string "sf123df" in
  let _412 = Bstr.of_string "412" in
  let aaa142 = Bstr.of_string "aaa142" in
  let aad124 = Bstr.of_string "aad124" in
  let empty = Bstr.sub ~off:3 ~len:0 asldfksaf in
  let s123 = Bstr.sub ~off:2 ~len:3 sf123df in
  let s412 = Bstr.sub ~off:0 ~len:3 _412 in
  let s142 = Bstr.sub ~off:3 ~len:3 aaa142 in
  let s124 = Bstr.sub ~off:3 ~len:3 aad124 in
  check (Bstr.to_string empty = "");
  check (Bstr.to_string s123 = "123");
  check (Bstr.to_string s412 = "412");
  check (Bstr.to_string s142 = "142");
  check (Bstr.to_string s124 = "124");
  check (Bstr.for_all (fun _ -> false) empty);
  check (Bstr.for_all (fun _ -> true) empty);
  check (Bstr.for_all (fun c -> Char.code c < 0x34) s123);
  check (Bstr.for_all (fun c -> Char.code c < 0x34) s412 == false);
  check (Bstr.for_all (fun c -> Char.code c < 0x34) s142 == false);
  check (Bstr.for_all (fun c -> Char.code c < 0x34) s124 == false)

let test60 =
  let descr = {text|trim|text} in
  Test.test ~title:"trim" ~descr @@ fun () ->
  let base = Bstr.of_string "00aaaabcdaaaa00" in
  let aaaabcdaaaa = Bstr.sub ~off:2 ~len:11 base in
  let aaaabcd = Bstr.sub ~off:2 ~len:7 base in
  let bcdaaaa = Bstr.sub ~off:6 ~len:7 base in
  let aaaa = Bstr.sub ~off:2 ~len:4 base in
  check (Bstr.(to_string (trim (of_string "\t abcd \t"))) = "abcd");
  check (Bstr.(to_string (trim aaaabcdaaaa)) = "aaaabcdaaaa");
  let drop_a = ( = ) 'a' in
  check (Bstr.(to_string (trim ~drop:drop_a aaaabcdaaaa)) = "bcd");
  check (Bstr.(to_string (trim ~drop:drop_a aaaabcd)) = "bcd");
  check (Bstr.(to_string (trim ~drop:drop_a bcdaaaa)) = "bcd");
  check Bstr.(is_empty (trim ~drop:drop_a aaaa));
  check Bstr.(is_empty (trim (of_string "    ")))

let is_white = function ' ' | '\t' -> true | _ -> false
let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

[@@@warning "-26"]

let test61 =
  let descr = {text|span|text} in
  Test.test ~title:"span" ~descr @@ fun () ->
  let test ?rev ?min ?max ?sat bstr (cl, cr) =
    let rl, rr = Bstr.span ?rev ?min ?max ?sat bstr in
    let t = Bstr.take ?rev ?min ?max ?sat bstr in
    let d = Bstr.drop ?rev ?min ?max ?sat bstr in
    let rev = Option.value ~default:false rev in
    check (Bstr.equal cl rl);
    check (Bstr.equal cr rr);
    check (Bstr.equal (if rev then cr else cl) t);
    check (Bstr.equal (if rev then cl else cr) d) in
  let invalid ?rev ?min ?max ?sat bstr =
    try let _ = Bstr.span ?rev ?min ?max ?sat bstr in
        check false
    with Invalid_argument _ -> check true
       | _exn -> check false in
  let base = Bstr.of_string "0ab cd0" in
  let ab_cd = Bstr.sub ~off:1 ~len:5 base in
  let ab = Bstr.sub ~off:1 ~len:2 base in
  let _cd = Bstr.sub ~off:3 ~len:3 base in
  let cd = Bstr.sub ~off:4 ~len:2 base in
  let ab_ = Bstr.sub ~off:1 ~len:3 base in
  let a = Bstr.sub ~off:1 ~len:1 base in
  let b_cd = Bstr.sub ~off:2 ~len:4 base in
  let b = Bstr.sub ~off:2 ~len:1 base in
  let d = Bstr.sub ~off:5 ~len:1 base in
  let ab_c = Bstr.sub ~off:1 ~len:4 base in
  test ~rev:false ~min:1 ~max:0 ab_cd (Bstr.sub ab_cd ~off:0 ~len:0, ab_cd);
  test ~rev:true ~min:1 ~max:0 ab_cd (ab_cd, Bstr.sub ab_cd ~off:5 ~len:0);
  test ~sat:is_white ab_cd (Bstr.sub ab_cd ~off:0 ~len:0, ab_cd);
  test ~sat:is_letter ab_cd (ab, _cd);
  test ~max:1 ~sat:is_letter ab_cd (a, b_cd);
  test ~max:0 ~sat:is_letter ab_cd (Bstr.sub ab_cd ~off:5 ~len:0, ab_cd);
  test ~rev:true ~sat:is_white ab_cd (ab_cd, Bstr.sub ab_cd ~off:5 ~len:0);
  test ~rev:true ~sat:is_letter ab_cd (ab_, cd);
  test ~rev:true ~max:1 ~sat:is_letter ab_cd (ab_c, d);
  test ~rev:true ~max:0 ~sat:is_letter ab_cd (ab_cd, Bstr.sub ab_cd ~off:5 ~len:0);
  test ~sat:is_letter ab (ab, Bstr.sub ab ~off:2 ~len:0);
  test ~max:1 ~sat:is_letter ab (a, b);
  test ~rev:true ~max:1 ~sat:is_letter ab (a, b);
  test ~max:1 ~sat:is_white ab (Bstr.sub ~off:0 ~len:0 ab, ab);
  test ~rev:true ~sat:is_white Bstr.empty (Bstr.empty, Bstr.empty);
  test ~sat:is_white Bstr.empty (Bstr.empty, Bstr.empty);
  invalid ~rev:false ~min:(-1) Bstr.empty;
  invalid ~rev:true ~min:(-1) Bstr.empty;
  invalid ~rev:false ~max:(-1) Bstr.empty;
  invalid ~rev:true ~max:(-1) Bstr.empty;
  test ~rev:false Bstr.empty (Bstr.empty, Bstr.empty);
  test ~rev:true Bstr.empty (Bstr.empty, Bstr.empty);
  test ~rev:false ~min:0 ~max:0 Bstr.empty (Bstr.empty, Bstr.empty);
  test ~rev:true ~min:0 ~max:0 Bstr.empty (Bstr.empty, Bstr.empty);
  test ~rev:false ~min:1 ~max:0 Bstr.empty (Bstr.empty, Bstr.empty);
  test ~rev:true ~min:1 ~max:0 Bstr.empty (Bstr.empty, Bstr.empty);
  test ~rev:false ~max:0 ab_cd (Bstr.sub ~off:0 ~len:0 ab_cd, ab_cd);
  test ~rev:true ~max:0 ab_cd (ab_cd, Bstr.sub ~off:5 ~len:0 ab_cd);
  test ~rev:false ~max:2 ab_cd (ab, _cd);
  test ~rev:true ~max:2 ab_cd (ab_, cd);
  test ~rev:false ~min:6 ab_cd (Bstr.sub ab_cd ~off:0 ~len:0, ab_cd);
  test ~rev:true ~min:6 ab_cd (ab_cd, Bstr.sub ab_cd ~off:5 ~len:0);
  test ~rev:false ab_cd (ab_cd, Bstr.sub ~off:5 ~len:0 ab_cd);
  test ~rev:true ab_cd (Bstr.sub ab_cd ~off:0 ~len:0, ab_cd);
  test ~rev:false ~max:30 ab_cd (ab_cd, Bstr.sub ~off:5 ~len:0 ab_cd);
  test ~rev:true ~max:30 ab_cd (Bstr.sub ab_cd ~off:0 ~len:0, ab_cd);
  test ~rev:false ~sat:is_white ab_cd (Bstr.sub ~off:0 ~len:0 ab_cd, ab_cd);
  test ~rev:true ~sat:is_white ab_cd (ab_cd, Bstr.sub ~off:5 ~len:0 ab_cd);
  test ~rev:false ~sat:is_letter ab_cd (ab, _cd);
  test ~rev:true ~sat:is_letter ab_cd (ab_, cd);
  test ~rev:false ~sat:is_letter ~max:0 ab_cd (Bstr.sub ~off:0 ~len:0 ab_cd, ab_cd);
  test ~rev:true ~sat:is_letter ~max:0 ab_cd (ab_cd, Bstr.sub ~off:5 ~len:0 ab_cd);
  test ~rev:false ~sat:is_letter ~max:1 ab_cd (a, b_cd);
  test ~rev:true ~sat:is_letter ~max:1 ab_cd (ab_c, d);
  test ~rev:false ~sat:is_letter ~min:2 ~max:1 ab_cd (Bstr.sub ~off:0 ~len:0 ab_cd, ab_cd);
  test ~rev:true ~sat:is_letter ~min:2 ~max:1 ab_cd (ab_cd, Bstr.sub ~off:5 ~len:0 ab_cd);
  test ~rev:false ~sat:is_letter ~min:3 ab_cd (Bstr.sub ~off:0 ~len:0 ab_cd, ab_cd);
  test ~rev:true ~sat:is_letter ~min:3 ab_cd (ab_cd, Bstr.sub ~off:5 ~len:0 ab_cd)


let ( / ) = Filename.concat

let () =
  let tests =
    [
      test01; test02; test03; test04; test05; test06; test07; test08; test09
    ; test10; test11; test12; test13; test14; test15; test16; test17; test18
    ; test19; test20; test21; test22; test23; test24; test25; test26; test27
    ; test28; test29; test30; test31; test32; test33; test34; test35; test36
    ; test37; test38; test39; test40; test41; test42; test43; test44; test45
    ; test46; test47; test48; test49; test50; test51; test52; test53; test54
    ; test55; test56; test57; test58; test59; test60; test61
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
