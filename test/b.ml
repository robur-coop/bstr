open Test

let test01 =
  let descr = {text|cstring|text} in
  Test.test ~title:"cstring" ~descr @@ fun () ->
  let buf = Bstr.create 0x7ff in
  let test str =
    let pos = ref 0 in
    let len = String.length str in
    Bin.encode_bstr Bin.cstring str buf pos;
    check (!pos == len + 1);
    check (Bstr.get_uint8 buf !pos == 0);
    check (Bstr.sub_string buf ~off:0 ~len = str);
    pos := 0;
    let str' = Bin.decode_bstr Bin.cstring buf pos in
    check (!pos == len + 1);
    check (str = str')
  in
  test "foo"; test "bar"

let test02 =
  let descr = {text|varint|text} in
  Test.test ~title:"varint" ~descr @@ fun () ->
  let buf = Bstr.create 0x7ff in
  let test value expected =
    let pos = ref 0 in
    Bin.encode_bstr Bin.varint value buf pos;
    let len = String.length expected in
    check (!pos == len);
    check (Bstr.sub_string buf ~off:0 ~len = expected);
    pos := 0;
    let value' = Bin.decode_bstr Bin.varint buf pos in
    check (!pos == len);
    check (value == value')
  in
  test 0 "\000";
  test 127 "\127";
  test 128 "\128\001";
  test 16384 "\128\128\001";
  test 88080384 "\128\128\128\042"

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
