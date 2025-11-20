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
    check (Bstr.get_uint8 buf len == 0);
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

let to_neint32 v =
  let buf = Bytes.create 4 in
  Bytes.set_int32_ne buf 0 v; Bytes.unsafe_to_string buf

let to_neint64 v =
  let buf = Bytes.create 8 in
  Bytes.set_int64_ne buf 0 v; Bytes.unsafe_to_string buf

let test03 =
  let descr = {text|endian|text} in
  Test.test ~title:"endian" ~descr @@ fun () ->
  let buf = Bstr.create 0x7ff in
  let t =
    let open Bin in
    record (fun a a' b b' ->
        check (a = a');
        check (b = b');
        (a, b))
    |+ field leint32 (fun (a, _) -> a)
    |+ field neint32 (fun (a, _) -> a)
    |+ field leint64 (fun (_, b) -> b)
    |+ field neint64 (fun (_, b) -> b)
    |> sealr
  in
  let test value expected =
    let pos = ref 0 in
    Bin.encode_bstr t value buf pos;
    let len = String.length expected in
    check (!pos == len);
    check (Bstr.sub_string buf ~off:0 ~len = expected);
    pos := 0;
    let value' = Bin.decode_bstr t buf pos in
    check (!pos = len);
    check (value = value')
  in
  test (0l, 0L) (String.make 24 '\000');
  let leint32 = "4\018\000\000"
  and neint32 = to_neint32 0x1234l
  and leint64 = "\205\171\000\000\000\000\000\000"
  and neint64 = to_neint64 0xabcdL in
  test (0x1234l, 0xabcdL) (leint32 ^ neint32 ^ leint64 ^ neint64)

let ( / ) = Filename.concat

let () =
  let tests = [ test01; test02; test03 ] in
  let ({ Test.directory } as runner) = Test.runner (Sys.getcwd () / "_tests") in
  let run idx test =
    Format.printf "test%03d: %!" (succ idx);
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
