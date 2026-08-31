open Test

let test01 =
  let descr = {text|cstring|text} in
  Test.test ~title:"cstring" ~descr @@ fun () ->
  let buf = Bstr.create 0x7ff in
  let enc = Bin.Staged.unstage (Bin.encode_bstr Bin.cstring) in
  let dec = Bin.Staged.unstage (Bin.decode_bstr Bin.cstring) in
  let test str =
    let pos = ref Bin.Off.zero in
    let len = String.length str in
    enc str buf pos;
    check ((!pos :> int) == len + 1);
    check (Bstr.get_uint8 buf len == 0);
    check (Bstr.sub_string buf ~off:0 ~len = str);
    pos := Bin.Off.zero;
    let str' = dec buf pos in
    check ((!pos :> int) == len + 1);
    check (str = str')
  in
  test "foo"; test "bar"

let test02 =
  let descr = {text|varint|text} in
  Test.test ~title:"varint" ~descr @@ fun () ->
  let buf = Bstr.create 0x7ff in
  let enc = Bin.Staged.unstage (Bin.encode_bstr Bin.varint) in
  let dec = Bin.Staged.unstage (Bin.decode_bstr Bin.varint) in
  let test value expected =
    let pos = ref Bin.Off.zero in
    enc value buf pos;
    let len = String.length expected in
    check ((!pos :> int) == len);
    check (Bstr.sub_string buf ~off:0 ~len = expected);
    pos := Bin.Off.zero;
    let value' = dec buf pos in
    check ((!pos :> int) == len);
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
  let enc = Bin.Staged.unstage (Bin.encode_bstr t) in
  let dec = Bin.Staged.unstage (Bin.decode_bstr t) in
  let test value expected =
    let pos = ref Bin.Off.zero in
    enc value buf pos;
    let len = String.length expected in
    check ((!pos :> int) == len);
    check (Bstr.sub_string buf ~off:0 ~len = expected);
    pos := Bin.Off.zero;
    let value' = dec buf pos in
    check ((!pos :> int) == len);
    check (value = value')
  in
  test (0l, 0L) (String.make 24 '\000');
  let leint32 = "4\018\000\000"
  and neint32 = to_neint32 0x1234l
  and leint64 = "\205\171\000\000\000\000\000\000"
  and neint64 = to_neint64 0xabcdL in
  test (0x1234l, 0xabcdL) (leint32 ^ neint32 ^ leint64 ^ neint64)

module DNS = struct
  type name = string list

  type flags = {
      qr: bool
    ; opcode: int
    ; aa: bool
    ; tc: bool
    ; rd: bool
    ; ra: bool
    ; z: int
    ; rcode: int
  }

  (*                                 1  1  1  1  1  1
       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                      ID                       |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                    QDCOUNT                    |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                    ANCOUNT                    |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                    NSCOUNT                    |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                    ARCOUNT                    |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   *)

  type header = {
      id: int
    ; flags: flags
    ; qdcount: int
    ; ancount: int
    ; nscount: int
    ; arcount: int
  }

  (*                                 1  1  1  1  1  1
       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                                               |
     /                     QNAME                     /
     /                                               /
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                     QTYPE                     |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                     QCLASS                    |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   *)

  type question = { qname: name; qtype: int; qclass: int }

  (*                                 1  1  1  1  1  1
       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                                               |
     /                                               /
     /                      NAME                     /
     |                                               |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                      TYPE                     |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                     CLASS                     |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                      TTL                      |
     |                                               |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
     |                   RDLENGTH                    |
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
     /                     RDATA                     /
     /                                               /
     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   *)

  type rr = { rname: name; rtype: int; rclass: int; ttl: int32; rdata: string }

  (* +---------------------+
     |        Header       |
     +---------------------+
     |       Question      | the question for the name server
     +---------------------+
     |        Answer       | RRs answering the question
     +---------------------+
   *)

  type t = { header: header; questions: question list; answers: rr list }

  let name =
    let open Bin in
    fix @@ fun name ->
    let fn len =
      if len == 0 then const []
      else
        record (fun label rest -> label :: rest)
        |+ field (bytes (fixed len)) List.hd
        |+ field name List.tl
        |> sealr
    in
    let gn = function [] -> 0 | label :: _ -> String.length label in
    let* t = (uint8, fn, gn) in
    t

  let flags =
    let open Bin in
    bits (B16 Big_endian) (fun qr opcode aa tc rd ra z rcode ->
        { qr; opcode; aa; tc; rd; ra; z; rcode })
    |* flag ~name:"qr" (fun t -> t.qr)
    |* bit ~name:"opcode" 4 (fun t -> t.opcode)
    |* flag ~name:"aa" (fun t -> t.aa)
    |* flag ~name:"tc" (fun t -> t.tc)
    |* flag ~name:"rd" (fun t -> t.rd)
    |* flag ~name:"ra" (fun t -> t.ra)
    |* bit ~name:"z" 3 (fun t -> t.z)
    |* bit ~name:"rcode" 4 (fun t -> t.rcode)
    |> sealb

  let header =
    let open Bin in
    record ~name:"header" (fun id flags qdcount ancount nscount arcount ->
        { id; flags; qdcount; ancount; nscount; arcount })
    |+ field ~name:"id" beuint16 (fun t -> t.id)
    |+ field ~name:"flags" flags (fun t -> t.flags)
    |+ field ~name:"qdcount" beuint16 (fun t -> t.qdcount)
    |+ field ~name:"ancount" beuint16 (fun t -> t.ancount)
    |+ field ~name:"nscount" beuint16 (fun t -> t.nscount)
    |+ field ~name:"arcount" beuint16 (fun t -> t.arcount)
    |> sealr

  let question =
    let open Bin in
    record ~name:"question" (fun qname qtype qclass -> { qname; qtype; qclass })
    |+ field ~name:"qname" name (fun t -> t.qname)
    |+ field ~name:"qtype" beuint16 (fun t -> t.qtype)
    |+ field ~name:"qclass" beuint16 (fun t -> t.qclass)
    |> sealr

  let rr =
    let open Bin in
    record ~name:"rr" (fun rname rtype rclass ttl rdata ->
        { rname; rtype; rclass; ttl; rdata })
    |+ field ~name:"name" name (fun t -> t.rname)
    |+ field ~name:"type" beuint16 (fun t -> t.rtype)
    |+ field ~name:"class" beuint16 (fun t -> t.rclass)
    |+ field ~name:"ttl" beint32 (fun t -> t.ttl)
    |+ field ~name:"rdata" (bytes (prefix beuint16)) (fun t -> t.rdata)
    |> sealr

  let message =
    let open Bin in
    let codec header =
      record ~name:"message" (fun questions answers ->
          { header; questions; answers })
      |+ field ~name:"questions"
           (list (fixed header.qdcount) question)
           (fun t -> t.questions)
      |+ field ~name:"answers"
           (list (fixed header.ancount) rr)
           (fun t -> t.answers)
      |> sealr
    in
    let update t =
      {
        t.header with
        qdcount= List.length t.questions
      ; ancount= List.length t.answers
      }
    in
    let* t = (header, codec, update) in
    t
end

let codec repr =
  let buf = Bstr.create 0x7ff in
  let enc = Bin.Staged.unstage (Bin.encode_bstr repr) in
  let dec = Bin.Staged.unstage (Bin.decode_bstr repr) in
  let dec_str = Bin.Staged.unstage (Bin.decode repr) in
  let encode value =
    let pos = ref Bin.Off.zero in
    enc value buf pos;
    let str = Bstr.sub_string buf ~off:0 ~len:(!pos :> int) in
    begin match Bin.size_of_value repr value with
    | Some len -> check (len == String.length str)
    | None -> ()
    end;
    str
  in
  let decode str =
    let pos = ref Bin.Off.zero in
    Bstr.blit_from_string str ~src_off:0 buf ~dst_off:0 ~len:(String.length str);
    let value = dec buf pos in
    check ((!pos :> int) == String.length str);
    let pos = ref Bin.Off.zero in
    let value' = dec_str str pos in
    check ((!pos :> int) == String.length str);
    check (value = value');
    value
  in
  (encode, decode)

let test04 =
  let descr = {text|dns name (fix)|text} in
  Test.test ~title:"test04" ~descr @@ fun () ->
  let encode, decode = codec DNS.name in
  let test ?(roundtrip = true) name expected =
    let str = encode name in
    check (String.equal str expected);
    if roundtrip then check (decode expected = name)
  in
  test [] "\000";
  test [ "com" ] "\003com\000";
  test [ "www"; "example"; "com" ] "\003www\007example\003com\000";
  test [ "a"; "b"; "c"; "d"; "e" ] "\001a\001b\001c\001d\001e\000";
  test ~roundtrip:false [ ""; "com" ] "\000";
  check (decode "\000" = []);
  let long = String.make 255 'a' in
  test [ long ] ("\255" ^ long ^ "\000");
  let deep = List.init 64 (fun idx -> Format.sprintf "l%02d" idx) in
  let expected =
    String.concat "" (List.map (fun l -> "\003" ^ l) deep) ^ "\000"
  in
  test deep expected

let test05 =
  let descr = {text|dns header flags|text} in
  Test.test ~title:"test05" ~descr @@ fun () ->
  let encode, decode = codec DNS.flags in
  let zero =
    {
      DNS.qr= false
    ; opcode= 0
    ; aa= false
    ; tc= false
    ; rd= false
    ; ra= false
    ; z= 0
    ; rcode= 0
    }
  in
  let test flags word =
    let expected =
      let buf = Bytes.create 2 in
      Bytes.set_uint16_be buf 0 word;
      Bytes.unsafe_to_string buf
    in
    let str = encode flags in
    check (String.length str == 2);
    check (String.equal str expected);
    check (decode expected = flags)
  in
  test zero 0x0000;
  test { zero with rd= true } 0x0100;
  test { zero with qr= true; rd= true; ra= true } 0x8180;
  test { zero with qr= true; rd= true; ra= true; rcode= 3 } 0x8183;
  test { zero with qr= true; aa= true; tc= true; rd= true } 0x8700;
  test { zero with qr= true } 0x8000;
  test { zero with opcode= 0xf } 0x7800;
  test { zero with aa= true } 0x0400;
  test { zero with tc= true } 0x0200;
  test { zero with rd= true } 0x0100;
  test { zero with ra= true } 0x0080;
  test { zero with z= 0x7 } 0x0070;
  test { zero with rcode= 0xf } 0x000f;
  (match encode { zero with opcode= 0x10 } with
  | _ -> check false
  | exception _ -> check true);
  match encode { zero with z= 0x8 } with
  | _ -> check false
  | exception _ -> check true

let test06 =
  let descr = {text|query|text} in
  Test.test ~title:"dns-query" ~descr @@ fun () ->
  let encode, decode = codec DNS.message in
  let flags =
    {
      DNS.qr= false
    ; opcode= 0
    ; aa= false
    ; tc= false
    ; rd= true
    ; ra= false
    ; z= 0
    ; rcode= 0
    }
  in
  let header =
    { DNS.id= 0x1234; flags; qdcount= 1; ancount= 0; nscount= 0; arcount= 0 }
  in
  let question =
    { DNS.qname= [ "www"; "example"; "com" ]; qtype= 1; qclass= 1 }
  in
  let query = { DNS.header; questions= [ question ]; answers= [] } in
  let expected =
    "\x12\x34\x01\x00\x00\x01\x00\x00\x00\x00\x00\x00"
    ^ "\003www\007example\003com\000" ^ "\x00\x01\x00\x01"
  in
  let str = encode query in
  check (String.length str == 33);
  check (String.equal str expected);
  check (decode expected = query);
  let query = { query with DNS.header= { header with DNS.qdcount= 0 } } in
  check (String.equal (encode query) expected)

let test07 =
  let descr = {text|response|text} in
  Test.test ~title:"test06" ~descr @@ fun () ->
  let encode, decode = codec DNS.message in
  let flags =
    {
      DNS.qr= true
    ; opcode= 0
    ; aa= false
    ; tc= false
    ; rd= true
    ; ra= true
    ; z= 0
    ; rcode= 0
    }
  in
  let header =
    { DNS.id= 0x1234; flags; qdcount= 1; ancount= 2; nscount= 0; arcount= 0 }
  in
  let name = [ "www"; "example"; "com" ] in
  let question = { DNS.qname= name; qtype= 1; qclass= 1 } in
  let a =
    {
      DNS.rname= name
    ; rtype= 1
    ; rclass= 1
    ; ttl= 300l
    ; rdata= "\x5d\xb8\xd8\x22"
    }
  in
  let cname =
    {
      DNS.rname= name
    ; rtype= 5
    ; rclass= 1
    ; ttl= 60l
    ; rdata= "\007example\003com\000"
    }
  in
  let response =
    { DNS.header; questions= [ question ]; answers= [ a; cname ] }
  in
  let name_wire = "\003www\007example\003com\000" in
  let expected =
    "\x12\x34\x81\x80\x00\x01\x00\x02\x00\x00\x00\x00" ^ name_wire
    ^ "\x00\x01\x00\x01" ^ name_wire
    ^ "\x00\x01\x00\x01\x00\x00\x01\x2c\x00\x04\x5d\xb8\xd8\x22" ^ name_wire
    ^ "\x00\x05\x00\x01\x00\x00\x00\x3c\x00\x0d" ^ "\007example\003com\000"
  in
  let str = encode response in
  check (String.equal str expected);
  check (decode expected = response);
  let empty =
    { response with DNS.header= { header with DNS.ancount= 0 }; answers= [] }
  in
  let str = encode empty in
  check (String.length str == 33);
  check (decode str = empty)

let ( / ) = Filename.concat

let () =
  let tests = [ test01; test02; test03; test04; test05; test06; test07 ] in
  let ({ Test.directory } as runner) = Test.runner (Sys.getcwd () / "_tests") in
  let run idx test =
    Format.printf "test%03d: %!" (succ idx);
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
