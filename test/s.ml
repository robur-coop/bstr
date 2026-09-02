open Test

let () = Printexc.record_backtrace true
let strf fmt = Format.asprintf fmt

let string_filter sat str =
  let buf = Buffer.create (String.length str) in
  String.iter (fun chr -> if sat chr then Buffer.add_char buf chr) str;
  Buffer.contents buf

let string_hex str =
  let buf = Buffer.create (String.length str * 2) in
  let fn chr = Buffer.add_string buf (strf "%02x" (Char.code chr)) in
  String.iter fn str; Buffer.contents buf

let random_string g len =
  String.init len @@ fun _ ->
  match Random.State.int g 4 with
  | 0 -> 'a'
  | 1 -> 'b'
  | 2 -> ','
  | _ -> Char.chr (Random.State.int g 256)

module Make (M : Slice.S) = struct
  let err : type a. (unit -> a) -> unit =
   fun fn ->
    match fn () with
    | exception Invalid_argument _ -> check true
    | _ -> check false

  let eq t str = check (String.equal (M.to_string t) str)

  let eq_bstr t bstr =
    check (String.equal (M.to_string t) (Bstr.to_string bstr))

  let eq_list lst bstrs =
    check (List.length lst == List.length bstrs);
    List.iter2 eq_bstr lst bstrs

  let embed str =
    let t = M.string ("<<<<" ^ str ^ ">>>>") in
    M.sub t ~off:4 ~len:(String.length str)

  let both str fn =
    fn (M.string str);
    fn (embed str)

  let bstr = Bstr.of_string

  let test_base name =
    let descr = {text|constructors, conversions and accessors|text} in
    Test.test ~title:(name ^ "-base") ~descr @@ fun () ->
    eq M.empty "";
    check (M.is_empty M.empty);
    check (M.length M.empty == 0);
    eq (M.string "abc") "abc";
    eq (M.string "abc" ~off:0 ~len:1) "a";
    eq (M.string "abc" ~off:1 ~len:1) "b";
    eq (M.string "abc" ~off:1 ~len:2) "bc";
    eq (M.string "abc" ~off:2 ~len:1) "c";
    eq (M.string "abc" ~off:2 ~len:0) "";
    eq (M.string "abc" ~off:1) "bc";
    err (fun () -> M.string "abc" ~off:2 ~len:2);
    err (fun () -> M.string "abc" ~off:(-1));
    check (M.length (M.create 8) == 8);
    eq (M.create 8) (String.make 8 '\000');
    err (fun () -> M.create (-1));
    eq (M.init 5 (fun idx -> Char.chr (Char.code 'a' + idx))) "abcde";
    err (fun () -> M.init (-1) (fun _ -> ' '));
    let str = String.init 6 (fun idx -> Char.chr (Char.code 'a' + idx)) in
    let t = M.string str in
    M.set t 0 'z';
    check (String.equal str "abcdef");
    let t = M.string ~off:0 ~len:6 str in
    M.set t 0 'z';
    check (String.equal str "abcdef");
    let t = M.string "abcdef" in
    let c = M.copy t in
    M.set c 0 'z';
    eq c "zbcdef";
    eq t "abcdef";
    both "abcdef" @@ fun t ->
    eq t "abcdef";
    eq (M.copy t) "abcdef";
    check (M.get t 0 == 'a');
    check (M.get t 5 == 'f');
    check (M.unsafe_get t 3 == 'd');
    err (fun () -> M.get t 6);
    err (fun () -> M.get t (-1));
    err (fun () -> M.set t 6 'z');
    eq (M.sub_string t ~off:2 ~len:3 |> M.string) "cde";
    err (fun () -> M.sub_string t ~off:4 ~len:3);
    check (M.chop t = Some 'a');
    check (M.chop ~rev:true t = Some 'f');
    check (M.chop M.empty = None);
    check (String.equal (M.hex t) (string_hex "abcdef"))

  let test_views name =
    let descr = {text|sub, shift and ranges are views|text} in
    Test.test ~title:(name ^ "-views") ~descr @@ fun () ->
    let t = M.string "abcdef" in
    let v = M.sub t ~off:2 ~len:2 in
    M.set v 0 'X';
    M.unsafe_set v 1 'Y';
    eq v "XY";
    eq t "abXYef";
    err (fun () -> M.set v 2 'Z');
    err (fun () -> M.get v 2);
    both "abcdef" @@ fun t ->
    eq (M.sub t ~off:0 ~len:6) "abcdef";
    eq (M.sub t ~off:2 ~len:2) "cd";
    eq (M.sub t ~off:6 ~len:0) "";
    err (fun () -> M.sub t ~off:2 ~len:5);
    err (fun () -> M.sub t ~off:(-1) ~len:1);
    eq (M.shift t 2) "cdef";
    eq (M.shift t 6) "";
    err (fun () -> M.shift t 7);
    err (fun () -> M.shift t (-1));
    let b = bstr "abcdef" in
    eq_bstr (M.with_range t) (Bstr.with_range b);
    eq_bstr (M.with_range ~first:2 t) (Bstr.with_range ~first:2 b);
    eq_bstr (M.with_range ~first:2 ~len:2 t) (Bstr.with_range ~first:2 ~len:2 b);
    eq_bstr
      (M.with_range ~first:(-2) ~len:3 t)
      (Bstr.with_range ~first:(-2) ~len:3 b);
    eq_bstr (M.with_range ~len:0 t) (Bstr.with_range ~len:0 b);
    eq_bstr
      (M.with_range ~first:4 ~len:100 t)
      (Bstr.with_range ~first:4 ~len:100 b);
    err (fun () -> M.with_range ~len:(-1) t);
    eq_bstr (M.with_index_range t) (Bstr.with_index_range b);
    eq_bstr
      (M.with_index_range ~first:1 ~last:3 t)
      (Bstr.with_index_range ~first:1 ~last:3 b);
    eq_bstr
      (M.with_index_range ~first:3 ~last:100 t)
      (Bstr.with_index_range ~first:3 ~last:100 b);
    eq (M.with_index_range ~first:4 ~last:1 t) ""

  let test_numbers name =
    let descr = {text|get_* and set_* on integers|text} in
    Test.test ~title:(name ^ "-numbers") ~descr @@ fun () ->
    let t = embed (String.make 16 '\000') in
    M.set_uint8 t 0 0xff;
    check (M.get_uint8 t 0 == 0xff);
    check (M.get_int8 t 0 == -1);
    M.set_int8 t 0 (-2);
    check (M.get_int8 t 0 == -2);
    check (M.get_uint8 t 0 == 0xfe);
    M.set_uint16_be t 1 0x1234;
    check (M.get_uint16_be t 1 == 0x1234);
    check (M.get_uint16_le t 1 == 0x3412);
    check (M.get_int16_be t 1 == 0x1234);
    M.set_uint16_le t 1 0x1234;
    check (M.get_uint16_le t 1 == 0x1234);
    check (M.get_uint16_be t 1 == 0x3412);
    M.set_int16_be t 1 0xffff;
    check (M.get_int16_be t 1 == -1);
    check (M.get_uint16_be t 1 == 0xffff);
    M.set_int16_le t 1 0xffff;
    check (M.get_int16_le t 1 == -1);
    M.set_int16_ne t 1 0x0102;
    check (M.get_int16_ne t 1 == 0x0102);
    check
      (M.get_uint16_ne t 1
      == if Sys.big_endian then M.get_uint16_be t 1 else M.get_uint16_le t 1);
    M.set_int32_be t 2 0x01020304l;
    check (M.get_int32_be t 2 = 0x01020304l);
    check (M.get_int32_le t 2 = 0x04030201l);
    M.set_int32_le t 2 0x01020304l;
    check (M.get_int32_le t 2 = 0x01020304l);
    M.set_int32_ne t 2 0x0a0b0c0dl;
    check (M.get_int32_ne t 2 = 0x0a0b0c0dl);
    check
      (M.get_int32_ne t 2
      = if Sys.big_endian then M.get_int32_be t 2 else M.get_int32_le t 2);
    M.set_int64_be t 4 0x0102030405060708L;
    check (M.get_int64_be t 4 = 0x0102030405060708L);
    check (M.get_int64_le t 4 = 0x0807060504030201L);
    M.set_int64_le t 4 0x0102030405060708L;
    check (M.get_int64_le t 4 = 0x0102030405060708L);
    M.set_int64_ne t 4 0x0102030405060708L;
    check
      (M.get_int64_ne t 4
      = if Sys.big_endian then M.get_int64_be t 4 else M.get_int64_le t 4);
    M.set_int16_le t 1 0x0102;
    check (M.get_uint8 t 1 == 0x02 && M.get_uint8 t 2 == 0x01);
    M.set_int16_be t 1 0x0102;
    check (M.get_uint8 t 1 == 0x01 && M.get_uint8 t 2 == 0x02);
    M.set_uint8 t 1 0x01;
    M.set_uint8 t 2 0x02;
    check (M.get_int16_le t 1 == 0x0201);
    check (M.get_int16_be t 1 == 0x0102);
    err (fun () -> M.get_int8 t 16);
    err (fun () -> M.get_uint8 t (-1));
    err (fun () -> M.get_uint16_be t 15);
    err (fun () -> M.get_int32_be t 13);
    err (fun () -> M.get_int64_be t 9);
    err (fun () -> M.set_uint8 t 16 0);
    err (fun () -> M.set_uint16_be t 15 0);
    err (fun () -> M.set_int32_be t 13 0l);
    err (fun () -> M.set_int64_be t 9 0L);
    check (M.get_int64_be t 8 = M.get_int64_be t 8)

  let test_predicates name =
    let descr = {text|predicates, comparisons and searches|text} in
    Test.test ~title:(name ^ "-predicates") ~descr @@ fun () ->
    let a = M.string "abc" and b = embed "abc" in
    check (M.equal a b);
    check (M.constant_equal a b);
    check (M.compare a b == 0);
    check (M.hash a == M.hash b);
    check (M.equal a (M.string "abd") == false);
    check (M.constant_equal a (M.string "abd") == false);
    check (M.constant_equal a (M.string "ab") == false);
    check
      (M.compare a (M.string "abd") == Bstr.compare (bstr "abc") (bstr "abd"));
    check (M.compare a (M.string "ab") == Bstr.compare (bstr "abc") (bstr "ab"));
    check
      (M.compare a (M.string "abcd") == Bstr.compare (bstr "abc") (bstr "abcd"));
    check
      (M.compare a (M.string "azc") == Bstr.compare (bstr "abc") (bstr "azc"));
    check
      (M.compare (M.string "azc") a == Bstr.compare (bstr "azc") (bstr "abc"));
    check
      (M.compare (M.string "0123456789") (M.string "012345678z")
      == Bstr.compare (bstr "0123456789") (bstr "012345678z"));
    check (M.equal M.empty M.empty);
    both "hello, world" @@ fun t ->
    let b = bstr "hello, world" in
    List.iter
      (fun affix ->
        check (M.is_prefix ~affix t == Bstr.is_prefix ~affix b);
        check (M.is_suffix ~affix t == Bstr.is_suffix ~affix b);
        check (M.is_infix ~affix t == Bstr.is_infix ~affix b))
      [
        ""; "h"; "hello"; "world"; "lo, wo"; "hello, world"; "hello, world!"
      ; "foo"; "bar"
      ];
    check (M.starts_with ~prefix:"hello" t);
    check (M.starts_with ~prefix:String.empty t);
    check (M.starts_with ~prefix:"world" t == false);
    check (M.ends_with ~suffix:"world" t);
    check (M.ends_with ~suffix:String.empty t);
    check (M.ends_with ~suffix:"hello" t == false);
    check (M.for_all (fun chr -> chr != '\000') t);
    check (M.for_all (fun chr -> chr == 'h') t == false);
    check (M.exists (fun chr -> chr == 'w') t);
    check (M.exists (fun chr -> chr == 'z') t == false);
    check (M.index t 'o' = Bstr.index b 'o');
    check (M.index t ~off:5 'o' = Bstr.index b ~off:5 'o');
    check (M.index t ~off:0 ~len:4 'o' = Bstr.index b ~off:0 ~len:4 'o');
    check (M.index t 'z' = None);
    check (M.contains t 'w' == Bstr.contains b 'w');
    check (M.contains t ~off:0 ~len:4 'w' == Bstr.contains b ~off:0 ~len:4 'w');
    err (fun () -> M.index t ~off:13 'o');
    err (fun () -> M.contains t ~off:0 ~len:13 'o')

  let test_extract name =
    let descr = {text|trim, span, cut, cuts and split_on_char|text} in
    Test.test ~title:(name ^ "-extract") ~descr @@ fun () ->
    both "  hello  " begin fun t ->
        let b = bstr "  hello  " in
        eq_bstr (M.trim t) (Bstr.trim b);
        eq_bstr
          (M.trim ~drop:(fun chr -> chr == ' ' || chr == 'h') t)
          (Bstr.trim ~drop:(fun chr -> chr == ' ' || chr == 'h') b)
      end;
    both "    " (fun t -> eq (M.trim t) "");
    both "aaabbb" begin fun t ->
        let b = bstr "aaabbb" in
        let sat chr = chr == 'a' in
        let a0, b0 = M.span ~sat t and a1, b1 = Bstr.span ~sat b in
        eq_bstr a0 a1;
        eq_bstr b0 b1;
        let a0, b0 = M.span ~rev:true ~sat:(fun chr -> chr == 'b') t
        and a1, b1 = Bstr.span ~rev:true ~sat:(fun chr -> chr == 'b') b in
        eq_bstr a0 a1;
        eq_bstr b0 b1;
        let a0, b0 = M.span ~min:4 ~sat t
        and a1, b1 = Bstr.span ~min:4 ~sat b in
        eq_bstr a0 a1;
        eq_bstr b0 b1;
        let a0, b0 = M.span ~max:2 ~sat t
        and a1, b1 = Bstr.span ~max:2 ~sat b in
        eq_bstr a0 a1;
        eq_bstr b0 b1;
        eq_bstr (M.take ~sat t) (Bstr.take ~sat b);
        eq_bstr (M.drop ~sat t) (Bstr.drop ~sat b);
        eq_bstr
          (M.take ~rev:true ~sat:(fun chr -> chr == 'b') t)
          (Bstr.take ~rev:true ~sat:(fun chr -> chr == 'b') b);
        eq_bstr
          (M.drop ~rev:true ~sat:(fun chr -> chr == 'b') t)
          (Bstr.drop ~rev:true ~sat:(fun chr -> chr == 'b') b);
        err (fun () -> M.span ~min:(-1) t);
        err (fun () -> M.span ~max:(-1) t)
      end;
    both "a=b=c" begin fun t ->
        let b = bstr "a=b=c" in
        (match (M.cut ~sep:"=" t, Bstr.cut ~sep:"=" b) with
        | Some (a0, b0), Some (a1, b1) -> eq_bstr a0 a1; eq_bstr b0 b1
        | _ -> check false);
        (match (M.cut ~rev:true ~sep:"=" t, Bstr.cut ~rev:true ~sep:"=" b) with
        | Some (a0, b0), Some (a1, b1) -> eq_bstr a0 a1; eq_bstr b0 b1
        | _ -> check false);
        check (M.cut ~sep:"z" t = None);
        err (fun () -> M.cut ~sep:"" t)
      end;
    both "a,b,,c" begin fun t ->
        let b = bstr "a,b,,c" in
        eq_list (M.cuts ~sep:"," t) (Bstr.cuts ~sep:"," b);
        eq_list
          (M.cuts ~empty:false ~sep:"," t)
          (Bstr.cuts ~empty:false ~sep:"," b);
        eq_list (M.cuts ~rev:true ~sep:"," t) (Bstr.cuts ~rev:true ~sep:"," b);
        eq_list
          (M.cuts ~rev:true ~empty:false ~sep:"," t)
          (Bstr.cuts ~rev:true ~empty:false ~sep:"," b);
        err (fun () -> M.cuts ~sep:"" t);
        eq_list (M.split_on_char ',' t) (Bstr.split_on_char ',' b);
        check (List.length (M.split_on_char ',' t) == 4)
      end;
    both "" @@ fun t ->
    eq (M.trim t) "";
    eq_list (M.split_on_char ',' t) (Bstr.split_on_char ',' (bstr ""))

  let test_concat name =
    let descr = {text|concat, append and extend|text} in
    Test.test ~title:(name ^ "-concat") ~descr @@ fun () ->
    let a = embed "foo" and b = embed "bar" and c = embed "" in
    let a' = bstr "foo" and b' = bstr "bar" and c' = bstr "" in
    eq_bstr (M.concat "-" [ a; b; c ]) (Bstr.concat "-" [ a'; b'; c' ]);
    eq_bstr (M.concat "" [ a; b ]) (Bstr.concat "" [ a'; b' ]);
    eq_bstr (M.concat "-" [ a ]) (Bstr.concat "-" [ a' ]);
    eq_bstr (M.concat "-" []) (Bstr.concat "-" []);
    eq (M.append a b) "foobar";
    eq (M.append a M.empty) "foo";
    eq_bstr (M.extend a 2 3) (Bstr.extend a' 2 3);
    eq_bstr (M.extend a (-1) 0) (Bstr.extend a' (-1) 0);
    eq_bstr (M.extend a 0 (-1)) (Bstr.extend a' 0 (-1));
    err (fun () -> M.extend a (-3) (-3));
    err (fun () -> Bstr.extend a' (-3) (-3));
    err (fun () -> M.extend a max_int max_int);
    let r = M.append a b in
    M.set r 0 'z'; eq a "foo"; eq r "zoobar"

  let test_traversals name =
    let descr = {text|iter, map, fold, filter and sequences|text} in
    Test.test ~title:(name ^ "-traversals") ~descr @@ fun () ->
    eq (M.of_seq Seq.empty) "";
    both "abcdef" @@ fun t ->
    let buf = Buffer.create 0x10 in
    M.iter (Buffer.add_char buf) t;
    check (String.equal (Buffer.contents buf) "abcdef");
    let buf = Buffer.create 0x10 in
    let fn idx chr = Buffer.add_string buf (strf "%d%c" idx chr) in
    M.iteri fn t;
    check (String.equal (Buffer.contents buf) "0a1b2c3d4e5f");
    eq (M.map Char.uppercase_ascii t) (String.map Char.uppercase_ascii "abcdef");
    let fn idx chr = if idx land 1 = 0 then Char.uppercase_ascii chr else chr in
    eq (M.mapi fn t) (String.mapi fn "abcdef");
    let fn acc chr = acc ^ String.make 1 chr in
    check (String.equal (M.fold_left fn "" t) "abcdef");
    let fn chr acc = acc ^ String.make 1 chr in
    check (String.equal (M.fold_right fn t "") "fedcba");
    let sat chr = chr != 'c' in
    eq (M.filter sat t) (string_filter sat "abcdef");
    eq (M.filter (fun _ -> false) t) "";
    eq (M.filter (fun _ -> true) t) "abcdef";
    let fn chr = if chr = 'c' then None else Some (Char.uppercase_ascii chr) in
    eq (M.filter_map fn t) "ABDEF";
    check (List.of_seq (M.to_seq t) = List.of_seq (String.to_seq "abcdef"));
    check (List.of_seq (M.to_seqi t) = List.of_seq (String.to_seqi "abcdef"));
    eq (M.of_seq (M.to_seq t)) "abcdef";
    let b = bstr "abcdef" in
    eq_bstr (M.map Char.uppercase_ascii t) (Bstr.map Char.uppercase_ascii b);
    let fn idx chr = if idx land 1 = 0 then Char.uppercase_ascii chr else chr in
    eq_bstr (M.mapi fn t) (Bstr.mapi fn b);
    eq_bstr (M.filter sat t) (Bstr.filter sat b);
    let fn chr = if chr = 'c' then None else Some (Char.uppercase_ascii chr) in
    eq_bstr (M.filter_map fn t) (Bstr.filter_map fn b);
    eq_bstr (M.append t t) (Bstr.append b b);
    let fn acc chr = acc ^ String.make 1 chr in
    check (String.equal (M.fold_left fn "" t) (Bstr.fold_left fn "" b));
    let fn chr acc = acc ^ String.make 1 chr in
    check (String.equal (M.fold_right fn t "") (Bstr.fold_right fn b ""));
    check (M.exists sat t == Bstr.exists sat b);
    check (M.exists (fun _ -> false) t == Bstr.exists (fun _ -> false) b);
    check (String.equal (M.hex t) (Bstr.hex b));
    check (M.hash t == Bstr.hash b);
    let buf = Buffer.create 0x10 and buf' = Buffer.create 0x10 in
    let fn buf idx chr = Buffer.add_string buf (strf "%d%c" idx chr) in
    M.iteri (fn buf) t;
    Bstr.iteri (fn buf') b;
    check (String.equal (Buffer.contents buf) (Buffer.contents buf'))

  let test_copies name =
    let descr = {text|fill, blit and blit_{from,to}_*|text} in
    Test.test ~title:(name ^ "-copies") ~descr @@ fun () ->
    let t = embed "abcdef" in
    M.fill t ~off:1 ~len:2 'z';
    eq t "azzdef";
    M.fill t 'y';
    eq t "yyyyyy";
    err (fun () -> M.fill t ~off:4 ~len:3 'x');
    err (fun () -> M.fill t ~off:(-1) 'x');
    let src = embed "0123456789" in
    let dst = embed "----" in
    M.blit src dst;
    eq dst "0123";
    M.blit (M.sub src ~off:6 ~len:2) dst;
    eq dst "6723";
    let dst = embed "------" in
    M.blit_from_bytes (Bytes.of_string "hello") ~src_off:0 dst 5;
    eq dst "hello-";
    M.blit_from_bytes (Bytes.of_string "hello") ~src_off:1 dst ~dst_off:1 4;
    eq dst "hello-";
    err (fun () ->
        M.blit_from_bytes (Bytes.of_string "hello") ~src_off:0 dst ~dst_off:2 5);
    err (fun () -> M.blit_from_bytes (Bytes.of_string "hello") ~src_off:1 dst 5);
    err (fun () ->
        M.blit_from_bytes (Bytes.of_string "hello") ~src_off:0 dst (-1));
    let dst = embed "------" in
    M.blit_from_string "world" ~src_off:0 dst ~dst_off:1 5;
    eq dst "-world";
    err (fun () -> M.blit_from_string "world" ~src_off:0 dst ~dst_off:2 5);
    let src = embed "abcdef" in
    let dst = Bytes.make 6 '-' in
    M.blit_to_bytes src dst ~dst_off:0 ~len:6;
    check (String.equal (Bytes.to_string dst) "abcdef");
    let dst = Bytes.make 6 '-' in
    M.blit_to_bytes src ~src_off:2 dst ~dst_off:1 ~len:2;
    check (String.equal (Bytes.to_string dst) "-cd---");
    err (fun () -> M.blit_to_bytes src ~src_off:2 dst ~dst_off:0 ~len:5);
    err (fun () -> M.blit_to_bytes src ~src_off:0 dst ~dst_off:2 ~len:6)

  let test_random name =
    let descr = {text|comparison with Bstr on random inputs|text} in
    Test.test ~title:(name ^ "-random") ~descr @@ fun () ->
    let g = Random.State.make [| 0xdeadbeef |] in
    let result = ref true in
    let check v = result := !result && v in
    let eq t str = result := !result && String.equal (M.to_string t) str in
    let eq_bstr t bstr =
      let v = String.equal (M.to_string t) (Bstr.to_string bstr) in
      result := !result && v
    in
    let eq_list lst bstrs =
      result := !result && List.length lst == List.length bstrs;
      List.iter2 eq_bstr lst bstrs
    in
    for _ = 0 to 200 do
      let str = random_string g (Random.State.int g 32) in
      let b = bstr str in
      both str @@ fun t ->
      eq t str;
      eq_bstr (M.trim t) (Bstr.trim b);
      eq_list (M.split_on_char ',' t) (Bstr.split_on_char ',' b);
      eq_list (M.cuts ~sep:"ab" t) (Bstr.cuts ~sep:"ab" b);
      eq_list
        (M.cuts ~rev:true ~empty:false ~sep:"ab" t)
        (Bstr.cuts ~rev:true ~empty:false ~sep:"ab" b);
      begin match (M.cut ~sep:"ab" t, Bstr.cut ~sep:"ab" b) with
      | Some (a0, b0), Some (a1, b1) -> eq_bstr a0 a1; eq_bstr b0 b1
      | None, None -> check true
      | _ -> check false
      end;
      check (M.index t 'a' = Bstr.index b 'a');
      check (M.contains t ',' == Bstr.contains b ',');
      check (M.is_infix ~affix:"ab" t == Bstr.is_infix ~affix:"ab" b);
      check (M.is_prefix ~affix:"ab" t == Bstr.is_prefix ~affix:"ab" b);
      check (M.is_suffix ~affix:"ab" t == Bstr.is_suffix ~affix:"ab" b);
      eq_bstr
        (M.take ~sat:(fun chr -> chr == 'a') t)
        (Bstr.take ~sat:(fun chr -> chr == 'a') b);
      check (String.equal (M.hex t) (string_hex str));
      check (String.equal (M.hex t) (Bstr.hex b));
      check (M.hash t == M.hash (M.string str));
      check (M.hash t == Bstr.hash b);
      eq (M.copy t) str;
      eq (M.of_seq (M.to_seq t)) str
    done;
    Test.check !result

  let tests name =
    [
      test_base name; test_views name; test_numbers name; test_predicates name
    ; test_extract name; test_concat name; test_traversals name
    ; test_copies name; test_random name
    ]
end

module Test_bstr = Make (Slice_bstr)
module Test_bytes = Make (Slice_bytes)

let test_make =
  let descr = {text|Slice_{bstr,bytes}.make|text} in
  Test.test ~title:"make" ~descr @@ fun () ->
  let err : type a. (unit -> a) -> unit =
   fun fn ->
    match fn () with
    | exception Invalid_argument _ -> check true
    | _ -> check false
  in
  let seq = String.equal in
  let bstr = Bstr.of_string "abcdef" in
  check (seq (Slice_bstr.to_string (Slice_bstr.make bstr)) "abcdef");
  check (seq (Slice_bstr.to_string (Slice_bstr.make ~off:2 bstr)) "cdef");
  check (seq (Slice_bstr.to_string (Slice_bstr.make ~off:2 ~len:2 bstr)) "cd");
  err (fun () -> Slice_bstr.make ~off:2 ~len:5 bstr);
  err (fun () -> Slice_bstr.make ~off:(-1) bstr);
  let bytes = Bytes.of_string "abcdef" in
  check (seq (Slice_bytes.to_string (Slice_bytes.make bytes)) "abcdef");
  check (seq (Slice_bytes.to_string (Slice_bytes.make ~off:2 bytes)) "cdef");
  let sbytes = Slice_bytes.make ~off:2 ~len:2 bytes in
  check (seq (Slice_bytes.to_string sbytes) "cd");
  err (fun () -> Slice_bytes.make ~off:2 ~len:5 bytes);
  err (fun () -> Slice_bytes.make ~off:(-1) bytes);
  Slice_bytes.set (Slice_bytes.make ~off:2 ~len:2 bytes) 0 'z';
  check (seq (Bytes.to_string bytes) "abzdef");
  Slice_bstr.set (Slice_bstr.make ~off:2 ~len:2 bstr) 0 'z';
  check (seq (Bstr.to_string bstr) "abzdef")

let test_overlap =
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
  let run : type a. (module Slice.S with type buf = a) -> a -> unit =
   fun (module M) buf ->
    let t = M.make buf in
    let ab = M.sub t ~off:5 ~len:5 in
    let cd = M.sub t ~off:0 ~len:5 in
    test (M.overlap ab cd) None;
    let ab = M.sub t ~off:0 ~len:5 in
    let cd = M.sub t ~off:5 ~len:5 in
    test (M.overlap ab cd) None;
    let ab = M.sub t ~off:0 ~len:6 in
    let cd = M.sub t ~off:5 ~len:5 in
    test (M.overlap ab cd) (Some (1, 5, 0));
    let ab = M.sub t ~off:5 ~len:5 in
    let cd = M.sub t ~off:0 ~len:6 in
    test (M.overlap ab cd) (Some (1, 0, 5));
    let ab = M.sub t ~off:0 ~len:8 in
    let cd = M.sub t ~off:2 ~len:8 in
    test (M.overlap ab cd) (Some (6, 2, 0));
    let ab = M.sub t ~off:0 ~len:10 in
    let cd = M.sub t ~off:2 ~len:8 in
    test (M.overlap ab cd) (Some (8, 2, 0));
    let ab = M.sub t ~off:0 ~len:10 in
    let cd = M.sub t ~off:2 ~len:6 in
    test (M.overlap ab cd) (Some (6, 2, 0));
    let ab = M.sub t ~off:0 ~len:8 in
    let cd = M.sub t ~off:0 ~len:10 in
    test (M.overlap ab cd) (Some (8, 0, 0));
    let ab = M.sub t ~off:2 ~len:6 in
    let cd = M.sub t ~off:0 ~len:10 in
    test (M.overlap ab cd) (Some (6, 0, 2));
    let ab = M.sub t ~off:2 ~len:8 in
    let cd = M.sub t ~off:0 ~len:10 in
    test (M.overlap ab cd) (Some (8, 0, 2));
    let ab = M.sub t ~off:2 ~len:8 in
    let cd = M.sub t ~off:0 ~len:8 in
    test (M.overlap ab cd) (Some (6, 0, 2));
    let ab = M.sub t ~off:2 ~len:4 in
    let cd = M.sub t ~off:4 ~len:4 in
    test (M.overlap ab cd) (Some (2, 2, 0));
    test (M.overlap t (M.string (String.make 10 '\000'))) None
  in
  run (module Slice_bstr) (Bstr.make 10 '\000');
  run (module Slice_bytes) (Bytes.make 10 '\000')

let ( / ) = Filename.concat

let () =
  let tests =
    Test_bstr.tests "bstr"
    @ Test_bytes.tests "bytes"
    @ [ test_make; test_overlap ]
  in
  let ({ Test.directory } as runner) = Test.runner (Sys.getcwd () / "_tests") in
  let run idx test =
    Format.printf "test%03d: %!" (succ idx);
    Test.run runner test;
    Format.printf "ok\n%!"
  in
  Format.printf "Run tests into %s\n%!" directory;
  List.iteri run tests
