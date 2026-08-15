open Bechamel
open Toolkit

type ipv4 = {
    version: int
  ; ihl: int
  ; tos: int
  ; total_length: int
  ; id: int
  ; flags: int
  ; ttl: int
  ; protocol: int
  ; checksum: int
  ; src: int32
  ; dst: int32
}

[@@@ocamlformat "disable"]

let packet =
  "\x45\x00\x00\x3c\
   \x1c\x46\x40\x00\
   \x40\x06\xb1\xe6\
   \xc0\xa8\x00\x68\
   \xc0\xa8\x00\x01"
[@@@ocamlformat "enable"]

let handwritten_decode bstr =
  let vihl = Bstr.get_uint8 bstr 0 in
  let tos = Bstr.get_uint8 bstr 1 in
  let total_length = Bstr.get_uint16_be bstr 2 in
  let id = Bstr.get_uint16_be bstr 4 in
  let ff = Bstr.get_uint16_be bstr 6 in
  let ttl = Bstr.get_uint8 bstr 8 in
  let protocol = Bstr.get_uint8 bstr 9 in
  let checksum = Bstr.get_uint16_be bstr 10 in
  let src = Bstr.get_int32_be bstr 12 in
  let dst = Bstr.get_int32_be bstr 16 in
  {
    version= vihl lsr 4
  ; ihl= vihl land 0x0f
  ; tos
  ; total_length
  ; id
  ; flags= ff lsr 13
  ; ttl
  ; protocol
  ; checksum
  ; src
  ; dst
  }

let handwritten_encode v bstr =
  Bstr.set_uint8 bstr 0 ((v.version lsl 4) lor v.ihl);
  Bstr.set_uint8 bstr 1 v.tos;
  Bstr.set_uint16_be bstr 2 v.total_length;
  Bstr.set_uint16_be bstr 4 v.id;
  Bstr.set_uint16_be bstr 6 (v.flags lsl 13);
  Bstr.set_uint8 bstr 8 v.ttl;
  Bstr.set_uint8 bstr 9 v.protocol;
  Bstr.set_uint16_be bstr 10 v.checksum;
  Bstr.set_int32_be bstr 12 v.src;
  Bstr.set_int32_be bstr 16 v.dst

let value = handwritten_decode (Bstr.of_string packet)
let len = String.length packet
let hi n = (n lsr 8) land 0xff
let lo n = n land 0xff

let bin_ipv4 =
  let fn vihl tos total_length id ff ttl protocol checksum src dst =
    {
      version= vihl lsr 4
    ; ihl= vihl land 0x0f
    ; tos
    ; total_length
    ; id
    ; flags= ff lsr 13
    ; ttl
    ; protocol
    ; checksum
    ; src
    ; dst
    }
  in
  let open Bin in
  record ~name:"ipv4" fn
  |+ field ~name:"vihl" uint8 (fun t -> (t.version lsl 4) lor t.ihl)
  |+ field ~name:"tos" uint8 (fun t -> t.tos)
  |+ field ~name:"total_length" beuint16 (fun t -> t.total_length)
  |+ field ~name:"id" beuint16 (fun t -> t.id)
  |+ field ~name:"flags" beuint16 (fun t -> t.flags lsl 13)
  |+ field ~name:"ttl" uint8 (fun t -> t.ttl)
  |+ field ~name:"protocol" uint8 (fun t -> t.protocol)
  |+ field ~name:"checksum" beuint16 (fun t -> t.checksum)
  |+ field ~name:"src" beint32 (fun t -> t.src)
  |+ field ~name:"dst" beint32 (fun t -> t.dst)
  |> sealr

let bin_decode_bstr = Bin.Staged.unstage (Bin.decode_bstr bin_ipv4)
let bin_decode_str = Bin.Staged.unstage (Bin.decode bin_ipv4)
let bin_encode_bstr = Bin.Staged.unstage (Bin.encode_bstr bin_ipv4)

let repr_ipv4 =
  let fn vihl tos tl_hi tl_lo id_hi id_lo ff_hi ff_lo ttl protocol ck_hi ck_lo
      src dst =
    {
      version= vihl lsr 4
    ; ihl= vihl land 0x0f
    ; tos
    ; total_length= (tl_hi lsl 8) lor tl_lo
    ; id= (id_hi lsl 8) lor id_lo
    ; flags= ((ff_hi lsl 8) lor ff_lo) lsr 13
    ; ttl
    ; protocol
    ; checksum= (ck_hi lsl 8) lor ck_lo
    ; src
    ; dst
    }
  in
  let open Repr in
  let byte = map char Char.code Char.chr in
  record "ipv4" fn
  |+ field "version_ihl" byte (fun t -> (t.version lsl 4) lor t.ihl)
  |+ field "tos" byte (fun t -> t.tos)
  |+ field "total_length_hi" byte (fun t -> hi t.total_length)
  |+ field "total_length_lo" byte (fun t -> lo t.total_length)
  |+ field "id_hi" byte (fun t -> hi t.id)
  |+ field "id_lo" byte (fun t -> lo t.id)
  |+ field "flags_hi" byte (fun t -> hi (t.flags lsl 13))
  |+ field "flags_lo" byte (fun t -> lo (t.flags lsl 13))
  |+ field "ttl" byte (fun t -> t.ttl)
  |+ field "protocol" byte (fun t -> t.protocol)
  |+ field "checksum_hi" byte (fun t -> hi t.checksum)
  |+ field "checksum_lo" byte (fun t -> lo t.checksum)
  |+ field "src" int32 (fun t -> t.src)
  |+ field "dst" int32 (fun t -> t.dst)
  |> sealr

let repr_decode = Repr.unstage (Repr.decode_bin repr_ipv4)
let repr_encode = Repr.unstage (Repr.encode_bin repr_ipv4)
let repr_to_string = Repr.unstage (Repr.to_bin_string repr_ipv4)
let repr_buffer = Buffer.create len

let repr_encode_buffer v =
  Buffer.clear repr_buffer;
  repr_encode v (Buffer.add_string repr_buffer)

let data_encoding_ipv4 =
  let open Data_encoding in
  let fwd t =
    ( (t.version lsl 4) lor t.ihl
    , t.tos
    , t.total_length
    , t.id
    , t.flags lsl 13
    , t.ttl
    , t.protocol
    , t.checksum
    , t.src
    , t.dst )
  in
  let bwd (vihl, tos, total_length, id, ff, ttl, protocol, checksum, src, dst) =
    {
      version= vihl lsr 4
    ; ihl= vihl land 0x0f
    ; tos
    ; total_length
    ; id
    ; flags= ff lsr 13
    ; ttl
    ; protocol
    ; checksum
    ; src
    ; dst
    }
  in
  conv fwd bwd
    (tup10 uint8 uint8 uint16 uint16 uint16 uint8 uint8 uint16 int32 int32)

let data_encoding_decode str =
  Data_encoding.Binary.of_string_exn data_encoding_ipv4 str

let data_encoding_encode v =
  Data_encoding.Binary.to_string_exn data_encoding_ipv4 v

let data_encoding_buffer = Bytes.create len

let data_encoding_encode_in_place =
 fun v ->
  let offset = 0 in
  let allowed_bytes = len in
  let buf = data_encoding_buffer in
  let open Data_encoding.Binary in
  let result = make_writer_state buf ~offset ~allowed_bytes in
  match result with
  | None -> assert false
  | Some st -> ignore (Data_encoding.Binary.write_exn data_encoding_ipv4 v st)

let wire_ipv4 =
  let open Wire in
  let f_vihl = Field.v "VersionIHL" uint8 in
  let f_tos = Field.v "TOS" uint8 in
  let f_total_length = Field.v "TotalLength" uint16be in
  let f_id = Field.v "Id" uint16be in
  let f_flags = Field.v "FlagsFragment" uint16be in
  let f_ttl = Field.v "TTL" uint8 in
  let f_protocol = Field.v "Protocol" uint8 in
  let f_checksum = Field.v "Checksum" uint16be in
  let f_src = Field.v "Src" int32be in
  let f_dst = Field.v "Dst" int32be in
  let open Codec in
  let fn vihl tos total_length id ff ttl protocol checksum src dst =
    {
      version= vihl lsr 4
    ; ihl= vihl land 0x0f
    ; tos
    ; total_length
    ; id
    ; flags= ff lsr 13
    ; ttl
    ; protocol
    ; checksum
    ; src= Stdlib.Int32.of_int src
    ; dst= Stdlib.Int32.of_int dst
    }
  in
  v "IPv4" fn
    [
      (f_vihl $ fun t -> (t.version lsl 4) lor t.ihl); (f_tos $ fun t -> t.tos)
    ; (f_total_length $ fun t -> t.total_length); (f_id $ fun t -> t.id)
    ; (f_flags $ fun t -> t.flags lsl 13); (f_ttl $ fun t -> t.ttl)
    ; (f_protocol $ fun t -> t.protocol); (f_checksum $ fun t -> t.checksum)
    ; (f_src $ fun t -> Stdlib.Int32.to_int t.src)
    ; (f_dst $ fun t -> Stdlib.Int32.to_int t.dst)
    ]

let wire_decode buf = Wire.Codec.decode_exn wire_ipv4 buf 0
let wire_buffer = Bytes.create len
let wire_encode v = Wire.Codec.encode wire_ipv4 v wire_buffer 0

let angstrom =
  let open Angstrom in
  let ( let* ) = ( >>= ) in
  let* vihl = any_uint8 in
  let* tos = any_uint8 in
  let* total_length = BE.any_uint16 in
  let* id = BE.any_uint16 in
  let* ff = BE.any_uint16 in
  let* ttl = any_uint8 in
  let* protocol = any_uint8 in
  let* checksum = BE.any_uint16 in
  let* src = BE.any_int32 in
  let* dst = BE.any_int32 in
  return
    {
      version= vihl lsr 4
    ; ihl= vihl land 0x0f
    ; tos
    ; total_length
    ; id
    ; flags= ff lsr 13
    ; ttl
    ; protocol
    ; checksum
    ; src
    ; dst
    }

let angstrom_decode str =
  match Angstrom.parse_string ~consume:All angstrom str with
  | Ok v -> v
  | Error msg -> failwith msg

let faraday_encode v =
  let t = Faraday.create len in
  Faraday.write_uint8 t ((v.version lsl 4) lor v.ihl);
  Faraday.write_uint8 t v.tos;
  Faraday.BE.write_uint16 t v.total_length;
  Faraday.BE.write_uint16 t v.id;
  Faraday.BE.write_uint16 t (v.flags lsl 13);
  Faraday.write_uint8 t v.ttl;
  Faraday.write_uint8 t v.protocol;
  Faraday.BE.write_uint16 t v.checksum;
  Faraday.BE.write_uint32 t v.src;
  Faraday.BE.write_uint32 t v.dst;
  Faraday.serialize_to_string t

let cstruct cs =
  let vihl = Cstruct.get_uint8 cs 0 in
  let tos = Cstruct.get_uint8 cs 1 in
  let total_length = Cstruct.BE.get_uint16 cs 2 in
  let id = Cstruct.BE.get_uint16 cs 4 in
  let ff = Cstruct.BE.get_uint16 cs 6 in
  let ttl = Cstruct.get_uint8 cs 8 in
  let protocol = Cstruct.get_uint8 cs 9 in
  let checksum = Cstruct.BE.get_uint16 cs 10 in
  let src = Cstruct.BE.get_uint32 cs 12 in
  let dst = Cstruct.BE.get_uint32 cs 16 in
  {
    version= vihl lsr 4
  ; ihl= vihl land 0x0f
  ; tos
  ; total_length
  ; id
  ; flags= ff lsr 13
  ; ttl
  ; protocol
  ; checksum
  ; src
  ; dst
  }

let () =
  let bstr = Bstr.of_string packet in
  let check name v = if v <> value then failwith ("decode: " ^ name) in
  check "bin (bstr)" (bin_decode_bstr bstr (ref Bin.Off.zero));
  check "bin (string)" (bin_decode_str packet (ref Bin.Off.zero));
  check "repr" (repr_decode packet (ref 0));
  check "data-encoding" (data_encoding_decode packet);
  check "wire" (wire_decode (Bytes.of_string packet));
  check "angstrom" (angstrom_decode packet);
  check "cstruct" (cstruct (Cstruct.of_string packet));
  let check name str = if str <> packet then failwith ("encode: " ^ name) in
  let buf = Bstr.create len in
  bin_encode_bstr value buf (ref Bin.Off.zero);
  check "bin (bstr)" (Bstr.to_string bstr);
  repr_encode_buffer value;
  check "repr" (Buffer.contents repr_buffer);
  check "repr (to_bin_string)" (repr_to_string value);
  check "data-encoding" (data_encoding_encode value);
  data_encoding_encode_in_place value;
  check "data-encoding (in place)" (Bytes.to_string data_encoding_buffer);
  Bytes.fill data_encoding_buffer 0 len '\x00';
  wire_encode value;
  check "wire" (Bytes.to_string wire_buffer);
  check "faraday" (faraday_encode value);
  let buf = Bstr.create len in
  handwritten_encode value buf;
  check "handwritten" (Bstr.to_string buf)

let decode_cases : (string * (unit -> ipv4)) list =
  let bstr = Bstr.of_string packet in
  let buf = Bytes.of_string packet in
  let cs = Cstruct.of_string packet in
  [
    ("bin (bstr)", fun () -> bin_decode_bstr bstr (ref Bin.Off.zero))
  ; ("bin (string)", fun () -> bin_decode_str packet (ref Bin.Off.zero))
  ; ("repr", fun () -> repr_decode packet (ref 0))
  ; ("data-encoding", fun () -> data_encoding_decode packet)
  ; ("wire", fun () -> wire_decode buf)
  ; ("angstrom", fun () -> angstrom_decode packet)
  ; ("cstruct", fun () -> cstruct cs)
  ; ("handwritten", fun () -> handwritten_decode bstr)
  ]

let encode_cases : (string * (unit -> unit)) list =
  let bstr = Bstr.create len in
  [
    ("bin (bstr)", fun () -> bin_encode_bstr value bstr (ref Bin.Off.zero))
  ; ("repr", fun () -> repr_encode_buffer value)
  ; ("repr (string)", fun () -> ignore (repr_to_string value))
  ; ("data-encoding", fun () -> data_encoding_encode_in_place value)
  ; ("data-encoding (string)", fun () -> ignore (data_encoding_encode value))
  ; ("wire", fun () -> wire_encode value)
  ; ("faraday", fun () -> ignore (faraday_encode value))
  ; ("handwritten", fun () -> handwritten_encode value bstr)
  ]

let tests cases =
  List.map (fun (name, fn) -> Test.make ~name (Staged.stage fn)) cases

let benchmark tests =
  let bootstrap = 0 and r_square = true and predictors = Measure.[| run |] in
  let ols = Analyze.ols ~bootstrap ~r_square ~predictors in
  let instances = Instance.[ monotonic_clock ] in
  let limit = 3000
  and stabilize = true
  and quota = Time.second 1.0
  and kde = Some 1000 in
  let cfg = Benchmark.cfg ~limit ~stabilize ~quota ~kde () in
  let raw = Benchmark.all cfg instances tests in
  let res = List.map (fun i -> Analyze.all ols i raw) instances in
  (Analyze.merge ols instances res, raw)

let nothing _ = Ok ()
let compare = String.compare

let () =
  let tests =
    match Sys.argv with
    | [| _; "encode" |] -> Test.make_grouped ~name:"encode" (tests encode_cases)
    | _ -> Test.make_grouped ~name:"decode" (tests decode_cases)
  in
  let res = benchmark tests in
  let res =
    let open Bechamel_js in
    let dst = Channel stdout
    and x_label = Measure.run
    and y_label = Measure.label Instance.monotonic_clock in
    emit ~dst nothing ~compare ~x_label ~y_label res
  in
  match res with Ok () -> () | Error (`Msg msg) -> failwith msg
