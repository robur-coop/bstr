let int_arity = ref 12
let static_arity = ref 4
let max_arity = ref None
let top = ref false
let input = ref None
let output = ref None
let marker = "(* static_int_operation *)"
let marker_top = "(* static_int_operation_top *)"

let pattern ~ints ~statics =
  let buf = Buffer.create 0x100 in
  for k = 1 to ints do
    Buffer.add_string buf
      (Printf.sprintf "Sint ({ iq= q%d; ish= s%d; imask= m%d }, " k k k)
  done;
  for k = 1 to statics do
    Buffer.add_string buf (Printf.sprintf "S1 (r%d, " k)
  done;
  Buffer.add_string buf "S0";
  Buffer.add_string buf (String.make (ints + statics) ')');
  Buffer.contents buf

let apply_ctor buf ~ints ~statics =
  Buffer.add_string buf "ctor";
  for k = 1 to ints + statics do
    Buffer.add_string buf (Printf.sprintf " v%d" k)
  done

let bindings buf ~ints ~statics ~bstr ~off =
  for k = 1 to ints do
    Buffer.add_string buf
      (Printf.sprintf "        let v%d = int_exec %s %s q%d s%d m%d in\n" k bstr
         off k k k)
  done;
  for k = 1 to statics do
    Buffer.add_string buf
      (Printf.sprintf "        let v%d = static_exec r%d %s %s in\n" (ints + k)
         k bstr off)
  done

let case ~ints ~statics =
  let buf = Buffer.create 0x100 in
  Buffer.add_string buf
    (Printf.sprintf "    | %s ->\n" (pattern ~ints ~statics));
  Buffer.add_string buf "      fun b o ->\n";
  bindings buf ~ints ~statics ~bstr:"b" ~off:"o";
  Buffer.add_string buf "        ";
  apply_ctor buf ~ints ~statics;
  Buffer.add_char buf '\n';
  Buffer.contents buf

let case_top ~ints ~statics =
  let buf = Buffer.create 0x100 in
  Buffer.add_string buf
    (Printf.sprintf "    | %s ->\n" (pattern ~ints ~statics));
  Buffer.add_string buf "      fun buf pos ->\n";
  Buffer.add_string buf "        let o = !pos in\n";
  Buffer.add_string buf "        let limit = Off.unsafe (S.length buf) in\n";
  Buffer.add_string buf "        if Off.(o +> n > limit) then\n";
  Buffer.add_string buf
    "          truncated_in rname ~limit ~offset:o ~need:n;\n";
  bindings buf ~ints ~statics ~bstr:"buf" ~off:"o";
  Buffer.add_string buf "        let v = ";
  apply_ctor buf ~ints ~statics;
  Buffer.add_string buf " in\n";
  Buffer.add_string buf "        pos := Off.(o +> n);\n";
  Buffer.add_string buf "        v\n";
  Buffer.contents buf

let generate ~top oc =
  let max_arity =
    match !max_arity with
    | Some value -> value
    | None -> Stdlib.max !int_arity !static_arity
  in
  let name = if top then "static_apply_top" else "static_apply" in
  let case = if top then case_top else case in
  output_string oc (Printf.sprintf "    (* BEGIN GENERATED %s (Sint) *)\n" name);
  for ints = 1 to !int_arity do
    for statics = 0 to !static_arity do
      if ints + statics <= max_arity then output_string oc (case ~ints ~statics)
    done
  done;
  output_string oc (Printf.sprintf "    (* END GENERATED %s (Sint) *)\n" name)

let run () =
  let oc, oc_finally =
    match !output with
    | Some filename ->
        let oc = open_out filename in
        let finally () = close_out oc in
        (oc, finally)
    | None -> (stdout, ignore)
  in
  Fun.protect ~finally:oc_finally @@ fun () ->
  match !input with
  | None -> generate ~top:!top oc
  | Some filename ->
      let ic = open_in_bin filename in
      let finally () = close_in ic in
      Fun.protect ~finally @@ fun () ->
      let rec go () =
        match input_line ic with
        | line ->
            let line' = String.trim line in
            if line' = marker then generate ~top:false oc
            else if line' = marker_top then generate ~top:true oc
            else (output_string oc line; output_string oc "\n");
            go ()
        | exception End_of_file -> ()
      in
      go ()

let usage =
  "static_int [-int arity] [-static arity] [-max arity] [-top] [-i input] [-o \
   output] generates the unrolled [Sint]/[S1] cases of [static_apply] and \
   [static_apply_top]. With [input], the line containing \"" ^ marker
  ^ "\" is replaced by the [static_apply] cases and the one containing \""
  ^ marker_top
  ^ "\" by the [static_apply_top] cases. Without [input], a single set of \
     cases is emitted: the [static_apply] ones, or the [static_apply_top] ones \
     with [-top]."

let failwith fmt = Format.kasprintf failwith fmt

let to_arity name var str =
  match int_of_string_opt str with
  | Some value when value >= 0 -> var := value
  | Some _ | None ->
      failwith "%s expects a non-negative integer, got %S" name str

let to_existing_filename var str =
  if Sys.file_exists str && Sys.is_directory str = false then var := Some str
  else failwith "%S does not exist" str

let to_non_existing_filename var str =
  if Sys.file_exists str = false then var := Some str
  else failwith "%S already exists" str

let args =
  [
    ( "-int"
    , Arg.String (to_arity "-int" int_arity)
    , "how many int arguments to unroll" )
  ; ( "-static"
    , Arg.String (to_arity "-static" static_arity)
    , "how many static arguments to unroll" )
  ; ( "-max"
    , Arg.String
        (fun str ->
          let var = ref 0 in
          to_arity "-max" var str;
          max_arity := Some !var)
    , "the maximal arity of a generated case (defaults to the largest of -int \
       and -static)" )
  ; ( "-top"
    , Arg.Set top
    , "emit the [static_apply_top] cases instead of the [static_apply] ones \
       (only meaningful without an input)" )
  ; ("-i", Arg.String (to_existing_filename input), "the input")
  ; ("-o", Arg.String (to_non_existing_filename output), "the output")
  ]

let () =
  Arg.parse args ignore usage;
  run ()
