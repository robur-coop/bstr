let module_to_be_replaced = ref "S"
let new_module = ref "String"
let input = ref None
let output = ref None

let splits ~sep str =
  let sep_len = String.length sep in
  if sep_len = 0 then invalid_arg "splits: empty separator not allowed";
  let str_len = String.length str in
  let max_sep_idx = sep_len - 1 in
  let max_str_idx = str_len - sep_len in
  let add_sub str ~start ~stop acc =
    if start = stop then "" :: acc
    else String.sub str start (stop - start) :: acc
  in
  let rec check_sep start i k acc =
    if k > max_sep_idx then
      let new_start = i + sep_len in
      scan new_start new_start (add_sub str ~start ~stop:i acc)
    else if str.[i + k] = sep.[k] then check_sep start i (k + 1) acc
    else scan start (i + 1) acc
  and scan start i acc =
    if i > max_str_idx then
      if start = 0 then [ str ]
      else List.rev (add_sub str ~start ~stop:str_len acc)
    else if str.[i] = sep.[0] then check_sep start i 1 acc
    else scan start (i + 1) acc
  in
  scan 0 0 []

let replace line module_to_be_replaced new_module =
  let line = splits ~sep:(module_to_be_replaced ^ ".") line in
  String.concat (new_module ^ ".") line

let run () =
  let ic, ic_finally =
    match !input with
    | Some filename ->
        let ic = open_in_bin filename in
        let finally () = close_in ic in
        (ic, finally)
    | None -> (stdin, ignore)
  in
  let oc, oc_finally =
    match !output with
    | Some filename ->
        let oc = open_out filename in
        let finally () = close_out oc in
        (oc, finally)
    | None -> (stdout, ignore)
  in
  Fun.protect ~finally:ic_finally @@ fun () ->
  Fun.protect ~finally:oc_finally @@ fun () ->
  let rec go () =
    match input_line ic with
    | line ->
        let line = replace line !module_to_be_replaced !new_module in
        output_string oc line; output_string oc "\n"; go ()
    | exception End_of_file -> ()
  in
  go ()

let usage =
  "generate [-m module_to_be_replaced] [-n new_module] [-i input] [-o output] \
   replaces all occurrences of [module_to_be_replaced] by [new_module] in \
   [input] to [output]."

let failwith fmt = Format.kasprintf failwith fmt

let to_existing_filename var str =
  if Sys.file_exists str && Sys.is_directory str = false then var := Some str
  else failwith "%S does not exist" str

let to_non_existing_filename var str =
  if Sys.file_exists str = false then var := Some str
  else failwith "%S already exists" str

let args =
  [
    ("-m", Arg.Set_string module_to_be_replaced, "the module to be replaced")
  ; ("-n", Arg.Set_string new_module, "the new module")
  ; ("-i", Arg.String (to_existing_filename input), "the input")
  ; ("-o", Arg.String (to_non_existing_filename output), "the output")
  ]

let () =
  Arg.parse args ignore usage;
  run ()
