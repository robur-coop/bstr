(*
 * Copyright (c) 2026 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let module_to_be_replaced = ref "S"
let new_module = ref "String"
let input = ref None
let output = ref None

let is_ident = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let replace line module_to_be_replaced new_module =
  let len = String.length line in
  let mlen = String.length module_to_be_replaced in
  let buf = Buffer.create len in
  let matches idx =
    idx + mlen < len
    && (idx = 0 || is_ident line.[idx - 1] = false)
    && line.[idx + mlen] = '.'
    && String.sub line idx mlen = module_to_be_replaced
  in
  let rec go idx =
    if idx >= len then Buffer.contents buf
    else if matches idx then begin
      Buffer.add_string buf new_module;
      Buffer.add_char buf '.';
      go (idx + mlen + 1)
    end
    else begin
      Buffer.add_char buf line.[idx];
      go (idx + 1)
    end
  in
  go 0

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
