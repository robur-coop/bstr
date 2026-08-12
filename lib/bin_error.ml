type kind =
  | Truncated of { need: int; have: int }
  | Out_of_range of { kind: string; value: string }
  | Unexpected_tag of { tag: int; expected: int list }
  | Length_mismatch of { expected: int; got: int }
  | Msg of string

type t = { context: string list; offset: int; kind: kind }

let pp_kind ppf = function
  | Truncated { need; have } ->
      Format.fprintf ppf "truncated input: %d byte(s) needed, %d available" need
        have
  | Out_of_range { kind; value } ->
      Format.fprintf ppf "value %s is out of range for %s" value kind
  | Unexpected_tag { tag; expected } ->
      let pp_sep ppf () = Format.fprintf ppf ", " in
      Format.fprintf ppf "unexpected tag %d, expected one of %a" tag
        (Format.pp_print_list ~pp_sep Format.pp_print_int)
        expected
  | Length_mismatch { expected; got } ->
      Format.fprintf ppf "expected %d element(s), got %d" expected got
  | Msg msg -> Format.pp_print_string ppf msg

let pp ppf { context; offset; kind } =
  Format.fprintf ppf "@[<v>%a@, at byte %d" pp_kind kind offset;
  if context <> [] then Format.fprintf ppf "@,in %s" (String.concat "." context);
  Format.fprintf ppf "@]"

exception Error of t

let to_string t = Format.asprintf "%a" pp t

let msgf fmt =
  let fn msg =
    raise_notrace (Error { context= []; offset= 0; kind= Msg msg })
  in
  Format.kasprintf fn fmt

let v ~offset kind = raise_notrace (Error { context= []; offset; kind })
let truncated ~offset ~need ~have = v ~offset (Truncated { need; have })
let out_of_range ~offset ~kind ~value = v ~offset (Out_of_range { kind; value })

let unexpected_tag ~offset ~tag ~expected =
  v ~offset (Unexpected_tag { tag; expected })

let length_mismatch ~offset ~expected ~got =
  v ~offset (Length_mismatch { expected; got })

let reraise_in name exn =
  match exn with
  | Error e -> raise_notrace (Error { e with context= name :: e.context })
  | exn -> raise exn
