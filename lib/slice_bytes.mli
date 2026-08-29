(** Slices on [bytes].

    This module implements {!module-type:Slice.S}: it offers exactly the same
    API as {!module:Slice_bstr} (and mirrors {!module:Bstr}), so that the same
    code can be used regardless of the underlying byte sequence.

    Unlike [Bytes.sub], {!val:sub} and {!val:shift} do not copy: they share the
    underlying [bytes] with the given slice. *)

include Slice.S with type buf = bytes
