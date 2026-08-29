(** Slices on a bigstring ({!type:Bstr.t}).

    This module implements {!module-type:Slice.S}: it offers exactly the same
    API as {!module:Slice_bytes} (and mirrors {!module:Bstr}), so that the same
    code can be used regardless of the underlying byte sequence.

    {!val:sub} and {!val:shift} are [O(1)]: they share the underlying bigstring
    with the given slice instead of calling {!val:Bstr.sub} (which is about 8
    times slower).

    This module is the result of a {i poor man}'s functor designed to avoid the
    inherent overhead that can arise from using functors in OCaml. In other
    words, the implementation is the same as that of [Slice_bytes]. *)

include Slice.S with type buf = Bstr.t
