# Bstr, Slice & Bin

This small set of libraries offers a homogeneous API between 3 types and their
derivations with the slice type, as well as a small DSL for decoding "packets"
(such as ARP or DNS) without too much difficulty.

The aim is to homogenize the 3 types string, bytes and bigstring and to derive
them with a slice type, giving the user all the levers needed to manipulate
packets, whether in the form of a bigstring, string or bytes. The slice view
avoids copying when it comes to decoding a packet and extracting a sub-part.
The slice also applies to bigstrings, whose `Bigarray.Array1.sub` is more
expensive.

This set of libraries is a synthesis of [astring][astring] (which offers a range
of useful functions as well as slice), [cstruct] (which offers a similar API for
bigstrings), the standard OCaml library and [repr][repr] for decoding/encoding
these values into OCaml records/variants.
