# Bstr, Slice & Bin

This small set of libraries offers a homogeneous API between 2 types and their
derivations with the slice type, as well as a small DSL for decoding "packets"
(such as ARP or DNS) without too much difficulty.

The aim is to homogenize the 2 types bytes and bigstring and to derive them
with a slice type, giving the user all the levers needed to manipulate byte
sequences, whether in the form of a bigstring or bytes. The slice view avoids
copying when it comes to decoding a packet and extracting a sub-part. The slice
also applies to bigstrings, whose `Bigarray.Array1.sub` is more expensive.

This set of libraries is a synthesis of [astring][astring] (which offers a range
of useful functions as well as slice), [cstruct][cstruct] (which offers a
similar API for bigstrings), [bigstringaf][bigstringaf] (which offers some other
useful functions), the standard OCaml library and [repr][repr] for
decoding/encoding these values into OCaml records/variants.

## About API

Here is an overview of the functions offered by `bstr` compared to other
libraries:

|                 | bstr | cstruct | bigstringaf | slice.bstr |
|-----------------|------|---------|-------------|------------|
|       `overlap` |   ✅ |      ❌ |          ❌ |         ✅ |
|        `memcpy` |   ✅ |      ❌ |          ✅ |         ✅ |
|       `memmove` |   ✅ |      ✅ |          ✅ |         ✅ |
|      fast `sub` |   ❌ |      ❌ |          ❌ |         ✅ |
|     fast `blit` |   ✅ |      ❌ |          ❌ |         ✅ |
| release GC lock |   ✅ |      ❌ |          ❌ |         ✅ |
| fast `contains` |   ✅ |      ❌ |          ✅ |         ✅ |

### Fast `sub`

`sub` is perhaps the most useful operation for a bigarray. In fact, unlike bytes
and strings, sub offers a view (equivalent or smaller) of a bigarray without
making a copy. If, for example, you need to decode[^1] a large sequence of bytes
(without having the notion of a "stream"), it may be useful to use the `sub`
operation to decode the information byte by byte and avoid copying throughout
the decoding process.

The implementation of `sub` proposed by `Bstr` is a little different from that
of the standard OCaml library. In fact, it is specialized for a bigarray of
dimension 1 containing bytes. In fact, the `Bigarray.Array1.sub` function is a
little more generic and `Bstr` takes the opportunity to "specialize" the
function according to our type.

However, according to the representation proposed by `Cstruct`, `Cstruct.sub`
remains **the fastest** operation compared to `Bstr` and `Bigstringaf`. If you
want to have the same performance as `Cstruct`, the specialized `Slice` module
for `Bstr.t` values is equivalent.

Here is a comparative table of the `sub` function between all implementations
(AMD Ryzen 9 7950X 16-Core Processor):

|       | bigstringaf |   bstr | cstruct | slice |
|-------|-------------|--------|---------|-------|
| `sub` |     20.0 ns | 17.8ns |   2.8ns | 2.4ns |

### Fast `blit`

`blit` from a string or a bytes is a little faster than `Bigstringaf` and
`Cstruct`. The difference basically lies in the fact that `Bstr.t` uses other
"tags" to describe the FFI with the C `memcpy` function (specifically the
[\[@untagged\]][untagged] tag).

Here is a comparative table of the `blit_from_string` function between all the
implementations:

|                    | bigstringaf |  bstr | cstruct |
|--------------------|-------------|-------|---------|
| `blit_from_string` |       5.1ns | 4.3ns |   4.7ns |

#### _mmaped_ or not? (GC lock)

There are 2 ways to copy bytes between two bigarrays:
- the "mmaped" version (`{memcpy,memmove}_mmaped`)
- the simple version (`{memcpy,memmove}`)

The first is quite specific because it releases the GC lock after a certain
number of bytes (4096) have been copied. This can be advantageous if you want
to make a large copy between two bigarrays in parallel in a `Thread`.

If we specify _mmaped_, it is because the copy between two bigarrays, one of
which **may** come from `Unix.map_file`, can also take time (and we may want to
do it in parallel in a `Thread`) since it involves reading/writing on the disk.

```ocaml
let copy_to_file bstr filename () =
  let len = Bstr.length bstr in
  let fd = Unix.openfile filename Unix.[ O_WRONLY ] 0o644 in
  let dst = Unix.map_file fd Bigarray.char Bigarray.c_layout false [| len |] in
  let dst = Bigarray.array1_of_genarray dst in
  Bstr.memcpy_mmaped bstr ~src_off:0 dst ~dst_off:0 ~len

let () =
  let th = Thread.create (copy_to_file bstr filename) () in
  (* do something else in true parallel of [copy_to_file]. *)
  (* the GC will not interrupt [th] during the copy. *)
  Thread.join th
```

The simple version does **not** release the GC lock and only applies the
desired function (`memmove` or `memcpy`).

#### `memmove` or `memcpy`?

`Bstr.blit` **always** uses the `memmove` function. However, it can be
advantageous to use `memcpy` in a fairly specific case: when you know that the
source refers to a memory area that is not shared with the destination.

To find out, you can use the `Bstr.overlap` function, which checks whether or
not the two bigarrays given have a common memory area.

[^1]: `Bin` is currently being designed with this in mind.

[astring]: https://github.com/dbuenzli/astring
[cstruct]: https://github.com/mirage/ocaml-cstruct
[repr]: https://github.com/mirage/repr
[bigstringaf]: https://github.com/inhabitedtype/bigstringaf
[untagged]: https://ocaml.org/manual/5.3/attributes.html
