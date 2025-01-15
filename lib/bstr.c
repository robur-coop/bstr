#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/m.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

CAMLprim value bstr_bytecode_ptr(value va) {
  CAMLparam1(va);
  CAMLlocal1(res);

  struct caml_ba_array *a = Caml_ba_array_val(va);
  void *src_a = a->data;
  res = caml_copy_nativeint((intnat)src_a);

  CAMLreturn(res);
}

intnat bstr_native_ptr(value va) {
  struct caml_ba_array *a = Caml_ba_array_val(va);
  return ((intnat)a->data);
}

#define bstr_uint8_off(ba, off) ((uint8_t *)Caml_ba_data_val(ba) + off)

void bstr_native_memcpy(value src, intnat src_off, value dst, intnat dst_off,
                        intnat len) {
  memcpy(bstr_uint8_off(src, src_off), bstr_uint8_off(dst, dst_off), len);
}

CAMLprim value bstr_bytecode_memcpy(value src, value src_off, value dst,
                                    value dst_off, value len) {
  CAMLparam5(src, src_off, dst, dst_off, len);
  bstr_native_memcpy(src, Unsigned_long_val(src_off), dst,
                     Unsigned_long_val(dst_off), Unsigned_long_val(len));
  CAMLreturn(Val_unit);
}

void bstr_native_memmove(value src, intnat src_off, value dst, intnat dst_off,
                         intnat len) {
  memmove(bstr_uint8_off(src, src_off), bstr_uint8_off(dst, dst_off), len);
}

CAMLprim value bstr_bytecode_memmove(value src, value src_off, value dst,
                                     value dst_off, value len) {
  CAMLparam5(src, src_off, dst, dst_off, len);
  bstr_native_memmove(src, Unsigned_long_val(src_off), dst,
                      Unsigned_long_val(dst_off), Unsigned_long_val(len));
  CAMLreturn(Val_unit);
}

intnat bstr_native_memcmp(value src, intnat src_off, value dst, intnat dst_off,
                          intnat len) {
  intnat res;
  res = memcmp(bstr_uint8_off(src, src_off), bstr_uint8_off(dst, dst_off), len);
  return (res);
}

CAMLprim value bstr_bytecode_memcmp(value src, value src_off, value dst,
                                    value dst_off, value len) {
  CAMLparam5(src, src_off, dst, dst_off, len);
  intnat res;
  res = bstr_native_memcmp(src, Unsigned_long_val(src_off), dst,
                           Unsigned_long_val(dst_off), Unsigned_long_val(len));
  CAMLreturn(Val_long(res));
}

#define __MEM1(name)                                                           \
  intnat bstr_native_##name(value src, intnat src_off, intnat src_len,         \
                            intnat va) {                                       \
    void *res = name(bstr_uint8_off(src, src_off), va, src_len);               \
    if (res == NULL)                                                           \
      return (-1);                                                             \
                                                                               \
    return ((intnat)(res - src));                                              \
  }                                                                            \
                                                                               \
  CAMLprim value bstr_bytecode_##name(value src, value src_off, value src_len, \
                                      value va) {                              \
    CAMLparam4(src, src_off, src_len, va);                                     \
    intnat res;                                                                \
    res =                                                                      \
        bstr_native_##name(src, Unsigned_long_val(src_off),                    \
                           Unsigned_long_val(src_len), Unsigned_long_val(va)); \
    CAMLreturn(Val_long(res));                                                 \
  }

__MEM1(memset)
__MEM1(memchr)

/* This function is **only** useful when accessing to a bigstring for an
 * architecture requiring alignment **and** for an OCaml executable in
 * bytecode. It concerns only 32-bits architectures.
 */

uint64_t bstr_native_get64u(value va, intnat off) {
#ifdef ARCH_ALIGN_INT64
  char b0, b1, b2, b3, b4, b5, b6, b7;
#endif
  struct caml_ba_array *a = Caml_ba_array_val(va);
  void *addr = &((unsigned char *)a->data)[off];
  uint64_t res;

#ifdef ARCH_ALIGN_INT64
  if (!((size_t)addr) & 0x7)
    res = *((uint64_t *)addr);
  else {
    b0 = ((unsigned char *)a->data)[off];
    b1 = ((unsigned char *)a->data)[off + 1];
    b2 = ((unsigned char *)a->data)[off + 2];
    b3 = ((unsigned char *)a->data)[off + 3];
    b4 = ((unsigned char *)a->data)[off + 4];
    b5 = ((unsigned char *)a->data)[off + 5];
    b6 = ((unsigned char *)a->data)[off + 6];
    b7 = ((unsigned char *)a->data)[off + 7];
#ifdef ARCH_BIG_ENDIAN
    res = (uint64_t)b0 << 56 | (uint64_t)b1 << 48 | (uint64_t)b2 << 40 |
          (uint64_t)b3 << 32 | (uint64_t)b4 << 24 | (uint64_t)b5 << 16 |
          (uint64_t)b6 << 8 | (uint64_t)b7;
#else
    res = (uint64_t)b7 << 56 | (uint64_t)b6 << 48 | (uint64_t)b5 << 40 |
          (uint64_t)b4 << 32 | (uint64_t)b3 << 24 | (uint64_t)b2 << 16 |
          (uint64_t)b1 << 8 | (uint64_t)b0;
#endif
  }
#else
  res = *((uint64_t *)addr);
#endif

  return (res);
}

CAMLprim value bstr_bytecode_get64u(value va, value off) {
  CAMLparam2(va, off);
  CAMLlocal1(res);

  uint64_t val = bstr_native_get64u(va, Unsigned_long_val(off));
  res = caml_copy_int64(val);

  CAMLreturn(res);
}
