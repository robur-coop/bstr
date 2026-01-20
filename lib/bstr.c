/*
 * Copyright (c) 2024 Romain Calascibetta <romain.calascibetta@gmail.com>
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
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/m.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

#ifndef CAML_BA_SUBARRAY
#define CAML_BA_SUBARRAY 0x800
#endif

CAMLextern void caml_enter_blocking_section(void);
CAMLextern void caml_leave_blocking_section(void);

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
#define bytes_uint8_off(buf, off) ((uint8_t *)Bytes_val(buf) + off)
#define LEAVE_RUNTIME_OP_CUTOFF 4096
#define is_mmaped(ba) ((ba)->flags & CAML_BA_MAPPED_FILE)

void bstr_native_memcpy_mmaped(value src, intnat src_off, value dst,
                               intnat dst_off, uintnat len) {
  int leave_runtime = (len > LEAVE_RUNTIME_OP_CUTOFF * sizeof(long));

  if (leave_runtime)
    caml_enter_blocking_section();
  memcpy(bstr_uint8_off(dst, dst_off), bstr_uint8_off(src, src_off), len);
  if (leave_runtime)
    caml_leave_blocking_section();
}

void bstr_native_memcpy(value src, intnat src_off, value dst, intnat dst_off,
                        intnat len) {
  memcpy(bstr_uint8_off(dst, dst_off), bstr_uint8_off(src, src_off), len);
}

CAMLprim value bstr_bytecode_memcpy(value src, value src_off, value dst,
                                    value dst_off, value len) {
  CAMLparam5(src, src_off, dst, dst_off, len);

  if (is_mmaped(Caml_ba_array_val(src)) || is_mmaped(Caml_ba_array_val(dst)))
    bstr_native_memcpy_mmaped(src, Unsigned_long_val(src_off), dst,
                              Unsigned_long_val(dst_off),
                              Unsigned_long_val(len));
  else
    bstr_native_memcpy(src, Unsigned_long_val(src_off), dst,
                       Unsigned_long_val(dst_off), Unsigned_long_val(len));

  CAMLreturn(Val_unit);
}

CAMLprim value bstr_native_memmove_mmaped(value src, intnat src_off, value dst,
                                          intnat dst_off, uintnat len) {
  CAMLparam2(src, dst);
  int leave_runtime = (len > LEAVE_RUNTIME_OP_CUTOFF * sizeof(long)) ||
                      is_mmaped(Caml_ba_array_val(src)) ||
                      is_mmaped(Caml_ba_array_val(dst));

  if (leave_runtime)
    caml_enter_blocking_section();
  memmove(bstr_uint8_off(dst, dst_off), bstr_uint8_off(src, src_off), len);
  if (leave_runtime)
    caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

void bstr_native_memmove(value src, intnat src_off, value dst, intnat dst_off,
                         intnat len) {
  memmove(bstr_uint8_off(dst, dst_off), bstr_uint8_off(src, src_off), len);
}

CAMLprim value bstr_bytecode_memmove(value src, value src_off, value dst,
                                     value dst_off, value len) {
  CAMLparam5(src, src_off, dst, dst_off, len);
  if (is_mmaped(Caml_ba_array_val(src)) || is_mmaped(Caml_ba_array_val(dst)))
    bstr_native_memmove_mmaped(src, Unsigned_long_val(src_off), dst,
                               Unsigned_long_val(dst_off),
                               Unsigned_long_val(len));
  else
    bstr_native_memmove(src, Unsigned_long_val(src_off), dst,
                        Unsigned_long_val(dst_off), Unsigned_long_val(len));
  CAMLreturn(Val_unit);
}

intnat bstr_native_memcmp(value s1, intnat s1_off, value s2, intnat s2_off,
                          intnat len) {
  intnat res;
  res = memcmp(bstr_uint8_off(s1, s1_off), bstr_uint8_off(s2, s2_off), len);
  return (res);
}

CAMLprim value bstr_bytecode_memcmp(value s1, value s1_off, value s2,
                                    value s2_off, value len) {
  CAMLparam5(s1, s1_off, s2, s2_off, len);
  intnat res;
  res = bstr_native_memcmp(s1, Unsigned_long_val(s1_off), s2,
                           Unsigned_long_val(s2_off), Unsigned_long_val(len));
  CAMLreturn(Val_long(res));
}

#define __MEM1(name)                                                           \
  intnat bstr_native_##name(value src, intnat src_off, intnat src_len,         \
                            intnat va) {                                       \
    uint8_t *res = name(bstr_uint8_off(src, src_off), va, src_len);            \
    if (res == NULL)                                                           \
      return (-1);                                                             \
                                                                               \
    return ((intnat)(res - bstr_uint8_off(src, src_off)));                     \
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

void bstr_native_unsafe_blit_from_bytes(value src, intnat src_off, value dst,
                                        intnat dst_off, intnat len) {
  memcpy(bstr_uint8_off(dst, dst_off), bytes_uint8_off(src, src_off), len);
}

CAMLprim value bstr_bytecode_unsafe_blit_from_bytes(value src, intnat src_off,
                                                    value dst, intnat dst_off,
                                                    intnat len) {
  CAMLparam5(src, src_off, dst, dst_off, len);
  memcpy(bstr_uint8_off(dst, Unsigned_long_val(dst_off)),
         bytes_uint8_off(src, Unsigned_long_val(src_off)),
         Unsigned_long_val(len));
  CAMLreturn(Val_unit);
}

void bstr_native_unsafe_blit_to_bytes(value src, intnat src_off, value dst,
                                      intnat dst_off, intnat len) {
  memcpy(bytes_uint8_off(dst, dst_off), bstr_uint8_off(src, src_off), len);
}

CAMLprim value bstr_bytecode_unsafe_blit_to_bytes(value src, intnat src_off,
                                                  value dst, intnat dst_off,
                                                  intnat len) {
  CAMLparam5(src, src_off, dst, dst_off, len);
  memcpy(bytes_uint8_off(dst, Unsigned_long_val(dst_off)),
         bstr_uint8_off(src, Unsigned_long_val(src_off)),
         Unsigned_long_val(len));
  CAMLreturn(Val_unit);
}

#include <stdatomic.h>

#define atomic_store_release(p, v)                                             \
  atomic_store_explicit((p), (v), memory_order_release)

CAMLextern struct custom_operations caml_ba_ops;

static void caml_ba_update_proxy(struct caml_ba_array *b1,
                                 struct caml_ba_array *b2) {
  struct caml_ba_proxy *proxy;
  /* Nothing to do for un-managed arrays */
  if ((b1->flags & CAML_BA_MANAGED_MASK) == CAML_BA_EXTERNAL)
    return;
  if (b1->proxy != NULL) {
    /* If b1 is already a proxy for a larger array, increment refcount of
       proxy */
    b2->proxy = b1->proxy;
#if OCAML_VERSION_MAJOR >= 5
    (void)atomic_fetch_add(&b1->proxy->refcount, 1);
#else
    b1->proxy->refcount += 1;
#endif
  } else {
    /* Otherwise, create proxy and attach it to both b1 and b2 */
    proxy = malloc(sizeof(struct caml_ba_proxy));
    if (proxy == NULL)
      caml_raise_out_of_memory();
#if OCAML_VERSION_MAJOR >= 5
    atomic_store_release(&proxy->refcount, 2);
#else
    proxy->refcount = 2;
#endif
    /* initial refcount: 2 = original array + sub array */
    proxy->data = b1->data;
    proxy->size = b1->flags & CAML_BA_MAPPED_FILE ? caml_ba_byte_size(b1) : 0;
    b1->proxy = proxy;
    b2->proxy = proxy;
  }
}

CAMLprim value bstr_native_unsafe_sub(value vbstr, intnat off, intnat len) {
  CAMLparam1(vbstr);
  CAMLlocal1(res);

  char *sub = (char *)Caml_ba_array_val(vbstr)->data + off * sizeof(char);
  res =
      caml_alloc_custom_mem(&caml_ba_ops, SIZEOF_BA_ARRAY + sizeof(intnat), 0);
  struct caml_ba_array *new;
  new = Caml_ba_array_val(res);
  new->data = sub;
  new->num_dims = 1;
  new->flags = Caml_ba_array_val(vbstr)->flags | CAML_BA_SUBARRAY;
  new->proxy = NULL;
  new->dim[0] = len;
  Custom_ops_val(res) = Custom_ops_val(vbstr);
  caml_ba_update_proxy(Caml_ba_array_val(vbstr), Caml_ba_array_val(res));
  CAMLreturn(res);
}

CAMLprim value bstr_bytecode_unsafe_sub(value vbstr, value voff, value vlen) {
  CAMLparam3(vbstr, voff, vlen);
  CAMLlocal1(res);

  intnat off = Unsigned_long_val(voff);
  intnat len = Unsigned_long_val(vlen);
  char *sub = (char *)Caml_ba_array_val(vbstr)->data + off * sizeof(char);
  res =
      caml_alloc_custom_mem(&caml_ba_ops, SIZEOF_BA_ARRAY + sizeof(intnat), 0);
  struct caml_ba_array *new;
  new = Caml_ba_array_val(res);
  new->data = sub;
  new->num_dims = 1;
  new->flags = Caml_ba_array_val(vbstr)->flags | CAML_BA_SUBARRAY;
  new->proxy = NULL;
  new->dim[0] = len;
  Custom_ops_val(res) = Custom_ops_val(vbstr);
  caml_ba_update_proxy(Caml_ba_array_val(vbstr), Caml_ba_array_val(res));
  CAMLreturn(res);
}

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
  if (!((size_t)addr & 0x7))
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
