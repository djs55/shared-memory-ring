(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Citrix Systems, Inc
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

(** Low-level memory primitives needed by the (lock-free) ring protocols *)

val zero: Cstruct.t -> unit
(** [zero c] sets every byte of the [c] Cstruct to zero. *)

val memory_barrier: unit -> unit
(** Execute a memory barrier preventing other CPU cores from seeing writes
    'crossing' the barrier. *)

val unsafe_load_uint32: Cstruct.t -> int -> int
(** [unsafe_load_uint32 buf ofs] returns an int containing the 32-bit value
    stored at [ofs] (measured in bytes) read with a single atomic load
    instruction. *)

val unsafe_save_uint32: Cstruct.t -> int -> int -> unit
(** [unsave_safe_uint32 buf ofs val] writes [val] at byte offset [ofs]
    using a single atomic store instruction. *)
