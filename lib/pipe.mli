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

(** Xen-style bidirectional pipes as used by Xenstore and the console *)

open S

module Reverse: functor(L: PIPE_LAYOUT) -> PIPE_LAYOUT
  with type t = L.t
   and type data = L.data
   and type position = L.position
(** Flip the layout around swapping the frontend and the backend *)

module Make: functor(L: XEN_PIPE_LAYOUT) -> PIPE
  with type 'a io = 'a Lwt.t
   and type t = L.t
   and type data = L.data
   and type position = L.position
(** Construct a pipe with a given memory layout *)
