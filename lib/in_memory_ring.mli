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
open S

module Make(E: EVENTS with type 'a io = 'a Lwt.t): sig
  include PIPE
    with type t = Cstruct.t
     and type data = Cstruct.t
     and type position = int32

  val create: Cstruct.t -> t
  (** Construct a ring from the given buffer. Note the buffer must contain
      valid data. To initialise a buffer, see [init] *)

  val init: Cstruct.t -> unit
  (** Initialise the buffer so it does not contain any readable data *)
end
(** Create a shared ring from the given memory buffer. Note the layout is
    decided by the implementation: it is not a standard protocol. *)
