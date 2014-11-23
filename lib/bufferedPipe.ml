(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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
open Memory
open Sexplib.Std
open Lwt

module Proxy(A: PIPE
  with type data = Cstruct.t
   and type position = int32
)(B: PIPE
  with type data = Cstruct.t
   and type position = int32) = struct
  let rec forever a b =
    let rec loop () =
      A.Reader.wait a 1
      >>= fun () ->
      let pos, data = A.Reader.available a in
      B.Writer.wait b 1
      >>= fun () ->
      let pos', data' = B.Writer.available b in
      (* XXX: need to intersect the two intervals *)
      Cstruct.blit data 0 data' 0 1;
      B.Writer.advance b (Int32.succ pos');
      A.Reader.advance a (Int32.succ pos);
      loop () in
    loop ()
end

module Buffer = In_memory_ring.Make(In_memory_events)

module Buffered(P: PIPE
  with type 'a io = 'a Lwt.t
   and type data = Cstruct.t
   and type position = int32
) = struct
  type position = int32 with sexp
  type data = Cstruct.t
  type 'a io = 'a Lwt.t

  type t = Buffer.t

  let port, channel = In_memory_events.listen 0

  let create ~buffer p =
    let buffer = Buffer.create channel buffer in
    let module LR = Proxy(Buffer)(P) in
    let _ = LR.forever buffer p in
    let module RL = Proxy(P)(Buffer) in
    let _ = RL.forever p buffer in
    buffer

  module Reader = Buffer.Reader

  module Writer = Buffer.Writer

end
