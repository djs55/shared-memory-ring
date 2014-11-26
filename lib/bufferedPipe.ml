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
  with type data = Cstruct.t list
   and type position = int32
   and type 'a io = 'a Lwt.t
)(B: PIPE
  with type data = Cstruct.t list
   and type position = int32
   and type 'a io = 'a Lwt.t) = struct
  let rec forever a b =
    let rec loop () =
      A.Reader.wait a 1
      >>= fun () ->
      let pos, data = A.Reader.available a in
      B.Writer.wait b 1
      >>= fun () ->
      let pos', data' = B.Writer.available b in
      let len = List.fold_left (+) 0 (List.map Cstruct.len data) in
      let len' = List.fold_left (+) 0 (List.map Cstruct.len data') in
      (* avoid overloading the output by writing more than this *)
      let rec take remaining = function
      | [] -> []
      | b :: bs ->
        let len_b = Cstruct.len b in
        if len_b < remaining
        then b :: (take (remaining - len_b) bs)
        else [ Cstruct.sub b 0 remaining ] in
      B.write b pos' (take (min len len') data)
      >>= function
      | `Ok pos'' ->
        B.Writer.advance b pos'';
        A.Reader.advance a pos'';
        loop ()
      | `Error m -> fail (Failure m) in
    loop ()
end

module BufferFrontend = In_memory_ring.Frontend(In_memory_events)
module BufferBackend  = In_memory_ring.Backend(In_memory_events)

module Buffered(P: PIPE
  with type 'a io = 'a Lwt.t
   and type data = Cstruct.t list
   and type position = int32
) = struct
  type position = int32 with sexp
  type data = Cstruct.t list
  type 'a io = 'a Lwt.t

  type t = BufferFrontend.t

  let port, channel = In_memory_events.listen 0

  let create ~buffer p =
    (* Create a backend and service it by proxying too and from
       the real pipe *)
    let backend = BufferBackend.create channel buffer in
    let module LR = Proxy(BufferBackend)(P) in
    let _ = LR.forever backend p in
    let module RL = Proxy(P)(BufferBackend) in
    let _ = RL.forever p backend in
    let channel = In_memory_events.connect 0 port in
    BufferFrontend.create channel buffer

  let init = BufferFrontend.init
  let write = BufferFrontend.write

  module Reader = BufferFrontend.Reader

  module Writer = BufferFrontend.Writer

end
