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

module Proxy(A: CHANNEL
  with type data = Cstruct.t list
   and type position = int32
   and type 'a io = 'a Lwt.t
)(B: CHANNEL
  with type data = Cstruct.t list
   and type position = int32
   and type 'a io = 'a Lwt.t) = struct
  let rec forever name a b =
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

module Make(E: EVENTS with type 'a io = 'a Lwt.t)(RW: XEN_BYTE_RING_LAYOUT) = struct
  module Raw = Ring.Make(E)(RW)

  module BufferFrontend = In_memory_ring.Frontend(In_memory_events)
  module BufferBackend  = In_memory_ring.Backend(In_memory_events)

  type position = int32 with sexp
  type data = Cstruct.t list
  type 'a io = 'a Lwt.t

  type t' =
  | Buffered of BufferFrontend.t
  | Unbuffered of Raw.t
  type t = t'

  let create ?buffer ev ring =
    let p = Raw.create ev ring in
    match buffer with
    | None -> Unbuffered p
    | Some buffer ->
      let port, channel = In_memory_events.listen 0 in
      let channel' = In_memory_events.connect 0 port in
      (* Create a backend and service it by proxying too and from
         the real pipe *)
      let backend = BufferBackend.create channel buffer in
      BufferBackend.init backend;
      let module LR = Proxy(BufferBackend)(Raw) in
      let _ = LR.forever "buffer->raw" backend p in
      let module RL = Proxy(Raw)(BufferBackend) in
      let _ = RL.forever "raw->buffer" p backend in
      Buffered (BufferFrontend.create channel' buffer)

  let init = function
  | Buffered t -> BufferFrontend.init t
  | Unbuffered t -> Raw.init t

  let write = function
  | Buffered t -> BufferFrontend.write t
  | Unbuffered t -> Raw.write t

  module Reader = struct
    type 'a io = 'a Lwt.t
    type t = t'
    type data = Cstruct.t list
    type position = int32 with sexp
    let advance t pos = match t with
    | Buffered t -> BufferFrontend.Reader.advance t pos
    | Unbuffered t -> Raw.Reader.advance t pos
    let available = function
    | Buffered t -> BufferFrontend.Reader.available t
    | Unbuffered t -> Raw.Reader.available t
    let wait t n = match t with
    | Buffered t -> BufferFrontend.Reader.wait t n
    | Unbuffered t -> Raw.Reader.wait t n
  end

  module Writer = struct
    type 'a io = 'a Lwt.t
    type t = t'
    type data = Cstruct.t list
    type position = int32 with sexp
    let advance t pos = match t with
    | Buffered t -> BufferFrontend.Writer.advance t pos
    | Unbuffered t -> Raw.Writer.advance t pos
    let available = function
    | Buffered t -> BufferFrontend.Writer.available t
    | Unbuffered t -> Raw.Writer.available t
    let wait t n = match t with
    | Buffered t -> BufferFrontend.Writer.wait t n
    | Unbuffered t -> Raw.Writer.wait t n
  end
end
