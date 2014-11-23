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

module Reverse(RW: PIPE_LAYOUT) = struct
  type t = RW.t
  type position = RW.position with sexp
  type data = RW.data

  (* Flip around the input and output, to reverse the sense of the
     frontend and backend. This is useful to generate both backend and
     frontend code from the same definition. *)
  let get_ring_input = RW.get_ring_output
  let get_ring_input_cons = RW.get_ring_output_cons
  let get_ring_input_prod = RW.get_ring_output_prod
  let set_ring_input_cons = RW.set_ring_output_cons
  let set_ring_input_prod = RW.set_ring_output_prod

  let get_ring_output = RW.get_ring_input
  let get_ring_output_cons = RW.get_ring_input_cons
  let get_ring_output_prod = RW.get_ring_input_prod
  let set_ring_output_cons = RW.set_ring_input_cons
  let set_ring_output_prod = RW.set_ring_input_prod
end

module Make(E: EVENTS with type 'a io = 'a Lwt.t)(RW: XEN_PIPE_LAYOUT) = struct
  type 'a io = 'a Lwt.t

  type t' = {
    buffer: Cstruct.t;
    channel: E.channel;
  }
  type t = t'

  type position = int32 with sexp

  type data = Cstruct.t

  let create channel buffer = { channel; buffer }

  let wait t f =
    let rec loop event =
      if f ()
      then return ()
      else begin
        E.recv t.channel event
        >>= fun event ->
        loop event
      end in
    loop E.initial

  module Writer = struct
    type 'a io = 'a Lwt.t
    type t = t'
    type data = Cstruct.t
    type position = int32 with sexp
    type channel = E.channel

    let wait t n =
      let output = RW.get_ring_output t.buffer in
      let output_length = Cstruct.len output in
      wait t
        (fun () ->
          let cons = Int32.to_int (RW.get_ring_output_cons t.buffer) in
          let prod = Int32.to_int (RW.get_ring_output_prod t.buffer) in
          let unread = prod - cons in
          let free_space = output_length - unread in
          free_space >= n
        )

    let available t =
      let output = RW.get_ring_output t.buffer in
      let output_length = Cstruct.len output in
      (* Remember: the producer and consumer indices can be >> output_length *)
      let cons = Int32.to_int (RW.get_ring_output_cons t.buffer) in
      let prod = Int32.to_int (RW.get_ring_output_prod t.buffer) in
      memory_barrier ();
      (* 0 <= cons', prod' <= output_length *)
      let cons' =
        let x = cons mod output_length in
        if x < 0 then x + output_length else x
      and prod' =
        let x = prod mod output_length in
        if x < 0 then x + output_length else x in
      let free_space =
        if prod - cons >= output_length
        then 0
        else
        if prod' >= cons'
        then output_length - prod' (* in this write, fill to the end *)
        else cons' - prod' in
      Int32.of_int prod, Cstruct.sub output prod' free_space

    let advance t prod' =
      memory_barrier ();
      let prod = RW.get_ring_output_prod t.buffer in
      RW.set_ring_output_prod t.buffer (max prod' prod);
      E.send t.channel
  end

  module Reader = struct
    type 'a io = 'a Lwt.t
    type t = t'
    type data = Cstruct.t
    type position = int32 with sexp
    type channel = E.channel

    let wait t n =
      wait t
        (fun () ->
          let cons = Int32.to_int (RW.get_ring_input_cons t.buffer) in
          let prod = Int32.to_int (RW.get_ring_input_prod t.buffer) in
          let unread = prod - cons in
          unread >= n
        )

    let available t =
      let input = RW.get_ring_input t.buffer in
      let input_length = Cstruct.len input in
      let cons = Int32.to_int (RW.get_ring_input_cons t.buffer) in
      let prod = Int32.to_int (RW.get_ring_input_prod t.buffer) in
      memory_barrier ();
      let cons' =
        let x = cons mod input_length in
        if x < 0 then x + input_length else x
      and prod' =
        let x = prod mod input_length in
        if x < 0 then x + input_length else x in
      let data_available =
        if prod = cons
        then 0
        else
        if prod' > cons'
        then prod' - cons'
        else input_length - cons' in (* read up to the last byte in the ring *)
      Int32.of_int cons, Cstruct.sub input cons' data_available

    let advance t (cons':int32) =
      let cons = RW.get_ring_input_cons t.buffer in
      RW.set_ring_input_cons t.buffer (max cons' cons);
      E.send t.channel
  end
end
