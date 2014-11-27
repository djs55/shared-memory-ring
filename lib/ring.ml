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
    ring: Cstruct.t;
    channel: E.channel;
  }
  type t = t'

  type position = int32 with sexp

  type data = Cstruct.t list

  let create channel ring = { channel; ring }

  let init t =
    RW.set_ring_input_cons t.ring 0l;
    RW.set_ring_input_prod t.ring 0l;
    RW.set_ring_output_cons t.ring 0l;
    RW.set_ring_output_prod t.ring 0l

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
    type data = Cstruct.t list
    type position = int32 with sexp
    type channel = E.channel

    let wait t n =
      let output = RW.get_ring_output t.ring in
      let output_length = Cstruct.len output in
      wait t
        (fun () ->
          let cons = Int32.to_int (RW.get_ring_output_cons t.ring) in
          let prod = Int32.to_int (RW.get_ring_output_prod t.ring) in
          let unread = prod - cons in
          let free_space = output_length - unread in
          free_space >= n
        )

    let available t =
      let output = RW.get_ring_output t.ring in
      let output_length = Cstruct.len output in
      (* Remember: the producer and consumer indices can be >> output_length *)
      let cons = Int32.to_int (RW.get_ring_output_cons t.ring) in
      let prod = Int32.to_int (RW.get_ring_output_prod t.ring) in
      memory_barrier ();
      (* 0 <= cons', prod' <= output_length *)
      let cons' =
        let x = cons mod output_length in
        if x < 0 then x + output_length else x
      and prod' =
        let x = prod mod output_length in
        if x < 0 then x + output_length else x in
      let data =
        if prod - cons >= output_length
        then [ Cstruct.sub output 0 0 ]
        else
        if prod' >= cons'
        then [ Cstruct.sub output prod' (output_length - prod'); Cstruct.sub output 0 cons' ]
        else [ Cstruct.sub output prod' (cons' - prod') ] in
      Int32.of_int prod, data

    let advance t prod' =
      memory_barrier ();
      let prod = RW.get_ring_output_prod t.ring in
      RW.set_ring_output_prod t.ring (max prod' prod);
      E.send t.channel

  end

  let write_one t ofs buffer =
    let buffer_len = Cstruct.len buffer in
    let output_len = Cstruct.len (RW.get_ring_output t.ring) in
    if buffer_len = 0
    then return (`Ok ofs)
    else if output_len < buffer_len
    then return (`Error (Printf.sprintf "write argument is longer (%d) than the total ring output buffer (%d)" buffer_len output_len))
    else begin
      Writer.wait t buffer_len
      >>= fun () ->
      let ofs', buffer's = Writer.available t in
      if ofs' < ofs
      then return (`Error (Printf.sprintf "stream has lost data (ofs' = %ld < ofs = %ld)" ofs' ofs))
      else begin
        let skip = Int32.(to_int (sub ofs' ofs)) in
        let buffer = Cstruct.shift buffer skip in
        let rec loop ofs' buffer buffer's = match buffer's with
        | [] ->
          assert (Cstruct.len buffer = 0); (* Or else wait returned too soon *)
          return (`Ok ofs')
        | buffer' :: buffer's ->
          let len = min (Cstruct.len buffer') (Cstruct.len buffer) in
          Cstruct.blit buffer 0 buffer' 0 len;
          let buffer = Cstruct.shift buffer len in
          let ofs' = Int32.(add ofs' (of_int len)) in
          loop ofs' buffer buffer's in
       loop ofs' buffer buffer's
     end
   end

  let write t ofs buffers =
    let rec loop ofs = function
    | [] -> return (`Ok ofs)
    | b::bs ->
      write_one t ofs b
      >>= function
      | `Ok ofs -> loop ofs bs
      | `Error m -> return (`Error m) in
    loop ofs buffers

  module Reader = struct
    type 'a io = 'a Lwt.t
    type t = t'
    type data = Cstruct.t list
    type position = int32 with sexp
    type channel = E.channel

    let wait t n =
      wait t
        (fun () ->
          let cons = Int32.to_int (RW.get_ring_input_cons t.ring) in
          let prod = Int32.to_int (RW.get_ring_input_prod t.ring) in
          let unread = prod - cons in
          unread >= n
        )

    let available t =
      let input = RW.get_ring_input t.ring in
      let input_length = Cstruct.len input in
      let cons = Int32.to_int (RW.get_ring_input_cons t.ring) in
      let prod = Int32.to_int (RW.get_ring_input_prod t.ring) in
      memory_barrier ();
      let cons' =
        let x = cons mod input_length in
        if x < 0 then x + input_length else x
      and prod' =
        let x = prod mod input_length in
        if x < 0 then x + input_length else x in
      let data =
        if prod = cons
        then [ Cstruct.sub input 0 0 ]
        else
          if prod' > cons'
          then [ Cstruct.sub input cons' (prod' - cons') ]
          else [ Cstruct.sub input cons' (input_length - cons'); Cstruct.sub input 0 prod' ] in
      Int32.of_int cons, data

    let advance t (cons':int32) =
      let cons = RW.get_ring_input_cons t.ring in
      RW.set_ring_input_cons t.ring (max cons' cons);
      E.send t.channel
  end
end
