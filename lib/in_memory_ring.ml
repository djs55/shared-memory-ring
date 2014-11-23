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
open Sexplib.Std
open Memory

module Layout = struct

  type t = Cstruct.t
  type data = Cstruct.t
  type position = int32 with sexp

  (* Place the producer/consumer pointers at the beginning and divide
     the rest of the buffer space into 2 equal parts for input and output *)
  let _input_cons  = 0
  let _input_prod  = _input_cons + 4
  let _output_cons = _input_prod + 4
  let _output_prod = _output_cons  + 4
  let get_ring_input       c =
    let data = Cstruct.shift c (_output_prod + 4) in
    let data_len = Cstruct.len data in
    Cstruct.sub data 0 (data_len / 2)
  let get_ring_output      c =
    let data = Cstruct.shift c (_output_prod + 4) in
    let data_len = Cstruct.len data in
    Cstruct.sub data (data_len / 2) (data_len / 2)
  let get_ring_output_cons c = Int32.of_int (unsafe_load_uint32 c _output_cons)
  let get_ring_output_prod c = Int32.of_int (unsafe_load_uint32 c _output_prod)
  let get_ring_input_cons  c = Int32.of_int (unsafe_load_uint32 c _input_cons)
  let get_ring_input_prod  c = Int32.of_int (unsafe_load_uint32 c _input_prod)
  let set_ring_output_cons c x = unsafe_save_uint32 c _output_cons (Int32.to_int x)
  let set_ring_output_prod c x = unsafe_save_uint32 c _output_prod (Int32.to_int x)
  let set_ring_input_cons  c x = unsafe_save_uint32 c _input_cons (Int32.to_int x)
  let set_ring_input_prod  c x = unsafe_save_uint32 c _input_prod (Int32.to_int x)
end

module Make(E: S.EVENTS with type 'a io = 'a Lwt.t) = struct
  include Pipe.Make(E)(Layout)

  let create buffer =
    (* We need space for the pointers and at least one byte in each direction *)
    if Cstruct.len buffer < (Layout._output_prod + 4 + 1 + 1)
    then invalid_arg "In_memory_ring.Make(...).create: buffer is impossibly small";
    buffer

  let init buffer = Memory.zero buffer
end
