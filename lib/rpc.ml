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

open Memory
open Printf

type buf = Cstruct.t

let sub t off len = Cstruct.sub t off len

let length t = Cstruct.len t

(*
  struct sring {
    RING_IDX req_prod, req_event;
    RING_IDX rsp_prod, rsp_event;
    uint8_t  netfront_smartpoll_active;
    uint8_t  pad[47];
  };
*)
  (* (* It's unsafe to use these since they use multi-byte load/stores *)
     cstruct ring_hdr {
     uint32_t req_prod;
     uint32_t req_event;
     uint32_t rsp_prod;
     uint32_t rsp_event;
     uint64_t stuff
     } as little_endian
  *)

  (* offsets in the header: *)
  let _req_prod  = 0
  let _req_event = 4
  let _rsp_prod  = 8
  let _rsp_event = 12

  let initialise ring =
    (* initialise the *_event fields to 1, and the rest to 0 *)
    unsafe_save_uint32 ring _req_prod  0;
    unsafe_save_uint32 ring _req_event 1;
    unsafe_save_uint32 ring _rsp_prod  0;
    unsafe_save_uint32 ring _rsp_event 1;

  type sring = {
    buf: Cstruct.t;         (* Overall I/O buffer *)
    header_size: int; (* Header of shared ring variables, in bits *)
    idx_size: int;    (* Size in bits of an index slot *)
    nr_ents: int;     (* Number of index entries *)
    name: string;     (* For pretty printing only *)
  }

  let of_buf ~buf ~idx_size ~name =
    initialise buf;
    let header_size = 4+4+4+4+48 in (* header bytes size of struct sring *)
    (* Round down to the nearest power of 2, so we can mask indices easily *)
    let round_down_to_nearest_2 x =
      int_of_float (2. ** (floor ( (log (float x)) /. (log 2.)))) in
    (* Free space in shared ring after header is accounted for *)
    let free_bytes = length buf - header_size in
    let nr_ents = round_down_to_nearest_2 (free_bytes / idx_size) in
    { name; buf; idx_size; nr_ents; header_size }

  let to_summary_string t =
    Printf.sprintf "ring %s header_size = %d; index slot size = %d; number of entries = %d" t.name t.header_size t.idx_size t.nr_ents

  let sring_rsp_prod sring =
    unsafe_load_uint32 sring.buf _rsp_prod
  let sring_req_prod sring =
    unsafe_load_uint32 sring.buf _req_prod
  let sring_req_event sring =
    memory_barrier ();
    unsafe_load_uint32 sring.buf _req_event
  let sring_rsp_event sring =
    memory_barrier ();
    unsafe_load_uint32 sring.buf _rsp_event

  let sring_push_requests sring req_prod =
    memory_barrier (); (* ensure requests are seen before the index is updated *)
    unsafe_save_uint32 sring.buf _req_prod req_prod

  let sring_push_responses sring rsp_prod =
    memory_barrier (); (* ensure requests are seen before the index is updated *)
    unsafe_save_uint32 sring.buf _rsp_prod rsp_prod

  let sring_set_rsp_event sring rsp_event =
    unsafe_save_uint32 sring.buf _rsp_event rsp_event;
    memory_barrier ()

  let sring_set_req_event sring req_event =
    unsafe_save_uint32 sring.buf _req_event req_event;
    memory_barrier ()

  let nr_ents sring = sring.nr_ents

  let slot sring idx =
    (* TODO should precalculate these and store in the sring? this is fast-path *)
    let idx = idx land (sring.nr_ents - 1) in
    let off = sring.header_size + (idx * sring.idx_size) in
    sub sring.buf off sring.idx_size

  module Front = struct

    type ('a,'b) t = {
      mutable req_prod_pvt: int;
      mutable rsp_cons: int;
      sring: sring;
    }

    let init ~sring =
      let req_prod_pvt = 0 in
      let rsp_cons = 0 in
      { req_prod_pvt; rsp_cons; sring }

    let slot t idx = slot t.sring idx
    let nr_ents t = t.sring.nr_ents

    let get_free_requests t =
      t.sring.nr_ents - (t.req_prod_pvt - t.rsp_cons)

    let is_ring_full t =
      get_free_requests t = 0

    let has_unconsumed_responses t =
      ((sring_rsp_prod t.sring) - t.rsp_cons) > 0

    let push_requests t =
      sring_push_requests t.sring t.req_prod_pvt

    let push_requests_and_check_notify t =
      let old_idx = sring_req_prod t.sring in
      let new_idx = t.req_prod_pvt in
      push_requests t;
      (new_idx - (sring_req_event t.sring)) < (new_idx - old_idx)

    let check_for_responses t =
      if has_unconsumed_responses t then
        true
      else begin
        sring_set_rsp_event t.sring (t.rsp_cons + 1);
        has_unconsumed_responses t
      end

    let next_req_id t =
      let s = t.req_prod_pvt in
      t.req_prod_pvt <- t.req_prod_pvt + 1;
      s

    let rec ack_responses t fn =
      let rsp_prod = sring_rsp_prod t.sring in
      while t.rsp_cons != rsp_prod do
        let slot_id = t.rsp_cons in
        let slot = slot t slot_id in
        fn slot;
        t.rsp_cons <- t.rsp_cons + 1;
      done;
      if check_for_responses t then ack_responses t fn

    let to_string t =
      let nr_unconsumed_responses = (sring_rsp_prod t.sring) - t.rsp_cons in
      let nr_free_requests = get_free_requests t in
      Printf.sprintf "Front { req_prod = %d; rsp_prod = %d; req_event = %d; rsp_event = %d; rsp_cons = %d; req_prod_pvt = %d; %s; %s }"
        (sring_req_prod t.sring)
        (sring_rsp_prod t.sring)
        (sring_req_event t.sring)
        (sring_rsp_event t.sring)
        t.rsp_cons
        t.req_prod_pvt
        (if nr_unconsumed_responses > 0
         then Printf.sprintf "%d unconsumed responses" nr_unconsumed_responses
         else "frontend has consumed all responses")
        (if nr_free_requests > 0
         then Printf.sprintf "%d free request slots" nr_free_requests
         else "all slots are full")
  end

  module Back = struct

    type ('a,'b) t = {
      mutable rsp_prod_pvt: int;
      mutable req_cons: int;
      sring: sring;
    }

    let init ~sring =
      let rsp_prod_pvt = 0 in
      let req_cons = 0 in
      { rsp_prod_pvt; req_cons; sring }

    let slot t idx = slot t.sring idx

    let nr_ents t = t.sring.nr_ents

    let has_unconsumed_requests t =
      let req = (sring_req_prod t.sring) - t.req_cons in
      let rsp = t.sring.nr_ents - (t.req_cons - t.rsp_prod_pvt) in
      if req < rsp then (req > 0) else (rsp > 0)

    let push_responses t =
      sring_push_responses t.sring t.rsp_prod_pvt

    let push_responses_and_check_notify t =
      let old_idx = sring_rsp_prod t.sring in
      let new_idx = t.rsp_prod_pvt in
      push_responses t;
      (new_idx - (sring_rsp_event t.sring)) < (new_idx - old_idx)

    let check_for_requests t =
      if has_unconsumed_requests t then
        true
      else begin
        sring_set_req_event t.sring (t.req_cons + 1);
        has_unconsumed_requests t
      end

    let next_res_id t =
      let s = t.rsp_prod_pvt in
      t.rsp_prod_pvt <- t.rsp_prod_pvt + 1;
      s

    let next_slot t =
      slot t (next_res_id t)

    let final_check_for_requests t =
      has_unconsumed_requests t ||
      begin
        sring_set_req_event t.sring (t.req_cons + 1);
        has_unconsumed_requests t
      end

    let more_to_do t =
      if t.rsp_prod_pvt = t.req_cons then
        final_check_for_requests t
      else
        has_unconsumed_requests t

    let to_string t =
      let req_prod = sring_req_prod t.sring in
      let rsp_prod = sring_rsp_prod t.sring in
      let req_event = sring_req_event t.sring in
      let rsp_event = sring_rsp_event t.sring in
      Printf.sprintf "{ req_prod=%d rsp_prod=%d req_event=%d rsp_event=%d rsp_prod_pvt=%d req_cons=%d }" req_prod rsp_prod req_event rsp_event t.rsp_prod_pvt t.req_cons

    let rec ack_requests t fn =
      let req_prod = sring_req_prod t.sring in
      while t.req_cons != req_prod do
        let slot_id = t.req_cons in
        let slot = slot t slot_id in
        t.req_cons <- t.req_cons + 1;
        fn slot;
      done;
      if check_for_requests t then ack_requests t fn
  end
