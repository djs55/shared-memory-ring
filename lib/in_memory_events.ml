(*
 * Copyright (C) Citrix Systems Inc.
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

(* FIXME: This is duplicated from ocaml-vchan. This should probably be pushed into
   xen-evtchn *)

let next_port = ref 0

let channels = Array.make 1024 0
let c = Lwt_condition.create ()

type state =
| Connected of int
| Closed
| Unbound
with sexp

let connected_to = Array.make 1024 Unbound

module Events = struct
  open Lwt

  type 'a io = 'a Lwt.t

  type port = int with sexp_of

  let port_of_string x = `Ok (int_of_string x)
  let string_of_port = string_of_int

  type channel = int with sexp_of
  let get () =
    incr next_port;
    !next_port - 1

  type event = int with sexp_of
  let initial = 0

  let rec recv channel event =
    if channels.(channel) > event
    then return channels.(channel)
    else
      Lwt_condition.wait c >>= fun () ->
      recv channel event

  let send channel =
    match connected_to.(channel) with
    | Connected otherend ->
      channels.(otherend) <- channels.(otherend) + 1;
      Lwt_condition.broadcast c ()
    | x ->
      let msg = Printf.sprintf "send: event channel %d in state %s" channel (Sexplib.Sexp.to_string_hum (sexp_of_state x)) in
      Printf.fprintf stderr "%s\n%!" msg;
      failwith msg

  let listen _ =
    let port = get () in
    match connected_to.(port) with
    | Connected _ -> assert false
    | _ ->
      Printf.fprintf stderr "%d listen\n%!" port;
      port, port

  let connect _ port =
    let port' = get () in
    connected_to.(port') <- Connected port;
    connected_to.(port) <- Connected port';
    Printf.fprintf stderr "%d <-> %d\n%!" port port';
    port'

  let close port =
    channels.(port) <- 0;
    Printf.fprintf stderr "%d close\n%!" port;
    connected_to.(port) <- Closed

  let assert_cleaned_up () =
    for i = 0 to Array.length connected_to - 1 do
      match connected_to.(i) with
      | Connected _ ->
        Printf.fprintf stderr "Some event channels are still connected\n%!";
        failwith "some event channels are still connected"
      | _ -> ()
    done
end
