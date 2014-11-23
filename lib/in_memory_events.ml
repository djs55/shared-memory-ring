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

module Events = struct
  open Lwt

  type 'a io = 'a Lwt.t

  type port = int with sexp_of

  let port_of_string x = `Ok (int_of_string x)
  let string_of_port = string_of_int

  type channel = int with sexp_of
  let get =
    let g = ref 0 in
    fun () ->
      incr g;
      !g - 1

  type event = int with sexp_of
  let initial = 0

  let channels = Array.make 1024 0
  let c = Lwt_condition.create ()

  let rec recv channel event =
    if channels.(channel) > event
    then return channels.(channel)
    else
      Lwt_condition.wait c >>= fun () ->
      recv channel event

  let connected_to = Array.make 1024 (-1)

  let send channel =
    let listening = connected_to.(channel) in
    if listening = -1 then begin
      Printf.fprintf stderr "send: event channel %d is closed\n%!" channel;
      failwith (Printf.sprintf "send: event channel %d is closed" channel);
    end;
    channels.(listening) <- channels.(listening) + 1;
    Lwt_condition.broadcast c ()

  let listen _ =
    let port = get () in
    port, port

  let connect _ port =
    let port' = get () in
    connected_to.(port') <- port;
    connected_to.(port) <- port';
    port'

  let close port =
    channels.(port) <- 0;
    connected_to.(port) <- -1

  let assert_cleaned_up () =
    for i = 0 to Array.length connected_to - 1 do
      if connected_to.(i) <> (-1) then begin
        Printf.fprintf stderr "Some event channels are still connected\n%!";
        failwith "some event channels are still connected"
      end
    done
end

include Events
