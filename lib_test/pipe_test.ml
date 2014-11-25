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
open Shared_memory_ring
open OUnit
open Lwt

let ( |> ) a b = b a
let id x = x

let alloc_page () =
	Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096

let length t = Cstruct.len t

let compare_bufs a b =
	assert_equal ~printer:string_of_int (Bigarray.Array1.dim a) (Bigarray.Array1.dim b);
	for i = 0 to Bigarray.Array1.dim a - 1 do
		let x = Bigarray.Array1.unsafe_get a i in
		let y = Bigarray.Array1.unsafe_get b i in
		assert_equal ~printer:(fun c -> Printf.sprintf "%02x" (int_of_char c)) x y
	done

let bigarray_to_string a =
	let s = String.make (Bigarray.Array1.dim a) '\000' in
	for i = 0 to Bigarray.Array1.dim a - 1 do
		s.[i] <- Bigarray.Array1.unsafe_get a i
	done;
	s

let cstruct_of_string s =
	let len = String.length s in
	let result = Cstruct.create len in
	Cstruct.blit_from_string s 0 result 0 len;
	result

let to_string x = String.concat "" (List.map Cstruct.to_string x)

module Frontend = Xenstore_ring.Frontend(In_memory_events)
module Backend = Xenstore_ring.Backend(In_memory_events)

let write_read () =
	let page = Cstruct.of_bigarray (alloc_page ()) in
	let port, channel = In_memory_events.listen 0 in
	let f = Frontend.create channel page in
	let channel' = In_memory_events.connect 0 port in
	let b = Backend.create channel' page in

        (* No data is available for reading *)
        let ofs, buffers = Backend.Reader.available b in
        let data = to_string buffers in
        assert_equal ~printer:Int32.to_string 0l ofs;
        assert_equal ~printer:(fun x -> String.escaped x) "" data;

        let message = "hello" in
	let t =
		Frontend.write f 0l [ cstruct_of_string message ]
		>>= function
                | `Ok ofs ->
                  Frontend.Writer.advance f ofs;
		  return (Backend.Reader.available b)
                | `Error x ->
                  fail (Failure x) in
	let ofs, buffers = Lwt_main.run t in
        let buffer = to_string buffers in
	assert_equal ~printer:Int32.to_string 0l ofs;
	assert_equal ~printer:string_of_int (String.length message) (String.length buffer);
	assert_equal ~printer:(fun x -> String.escaped x) message buffer

let _ =
  let suite = "ring" >:::
    [
		"write then read" >:: write_read;
    ] in
  OUnit2.run_test_tt_main (OUnit.ounit2_of_ounit1 suite)
