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

module type M = sig
  module Frontend: sig
	  include S.CHANNEL
      with type data = Cstruct.t list
       and type position = int32
       and type 'a io = 'a Lwt.t
		val create: ?buffer:Cstruct.t -> Shared_memory_ring.In_memory_events.channel -> Cstruct.t -> t
	end
  module Backend: sig
	  include S.CHANNEL
      with type data = Cstruct.t list
       and type position = int32
       and type 'a io = 'a Lwt.t
		val create: ?buffer:Cstruct.t -> Shared_memory_ring.In_memory_events.channel -> Cstruct.t -> t
  end
end

module Write_read(X: M) = struct
	let test buffer_size () =
	  let frontend_buffer = match buffer_size with
		| 0 -> None
		| n -> Some (Cstruct.create n) in
    let backend_buffer = match buffer_size with
    | 0 -> None
    | n -> Some (Cstruct.create n) in
		let page = Cstruct.of_bigarray (alloc_page ()) in
		let port, channel = In_memory_events.listen 0 in
		let f = X.Frontend.create ?buffer:frontend_buffer channel page in
		let channel' = In_memory_events.connect 0 port in
		let b = X.Backend.create ?buffer:backend_buffer channel' page in
		let message = "hello" in
		(* No data is available for reading *)
		let rec loop ofs =
			if ofs < Int32.(mul 1024l (of_int (String.length message))) then begin
				let ofs', buffers = X.Backend.Reader.available b in
				let data = to_string buffers in
				assert_equal ~printer:Int32.to_string ofs ofs';
				assert_equal ~printer:(fun x -> String.escaped x) "" data;
				let t =
					X.Frontend.write f ofs [ cstruct_of_string message ]
					>>= function
					| `Ok ofs ->
						X.Frontend.Writer.advance f ofs;
						X.Backend.Reader.wait b (String.length message)
						>>= fun () ->
						return (X.Backend.Reader.available b)
					| `Error x ->
						fail (Failure x) in
					let ofs', buffers = Lwt_main.run t in
					let buffer = to_string buffers in
					assert_equal ~printer:Int32.to_string ofs ofs';
					assert_equal ~printer:string_of_int (String.length message) (String.length buffer);
					assert_equal ~printer:(fun x -> String.escaped x) message buffer;
					let ofs' = Int32.(add ofs' (of_int (String.length buffer))) in
					X.Backend.Reader.advance b ofs';
				loop ofs'
			end in
	loop 0l
end

module Xenstore_write_read = Write_read(struct
	module Frontend = Xenstore_ring.Frontend(In_memory_events)
	module Backend = Xenstore_ring.Backend(In_memory_events)
end)

let _ =
  let suite = "ring" >:::
    [
		"unbuffered write then read" >:: Xenstore_write_read.test 0;
    "buffered write then read" >:: Xenstore_write_read.test 32;
		"buffered write then read" >:: Xenstore_write_read.test 64;
		"buffered write then read" >:: Xenstore_write_read.test 512;
		"buffered write then read" >:: Xenstore_write_read.test 1024;
    ] in
  OUnit2.run_test_tt_main (OUnit.ounit2_of_ounit1 suite)
