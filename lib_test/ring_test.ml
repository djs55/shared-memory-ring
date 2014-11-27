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

module Xenstore_frontend = Xenstore_ring.Frontend(In_memory_events.Events)
module Xenstore_backend = Xenstore_ring.Backend(In_memory_events.Events)

let with_xenstores fn =
	let b1 = alloc_page () in
	let b2 = alloc_page () in
	Memory.zero (Cstruct.of_bigarray b1);
	Memory.zero (Cstruct.of_bigarray b2);
	let a = Cstruct.of_bigarray b1 in
	let b = Old_ring.C_Xenstore.of_buf b2 in
        let port, channel = In_memory_events.Events.listen 0 in
				let channel' = In_memory_events.Events.connect 0 port in
        let f = Xenstore_frontend.create channel a in
Old_ring.C_Xenstore.zero b;
				let backend = Xenstore_backend.create channel' (Cstruct.of_bigarray b2) in
        Xenstore_frontend.init f;
	fn b1 b2 f b backend

let xenstore_init () =
	with_xenstores
		(fun b1 b2 _ _ _ ->
			compare_bufs b1 b2
		)

let xenstore_hello () =
	let msg = "hello" in
	let buf = String.make 16 '\000' in
	with_xenstores
		(fun b1 b2 a b backend ->
			let t =
		  	Xenstore_frontend.write a 0l [ cstruct_of_string msg ]
			  >>= function
				| `Ok ofs ->
					Xenstore_frontend.Writer.advance a ofs;
					return ()
				| `Error msg -> fail (Failure msg) in
			Lwt_main.run t;
			let _ = Old_ring.C_Xenstore.unsafe_write b msg (String.length msg) in
			compare_bufs b1 b2;
			let ofs, buffers = Xenstore_backend.Reader.available backend in
			Xenstore_backend.Reader.advance backend ofs;
			let x = to_string buffers in
			assert_equal ~printer:String.escaped msg x;
			let x = Old_ring.C_Xenstore.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:String.escaped msg (String.sub buf 0 x);
			()
		)

cstruct ring {
	uint8_t output[1024];
	uint8_t input[1024];
	uint32_t output_cons;
	uint32_t output_prod;
	uint32_t input_cons;
	uint32_t input_prod
} as little_endian

let check_signed_unsigned_write () =
	(* Check for errors performing comparison across int32 max_int *)
	let msg = "this is a test" in
	let ofs = Int32.(succ (succ max_int)) in
	with_xenstores
		(fun b1 b2 a b backend ->
			set_ring_output_cons (Cstruct.of_bigarray b1) ofs;
			set_ring_output_prod (Cstruct.of_bigarray b1) ofs;
			set_ring_output_cons (Cstruct.of_bigarray b2) ofs;
			set_ring_output_prod (Cstruct.of_bigarray b2) ofs;
			let t =
				Xenstore_frontend.write a 0l [ cstruct_of_string msg ]
				>>= function
				| `Ok ofs ->
					Xenstore_frontend.Writer.advance a ofs;
					return ()
				| `Error msg -> fail (Failure msg) in
			Lwt_main.run t;
			let _ = Old_ring.C_Xenstore.unsafe_write b msg (String.length msg) in
			compare_bufs b1 b2;
		)

let check_signed_unsigned_read () =
	let msg = "this is a test" in
	let buf = String.make (String.length msg) '\000' in
	with_xenstores
		(fun b1 b2 a b backend ->
			set_ring_output_cons (Cstruct.of_bigarray b1) (Int32.(pred (pred max_int)));
			set_ring_output_prod (Cstruct.of_bigarray b1) (Int32.(succ (succ max_int)));
			set_ring_output_cons (Cstruct.of_bigarray b2) (Int32.(pred (pred max_int)));
 			set_ring_output_prod (Cstruct.of_bigarray b2) (Int32.(succ (succ max_int)));
			let ofs, _ = Xenstore_backend.Reader.available backend in
			Xenstore_backend.Reader.advance backend ofs;
			let _ = Old_ring.C_Xenstore.Back.unsafe_read b buf (String.length buf) in
			compare_bufs b1 b2;
		)


module Console_frontend = Console_ring.Frontend(In_memory_events.Events)
module Console_backend = Console_ring.Backend(In_memory_events.Events)

let with_consoles fn =
	let b1 = alloc_page () in
	let b2 = alloc_page () in
	Memory.zero (Cstruct.of_bigarray b1);
	Memory.zero (Cstruct.of_bigarray b2);
	let a = Cstruct.of_bigarray b1 in
	let b = Old_ring.C_Console.of_buf b2 in
				let port, channel = In_memory_events.Events.listen 0 in
				let channel' = In_memory_events.Events.connect 0 port in
				let f = Console_frontend.create channel a in
Old_ring.C_Console.zero b;
				let backend = Console_backend.create channel' (Cstruct.of_bigarray b2) in
				Console_frontend.init f;
	fn b1 b2 f b backend

let console_init () =
	with_consoles
		(fun b1 b2 _ _ _ ->
			compare_bufs b1 b2
		)

let console_hello () =
	let msg = "hello" in
	let buf = String.make 16 '\000' in
	with_consoles
		(fun b1 b2 a b backend ->
			let t =
				Console_frontend.write a 0l [ cstruct_of_string msg ]
				>>= function
				| `Ok ofs ->
					Console_frontend.Writer.advance a ofs;
					return ()
				| `Error msg -> fail (Failure msg) in
			Lwt_main.run t;
			let _ = Old_ring.C_Console.unsafe_write b msg (String.length msg) in
			compare_bufs b1 b2;

			let ofs, buffers = Console_backend.Reader.available backend in
			Console_backend.Reader.advance backend ofs;
			let x = to_string buffers in

			assert_equal msg x;
			let x = Old_ring.C_Console.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal msg (String.sub buf 0 x);
			()
		)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test shared memory ring code";

  let suite = "ring" >:::
    [
		"xenstore_init" >:: xenstore_init;
(* XXX need to diagnose the ARM failure:
		"check_signed_unsigned_read" >:: check_signed_unsigned_read;
		"check_signed_unsigned_write" >:: check_signed_unsigned_write;
*)
		"xenstore_hello" >:: xenstore_hello;
		"console_init" >:: console_init;
		"console_hello" >:: console_hello;
    ] in
  run_test_tt ~verbose:!verbose suite
