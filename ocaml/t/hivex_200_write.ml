(* hivex OCaml bindings
 * Copyright (C) 2009-2010 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)

(* Test some significant write operations.  Take the minimal hive
 * and algorithmically construct a large, deep hive.
 *)

open Unix
open Printf
let (//) = Filename.concat
let srcdir = try Sys.getenv "srcdir" with Not_found -> "."

let () =
  let h = Hivex.open_file (srcdir // "../images/minimal") [Hivex.OPEN_WRITE] in

  let degrees = [| 3; 1; 4; 1; 5; 9; 2 |] (* ~1000 nodes *) in
  let numbers = [| "Zero"; "One"; "Two"; "Three"; "Four";
                   "Five"; "Six"; "Seven"; "Eight"; "Nine" |] in
  let animals = [| "Horse"; "Ant"; "Mouse"; "Rabbit"; "Cat";
                   "Giraffe"; "Kangaroo"; "Tiger"; "Zebra"; "Elephant" |] in

  let rec iter depth posn parent =
    if depth < Array.length degrees then (
      let degree = degrees.(depth) in
      for i = 0 to degree-1 do
        let node_name = numbers.(depth) ^ " " ^ animals.(i) in
        let node = Hivex.node_add_child h parent node_name in
        iter (depth+1) i node
      done;
      let values = Array.init (10-posn) (
        fun i ->
          { Hivex.key = animals.(i);
            t = Hivex.REG_SZ;
            value = utf16le_of_ascii numbers.(i) }
      ) in
      Hivex.node_set_values h parent values
    )

  (* Make a nul-terminated UTF16-LE string from an ASCII string. *)
  and utf16le_of_ascii str =
    let len = String.length str in
    let len' = len * 2 + 2 in
    let str' = Bytes.create len' in
    for i = 0 to len-1 do
      Bytes.set str' (i*2) str.[i];
      Bytes.set str' (i*2+1) '\000'
    done;
    Bytes.set str' (len'-2) '\000';
    Bytes.set str' (len'-1) '\000';
    Bytes.to_string str'
  in
  iter 0 0 (Hivex.root h);

  (* Discard the changes. *)
  Hivex.close h;

  (* Gc.compact is a good way to ensure we don't have
   * heap corruption or double-freeing.
   *)
  Gc.compact ()
