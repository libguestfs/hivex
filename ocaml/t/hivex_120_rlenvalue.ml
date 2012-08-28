(* hivex OCaml bindings
 * Copyright (C) 2009-2010, 2012 Red Hat Inc.
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

(* Demonstrate value_data_cell_offset by looking at the value data at
 * "\$$$PROTO.HIV\ModerateValueParent\33Bytes", verified to be at file
 * offset 8680 (0x21e8) of the hive rlenvalue_test_hive.  The returned
 * length and offset for this value cell should be 37 bytes, position
 * 8712.
 *)

open Unix
open Printf
let (//) = Filename.concat
let srcdir = try Sys.getenv "srcdir" with Not_found -> "."

let () =
  let h = Hivex.open_file (srcdir // "../images/rlenvalue_test_hive") [] in
  let root = Hivex.root h in
  let moderate_value_node = Hivex.node_get_child h root "ModerateValueParent" in
  let moderate_value_value = Hivex.node_get_value h moderate_value_node "33Bytes" in
  let (data_len, data_off) = Hivex.value_data_cell_offset h moderate_value_value in
  assert ( (data_off == (Obj.magic 8712:Hivex.value)) && (data_len == 37) );

  Hivex.close h;

  (* Gc.compact is a good way to ensure we don't have
   * heap corruption or double-freeing.
   *)
  Gc.compact ()
