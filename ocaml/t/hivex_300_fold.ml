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

(* Fold over the large hive. *)

open Unix
open Printf
let (//) = Filename.concat

(* This is a generic function to fold over hives.
 *   fn : 'a -> node -> 'a is called for each node
 *   fv : 'a -> node -> value array -> 'a is called for the values at each node
 *)
let hive_fold h fn fv a root =
  let rec fold a node =
    let a = fn a node in
    let a = fv a node (Hivex.node_values h node) in
    Array.fold_left fold a (Hivex.node_children h node)
  in
  fold a root

let () =
  let h = Hivex.open_file ("../images/large") [] in

  (* Count the number of nodes and values in the hive. *)
  let count_node (nodes, values) _ = (nodes+1, values) in
  let count_values (nodes, values) _ vs = (nodes, values + Array.length vs) in
  let root = Hivex.root h in
  let (nodes, values) = hive_fold h count_node count_values (0, 0) root in
  printf "large test hive contains %d nodes and %d values\n%!" nodes values;

  Hivex.close h;

  (* Gc.compact is a good way to ensure we don't have
   * heap corruption or double-freeing.
   *)
  Gc.compact ()
