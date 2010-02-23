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

(* Test that the root of the minimal hive exists. *)

open Unix
open Printf
let (//) = Filename.concat
let srcdir = try Sys.getenv "srcdir" with Not_found -> "."

let () =
  let h = Hivex.open_file (srcdir // "../images/minimal") [] in
  ignore (Hivex.root h);
  Hivex.close h;

  (* Gc.compact is a good way to ensure we don't have
   * heap corruption or double-freeing.
   *)
  Gc.compact ()
