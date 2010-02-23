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

(* Test that the handle is GC'd (closed) when unreachable.
 *
 * XXX Actually we cannot really test that, but at least make
 * sure there is no error.
 *)

open Unix
open Printf
let (//) = Filename.concat
let srcdir = try Sys.getenv "srcdir" with Not_found -> "."

let () =
  ignore (Hivex.open_file (srcdir // "../images/minimal") []);
  Gc.compact ()
