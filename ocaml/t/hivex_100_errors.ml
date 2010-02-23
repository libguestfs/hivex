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

(* Test different types of error handling used by the API. *)

open Unix
open Printf
let (//) = Filename.concat
let srcdir = try Sys.getenv "srcdir" with Not_found -> "."

let () =
  printf "01 non-existent file\n%!";
  (try
     ignore (Hivex.open_file "no_such_file" []);
     failwith "no exception thrown when opening a non-existent file"
   with
   | Hivex.Error ("open", ENOENT, _) -> () (* ok *)
   (* let any other exception escape and stop the test *)
  );

  printf "02 closed handle\n%!";
  let h = Hivex.open_file (srcdir // "../images/minimal") [] in
  Hivex.close h;
  (try
     ignore (Hivex.root h)
   with
   | Hivex.Handle_closed "root" -> () (* ok *)
   (* let any other exception escape and stop the test *)
  );

  printf "03 write to read-only file\n%!";
  let h = Hivex.open_file (srcdir // "../images/minimal") [] in
  (try
     ignore (Hivex.node_add_child h (Hivex.root h) "Foo")
   with
   | Hivex.Error ("node_add_child", EROFS, _) -> () (* ok *)
   (* let any other exception escape and stop the test *)
  );
  Hivex.close h;

  printf "04 node_get_child node not found\n%!";
  let h = Hivex.open_file (srcdir // "../images/minimal") [] in
  (try
     ignore (Hivex.node_get_child h (Hivex.root h) "NoSuchNode")
   with
   | Not_found -> () (* ok *)
   (* let any other exception escape and stop the test *)
  );
  Hivex.close h;

  (* Gc.compact is a good way to ensure we don't have
   * heap corruption or double-freeing.
   *)
  Gc.compact ()
