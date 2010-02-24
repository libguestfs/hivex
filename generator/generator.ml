#!/usr/bin/env ocaml
(* hivex
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(* This script generates language bindings and some documentation for
 * hivex.
 * 
 * After editing this file, run it (./generator/generator.ml) to
 * regenerate all the output files.  'make' will rerun this
 * automatically when necessary.  Note that if you are using a separate
 * build directory you must run generator.ml from the _source_
 * directory.
 * 
 * IMPORTANT: This script should NOT print any warnings.  If it prints
 * warnings, you should treat them as errors.
 * 
 * OCaml tips: (1) In emacs, install tuareg-mode to display and format
 * OCaml code correctly.  'vim' comes with a good OCaml editing mode by
 * default.  (2) Read the resources at http://ocaml-tutorial.org/
 *)

#load "unix.cma";;
#load "str.cma";;
#directory "+xml-light";;
#load "xml-light.cma";;

open Unix
open Printf

type style = ret * args
and ret =
  | RErr                                (* 0 = ok, -1 = error *)
  | RHive                               (* Returns a hive_h or NULL. *)
  | RNode                               (* Returns hive_node_h or 0. *)
  | RNodeList                           (* Returns hive_node_h* or NULL. *)
  | RValue                              (* Returns hive_value_h or 0. *)
  | RValueList                          (* Returns hive_value_h* or NULL. *)
  | RString                             (* Returns char* or NULL. *)
  | RStringList                         (* Returns char** or NULL. *)
  | RLenType                            (* See hivex_value_type. *)
  | RLenTypeVal                         (* See hivex_value_value. *)
  | RInt32                              (* Returns int32. *)
  | RInt64                              (* Returns int64. *)

and args = argt list                    (* List of parameters. *)

and argt =                              (* Note, cannot be NULL/0 unless it
                                           says so explicitly below. *)
  | AHive                               (* hive_h* *)
  | ANode of string                     (* hive_node_h *)
  | AValue of string                    (* hive_value_h *)
  | AString of string                   (* char* *)
  | AStringNullable of string           (* char* (can be NULL) *)
  | AOpenFlags                          (* HIVEX_OPEN_* flags list. *)
  | AUnusedFlags                        (* Flags arg that is always 0 *)
  | ASetValues                          (* See hivex_node_set_values. *)

(* Hive types, from:
 * https://secure.wikimedia.org/wikipedia/en/wiki/Windows_Registry#Keys_and_values
 * 
 * It's unfortunate that in our original C binding we strayed away from
 * the names that Windows uses (eg. REG_SZ for strings).  We include
 * both our names and the Windows names.
 *)
let hive_types = [
  0, "none", "NONE",
    "Just a key without a value";
  1, "string", "SZ",
    "A Windows string (encoding is unknown, but often UTF16-LE)";
  2, "expand_string", "EXPAND_SZ",
    "A Windows string that contains %env% (environment variable expansion)";
  3, "binary", "BINARY",
    "A blob of binary";
  4, "dword", "DWORD",
    "DWORD (32 bit integer), little endian";
  5, "dword_be", "DWORD_BIG_ENDIAN",
    "DWORD (32 bit integer), big endian";
  6, "link", "LINK",
    "Symbolic link to another part of the registry tree";
  7, "multiple_strings", "MULTI_SZ",
    "Multiple Windows strings.  See http://blogs.msdn.com/oldnewthing/archive/2009/10/08/9904646.aspx";
  8, "resource_list", "RESOURCE_LIST",
    "Resource list";
  9, "full_resource_description", "FULL_RESOURCE_DESCRIPTOR",
    "Resource descriptor";
  10, "resource_requirements_list", "RESOURCE_REQUIREMENTS_LIST",
    "Resouce requirements list";
  11, "qword", "QWORD",
    "QWORD (64 bit integer), unspecified endianness but usually little endian"
]

(* Open flags (bitmask passed to AOpenFlags) *)
let open_flags = [
  1, "VERBOSE", "Verbose messages";
  2, "DEBUG", "Debug messages";
  4, "WRITE", "Enable writes to the hive";
]

(* The API calls. *)
let functions = [
  "open", (RHive, [AString "filename"; AOpenFlags]),
    "open a hive file",
    "\
Opens the hive named C<filename> for reading.

Flags is an ORed list of the open flags (or C<0> if you don't
want to pass any flags).  These flags are defined:

=over 4

=item HIVEX_OPEN_VERBOSE

Verbose messages.

=item HIVEX_OPEN_DEBUG

Very verbose messages, suitable for debugging problems in the library
itself.

This is also selected if the C<HIVEX_DEBUG> environment variable
is set to 1.

=item HIVEX_OPEN_WRITE

Open the hive for writing.  If omitted, the hive is read-only.

See L<hivex(3)/WRITING TO HIVE FILES>.

=back";

  "close", (RErr, [AHive]),
    "close a hive handle",
    "\
Close a hive handle and free all associated resources.

Note that any uncommitted writes are I<not> committed by this call,
but instead are lost.  See L<hivex(3)/WRITING TO HIVE FILES>.";

  "root", (RNode, [AHive]),
    "return the root node of the hive",
    "\
Return root node of the hive.  All valid registries must contain
a root node.";

  "node_name", (RString, [AHive; ANode "node"]),
    "return the name of the node",
    "\
Return the name of the node.

Note that the name of the root node is a dummy, such as
C<$$$PROTO.HIV> (other names are possible: it seems to depend on the
tool or program that created the hive in the first place).  You can
only know the \"real\" name of the root node by knowing which registry
file this hive originally comes from, which is knowledge that is
outside the scope of this library.";

  "node_children", (RNodeList, [AHive; ANode "node"]),
    "return children of node",
    "\
Return an array of nodes which are the subkeys
(children) of C<node>.";

  "node_get_child", (RNode, [AHive; ANode "node"; AString "name"]),
    "return named child of node",
    "\
Return the child of node with the name C<name>, if it exists.

The name is matched case insensitively.

If the child node does not exist, this returns 0 without
setting errno.";

  "node_parent", (RNode, [AHive; ANode "node"]),
    "return the parent of node",
    "\
Return the parent of C<node>.

The parent pointer of the root node in registry files that we
have examined seems to be invalid, and so this function will
return an error if called on the root node.";

  "node_values", (RValueList, [AHive; ANode "node"]),
    "return (key, value) pairs attached to a node",
    "\
Return the array of (key, value) pairs attached to this node.";

  "node_get_value", (RValue, [AHive; ANode "node"; AString "key"]),
    "return named key at node",
    "\
Return the value attached to this node which has the name C<key>,
if it exists.

The key name is matched case insensitively.

Note that to get the default key, you should pass the empty
string C<\"\"> here.  The default key is often written C<\"@\">, but
inside hives that has no meaning and won't give you the
default key.";

  "value_key", (RString, [AHive; AValue "val"]),
    "return the key of a (key, value) pair",
    "\
Return the key (name) of a (key, value) pair.  The name
is reencoded as UTF-8 and returned as a string.

The string should be freed by the caller when it is no longer needed.

Note that this function can return a zero-length string.  In the
context of Windows Registries, this means that this value is the
default key for this node in the tree.  This is usually written
as C<\"@\">.";

  "value_type", (RLenType, [AHive; AValue "val"]),
    "return data length and data type of a value",
    "\
Return the data length and data type of the value in this (key, value)
pair.  See also C<hivex_value_value> which returns all this
information, and the value itself.  Also, C<hivex_value_*> functions
below which can be used to return the value in a more useful form when
you know the type in advance.";

  "value_value", (RLenTypeVal, [AHive; AValue "val"]),
    "return data length, data type and data of a value",
    "\
Return the value of this (key, value) pair.  The value should
be interpreted according to its type (see C<hive_type>).";

  "value_string", (RString, [AHive; AValue "val"]),
    "return value as a string",
    "\
If this value is a string, return the string reencoded as UTF-8
(as a C string).  This only works for values which have type
C<hive_t_string>, C<hive_t_expand_string> or C<hive_t_link>.";

  "value_multiple_strings", (RStringList, [AHive; AValue "val"]),
    "return value as multiple strings",
    "\
If this value is a multiple-string, return the strings reencoded
as UTF-8 (as a NULL-terminated array of C strings).  This only
works for values which have type C<hive_t_multiple_strings>.";

  "value_dword", (RInt32, [AHive; AValue "val"]),
    "return value as a DWORD",
    "\
If this value is a DWORD (Windows int32), return it.  This only works
for values which have type C<hive_t_dword> or C<hive_t_dword_be>.";

  "value_qword", (RInt64, [AHive; AValue "val"]),
    "return value as a QWORD",
    "\
If this value is a QWORD (Windows int64), return it.  This only
works for values which have type C<hive_t_qword>.";

  "commit", (RErr, [AHive; AStringNullable "filename"; AUnusedFlags]),
    "commit (write) changes to file",
    "\
Commit (write) any changes which have been made.

C<filename> is the new file to write.  If C<filename> is NULL then we
overwrite the original file (ie. the file name that was passed to
C<hivex_open>).  C<flags> is not used, always pass 0.

Note this does not close the hive handle.  You can perform further
operations on the hive after committing, including making more
modifications.  If you no longer wish to use the hive, call
C<hivex_close> after this.";

  "node_add_child", (RNode, [AHive; ANode "parent"; AString "name"]),
    "add child node",
    "\
Add a new child node named C<name> to the existing node C<parent>.
The new child initially has no subnodes and contains no keys or
values.  The sk-record (security descriptor) is inherited from
the parent.

The parent must not have an existing child called C<name>, so if you
want to overwrite an existing child, call C<hivex_node_delete_child>
first.";

  "node_delete_child", (RErr, [AHive; ANode "node"]),
    "delete child node",
    "\
Delete the node C<node>.  All values at the node and all subnodes are
deleted (recursively).  The C<node> handle and the handles of all
subnodes become invalid.  You cannot delete the root node.";

  "node_set_values", (RErr, [AHive; ANode "node"; ASetValues; AUnusedFlags]),
    "set (key, value) pairs at a node",
    "\
This call can be used to set all the (key, value) pairs stored in C<node>.

C<node> is the node to modify.  C<values> is an array of (key, value)
pairs.  There should be C<nr_values> elements in this array.  C<flags>
is not used, always pass 0.

Any existing values stored at the node are discarded, and their
C<hive_value_h> handles become invalid.  Thus you can remove all
values stored at C<node> by passing C<nr_values = 0>.

Note that this library does not offer a way to modify just a single
key at a node.  We don't implement a way to do this efficiently.";
]

(* Used to memoize the result of pod2text. *)
let pod2text_memo_filename = "generator/.pod2text.data"
let pod2text_memo : ((int * string * string), string list) Hashtbl.t =
  try
    let chan = open_in pod2text_memo_filename in
    let v = input_value chan in
    close_in chan;
    v
  with
    _ -> Hashtbl.create 13
let pod2text_memo_updated () =
  let chan = open_out pod2text_memo_filename in
  output_value chan pod2text_memo;
  close_out chan

(* Useful functions.
 * Note we don't want to use any external OCaml libraries which
 * makes this a bit harder than it should be.
 *)
module StringMap = Map.Make (String)

let failwithf fs = ksprintf failwith fs

let unique = let i = ref 0 in fun () -> incr i; !i

let replace_char s c1 c2 =
  let s2 = String.copy s in
  let r = ref false in
  for i = 0 to String.length s2 - 1 do
    if String.unsafe_get s2 i = c1 then (
      String.unsafe_set s2 i c2;
      r := true
    )
  done;
  if not !r then s else s2

let isspace c =
  c = ' '
  (* || c = '\f' *) || c = '\n' || c = '\r' || c = '\t' (* || c = '\v' *)

let triml ?(test = isspace) str =
  let i = ref 0 in
  let n = ref (String.length str) in
  while !n > 0 && test str.[!i]; do
    decr n;
    incr i
  done;
  if !i = 0 then str
  else String.sub str !i !n

let trimr ?(test = isspace) str =
  let n = ref (String.length str) in
  while !n > 0 && test str.[!n-1]; do
    decr n
  done;
  if !n = String.length str then str
  else String.sub str 0 !n

let trim ?(test = isspace) str =
  trimr ~test (triml ~test str)

let rec find s sub =
  let len = String.length s in
  let sublen = String.length sub in
  let rec loop i =
    if i <= len-sublen then (
      let rec loop2 j =
        if j < sublen then (
          if s.[i+j] = sub.[j] then loop2 (j+1)
          else -1
        ) else
          i (* found *)
      in
      let r = loop2 0 in
      if r = -1 then loop (i+1) else r
    ) else
      -1 (* not found *)
  in
  loop 0

let rec replace_str s s1 s2 =
  let len = String.length s in
  let sublen = String.length s1 in
  let i = find s s1 in
  if i = -1 then s
  else (
    let s' = String.sub s 0 i in
    let s'' = String.sub s (i+sublen) (len-i-sublen) in
    s' ^ s2 ^ replace_str s'' s1 s2
  )

let rec string_split sep str =
  let len = String.length str in
  let seplen = String.length sep in
  let i = find str sep in
  if i = -1 then [str]
  else (
    let s' = String.sub str 0 i in
    let s'' = String.sub str (i+seplen) (len-i-seplen) in
    s' :: string_split sep s''
  )

let files_equal n1 n2 =
  let cmd = sprintf "cmp -s %s %s" (Filename.quote n1) (Filename.quote n2) in
  match Sys.command cmd with
  | 0 -> true
  | 1 -> false
  | i -> failwithf "%s: failed with error code %d" cmd i

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
      match f x with
      | Some y -> y :: filter_map f xs
      | None -> filter_map f xs

let rec find_map f = function
  | [] -> raise Not_found
  | x :: xs ->
      match f x with
      | Some y -> y
      | None -> find_map f xs

let iteri f xs =
  let rec loop i = function
    | [] -> ()
    | x :: xs -> f i x; loop (i+1) xs
  in
  loop 0 xs

let mapi f xs =
  let rec loop i = function
    | [] -> []
    | x :: xs -> let r = f i x in r :: loop (i+1) xs
  in
  loop 0 xs

let count_chars c str =
  let count = ref 0 in
  for i = 0 to String.length str - 1 do
    if c = String.unsafe_get str i then incr count
  done;
  !count

let name_of_argt = function
  | AHive -> "h"
  | ANode n | AValue n | AString n | AStringNullable n -> n
  | AOpenFlags | AUnusedFlags -> "flags"
  | ASetValues -> "values"

(* Check function names etc. for consistency. *)
let check_functions () =
  let contains_uppercase str =
    let len = String.length str in
    let rec loop i =
      if i >= len then false
      else (
        let c = str.[i] in
        if c >= 'A' && c <= 'Z' then true
        else loop (i+1)
      )
    in
    loop 0
  in

  (* Check function names. *)
  List.iter (
    fun (name, _, _, _) ->
      if String.length name >= 7 && String.sub name 0 7 = "hivex" then
        failwithf "function name %s does not need 'hivex' prefix" name;
      if name = "" then
        failwithf "function name is empty";
      if name.[0] < 'a' || name.[0] > 'z' then
        failwithf "function name %s must start with lowercase a-z" name;
      if String.contains name '-' then
        failwithf "function name %s should not contain '-', use '_' instead."
          name
  ) functions;

  (* Check function parameter/return names. *)
  List.iter (
    fun (name, style, _, _) ->
      let check_arg_ret_name n =
        if contains_uppercase n then
          failwithf "%s param/ret %s should not contain uppercase chars"
            name n;
        if String.contains n '-' || String.contains n '_' then
          failwithf "%s param/ret %s should not contain '-' or '_'"
            name n;
        if n = "value" then
          failwithf "%s has a param/ret called 'value', which causes conflicts in the OCaml bindings, use something like 'val' or a more descriptive name" name;
        if n = "int" || n = "char" || n = "short" || n = "long" then
          failwithf "%s has a param/ret which conflicts with a C type (eg. 'int', 'char' etc.)" name;
        if n = "i" || n = "n" then
          failwithf "%s has a param/ret called 'i' or 'n', which will cause some conflicts in the generated code" name;
        if n = "argv" || n = "args" then
          failwithf "%s has a param/ret called 'argv' or 'args', which will cause some conflicts in the generated code" name;

        (* List Haskell, OCaml and C keywords here.
         * http://www.haskell.org/haskellwiki/Keywords
         * http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#operator-char
         * http://en.wikipedia.org/wiki/C_syntax#Reserved_keywords
         * Formatted via: cat c haskell ocaml|sort -u|grep -vE '_|^val$' \
         *   |perl -pe 's/(.+)/"$1";/'|fmt -70
         * Omitting _-containing words, since they're handled above.
         * Omitting the OCaml reserved word, "val", is ok,
         * and saves us from renaming several parameters.
         *)
        let reserved = [
          "and"; "as"; "asr"; "assert"; "auto"; "begin"; "break"; "case";
          "char"; "class"; "const"; "constraint"; "continue"; "data";
          "default"; "deriving"; "do"; "done"; "double"; "downto"; "else";
          "end"; "enum"; "exception"; "extern"; "external"; "false"; "float";
          "for"; "forall"; "foreign"; "fun"; "function"; "functor"; "goto";
          "hiding"; "if"; "import"; "in"; "include"; "infix"; "infixl";
          "infixr"; "inherit"; "initializer"; "inline"; "instance"; "int";
          "interface";
          "land"; "lazy"; "let"; "long"; "lor"; "lsl"; "lsr"; "lxor";
          "match"; "mdo"; "method"; "mod"; "module"; "mutable"; "new";
          "newtype"; "object"; "of"; "open"; "or"; "private"; "qualified";
          "rec"; "register"; "restrict"; "return"; "short"; "sig"; "signed";
          "sizeof"; "static"; "struct"; "switch"; "then"; "to"; "true"; "try";
          "type"; "typedef"; "union"; "unsigned"; "virtual"; "void";
          "volatile"; "when"; "where"; "while";
          ] in
        if List.mem n reserved then
          failwithf "%s has param/ret using reserved word %s" name n;
      in

      List.iter (fun arg -> check_arg_ret_name (name_of_argt arg)) (snd style)
  ) functions;

  (* Check short descriptions. *)
  List.iter (
    fun (name, _, shortdesc, _) ->
      if shortdesc.[0] <> Char.lowercase shortdesc.[0] then
        failwithf "short description of %s should begin with lowercase." name;
      let c = shortdesc.[String.length shortdesc-1] in
      if c = '\n' || c = '.' then
        failwithf "short description of %s should not end with . or \\n." name
  ) functions;

  (* Check long dscriptions. *)
  List.iter (
    fun (name, _, _, longdesc) ->
      if longdesc.[String.length longdesc-1] = '\n' then
        failwithf "long description of %s should not end with \\n." name
  ) functions

(* 'pr' prints to the current output file. *)
let chan = ref Pervasives.stdout
let lines = ref 0
let pr fs =
  ksprintf
    (fun str ->
       let i = count_chars '\n' str in
       lines := !lines + i;
       output_string !chan str
    ) fs

let copyright_years =
  let this_year = 1900 + (localtime (time ())).tm_year in
  if this_year > 2009 then sprintf "2009-%04d" this_year else "2009"

(* Generate a header block in a number of standard styles. *)
type comment_style =
  | CStyle | CPlusPlusStyle | HashStyle | OCamlStyle | HaskellStyle
  | PODCommentStyle
type license = GPLv2plus | LGPLv2plus | GPLv2 | LGPLv2

let generate_header ?(extra_inputs = []) comment license =
  let inputs = "generator/generator.ml" :: extra_inputs in
  let c = match comment with
    | CStyle ->         pr "/* "; " *"
    | CPlusPlusStyle -> pr "// "; "//"
    | HashStyle ->      pr "# ";  "#"
    | OCamlStyle ->     pr "(* "; " *"
    | HaskellStyle ->   pr "{- "; "  "
    | PODCommentStyle -> pr "=begin comment\n\n "; "" in
  pr "hivex generated file\n";
  pr "%s WARNING: THIS FILE IS GENERATED FROM:\n" c;
  List.iter (pr "%s   %s\n" c) inputs;
  pr "%s ANY CHANGES YOU MAKE TO THIS FILE WILL BE LOST.\n" c;
  pr "%s\n" c;
  pr "%s Copyright (C) %s Red Hat Inc.\n" c copyright_years;
  pr "%s Derived from code by Petter Nordahl-Hagen under a compatible license:\n" c;
  pr "%s   Copyright (c) 1997-2007 Petter Nordahl-Hagen.\n" c;
  pr "%s Derived from code by Markus Stephany under a compatible license:\n" c;
  pr "%s   Copyright (c)2000-2004, Markus Stephany.\n" c;
  pr "%s\n" c;
  (match license with
   | GPLv2plus ->
       pr "%s This program is free software; you can redistribute it and/or modify\n" c;
       pr "%s it under the terms of the GNU General Public License as published by\n" c;
       pr "%s the Free Software Foundation; either version 2 of the License, or\n" c;
       pr "%s (at your option) any later version.\n" c;
       pr "%s\n" c;
       pr "%s This program is distributed in the hope that it will be useful,\n" c;
       pr "%s but WITHOUT ANY WARRANTY; without even the implied warranty of\n" c;
       pr "%s MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" c;
       pr "%s GNU General Public License for more details.\n" c;
       pr "%s\n" c;
       pr "%s You should have received a copy of the GNU General Public License along\n" c;
       pr "%s with this program; if not, write to the Free Software Foundation, Inc.,\n" c;
       pr "%s 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.\n" c;

   | LGPLv2plus ->
       pr "%s This library is free software; you can redistribute it and/or\n" c;
       pr "%s modify it under the terms of the GNU Lesser General Public\n" c;
       pr "%s License as published by the Free Software Foundation; either\n" c;
       pr "%s version 2 of the License, or (at your option) any later version.\n" c;
       pr "%s\n" c;
       pr "%s This library is distributed in the hope that it will be useful,\n" c;
       pr "%s but WITHOUT ANY WARRANTY; without even the implied warranty of\n" c;
       pr "%s MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n" c;
       pr "%s Lesser General Public License for more details.\n" c;
       pr "%s\n" c;
       pr "%s You should have received a copy of the GNU Lesser General Public\n" c;
       pr "%s License along with this library; if not, write to the Free Software\n" c;
       pr "%s Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA\n" c;

   | GPLv2 ->
       pr "%s This program is free software; you can redistribute it and/or modify\n" c;
       pr "%s it under the terms of the GNU General Public License as published by\n" c;
       pr "%s the Free Software Foundation; version 2 of the License only.\n" c;
       pr "%s\n" c;
       pr "%s This program is distributed in the hope that it will be useful,\n" c;
       pr "%s but WITHOUT ANY WARRANTY; without even the implied warranty of\n" c;
       pr "%s MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" c;
       pr "%s GNU General Public License for more details.\n" c;
       pr "%s\n" c;
       pr "%s You should have received a copy of the GNU General Public License along\n" c;
       pr "%s with this program; if not, write to the Free Software Foundation, Inc.,\n" c;
       pr "%s 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.\n" c;

   | LGPLv2 ->
       pr "%s This library is free software; you can redistribute it and/or\n" c;
       pr "%s modify it under the terms of the GNU Lesser General Public\n" c;
       pr "%s License as published by the Free Software Foundation; either\n" c;
       pr "%s version 2.1 of the License only.\n" c;
       pr "%s\n" c;
       pr "%s This library is distributed in the hope that it will be useful,\n" c;
       pr "%s but WITHOUT ANY WARRANTY; without even the implied warranty of\n" c;
       pr "%s MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n" c;
       pr "%s Lesser General Public License for more details.\n" c;
       pr "%s\n" c;
       pr "%s You should have received a copy of the GNU Lesser General Public\n" c;
       pr "%s License along with this library; if not, write to the Free Software\n" c;
       pr "%s Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA\n" c;
  );
  (match comment with
   | CStyle -> pr " */\n"
   | CPlusPlusStyle
   | HashStyle -> ()
   | OCamlStyle -> pr " *)\n"
   | HaskellStyle -> pr "-}\n"
   | PODCommentStyle -> pr "\n=end comment\n"
  );
  pr "\n"

(* Start of main code generation functions below this line. *)

let rec generate_c_header () =
  generate_header CStyle LGPLv2;

  pr "\
#ifndef HIVEX_H_
#define HIVEX_H_

#include <stdint.h>

#ifdef __cplusplus
extern \"C\" {
#endif

/* NOTE: This API is documented in the man page hivex(3). */

/* Hive handle. */
typedef struct hive_h hive_h;

/* Nodes and values. */
typedef size_t hive_node_h;
typedef size_t hive_value_h;

/* Pre-defined types. */
enum hive_type {
";
  List.iter (
    fun (t, old_style, new_style, description) ->
      pr "  /* %s */\n" description;
      pr "  hive_t_REG_%s,\n" new_style;
      pr "#define hive_t_%s hive_t_REG_%s\n" old_style new_style;
      pr "\n"
  ) hive_types;
  pr "\
};

typedef enum hive_type hive_type;

/* Bitmask of flags passed to hivex_open. */
";
  List.iter (
    fun (v, flag, description) ->
      pr "  /* %s */\n" description;
      pr "#define HIVEX_OPEN_%-10s %d\n" flag v;
  ) open_flags;
  pr "\n";

  pr "\
/* Array of (key, value) pairs passed to hivex_node_set_values. */
struct hive_set_value {
  char *key;
  hive_type t;
  size_t len;
  char *value;
};
typedef struct hive_set_value hive_set_value;

";

  pr "/* Functions. */\n";

  (* Function declarations. *)
  List.iter (
    fun (shortname, style, _, _) ->
      let name = "hivex_" ^ shortname in
      generate_c_prototype ~extern:true name style
  ) functions;

  (* The visitor pattern. *)
  pr "
/* Visit all nodes.  This is specific to the C API and is not made
 * available to other languages.  This is because of the complexity
 * of binding callbacks in other languages, but also because other
 * languages make it much simpler to iterate over a tree.
 */
struct hivex_visitor {
  int (*node_start) (hive_h *, void *opaque, hive_node_h, const char *name);
  int (*node_end) (hive_h *, void *opaque, hive_node_h, const char *name);
  int (*value_string) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *str);
  int (*value_multiple_strings) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, char **argv);
  int (*value_string_invalid_utf16) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *str);
  int (*value_dword) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, int32_t);
  int (*value_qword) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, int64_t);
  int (*value_binary) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);
  int (*value_none) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);
  int (*value_other) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);
  int (*value_any) (hive_h *, void *opaque, hive_node_h, hive_value_h, hive_type t, size_t len, const char *key, const char *value);
};

#define HIVEX_VISIT_SKIP_BAD 1

extern int hivex_visit (hive_h *h, const struct hivex_visitor *visitor, size_t len, void *opaque, int flags);
extern int hivex_visit_node (hive_h *h, hive_node_h node, const struct hivex_visitor *visitor, size_t len, void *opaque, int flags);

";

  (* Finish the header file. *)
  pr "\
#ifdef __cplusplus
}
#endif

#endif /* HIVEX_H_ */
"

and generate_c_prototype ?(extern = false) name style =
  if extern then pr "extern ";
  (match fst style with
   | RErr -> pr "int "
   | RHive -> pr "hive_h *"
   | RNode -> pr "hive_node_h "
   | RNodeList -> pr "hive_node_h *"
   | RValue -> pr "hive_value_h "
   | RValueList -> pr "hive_value_h *"
   | RString -> pr "char *"
   | RStringList -> pr "char **"
   | RLenType -> pr "int "
   | RLenTypeVal -> pr "char *"
   | RInt32 -> pr "int32_t "
   | RInt64 -> pr "int64_t "
  );
  pr "%s (" name;
  let comma = ref false in
  List.iter (
    fun arg ->
      if !comma then pr ", "; comma := true;
      match arg with
      | AHive -> pr "hive_h *h"
      | ANode n -> pr "hive_node_h %s" n
      | AValue n -> pr "hive_value_h %s" n
      | AString n | AStringNullable n -> pr "const char *%s" n
      | AOpenFlags | AUnusedFlags -> pr "int flags"
      | ASetValues -> pr "size_t nr_values, const hive_set_value *values"
  ) (snd style);
  (match fst style with
   | RLenType | RLenTypeVal -> pr ", hive_type *t, size_t *len"
   | _ -> ()
  );
  pr ");\n"

and generate_c_pod () =
  generate_header PODCommentStyle GPLv2;

  pr "\
  =encoding utf8

=head1 NAME

hivex - Windows Registry \"hive\" extraction library

=head1 SYNOPSIS

 #include <hivex.h>
 
";
  List.iter (
    fun (shortname, style, _, _) ->
      let name = "hivex_" ^ shortname in
      pr " ";
      generate_c_prototype ~extern:false name style;
  ) functions;

  pr "\

Link with I<-lhivex>.

=head1 DESCRIPTION

libhivex is a library for extracting the contents of Windows Registry
\"hive\" files.  It is designed to be secure against buggy or malicious
registry files.

Unlike many other tools in this area, it doesn't use the textual .REG
format for output, because parsing that is as much trouble as parsing
the original binary format.  Instead it makes the file available
through a C API, or there is a separate program to export the hive as
XML (see L<hivexml(1)>), or to navigate the file (see L<hivexsh(1)>).

=head1 TYPES

=head2 hive_h *

This handle describes an open hive file.

=head2 hive_node_h

This is a node handle, an integer but opaque outside the library.
Valid node handles cannot be 0.  The library returns 0 in some
situations to indicate an error.

=head2 hive_type

The enum below describes the possible types for the value(s)
stored at each node.  Note that you should not trust the
type field in a Windows Registry, as it very often has no
relationship to reality.  Some applications use their own
types.  The encoding of strings is not specified.  Some
programs store everything (including strings) in binary blobs.

 enum hive_type {
";
  List.iter (
    fun (t, _, new_style, description) ->
      pr "   /* %s */\n" description;
      pr "   hive_t_REG_%s = %d,\n" new_style t
  ) hive_types;
  pr "\
 };

=head2 hive_value_h

This is a value handle, an integer but opaque outside the library.
Valid value handles cannot be 0.  The library returns 0 in some
situations to indicate an error.

=head2 hive_set_value

The typedef C<hive_set_value> is used in conjunction with the
C<hivex_node_set_values> call described below.

 struct hive_set_value {
   char *key;     /* key - a UTF-8 encoded ASCIIZ string */
   hive_type t;   /* type of value field */
   size_t len;    /* length of value field in bytes */
   char *value;   /* value field */
 };
 typedef struct hive_set_value hive_set_value;

To set the default value for a node, you have to pass C<key = \"\">.

Note that the C<value> field is just treated as a list of bytes, and
is stored directly in the hive.  The caller has to ensure correct
encoding and endianness, for example converting dwords to little
endian.

The correct type and encoding for values depends on the node and key
in the registry, the version of Windows, and sometimes even changes
between versions of Windows for the same key.  We don't document it
here.  Often it's not documented at all.

=head1 FUNCTIONS

";
  List.iter (
    fun (shortname, style, _, longdesc) ->
      let name = "hivex_" ^ shortname in
      pr "=head2 %s\n" name;
      pr "\n";
      generate_c_prototype ~extern:false name style;
      pr "\n";
      pr "%s\n" longdesc;
      pr "\n";
      (match fst style with
       | RErr ->
           pr "\
Returns 0 on success.
On error this returns -1 and sets errno.\n\n"
       | RHive ->
           pr "\
Returns a new hive handle.
On error this returns NULL and sets errno.\n\n"
       | RNode ->
           pr "\
Returns a node handle.
On error this returns 0 and sets errno.\n\n"
       | RNodeList ->
           pr "\
Returns a 0-terminated array of nodes.
The array must be freed by the caller when it is no longer needed.
On error this returns NULL and sets errno.\n\n"
       | RValue ->
           pr "\
Returns a value handle.
On error this returns 0 and sets errno.\n\n"
       | RValueList ->
           pr "\
Returns a 0-terminated array of values.
The array must be freed by the caller when it is no longer needed.
On error this returns NULL and sets errno.\n\n"
       | RString ->
           pr "\
Returns a string.
The string must be freed by the caller when it is no longer needed.
On error this returns NULL and sets errno.\n\n"
       | RStringList ->
           pr "\
Returns a NULL-terminated array of C strings.
The strings and the array must all be freed by the caller when
they are no longer needed.
On error this returns NULL and sets errno.\n\n"
       | RLenType ->
           pr "\
Returns 0 on success.
On error this returns NULL and sets errno.\n\n"
       | RLenTypeVal ->
           pr "\
The value is returned as an array of bytes (of length C<len>).
The value must be freed by the caller when it is no longer needed.
On error this returns NULL and sets errno.\n\n"
       | RInt32 | RInt64 -> ()
      );
  ) functions;

  pr "\
=head1 WRITING TO HIVE FILES

The hivex library supports making limited modifications to hive files.
We have tried to implement this very conservatively in order to reduce
the chance of corrupting your registry.  However you should be careful
and take back-ups, since Microsoft has never documented the hive
format, and so it is possible there are nuances in the
reverse-engineered format that we do not understand.

To be able to modify a hive, you must pass the C<HIVEX_OPEN_WRITE>
flag to C<hivex_open>, otherwise any write operation will return with
errno C<EROFS>.

The write operations shown below do not modify the on-disk file
immediately.  You must call C<hivex_commit> in order to write the
changes to disk.  If you call C<hivex_close> without committing then
any writes are discarded.

Hive files internally consist of a \"memory dump\" of binary blocks
(like the C heap), and some of these blocks can be unused.  The hivex
library never reuses these unused blocks.  Instead, to ensure
robustness in the face of the partially understood on-disk format,
hivex only allocates new blocks after the end of the file, and makes
minimal modifications to existing structures in the file to point to
these new blocks.  This makes hivex slightly less disk-efficient than
it could be, but disk is cheap, and registry modifications tend to be
very small.

When deleting nodes, it is possible that this library may leave
unreachable live blocks in the hive.  This is because certain parts of
the hive disk format such as security (sk) records and big data (db)
records and classname fields are not well understood (and not
documented at all) and we play it safe by not attempting to modify
them.  Apart from wasting a little bit of disk space, it is not
thought that unreachable blocks are a problem.

=head2 WRITE OPERATIONS WHICH ARE NOT SUPPORTED

=over 4

=item *

Changing the root node.

=item *

Creating a new hive file from scratch.  This is impossible at present
because not all fields in the header are understood.

=item *

Modifying or deleting single values at a node.

=item *

Modifying security key (sk) records or classnames.
Previously we did not understand these records.  However now they
are well-understood and we could add support if it was required
(but nothing much really uses them).

=back

=head1 VISITING ALL NODES

The visitor pattern is useful if you want to visit all nodes
in the tree or all nodes below a certain point in the tree.

First you set up your own C<struct hivex_visitor> with your
callback functions.

Each of these callback functions should return 0 on success or -1
on error.  If any callback returns -1, then the entire visit
terminates immediately.  If you don't need a callback function at
all, set the function pointer to NULL.

 struct hivex_visitor {
   int (*node_start) (hive_h *, void *opaque, hive_node_h, const char *name);
   int (*node_end) (hive_h *, void *opaque, hive_node_h, const char *name);
   int (*value_string) (hive_h *, void *opaque, hive_node_h, hive_value_h,
         hive_type t, size_t len, const char *key, const char *str);
   int (*value_multiple_strings) (hive_h *, void *opaque, hive_node_h,
         hive_value_h, hive_type t, size_t len, const char *key, char **argv);
   int (*value_string_invalid_utf16) (hive_h *, void *opaque, hive_node_h,
         hive_value_h, hive_type t, size_t len, const char *key,
         const char *str);
   int (*value_dword) (hive_h *, void *opaque, hive_node_h, hive_value_h,
         hive_type t, size_t len, const char *key, int32_t);
   int (*value_qword) (hive_h *, void *opaque, hive_node_h, hive_value_h,
         hive_type t, size_t len, const char *key, int64_t);
   int (*value_binary) (hive_h *, void *opaque, hive_node_h, hive_value_h,
         hive_type t, size_t len, const char *key, const char *value);
   int (*value_none) (hive_h *, void *opaque, hive_node_h, hive_value_h,
         hive_type t, size_t len, const char *key, const char *value);
   int (*value_other) (hive_h *, void *opaque, hive_node_h, hive_value_h,
         hive_type t, size_t len, const char *key, const char *value);
   /* If value_any callback is not NULL, then the other value_*
    * callbacks are not used, and value_any is called on all values.
    */
   int (*value_any) (hive_h *, void *opaque, hive_node_h, hive_value_h,
         hive_type t, size_t len, const char *key, const char *value);
 };

=over 4

=item hivex_visit

 int hivex_visit (hive_h *h, const struct hivex_visitor *visitor, size_t len, void *opaque, int flags);

Visit all the nodes recursively in the hive C<h>.

C<visitor> should be a C<hivex_visitor> structure with callback
fields filled in as required (unwanted callbacks can be set to
NULL).  C<len> must be the length of the 'visitor' struct (you
should pass C<sizeof (struct hivex_visitor)> for this).

This returns 0 if the whole recursive visit was completed
successfully.  On error this returns -1.  If one of the callback
functions returned an error than we don't touch errno.  If the
error was generated internally then we set errno.

You can skip bad registry entries by setting C<flag> to
C<HIVEX_VISIT_SKIP_BAD>.  If this flag is not set, then a bad registry
causes the function to return an error immediately.

This function is robust if the registry contains cycles or
pointers which are invalid or outside the registry.  It detects
these cases and returns an error.

=item hivex_visit_node

 int hivex_visit_node (hive_h *h, hive_node_h node, const struct hivex_visitor *visitor, size_t len, void *opaque);

Same as C<hivex_visit> but instead of starting out at the root, this
starts at C<node>.

=back

=head1 THE STRUCTURE OF THE WINDOWS REGISTRY

Note: To understand the relationship between hives and the common
Windows Registry keys (like C<HKEY_LOCAL_MACHINE>) please see the
Wikipedia page on the Windows Registry.

The Windows Registry is split across various binary files, each
file being known as a \"hive\".  This library only handles a single
hive file at a time.

Hives are n-ary trees with a single root.  Each node in the tree
has a name.

Each node in the tree (including non-leaf nodes) may have an
arbitrary list of (key, value) pairs attached to it.  It may
be the case that one of these pairs has an empty key.  This
is referred to as the default key for the node.

The (key, value) pairs are the place where the useful data is
stored in the registry.  The key is always a string (possibly the
empty string for the default key).  The value is a typed object
(eg. string, int32, binary, etc.).

=head2 RELATIONSHIP TO .REG FILES

Although this library does not care about or deal with Windows reg
files, it's useful to look at the relationship between the registry
itself and reg files because they are so common.

A reg file is a text representation of the registry, or part of the
registry.  The actual registry hives that Windows uses are binary
files.  There are a number of Windows and Linux tools that let you
generate reg files, or merge reg files back into the registry hives.
Notable amongst them is Microsoft's REGEDIT program (formerly known as
REGEDT32).

A typical reg file will contain many sections looking like this:

 [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Stack]
 \"@\"=\"Generic Stack\"
 \"TileInfo\"=\"prop:System.FileCount\"
 \"TilePath\"=str(2):\"%%systemroot%%\\\\system32\"
 \"ThumbnailCutoff\"=dword:00000000
 \"FriendlyTypeName\"=hex(2):40,00,25,00,53,00,79,00,73,00,74,00,65,00,6d,00,52,00,6f,00,\\
  6f,00,74,00,25,00,5c,00,53,00,79,00,73,00,74,00,65,00,6d,00,\\
  33,00,32,00,5c,00,73,00,65,00,61,00,72,00,63,00,68,00,66,00,\\
  6f,00,6c,00,64,00,65,00,72,00,2e,00,64,00,6c,00,6c,00,2c,00,\\
  2d,00,39,00,30,00,32,00,38,00,00,00,d8

Taking this one piece at a time:

 [HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\Stack]

This is the path to this node in the registry tree.  The first part,
C<HKEY_LOCAL_MACHINE\\SOFTWARE> means that this comes from a hive
(file) called C<SOFTWARE>.  C<\\Classes\\Stack> is the real path part,
starting at the root node of the C<SOFTWARE> hive.

Below the node name is a list of zero or more key-value pairs.  Any
interior or leaf node in the registry may have key-value pairs
attached.

 \"@\"=\"Generic Stack\"

This is the \"default key\".  In reality (ie. inside the binary hive)
the key string is the empty string.  In reg files this is written as
C<@> but this has no meaning either in the hives themselves or in this
library.  The value is a string (type 1 - see C<enum hive_type>
above).

 \"TileInfo\"=\"prop:System.FileCount\"

This is a regular (key, value) pair, with the value being a type 1
string.  Note that inside the binary file the string is likely to be
UTF-16 encoded.  This library converts to and from UTF-8 strings
transparently.

 \"TilePath\"=str(2):\"%%systemroot%%\\\\system32\"

The value in this case has type 2 (expanded string) meaning that some
%%...%% variables get expanded by Windows.  (This library doesn't know
or care about variable expansion).

 \"ThumbnailCutoff\"=dword:00000000

The value in this case is a dword (type 4).

 \"FriendlyTypeName\"=hex(2):40,00,....

This value is an expanded string (type 2) represented in the reg file
as a series of hex bytes.  In this case the string appears to be a
UTF-16 string.

=head1 NOTE ON THE USE OF ERRNO

Many functions in this library set errno to indicate errors.  These
are the values of errno you may encounter (this list is not
exhaustive):

=over 4

=item ENOTSUP

Corrupt or unsupported Registry file format.

=item ENOKEY

Missing root key.

=item EINVAL

Passed an invalid argument to the function.

=item EFAULT

Followed a Registry pointer which goes outside
the registry or outside a registry block.

=item ELOOP

Registry contains cycles.

=item ERANGE

Field in the registry out of range.

=item EEXIST

Registry key already exists.

=item EROFS

Tried to write to a registry which is not opened for writing.

=back

=head1 ENVIRONMENT VARIABLES

=over 4

=item HIVEX_DEBUG

Setting HIVEX_DEBUG=1 will enable very verbose messages.  This is
useful for debugging problems with the library itself.

=back

=head1 SEE ALSO

L<hivexml(1)>,
L<hivexsh(1)>,
L<virt-win-reg(1)>,
L<guestfs(3)>,
L<http://libguestfs.org/>,
L<virt-cat(1)>,
L<virt-edit(1)>,
L<http://en.wikipedia.org/wiki/Windows_Registry>.

=head1 AUTHORS

Richard W.M. Jones (C<rjones at redhat dot com>)

=head1 COPYRIGHT

Copyright (C) 2009-2010 Red Hat Inc.

Derived from code by Petter Nordahl-Hagen under a compatible license:
Copyright (C) 1997-2007 Petter Nordahl-Hagen.

Derived from code by Markus Stephany under a compatible license:
Copyright (C) 2000-2004 Markus Stephany.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation;
version 2.1 of the License only.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
"

let output_to filename k =
  let filename_new = filename ^ ".new" in
  chan := open_out filename_new;
  k ();
  close_out !chan;
  chan := Pervasives.stdout;

  (* Is the new file different from the current file? *)
  if Sys.file_exists filename && files_equal filename filename_new then
    unlink filename_new                 (* same, so skip it *)
  else (
    (* different, overwrite old one *)
    (try chmod filename 0o644 with Unix_error _ -> ());
    rename filename_new filename;
    chmod filename 0o444;
    printf "written %s\n%!" filename;
  )

let perror msg = function
  | Unix_error (err, _, _) ->
      eprintf "%s: %s\n" msg (error_message err)
  | exn ->
      eprintf "%s: %s\n" msg (Printexc.to_string exn)

(* Main program. *)
let () =
  let lock_fd =
    try openfile "configure.ac" [O_RDWR] 0
    with
    | Unix_error (ENOENT, _, _) ->
        eprintf "\
You are probably running this from the wrong directory.
Run it from the top source directory using the command
  generator/generator.ml
";
        exit 1
    | exn ->
        perror "open: configure.ac" exn;
        exit 1 in

  (* Acquire a lock so parallel builds won't try to run the generator
   * twice at the same time.  Subsequent builds will wait for the first
   * one to finish.  Note the lock is released implicitly when the
   * program exits.
   *)
  (try lockf lock_fd F_LOCK 1
   with exn ->
     perror "lock: configure.ac" exn;
     exit 1);

  check_functions ();

  output_to "lib/hivex.h" generate_c_header;
  output_to "lib/hivex.pod" generate_c_pod;

  (* Always generate this file last, and unconditionally.  It's used
   * by the Makefile to know when we must re-run the generator.
   *)
  let chan = open_out "generator/stamp-generator" in
  fprintf chan "1\n";
  close_out chan;

  printf "generated %d lines of code\n" !lines
