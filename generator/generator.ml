#!/usr/bin/env ocaml
(* hivex
 * Copyright (C) 2009-2011 Red Hat Inc.
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

#directory "+unix";;
#load "unix.cma";;
#directory "+str";;
#load "str.cma";;

open Unix
open Printf

type style = ret * args
and ret =
  | RErr                                (* 0 = ok, -1 = error *)
  | RErrDispose                         (* Disposes handle, see hivex_close. *)
  | RHive                               (* Returns a hive_h or NULL. *)
  | RSize                               (* Returns size_t or 0. *)
  | RNode                               (* Returns hive_node_h or 0. *)
  | RNodeNotFound                       (* See hivex_node_get_child. *)
  | RNodeList                           (* Returns hive_node_h* or NULL. *)
  | RValue                              (* Returns hive_value_h or 0. *)
  | RValueList                          (* Returns hive_value_h* or NULL. *)
  | RLenValue                           (* Returns offset and length of value. *)
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
  | ASetValue                           (* See hivex_node_set_value. *)

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
let max_hive_type = 11

(* Open flags (bitmask passed to AOpenFlags) *)
let open_flags = [
  1, "VERBOSE", "Verbose messages";
  2, "DEBUG", "Debug messages";
  4, "WRITE", "Enable writes to the hive";
  8, "UNSAFE", "Enable heuristics to allow read/write of corrupted hives";
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

=item HIVEX_OPEN_UNSAFE

Open the hive in unsafe mode that enables heuristics to handle corrupted hives.

This may allow to read or write registry keys/values that appear intact in an
otherwise corrupted hive. Use at your own risk.

=back";

  "close", (RErrDispose, [AHive]),
    "close a hive handle",
    "\
Close a hive handle and free all associated resources.

Note that any uncommitted writes are I<not> committed by this call,
but instead are lost.  See L<hivex(3)/WRITING TO HIVE FILES>.";

  "root", (RNode, [AHive]),
    "return the root node of the hive",
    "\
Return root node of the hive.  All valid hives must contain a root node.";

  "last_modified", (RInt64, [AHive]),
    "return the modification time from the header of the hive",
    "\
Return the modification time from the header of the hive.

The returned value is a Windows filetime.
To convert this to a Unix C<time_t> see:
L<http://stackoverflow.com/questions/6161776/convert-windows-filetime-to-second-in-unix-linux/6161842#6161842>";

  "node_name", (RString, [AHive; ANode "node"]),
    "return the name of the node",
    "\
Return the name of the node.

Note that the name of the root node is a dummy, such as
C<$$$PROTO.HIV> (other names are possible: it seems to depend on the
tool or program that created the hive in the first place).  You can
only know the \"real\" name of the root node by knowing which registry
file this hive originally comes from, which is knowledge that is
outside the scope of this library.

The name is recoded to UTF-8 and may contain embedded NUL characters.";

  "node_name_len", (RSize, [AHive; ANode "node"]),
    "return the length of a node's name",
    "\
Return the length of the node name as produced by C<hivex_node_name>.";

  "node_timestamp", (RInt64, [AHive; ANode "node"]),
    "return the modification time of the node",
    "\
Return the modification time of the node.

The returned value is a Windows filetime.
To convert this to a Unix C<time_t> see:
L<http://stackoverflow.com/questions/6161776/convert-windows-filetime-to-second-in-unix-linux/6161842#6161842>";

  "node_children", (RNodeList, [AHive; ANode "node"]),
    "return children of node",
    "\
Return an array of nodes which are the subkeys
(children) of C<node>.";

  "node_get_child", (RNodeNotFound, [AHive; ANode "node"; AString "name"]),
    "return named child of node",
    "\
Return the child of node with the name C<name>, if it exists.

The name is matched case insensitively.";

  "node_nr_children", (RSize, [AHive; ANode "node"]),
    "return the number of children of a node",
    "\
Return the number of nodes as produced by C<hivex_node_children>.";

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

  "node_nr_values", (RSize, [AHive; ANode "node"]),
    "return the number of values attached to a node",
    "\
Return the number of (key, value) pairs attached to this node
as produced by C<hivex_node_values>.";

  "value_key_len", (RSize, [AHive; AValue "val"]),
    "return the length of a value's key",
    "\
Return the length of the key (name) of a (key, value) pair as produced
by C<hivex_value_key>. The length can legitimately be 0, so errno is 
the necessary mechanism to check for errors.

In the context of Windows Registries, a zero-length name means
that this value is the default key for this node in the tree.
This is usually written as C<\"@\">.

The key is recoded to UTF-8 and may contain embedded NUL characters.";

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

  "node_struct_length", (RSize, [AHive; ANode "node"]),
    "return the length of a node",
    "\
Return the length of the node data structure.";

  "value_struct_length", (RSize, [AHive; AValue "val"]),
    "return the length of a value data structure",
    "\
Return the length of the value data structure.";

  "value_data_cell_offset", (RLenValue, [AHive; AValue "val"]),
    "return the offset and length of a value data cell",
    "\
Return the offset and length of the value's data cell.

The data cell is a registry structure that contains the length
(a 4 byte, little endian integer) followed by the data.

If the length of the value is less than or equal to 4 bytes
then the offset and length returned by this function is zero
as the data is inlined in the value.

Returns 0 and sets errno on error.";

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
as UTF-8 (in C, as a NULL-terminated array of C strings, in other
language bindings, as a list of strings).  This only
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

C<filename> is the new file to write.  If C<filename> is null/undefined
then we overwrite the original file (ie. the file name that was passed to
C<hivex_open>).

Note this does not close the hive handle.  You can perform further
operations on the hive after committing, including making more
modifications.  If you no longer wish to use the hive, then you
should close the handle after committing.";

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
This call can be used to set all the (key, value) pairs
stored in C<node>.

C<node> is the node to modify.";

  "node_set_value", (RErr, [AHive; ANode "node"; ASetValue; AUnusedFlags]),
    "set a single (key, value) pair at a given node",
    "\
This call can be used to replace a single C<(key, value)> pair
stored in C<node>.  If the key does not already exist, then a
new key is added.  Key matching is case insensitive.

C<node> is the node to modify.";
]

let f_len_exists n =
  List.exists
    (function (cand, _, _, _) -> cand = (String.concat "" [n; "_len"]))
    functions

(* Useful functions.
 * Note we don't want to use any external OCaml libraries which
 * makes this a bit harder than it should be.
 *)
module StringMap = Map.Make (String)

let failwithf fs = ksprintf failwith fs

let unique = let i = ref 0 in fun () -> incr i; !i

let replace_char s c1 c2 =
  let s2 = Bytes.of_string s in
  let r = ref false in
  for i = 0 to String.length s - 1 do
    if String.unsafe_get s i = c1 then (
      Bytes.unsafe_set s2 i c2;
      r := true
    )
  done;
  if not !r then s else Bytes.to_string s2

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

(* Used to memoize the result of pod2text. *)
let pod2text_memo_filename = "generator/.pod2text.data.version.2"
let pod2text_memo : ((int option * bool * bool * string * string), string list) Hashtbl.t =
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

(* Useful if you need the longdesc POD text as plain text.  Returns a
 * list of lines.
 *
 * Because this is very slow (the slowest part of autogeneration),
 * we memoize the results.
 *)
let pod2text ?width ?(trim = true) ?(discard = true) name longdesc =
  let key = width, trim, discard, name, longdesc in
  try Hashtbl.find pod2text_memo key
  with Not_found ->
    let filename, chan = Filename.open_temp_file "gen" ".tmp" in
    fprintf chan "=head1 %s\n\n%s\n" name longdesc;
    close_out chan;
    let cmd =
      match width with
      | Some width ->
          sprintf "pod2text -w %d %s" width (Filename.quote filename)
      | None ->
          sprintf "pod2text %s" (Filename.quote filename) in
    let chan = open_process_in cmd in
    let lines = ref [] in
    let rec loop i =
      let line = input_line chan in
      if i = 1 && discard then  (* discard the first line of output *)
        loop (i+1)
      else (
        let line = if trim then triml line else line in
        lines := line :: !lines;
        loop (i+1)
      ) in
    let lines = try loop 1 with End_of_file -> List.rev !lines in
    unlink filename;
    (match close_process_in chan with
     | WEXITED 0 -> ()
     | WEXITED i ->
         failwithf "pod2text: process exited with non-zero status (%d)" i
     | WSIGNALED i | WSTOPPED i ->
         failwithf "pod2text: process signalled or stopped by signal %d" i
    );
    Hashtbl.add pod2text_memo key lines;
    pod2text_memo_updated ();
    lines

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
  | ASetValue -> "val"

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
      if shortdesc.[0] <> Char.lowercase_ascii shortdesc.[0] then
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
let chan = ref Stdlib.stdout
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
       pr "%s License as published by the Free Software Foundation;\n" c;
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

#include <stdlib.h>
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

#include <errno.h>
#ifdef ENOKEY
# define HIVEX_NO_KEY ENOKEY
#else
# define HIVEX_NO_KEY ENOENT
#endif

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
   | RErrDispose -> pr "int "
   | RHive -> pr "hive_h *"
   | RSize -> pr "size_t "
   | RNode -> pr "hive_node_h "
   | RNodeNotFound -> pr "hive_node_h "
   | RNodeList -> pr "hive_node_h *"
   | RValue -> pr "hive_value_h "
   | RValueList -> pr "hive_value_h *"
   | RString -> pr "char *"
   | RStringList -> pr "char **"
   | RLenValue -> pr "hive_value_h "
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
      | ASetValue -> pr "const hive_set_value *val"
  ) (snd style);
  (match fst style with
   | RLenType | RLenTypeVal -> pr ", hive_type *t, size_t *len"
   | RLenValue -> pr ", size_t *len"
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

Hivex is a library for extracting the contents of Windows Registry
\"hive\" files.  It is designed to be secure against buggy or malicious
registry files.

Unlike other tools in this area, it doesn't use the textual .REG
format, because parsing that is as much trouble as parsing the
original binary format.  Instead it makes the file available
through a C API, and then wraps this API in higher level scripting
and GUI tools.

There is a separate program to export the hive as XML
(see L<hivexml(1)>), or to navigate the file (see L<hivexsh(1)>).
There is also a Perl script to export and merge the
file as a textual .REG (regedit) file, see L<hivexregedit(1)>.

If you just want to export or modify the Registry of a Windows
virtual machine, you should look at L<virt-win-reg(1)>.

Hivex is also comes with language bindings for
OCaml, Perl, Python and Ruby.

=head1 TYPES

=head2 C<hive_h *>

This handle describes an open hive file.

=head2 C<hive_node_h>

This is a node handle, an integer but opaque outside the library.
Valid node handles cannot be 0.  The library returns 0 in some
situations to indicate an error.

=head2 C<hive_type>

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

=head2 C<hive_value_h>

This is a value handle, an integer but opaque outside the library.
Valid value handles cannot be 0.  The library returns 0 in some
situations to indicate an error.

=head2 C<hive_set_value>

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
      pr "\n ";
      generate_c_prototype ~extern:false name style;
      pr "\n";
      pr "%s\n" longdesc;
      pr "\n";

      if List.mem AUnusedFlags (snd style) then
        pr "The flags parameter is unused.  Always pass 0.\n\n";

      if List.mem ASetValues (snd style) then
        pr "C<values> is an array of (key, value) pairs.  There
should be C<nr_values> elements in this array.

Any existing values stored at the node are discarded, and their
C<hive_value_h> handles become invalid.  Thus you can remove all
values stored at C<node> by passing C<nr_values = 0>.\n\n";

      if List.mem ASetValue (snd style) then
        pr "C<value> is a single (key, value) pair.

Existing C<hive_value_h> handles become invalid.\n\n";

      (match fst style with
       | RErr ->
           pr "\
Returns 0 on success.
On error this returns -1 and sets errno.\n\n"
       | RErrDispose ->
           pr "\
Returns 0 on success.
On error this returns -1 and sets errno.

This function frees the hive handle (even if it returns an error).
The hive handle must not be used again after calling this function.\n\n"
       | RHive ->
           pr "\
Returns a new hive handle.
On error this returns NULL and sets errno.\n\n"
       | RSize ->
           pr "\
Returns a size.
On error this returns 0 and sets errno.\n\n"
       | RNode ->
           pr "\
Returns a node handle.
On error this returns 0 and sets errno.\n\n"
       | RNodeNotFound ->
           pr "\
Returns a node handle.
If the node was not found, this returns 0 without setting errno.
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
On error this returns -1 and sets errno.\n\n"
       | RLenValue ->
           pr "\
Returns a value handle.
On error this returns 0 and sets errno.\n\n"
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
because not all fields in the header are understood.  In the hivex
source tree is a file called C<images/minimal> which could be used as
the basis for a new hive (but I<caveat emptor>).

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

The hivex C library does not care about or deal with Windows .REG
files.  Instead we push this complexity up to the Perl
L<Win::Hivex(3)> library and the Perl programs
L<hivexregedit(1)> and L<virt-win-reg(1)>.
Nevertheless it is useful to look at the relationship between the
Registry and .REG files because they are so common.

A .REG file is a textual representation of the registry, or part of the
registry.  The actual registry hives that Windows uses are binary
files.  There are a number of Windows and Linux tools that let you
generate .REG files, or merge .REG files back into the registry hives.
Notable amongst them is Microsoft's REGEDIT program (formerly known as
REGEDT32).

A typical .REG file will contain many sections looking like this:

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
file called C<C:\\WINDOWS\\SYSTEM32\\CONFIG\\SOFTWARE>.
C<\\Classes\\Stack> is the real path part,
starting at the root node of the C<SOFTWARE> hive.

Below the node name is a list of zero or more key-value pairs.  Any
interior or leaf node in the registry may have key-value pairs
attached.

 \"@\"=\"Generic Stack\"

This is the \"default key\".  In reality (ie. inside the binary hive)
the key string is the empty string.  In .REG files this is written as
C<@> but this has no meaning either in the hives themselves or in this
library.  The value is a string (type 1 - see C<enum hive_type>
above).

 \"TileInfo\"=\"prop:System.FileCount\"

This is a regular (key, value) pair, with the value being a type 1
string.  Note that inside the binary file the string is likely to be
UTF-16LE encoded.  This library converts to and from UTF-8 strings
transparently in some cases.

 \"TilePath\"=str(2):\"%%systemroot%%\\\\system32\"

The value in this case has type 2 (expanded string) meaning that some
%%...%% variables get expanded by Windows.  (This library doesn't know
or care about variable expansion).

 \"ThumbnailCutoff\"=dword:00000000

The value in this case is a dword (type 4).

 \"FriendlyTypeName\"=hex(2):40,00,....

This value is an expanded string (type 2) represented in the .REG file
as a series of hex bytes.  In this case the string appears to be a
UTF-16LE string.

=head1 NOTE ON THE USE OF ERRNO

Many functions in this library set errno to indicate errors.  These
are the values of errno you may encounter (this list is not
exhaustive):

=over 4

=item ENOTSUP

Corrupt or unsupported Registry file format.

=item HIVEX_NO_KEY

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

L<hivexget(1)>,
L<hivexml(1)>,
L<hivexsh(1)>,
L<hivexregedit(1)>,
L<virt-win-reg(1)>,
L<Win::Hivex(3)>,
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

(* Generate the linker script which controls the visibility of
 * symbols in the public ABI and ensures no other symbols get
 * exported accidentally.
 *)
and generate_linker_script () =
  generate_header HashStyle GPLv2plus;

  let globals = [
    "hivex_visit";
    "hivex_visit_node"
  ] in

  let functions =
    List.map (fun (name, _, _, _) -> "hivex_" ^ name)
      functions in
  let globals = List.sort compare (globals @ functions) in

  pr "{\n";
  pr "    global:\n";
  List.iter (pr "        %s;\n") globals;
  pr "\n";

  pr "    local:\n";
  pr "        *;\n";
  pr "};\n"

and generate_ocaml_interface () =
  generate_header OCamlStyle LGPLv2plus;

  pr "\
type t
(** A [hive_h] hive file handle. *)

type node
type value
(** Nodes and values. *)

exception Error of string * Unix.error * string
(** Error raised by a function.

    The first parameter is the name of the function which raised the error.
    The second parameter is the errno (see the [Unix] module).  The third
    parameter is a human-readable string corresponding to the errno.

    See hivex(3) for a partial list of interesting errno values that
    can be generated by the library. *)
exception Handle_closed of string
(** This exception is raised if you call a function on a closed handle. *)

type hive_type =
";
  iteri (
    fun i ->
      fun (t, _, new_style, description) ->
        assert (i = t);
        pr "  | REG_%s (** %s *)\n" new_style description
  ) hive_types;

  pr "\
  | REG_UNKNOWN of int32 (** unknown type *)
(** Hive type field. *)

type open_flag =
";
  iteri (
    fun i ->
      fun (v, flag, description) ->
        assert (1 lsl i = v);
        pr "  | OPEN_%s (** %s *)\n" flag description
  ) open_flags;

  pr "\
(** Open flags for {!open_file} call. *)

type set_value = {
  key : string;
  t : hive_type;
  value : string;
}
(** (key, value) pair passed (as an array) to {!node_set_values}. *)
";

  List.iter (
    fun (name, style, shortdesc, _) ->
      pr "\n";
      generate_ocaml_prototype name style;
      pr "(** %s *)\n" shortdesc
  ) functions

and generate_ocaml_implementation () =
  generate_header OCamlStyle LGPLv2plus;

  pr "\
type t
type node = int
type value = int

exception Error of string * Unix.error * string
exception Handle_closed of string

(* Give the exceptions names, so they can be raised from the C code. *)
let () =
  Callback.register_exception \"ocaml_hivex_error\"
    (Error (\"\", Unix.EUNKNOWNERR 0, \"\"));
  Callback.register_exception \"ocaml_hivex_closed\" (Handle_closed \"\")

type hive_type =
";
  iteri (
    fun i ->
      fun (t, _, new_style, _) ->
        assert (i = t);
        pr "  | REG_%s\n" new_style
  ) hive_types;

  pr "\
  | REG_UNKNOWN of int32

type open_flag =
";
  iteri (
    fun i ->
      fun (v, flag, description) ->
        assert (1 lsl i = v);
        pr "  | OPEN_%s (** %s *)\n" flag description
  ) open_flags;

  pr "\

type set_value = {
  key : string;
  t : hive_type;
  value : string;
}

";

  List.iter (
    fun (name, style, _, _) ->
      generate_ocaml_prototype ~is_external:true name style
  ) functions

and generate_ocaml_prototype ?(is_external = false) name style =
  let ocaml_name = if name = "open" then "open_file" else name in

  if is_external then pr "external " else pr "val ";
  pr "%s : " ocaml_name;
  List.iter (
    function
    | AHive -> pr "t -> "
    | ANode _ -> pr "node -> "
    | AValue _ -> pr "value -> "
    | AString _ -> pr "string -> "
    | AStringNullable _ -> pr "string option -> "
    | AOpenFlags -> pr "open_flag list -> "
    | AUnusedFlags -> ()
    | ASetValues -> pr "set_value array -> "
    | ASetValue -> pr "set_value -> "
  ) (snd style);
  (match fst style with
   | RErr -> pr "unit" (* all errors are turned into exceptions *)
   | RErrDispose -> pr "unit"
   | RHive -> pr "t"
   | RSize -> pr "int64"
   | RNode -> pr "node"
   | RNodeNotFound -> pr "node"
   | RNodeList -> pr "node array"
   | RValue -> pr "value"
   | RValueList -> pr "value array"
   | RString -> pr "string"
   | RStringList -> pr "string array"
   | RLenType -> pr "hive_type * int"
   | RLenValue -> pr "int * value"
   | RLenTypeVal -> pr "hive_type * string"
   | RInt32 -> pr "int32"
   | RInt64 -> pr "int64"
  );
  if is_external then
    pr " = \"ocaml_hivex_%s\"" name;
  pr "\n"

and generate_ocaml_c () =
  generate_header CStyle LGPLv2plus;

  pr "\
#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>

#include <caml/config.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#ifdef HAVE_CAML_UNIXSUPPORT_H
#include <caml/unixsupport.h>
#else
extern value unix_error_of_code (int errcode);
#endif

#ifndef HAVE_CAML_RAISE_WITH_ARGS
static void
caml_raise_with_args (value tag, int nargs, value args[])
{
  CAMLparam1 (tag);
  CAMLxparamN (args, nargs);
  value bucket;
  int i;

  bucket = caml_alloc_small (1 + nargs, 0);
  Field(bucket, 0) = tag;
  for (i = 0; i < nargs; i++) Field(bucket, 1 + i) = args[i];
  caml_raise(bucket);
  CAMLnoreturn;
}
#endif

/* Replacement if caml_alloc_initialized_string is missing, added
 * to OCaml runtime in 2017.
 */
#ifndef HAVE_CAML_ALLOC_INITIALIZED_STRING
static inline value
caml_alloc_initialized_string (mlsize_t len, const char *p)
{
  value sv = caml_alloc_string (len);
  memcpy ((char *) String_val (sv), p, len);
  return sv;
}
#endif

#include <hivex.h>

#define Hiveh_val(v) (*((hive_h **)Data_custom_val(v)))
static value Val_hiveh (hive_h *);
static int HiveOpenFlags_val (value);
static hive_set_value *HiveSetValue_val (value);
static hive_set_value *HiveSetValues_val (value);
static hive_type HiveType_val (value);
static value Val_hive_type (hive_type);
static value copy_int_array (size_t *);
static value copy_type_len (size_t, hive_type);
static value copy_len_value (size_t, hive_value_h);
static value copy_type_value (const char *, size_t, hive_type);
static void raise_error (const char *) Noreturn;
static void raise_closed (const char *) Noreturn;

";

  (* The wrappers. *)
  List.iter (
    fun (name, style, _, _) ->
      pr "/* Automatically generated wrapper for function\n";
      pr " * "; generate_ocaml_prototype name style;
      pr " */\n";
      pr "\n";

      let c_params =
        List.map (function
                  | ASetValues -> ["nrvalues"; "values"]
                  | AUnusedFlags -> ["0"]
                  | arg -> [name_of_argt arg]) (snd style) in
      let c_params =
        match fst style with
        | RLenType | RLenTypeVal -> c_params @ [["&t"; "&len"]]
        | RLenValue -> c_params @ [["&len"]]
        | _ -> c_params in
      let c_params = List.concat c_params in

      let params =
        filter_map (function
                    | AUnusedFlags -> None
                    | arg -> Some (name_of_argt arg ^ "v")) (snd style) in

      pr "/* Emit prototype to appease gcc's -Wmissing-prototypes. */\n";
      pr "CAMLprim value ocaml_hivex_%s (value %s" name (List.hd params);
      List.iter (pr ", value %s") (List.tl params); pr ");\n";
      pr "\n";

      pr "CAMLprim value\n";
      pr "ocaml_hivex_%s (value %s" name (List.hd params);
      List.iter (pr ", value %s") (List.tl params);
      pr ")\n";
      pr "{\n";

      pr "  CAMLparam%d (%s);\n"
        (List.length params) (String.concat ", " params);
      pr "  CAMLlocal1 (rv);\n";
      pr "\n";

      List.iter (
        function
        | AHive ->
            pr "  hive_h *h = Hiveh_val (hv);\n";
            pr "  if (h == NULL)\n";
            pr "    raise_closed (\"%s\");\n" name
        | ANode n ->
            pr "  hive_node_h %s = Int_val (%sv);\n" n n
        | AValue n ->
            pr "  hive_value_h %s = Int_val (%sv);\n" n n
        | AString n ->
            pr "  const char *%s = String_val (%sv);\n" n n
        | AStringNullable n ->
            pr "  const char *%s =\n" n;
            pr "    %sv != Val_int (0) ? String_val (Field (%sv, 0)) : NULL;\n"
              n n
        | AOpenFlags ->
            pr "  int flags = HiveOpenFlags_val (flagsv);\n"
        | AUnusedFlags -> ()
        | ASetValues ->
            pr "  int nrvalues = Wosize_val (valuesv);\n";
            pr "  hive_set_value *values = HiveSetValues_val (valuesv);\n"
        | ASetValue ->
            pr "  hive_set_value *val = HiveSetValue_val (valv);\n"
      ) (snd style);
      pr "\n";

      let error_code =
        match fst style with
        | RErr -> pr "  int r;\n"; "-1"
        | RErrDispose -> pr "  int r;\n"; "-1"
        | RHive -> pr "  hive_h *r;\n"; "NULL"
        | RSize -> pr "  size_t r;\n"; "0"
        | RNode -> pr "  hive_node_h r;\n"; "0"
        | RNodeNotFound ->
            pr "  errno = 0;\n";
            pr "  hive_node_h r;\n";
            "0 && errno != 0"
        | RNodeList -> pr "  hive_node_h *r;\n"; "NULL"
        | RValue -> pr "  hive_value_h r;\n"; "0"
        | RValueList -> pr "  hive_value_h *r;\n"; "NULL"
        | RString -> pr "  char *r;\n"; "NULL"
        | RStringList -> pr "  char **r;\n"; "NULL"
        | RLenType ->
            pr "  int r;\n";
            pr "  size_t len;\n";
            pr "  hive_type t;\n";
            "-1"
        | RLenValue ->
            pr "  errno = 0;";
            pr "  hive_value_h r;\n";
            pr "  size_t len;\n";
            "0 && errno != 0"
        | RLenTypeVal ->
            pr "  char *r;\n";
            pr "  size_t len;\n";
            pr "  hive_type t;\n";
            "NULL"
        | RInt32 ->
            pr "  errno = 0;\n";
            pr "  int32_t r;\n";
            "-1 && errno != 0"
        | RInt64 ->
            pr "  errno = 0;\n";
            pr "  int64_t r;\n";
            "-1 && errno != 0" in

      (* The libguestfs OCaml bindings call enter_blocking_section
       * here.  However I don't think that is safe, because we are
       * holding pointers to caml strings during the call, and these
       * could be moved or freed by other threads.  In any case, there
       * is very little reason to enter_blocking_section for any hivex
       * call, so don't do it.  XXX
       *)
      (*pr "  caml_enter_blocking_section ();\n";*)
      pr "  r = hivex_%s (%s);\n" name (String.concat ", " c_params);
      (*pr "  caml_leave_blocking_section ();\n";*)
      pr "\n";

      (* Dispose of the hive handle (even if hivex_close returns error). *)
      (match fst style with
       | RErrDispose ->
           pr "  /* So we don't double-free in the finalizer. */\n";
           pr "  Hiveh_val (hv) = NULL;\n";
           pr "\n";
       | _ -> ()
      );

      List.iter (
        function
        | AHive | ANode _ | AValue _ | AString _ | AStringNullable _
        | AOpenFlags | AUnusedFlags -> ()
        | ASetValues ->
            pr "  free (values);\n";
            pr "\n";
        | ASetValue ->
            pr "  free (val);\n";
            pr "\n";
      ) (snd style);

      (* Check for errors. *)
      pr "  if (r == %s)\n" error_code;
      pr "    raise_error (\"%s\");\n" name;
      pr "\n";

      (match fst style with
       | RErr -> pr "  rv = Val_unit;\n"
       | RErrDispose -> pr "  rv = Val_unit;\n"
       | RHive -> pr "  rv = Val_hiveh (r);\n"
       | RSize -> pr "  rv = caml_copy_int64 (r);\n"
       | RNode -> pr "  rv = Val_int (r);\n"
       | RNodeNotFound ->
           pr "  if (r == 0)\n";
           pr "    caml_raise_not_found ();\n";
           pr "\n";
           pr "  rv = Val_int (r);\n"
       | RNodeList ->
           pr "  rv = copy_int_array (r);\n";
           pr "  free (r);\n"
       | RValue -> pr "  rv = Val_int (r);\n"
       | RValueList ->
           pr "  rv = copy_int_array (r);\n";
           pr "  free (r);\n"
       | RString ->
           if f_len_exists name then (
             pr "  size_t sz;\n  sz = hivex_%s_len (%s);\n"
               name (String.concat ", " c_params);
             pr "  rv = caml_alloc_initialized_string (sz, r);\n"
           ) else
             pr "  rv = caml_copy_string (r);\n";
           pr "  free (r);\n"
       | RStringList ->
           pr "  rv = caml_copy_string_array ((const char **) r);\n";
           pr "  int i;\n";
           pr "  for (i = 0; r[i] != NULL; ++i) free (r[i]);\n";
           pr "  free (r);\n"
       | RLenType -> pr "  rv = copy_type_len (len, t);\n"
       | RLenValue -> pr "  rv = copy_len_value (len, r);\n"
       | RLenTypeVal ->
           pr "  rv = copy_type_value (r, len, t);\n";
           pr "  free (r);\n"
       | RInt32 -> pr "  rv = caml_copy_int32 (r);\n"
       | RInt64 -> pr "  rv = caml_copy_int64 (r);\n"
      );

      pr "  CAMLreturn (rv);\n";
      pr "}\n";
      pr "\n";

  ) functions;

  pr "\
static int
HiveOpenFlags_val (value v)
{
  int flags = 0;
  value v2;

  while (v != Val_int (0)) {
    v2 = Field (v, 0);
    flags |= 1 << Int_val (v2);
    v = Field (v, 1);
  }

  return flags;
}

static hive_set_value *
HiveSetValue_val (value v)
{
  hive_set_value *val = malloc (sizeof (hive_set_value));

  if (val == NULL) caml_raise_out_of_memory ();

  val->key = (char *) String_val (Field (v, 0));
  val->t = HiveType_val (Field (v, 1));
  val->len = caml_string_length (Field (v, 2));
  val->value = (char *) String_val (Field (v, 2));

  return val;
}

static hive_set_value *
HiveSetValues_val (value v)
{
  size_t nr_values = Wosize_val (v);
  hive_set_value *values = malloc (nr_values * sizeof (hive_set_value));
  size_t i;
  value v2;

  if (values == NULL) caml_raise_out_of_memory ();

  for (i = 0; i < nr_values; ++i) {
    v2 = Field (v, i);
    values[i].key = (char *) String_val (Field (v2, 0));
    values[i].t = HiveType_val (Field (v2, 1));
    values[i].len = caml_string_length (Field (v2, 2));
    values[i].value = (char *) String_val (Field (v2, 2));
  }

  return values;
}

static hive_type
HiveType_val (value v)
{
  if (Is_long (v))
    return Int_val (v); /* REG_NONE etc. */
  else
    return Int32_val (Field (v, 0)); /* REG_UNKNOWN of int32 */
}

static value
Val_hive_type (hive_type t)
{
  CAMLparam0 ();
  CAMLlocal2 (rv, v);

  if (t <= %d)
    CAMLreturn (Val_int (t));
  else {
    rv = caml_alloc (1, 0); /* REG_UNKNOWN of int32 */
    v = caml_copy_int32 (t);
    caml_modify (&Field (rv, 0), v);
    CAMLreturn (rv);
  }
}

static value
copy_int_array (size_t *xs)
{
  CAMLparam0 ();
  CAMLlocal2 (v, rv);
  size_t nr, i;

  for (nr = 0; xs[nr] != 0; ++nr)
    ;
  if (nr == 0)
    CAMLreturn (Atom (0));
  else {
    rv = caml_alloc (nr, 0);
    for (i = 0; i < nr; ++i) {
      v = Val_int (xs[i]);
      Store_field (rv, i, v); /* Safe because v is not a block. */
    }
    CAMLreturn (rv);
  }
}

static value
copy_type_len (size_t len, hive_type t)
{
  CAMLparam0 ();
  CAMLlocal2 (v, rv);

  rv = caml_alloc (2, 0);
  v = Val_hive_type (t);
  Store_field (rv, 0, v);
  v = Val_int (len);
  Store_field (rv, 1, v);
  CAMLreturn (rv);
}

static value
copy_len_value (size_t len, hive_value_h r)
{
  CAMLparam0 ();
  CAMLlocal2 (v, rv);

  rv = caml_alloc (2, 0);
  v = Val_int (len);
  Store_field (rv, 0, v);
  v = Val_int (r);
  Store_field (rv, 1, v);
  CAMLreturn (rv);
}

static value
copy_type_value (const char *r, size_t len, hive_type t)
{
  CAMLparam0 ();
  CAMLlocal2 (v, rv);

  rv = caml_alloc (2, 0);
  v = Val_hive_type (t);
  Store_field (rv, 0, v);
  v = caml_alloc_initialized_string (len, r);
  caml_modify (&Field (rv, 1), v);
  CAMLreturn (rv);
}

/* Raise exceptions. */
static void
raise_error (const char *function)
{
  /* Save errno early in case it gets trashed. */
  int err = errno;

  CAMLparam0 ();
  CAMLlocal3 (v1, v2, v3);

  v1 = caml_copy_string (function);
  v2 = unix_error_of_code (err);
  v3 = caml_copy_string (strerror (err));
  value vvv[] = { v1, v2, v3 };
  caml_raise_with_args (*caml_named_value (\"ocaml_hivex_error\"), 3, vvv);

  CAMLnoreturn;
}

static void
raise_closed (const char *function)
{
  CAMLparam0 ();
  CAMLlocal1 (v);

  v = caml_copy_string (function);
  caml_raise_with_arg (*caml_named_value (\"ocaml_hivex_closed\"), v);

  CAMLnoreturn;
}

/* Allocate handles and deal with finalization. */
static void
hivex_finalize (value hv)
{
  hive_h *h = Hiveh_val (hv);
  if (h) hivex_close (h);
}

static struct custom_operations hivex_custom_operations = {
  (char *) \"hivex_custom_operations\",
  hivex_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value
Val_hiveh (hive_h *h)
{
  CAMLparam0 ();
  CAMLlocal1 (rv);

  rv = caml_alloc_custom (&hivex_custom_operations,
                          sizeof (hive_h *), 0, 1);
  Hiveh_val (rv) = h;

  CAMLreturn (rv);
}
" max_hive_type

and generate_perl_pm () =
  generate_header HashStyle LGPLv2plus;

  pr "\
=pod

=head1 NAME

Win::Hivex - Perl bindings for reading and writing Windows Registry hive files

=head1 SYNOPSIS

 use Win::Hivex;

 $h = Win::Hivex->open ('SOFTWARE');
 $root_node = $h->root ();
 print $h->node_name ($root_node);

=head1 DESCRIPTION

The C<Win::Hivex> module provides a Perl XS binding to the
L<hivex(3)> API for reading and writing Windows Registry binary
hive files.

=head1 ERRORS

All errors turn into calls to C<croak> (see L<Carp(3)>).

=head1 METHODS

=over 4

=cut

package Win::Hivex;

use strict;
use warnings;

require XSLoader;
XSLoader::load ('Win::Hivex');

=item open

 $h = Win::Hivex->open ($filename,";

  List.iter (
    fun (_, flag, _) ->
      pr "\n                        [%s => 1,]" (String.lowercase_ascii flag)
  ) open_flags;

  pr ")

Open a Windows Registry binary hive file.

The C<verbose> and C<debug> flags enable different levels of
debugging messages.

The C<write> flag is required if you will be modifying the
hive file (see L<hivex(3)/WRITING TO HIVE FILES>).

This function returns a hive handle.  The hive handle is
closed automatically when its reference count drops to 0.

=cut

sub open {
  my $proto = shift;
  my $class = ref ($proto) || $proto;
  my $filename = shift;
  my %%flags = @_;
  my $flags = 0;

";

  List.iter (
    fun (n, flag, description) ->
      pr "  # %s\n" description;
      pr "  $flags += %d if $flags{%s};\n" n (String.lowercase_ascii flag)
  ) open_flags;

  pr "\

  my $self = Win::Hivex::_open ($filename, $flags);
  bless $self, $class;
  return $self;
}

";

  List.iter (
    fun (name, style, _, longdesc) ->
      (* The close call isn't explicit in Perl: handles are closed
       * when their reference count drops to 0.
       *
       * The open call is coded specially in Perl.
       *
       * Therefore we don't generate prototypes for these two calls:
       *)
      if fst style <> RErrDispose && List.hd (snd style) = AHive then (
        let longdesc = replace_str longdesc "C<hivex_" "C<" in
        pr "=item %s\n\n " name;
        generate_perl_prototype name style;
        pr "\n\n";
        pr "%s\n\n" longdesc;

        (match fst style with
         | RErr
         | RErrDispose
         | RHive
         | RString
         | RStringList
         | RLenType
         | RLenValue
         | RLenTypeVal
         | RInt32
         | RInt64 -> ()
         | RSize ->
             pr "\
This returns a size.\n\n"
         | RNode ->
             pr "\
This returns a node handle.\n\n"
         | RNodeNotFound ->
             pr "\
This returns a node handle, or C<undef> if the node was not found.\n\n"
         | RNodeList ->
             pr "\
This returns a list of node handles.\n\n"
         | RValue ->
             pr "\
This returns a value handle.\n\n"
         | RValueList ->
             pr "\
This returns a list of value handles.\n\n"
        );

        if List.mem ASetValues (snd style) then
          pr "C<@values> is an array of (keys, value) pairs.
Each element should be a hashref containing C<key>, C<t> (type)
and C<data>.

Any existing values stored at the node are discarded, and their
C<value> handles become invalid.  Thus you can remove all
values stored at C<node> by passing C<@values = []>.\n\n"
      )
  ) functions;

  pr "\
=cut

1;

=back

=head1 COPYRIGHT

Copyright (C) %s Red Hat Inc.

=head1 LICENSE

Please see the file COPYING.LIB for the full license.

=head1 SEE ALSO

L<hivex(3)>,
L<hivexsh(1)>,
L<http://libguestfs.org>,
L<Sys::Guestfs(3)>.

=cut
" copyright_years

and generate_perl_prototype name style =
  (* Return type. *)
  (match fst style with
   | RErr
   | RErrDispose -> ()
   | RHive -> pr "$h = "
   | RSize -> pr "$size = "
   | RNode
   | RNodeNotFound -> pr "$node = "
   | RNodeList -> pr "@nodes = "
   | RValue -> pr "$value = "
   | RValueList -> pr "@values = "
   | RString -> pr "$string = "
   | RStringList -> pr "@strings = "
   | RLenType -> pr "($type, $len) = "
   | RLenValue -> pr "($len, $value) = "
   | RLenTypeVal -> pr "($type, $data) = "
   | RInt32 -> pr "$int32 = "
   | RInt64 -> pr "$int64 = "
  );

  let args = List.tl (snd style) in

  (* AUnusedFlags is dropped in the bindings. *)
  let args = List.filter ((<>) AUnusedFlags) args in

  pr "$h->%s (" name;

  let comma = ref false in
  List.iter (
    fun arg ->
      if !comma then pr ", "; comma := true;
      match arg with
      | AHive -> pr "$h"
      | ANode n
      | AValue n
      | AString n -> pr "$%s" n
      | AStringNullable n -> pr "[$%s|undef]" n
      | AOpenFlags -> pr "[flags]"
      | AUnusedFlags -> assert false
      | ASetValues -> pr "\\@values"
      | ASetValue -> pr "$val"
  ) args;

  pr ")"

and generate_perl_xs () =
  generate_header CStyle LGPLv2plus;

  pr "\
#include \"EXTERN.h\"
#include \"perl.h\"
#include \"XSUB.h\"

#include <string.h>
#include <hivex.h>
#include <inttypes.h>

/* For Perl < 5.12 */
#ifndef newSVpvn_utf8
#define newSVpvn_utf8(a,b,u) newSVpvn((a),(b))
#endif

static SV *
my_newSVll(long long val) {
#ifdef USE_64_BIT_ALL
  return newSViv(val);
#else
  char buf[100];
  int len;
  len = snprintf(buf, 100, \"%%\" PRId64, val);
  return newSVpv(buf, len);
#endif
}

#if 0
static SV *
my_newSVull(unsigned long long val) {
#ifdef USE_64_BIT_ALL
  return newSVuv(val);
#else
  char buf[100];
  int len;
  len = snprintf(buf, 100, \"%%\" PRIu64, val);
  return newSVpv(buf, len);
#endif
}
#endif

#if 0
/* http://www.perlmonks.org/?node_id=680842 */
static char **
XS_unpack_charPtrPtr (SV *arg) {
  char **ret;
  AV *av;
  I32 i;

  if (!arg || !SvOK (arg) || !SvROK (arg) || SvTYPE (SvRV (arg)) != SVt_PVAV)
    croak (\"array reference expected\");

  av = (AV *)SvRV (arg);
  ret = malloc ((av_len (av) + 1 + 1) * sizeof (char *));
  if (!ret)
    croak (\"malloc failed\");

  for (i = 0; i <= av_len (av); i++) {
    SV **elem = av_fetch (av, i, 0);

    if (!elem || !*elem)
      croak (\"missing element in list\");

    ret[i] = SvPV_nolen (*elem);
  }

  ret[i] = NULL;

  return ret;
}
#endif

/* Handle set_values parameter. */
typedef struct pl_set_values {
  size_t nr_values;
  hive_set_value *values;
} pl_set_values;

static pl_set_values
unpack_pl_set_values (SV *sv)
{
  pl_set_values ret;
  AV *av;
  I32 i;

  if (!sv || !SvOK (sv) || !SvROK (sv) || SvTYPE (SvRV (sv)) != SVt_PVAV)
    croak (\"array reference expected\");

  av = (AV *)SvRV(sv);
  ret.nr_values = av_len (av) + 1;
  ret.values = malloc (ret.nr_values * sizeof (hive_set_value));
  if (!ret.values)
    croak (\"malloc failed\");

  for (i = 0; i <= av_len (av); i++) {
    SV **hvp = av_fetch (av, i, 0);

    if (!hvp || !*hvp || !SvROK (*hvp) || SvTYPE (SvRV (*hvp)) != SVt_PVHV)
      croak (\"missing element in list or not a hash ref\");

    HV *hv = (HV *)SvRV(*hvp);

    SV **svp;
    svp = hv_fetch (hv, \"key\", 3, 0);
    if (!svp || !*svp)
      croak (\"missing 'key' in hash\");
    ret.values[i].key = SvPV_nolen (*svp);

    svp = hv_fetch (hv, \"t\", 1, 0);
    if (!svp || !*svp)
      croak (\"missing 't' in hash\");
    ret.values[i].t = SvIV (*svp);

    svp = hv_fetch (hv, \"value\", 5, 0);
    if (!svp || !*svp)
      croak (\"missing 'value' in hash\");
    ret.values[i].value = SvPV (*svp, ret.values[i].len);
  }

  return ret;
}

static hive_set_value *
unpack_set_value (SV *sv)
{
  hive_set_value *ret;

  if (!sv || !SvROK (sv) || SvTYPE (SvRV (sv)) != SVt_PVHV)
    croak (\"not a hash ref\");

  ret = malloc (sizeof (hive_set_value));
  if (ret == NULL)
    croak (\"malloc failed\");

  HV *hv = (HV *)SvRV(sv);

  SV **svp;
  svp = hv_fetch (hv, \"key\", 3, 0);
  if (!svp || !*svp)
    croak (\"missing 'key' in hash\");
  ret->key = SvPV_nolen (*svp);

  svp = hv_fetch (hv, \"t\", 1, 0);
  if (!svp || !*svp)
    croak (\"missing 't' in hash\");
  ret->t = SvIV (*svp);

  svp = hv_fetch (hv, \"value\", 5, 0);
  if (!svp || !*svp)
    croak (\"missing 'value' in hash\");
  ret->value = SvPV (*svp, ret->len);

  return ret;
}

MODULE = Win::Hivex  PACKAGE = Win::Hivex

PROTOTYPES: ENABLE

hive_h *
_open (filename, flags)
      char *filename;
      int flags;
   CODE:
      RETVAL = hivex_open (filename, flags);
      if (!RETVAL)
        croak (\"hivex_open: %%s: %%s\", filename, strerror (errno));
 OUTPUT:
      RETVAL

void
DESTROY (h)
      hive_h *h;
 PPCODE:
      if (hivex_close (h) == -1)
        croak (\"hivex_close: %%s\", strerror (errno));

";

  List.iter (
    fun (name, style, _, longdesc) ->
      (* The close and open calls are handled specially above. *)
      if fst style <> RErrDispose && List.hd (snd style) = AHive then (
        (match fst style with
         | RErr -> pr "void\n"
         | RErrDispose -> failwith "perl bindings cannot handle a call which disposes of the handle"
         | RHive -> failwith "perl bindings cannot handle a call which returns a handle"
         | RSize
         | RNode
         | RNodeNotFound
         | RValue
         | RString -> pr "SV *\n"
         | RNodeList
         | RValueList
         | RStringList
         | RLenType
         | RLenValue
         | RLenTypeVal -> pr "void\n"
         | RInt32 -> pr "SV *\n"
         | RInt64 -> pr "SV *\n"
        );

        (* Call and arguments. *)
        let perl_params =
          filter_map (function
                      | AUnusedFlags -> None
                      | arg -> Some (name_of_argt arg)) (snd style) in

        let c_params =
          List.map (function
                    | AUnusedFlags -> "0"
                    | ASetValues -> "values.nr_values, values.values"
                    | arg -> name_of_argt arg) (snd style) in

        pr "%s (%s)\n" name (String.concat ", " perl_params);
        iteri (
          fun i ->
            function
            | AHive ->
                pr "      hive_h *h;\n"
            | ANode n
            | AValue n ->
                pr "      int %s;\n" n
            | AString n ->
                pr "      char *%s;\n" n
            | AStringNullable n ->
                (* http://www.perlmonks.org/?node_id=554277 *)
                pr "      char *%s = SvOK(ST(%d)) ? SvPV_nolen(ST(%d)) : NULL;\n" n i i
            | AOpenFlags ->
                pr "      int flags;\n"
            | AUnusedFlags -> ()
            | ASetValues ->
                pr "      pl_set_values values = unpack_pl_set_values (ST(%d));\n" i
            | ASetValue ->
                pr "      hive_set_value *val = unpack_set_value (ST(%d));\n" i
        ) (snd style);

        let free_args () =
          List.iter (
            function
            | ASetValues ->
                pr "      free (values.values);\n"
            | ASetValue ->
                pr "      free (val);\n"
            | AHive | ANode _ | AValue _ | AString _ | AStringNullable _
            | AOpenFlags | AUnusedFlags -> ()
          ) (snd style)
        in

        (* Code. *)
        (match fst style with
         | RErr ->
             pr "PREINIT:\n";
             pr "      int r;\n";
             pr " PPCODE:\n";
             pr "      r = hivex_%s (%s);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == -1)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;

         | RErrDispose -> assert false
         | RHive -> assert false

         | RSize
         | RNode
         | RValue ->
             pr "PREINIT:\n";
             pr "      /* hive_node_h = hive_value_h = size_t so we cheat\n";
             pr "         here to simplify the generator */\n";
             pr "      size_t r;\n";
             pr "   CODE:\n";
             pr "      r = hivex_%s (%s);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == 0)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      RETVAL = newSViv (r);\n";
             pr " OUTPUT:\n";
             pr "      RETVAL\n"

         | RNodeNotFound ->
             pr "PREINIT:\n";
             pr "      hive_node_h r;\n";
             pr "   CODE:\n";
             pr "      errno = 0;\n";
             pr "      r = hivex_%s (%s);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == 0 && errno != 0)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      if (r == 0)\n";
             pr "        RETVAL = &PL_sv_undef;\n";
             pr "      else\n";
             pr "        RETVAL = newSViv (r);\n";
             pr " OUTPUT:\n";
             pr "      RETVAL\n"

         | RString ->
             pr "PREINIT:\n";
             pr "      char *r;\n";
             pr "   CODE:\n";
             pr "      r = hivex_%s (%s);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == NULL)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             if f_len_exists name then
               pr "      RETVAL = newSVpvn_utf8 (r, hivex_%s_len (%s), 1);\n"
                 name (String.concat ", " c_params)
             else
               pr "      RETVAL = newSVpv (r, 0);\n";
             pr "      free (r);\n";
             pr " OUTPUT:\n";
             pr "      RETVAL\n"

         | RNodeList
         | RValueList ->
             pr "PREINIT:\n";
             pr "      size_t *r;\n";
             pr "      int i, n;\n";
             pr " PPCODE:\n";
             pr "      r = hivex_%s (%s);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == NULL)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      for (n = 0; r[n] != 0; ++n) /**/;\n";
             pr "      EXTEND (SP, n);\n";
             pr "      for (i = 0; i < n; ++i)\n";
             pr "        PUSHs (sv_2mortal (newSViv (r[i])));\n";
             pr "      free (r);\n";

         | RStringList ->
             pr "PREINIT:\n";
             pr "      char **r;\n";
             pr "      int i, n;\n";
             pr " PPCODE:\n";
             pr "      r = hivex_%s (%s);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == NULL)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      for (n = 0; r[n] != NULL; ++n) /**/;\n";
             pr "      EXTEND (SP, n);\n";
             pr "      for (i = 0; i < n; ++i) {\n";
             pr "        PUSHs (sv_2mortal (newSVpv (r[i], 0)));\n";
             pr "        free (r[i]);\n";
             pr "      }\n";
             pr "      free (r);\n";

         | RLenType ->
             pr "PREINIT:\n";
             pr "      int r;\n";
             pr "      size_t len;\n";
             pr "      hive_type type;\n";
             pr " PPCODE:\n";
             pr "      r = hivex_%s (%s, &type, &len);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == -1)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      EXTEND (SP, 2);\n";
             pr "      PUSHs (sv_2mortal (newSViv (type)));\n";
             pr "      PUSHs (sv_2mortal (newSViv (len)));\n";

         | RLenValue ->
             pr "PREINIT:\n";
             pr "      hive_value_h r;\n";
             pr "      size_t len;\n";
             pr " PPCODE:\n";
             pr "      errno = 0;\n";
             pr "      r = hivex_%s (%s, &len);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == 0 && errno)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      EXTEND (SP, 2);\n";
             pr "      PUSHs (sv_2mortal (newSViv (len)));\n";
             pr "      PUSHs (sv_2mortal (newSViv (r)));\n";

         | RLenTypeVal ->
             pr "PREINIT:\n";
             pr "      char *r;\n";
             pr "      size_t len;\n";
             pr "      hive_type type;\n";
             pr " PPCODE:\n";
             pr "      r = hivex_%s (%s, &type, &len);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == NULL)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      EXTEND (SP, 2);\n";
             pr "      PUSHs (sv_2mortal (newSViv (type)));\n";
             pr "      PUSHs (sv_2mortal (newSVpvn (r, len)));\n";
             pr "      free (r);\n";

         | RInt32 ->
             pr "PREINIT:\n";
             pr "      int32_t r;\n";
             pr "   CODE:\n";
             pr "      errno = 0;\n";
             pr "      r = hivex_%s (%s);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == -1 && errno != 0)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      RETVAL = newSViv (r);\n";
             pr " OUTPUT:\n";
             pr "      RETVAL\n"

         | RInt64 ->
             pr "PREINIT:\n";
             pr "      int64_t r;\n";
             pr "   CODE:\n";
             pr "      errno = 0;\n";
             pr "      r = hivex_%s (%s);\n"
               name (String.concat ", " c_params);
             free_args ();
             pr "      if (r == -1 && errno != 0)\n";
             pr "        croak (\"%%s: %%s\", \"%s\", strerror (errno));\n"
               name;
             pr "      RETVAL = my_newSVll (r);\n";
             pr " OUTPUT:\n";
             pr "      RETVAL\n"
        );
        pr "\n"
      )
  ) functions

and generate_python_c () =
  generate_header CStyle LGPLv2plus;

  pr "\
#include <config.h>

#define PY_SSIZE_T_CLEAN 1
#include <Python.h>

#if PY_VERSION_HEX < 0x02050000
typedef int Py_ssize_t;
#define PY_SSIZE_T_MAX INT_MAX
#define PY_SSIZE_T_MIN INT_MIN
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include \"hivex.h\"

static hive_h *
get_handle (PyObject *obj)
{
  assert (obj);
  assert (obj != Py_None);
  return (hive_h *) PyCapsule_GetPointer(obj, \"hive_h\");
}

static PyObject *
put_handle (hive_h *h)
{
  assert (h);
  return PyCapsule_New ((void *) h, \"hive_h\", NULL);
}

/* This returns pointers into the Python objects, which should
 * not be freed.
 */
static int
get_value (PyObject *v, hive_set_value *ret)
{
  PyObject *obj;
  PyObject *bytes;

  if (!PyDict_Check (v)) {
    PyErr_SetString (PyExc_TypeError, \"expected dictionary type for value\");
    return -1;
  }

  obj = PyDict_GetItemString (v, \"key\");
  if (!obj) {
    PyErr_SetString (PyExc_KeyError, \"no 'key' element in dictionary\");
    return -1;
  }
  if (PyUnicode_Check (obj)) {
    /* TODO: use PyUnicode_DecodeASCII or PyUnicode_AsUTF16String instead? */
    bytes = PyUnicode_AsUTF8String (obj);
    if (bytes == NULL) {
      PyErr_SetString (PyExc_ValueError, \"failed to decode 'key'\");
      return -1;
    }
    ret->key = PyBytes_AS_STRING (bytes);
  } else if (PyBytes_Check (obj)) {
    ret->key = PyBytes_AS_STRING (obj);
  } else {
    PyErr_SetString (PyExc_TypeError, \"expected bytes type for 'key'\");
    return -1;
  }

  obj = PyDict_GetItemString (v, \"t\");
  if (!obj) {
    PyErr_SetString (PyExc_KeyError, \"no 't' element in dictionary\");
    return -1;
  }
  ret->t = PyLong_AsLong (obj);
  if (PyErr_Occurred ()) {
    PyErr_SetString (PyExc_TypeError, \"expected int type for 't'\");
    return -1;
  }

  obj = PyDict_GetItemString (v, \"value\");
  if (!obj) {
    PyErr_SetString (PyExc_KeyError, \"no 'value' element in dictionary\");
    return -1;
  }
  /* Support bytes only. As the registry can use multiple character sets, reject
   * Unicode str types and let the caller handle conversion to nul-terminated
   * UTF-16-LE, ASCII, etc. as necessary. This means that 'x' and b'x' are valid
   * in Python 2 (but not u'x') but that in Python 3, only b'x' is valid. */
  if (PyBytes_Check (obj)) {
    ret->len = PyBytes_GET_SIZE (obj);
    ret->value = PyBytes_AS_STRING (obj);
  } else {
    PyErr_SetString (PyExc_TypeError, \"expected bytes type for 'value'\");
    return -1;
  }

  return 0;
}

typedef struct py_set_values {
  size_t nr_values;
  hive_set_value *values;
} py_set_values;

static int
get_values (PyObject *v, py_set_values *ret)
{
  Py_ssize_t slen;
  size_t len, i;

  if (!PyList_Check (v)) {
    PyErr_SetString (PyExc_RuntimeError, \"expecting a list parameter\");
    return -1;
  }

  slen = PyList_Size (v);
  if (slen < 0) {
    PyErr_SetString (PyExc_RuntimeError, \"get_string_list: PyList_Size failure\");
    return -1;
  }
  len = (size_t) slen;
  ret->nr_values = len;
  ret->values = malloc (len * sizeof (hive_set_value));
  if (!ret->values) {
    PyErr_NoMemory ();
    return -1;
  }

  for (i = 0; i < len; ++i) {
    if (get_value (PyList_GetItem (v, i), &(ret->values[i])) == -1) {
      free (ret->values);
      return -1;
    }
  }

  return 0;
}

static PyObject *
put_string_list (char * const * const argv)
{
  PyObject *list;
  size_t argc, i;

  for (argc = 0; argv[argc] != NULL; ++argc)
    ;

  list = PyList_New (argc);
  for (i = 0; i < argc; ++i) {
    PyList_SetItem (list, i, PyUnicode_DecodeUTF8 (argv[i], strlen (argv[i]), NULL));
  }

  return list;
}

static void
free_strings (char **argv)
{
  size_t argc;

  for (argc = 0; argv[argc] != NULL; ++argc)
    free (argv[argc]);
  free (argv);
}

/* Since hive_node_t is the same as hive_value_t this also works for values. */
static PyObject *
put_node_list (hive_node_h *nodes)
{
  PyObject *list;
  size_t argc, i;

  for (argc = 0; nodes[argc] != 0; ++argc)
    ;

  list = PyList_New (argc);
  for (i = 0; i < argc; ++i)
    PyList_SetItem (list, i, PyLong_FromLongLong ((long) nodes[i]));

  return list;
}

static PyObject *
put_len_type (size_t len, hive_type t)
{
  PyObject *r = PyTuple_New (2);
  PyTuple_SetItem (r, 0, PyLong_FromLong ((long) t));
  PyTuple_SetItem (r, 1, PyLong_FromLongLong ((long) len));
  return r;
}

static PyObject *
put_len_val (size_t len, hive_value_h value)
{
  PyObject *r = PyTuple_New (2);
  PyTuple_SetItem (r, 0, PyLong_FromLongLong ((long) len));
  PyTuple_SetItem (r, 1, PyLong_FromLongLong ((long) value));
  return r;
}

static PyObject *
put_val_type (char *val, size_t len, hive_type t)
{
  PyObject *r = PyTuple_New (2);
  PyTuple_SetItem (r, 0, PyLong_FromLong ((long) t));
  PyTuple_SetItem (r, 1, PyBytes_FromStringAndSize (val, len));
  return r;
}

";

  (* Generate functions. *)
  List.iter (
    fun (name, style, _, longdesc) ->
      pr "static PyObject *\n";
      pr "py_hivex_%s (PyObject *self, PyObject *args)\n" name;
      pr "{\n";
      pr "  PyObject *py_r;\n";

      let error_code =
        match fst style with
        | RErr -> pr "  int r;\n"; "-1"
        | RErrDispose -> pr "  int r;\n"; "-1"
        | RHive -> pr "  hive_h *r;\n"; "NULL"
        | RSize -> pr "  size_t r;\n"; "0"
        | RNode -> pr "  hive_node_h r;\n"; "0"
        | RNodeNotFound ->
            pr "  errno = 0;\n";
            pr "  hive_node_h r;\n";
            "0 && errno != 0"
        | RNodeList -> pr "  hive_node_h *r;\n"; "NULL"
        | RValue -> pr "  hive_value_h r;\n"; "0"
        | RValueList -> pr "  hive_value_h *r;\n"; "NULL"
        | RString -> pr "  char *r;\n"; "NULL"
        | RStringList -> pr "  char **r;\n"; "NULL"
        | RLenType ->
            pr "  int r;\n";
            pr "  size_t len;\n";
            pr "  hive_type t;\n";
            "-1"
        | RLenValue ->
            pr "  errno = 0;\n";
            pr "  int r;\n";
            pr "  size_t len;\n";
            "0 && errno != 0"
        | RLenTypeVal ->
            pr "  char *r;\n";
            pr "  size_t len;\n";
            pr "  hive_type t;\n";
            "NULL"
        | RInt32 ->
            pr "  errno = 0;\n";
            pr "  int32_t r;\n";
            "-1 && errno != 0"
        | RInt64 ->
            pr "  errno = 0;\n";
            pr "  int64_t r;\n";
            "-1 && errno != 0" in

      (* Call and arguments. *)
      let c_params =
        List.map (function
                  | AUnusedFlags -> "0"
                  | ASetValues -> "values.nr_values, values.values"
                  | ASetValue -> "&val"
                  | arg -> name_of_argt arg) (snd style) in
      let c_params =
        match fst style with
        | RLenType | RLenTypeVal -> c_params @ ["&t"; "&len"]
        | RLenValue -> c_params @ ["&len"]
        | _ -> c_params in

      List.iter (
        function
        | AHive ->
            pr "  hive_h *h;\n";
            pr "  PyObject *py_h;\n"
        | ANode n
        | AValue n ->
            pr "  long %s;\n" n
        | AString n
        | AStringNullable n ->
            pr "  char *%s;\n" n
        | AOpenFlags ->
            pr "  int flags;\n"
        | AUnusedFlags -> ()
        | ASetValues ->
            pr "  py_set_values values;\n";
            pr "  PyObject *py_values;\n"
        | ASetValue ->
            pr "  hive_set_value val;\n";
            pr "  PyObject *py_val;\n"
      ) (snd style);

      pr "\n";

      (* Convert the required parameters. *)
      pr "  if (!PyArg_ParseTuple (args, (char *) \"";
      List.iter (
        function
        | AHive ->
            pr "O"
        | ANode n
        | AValue n ->
            pr "l"
        | AString n ->
            pr "s"
        | AStringNullable n ->
            pr "z"
        | AOpenFlags ->
            pr "i"
        | AUnusedFlags -> ()
        | ASetValues
        | ASetValue ->
            pr "O"
      ) (snd style);

      pr ":hivex_%s\"" name;

      List.iter (
        function
        | AHive ->
            pr ", &py_h"
        | ANode n
        | AValue n ->
            pr ", &%s" n
        | AString n
        | AStringNullable n ->
            pr ", &%s" n
        | AOpenFlags ->
            pr ", &flags"
        | AUnusedFlags -> ()
        | ASetValues ->
            pr ", &py_values"
        | ASetValue ->
            pr ", &py_val"
        ) (snd style);

      pr "))\n";
      pr "    return NULL;\n";

      (* Convert some Python argument types to C. *)
      List.iter (
        function
        | AHive ->
            pr "  h = get_handle (py_h);\n"
        | ANode _
        | AValue _
        | AString _
        | AStringNullable _
        | AOpenFlags
        | AUnusedFlags -> ()
        | ASetValues ->
            pr "  if (get_values (py_values, &values) == -1)\n";
            pr "    return NULL;\n"
        | ASetValue ->
            pr "  if (get_value (py_val, &val) == -1)\n";
            pr "    return NULL;\n"
      ) (snd style);

      (* Call the C function. *)
      pr "  r = hivex_%s (%s);\n" name (String.concat ", " c_params);

      (* Free up arguments. *)
      List.iter (
        function
        | AHive | ANode _ | AValue _
        | AString _ | AStringNullable _
        | AOpenFlags | AUnusedFlags -> ()
        | ASetValues ->
            pr "  free (values.values);\n"
        | ASetValue -> ()
      ) (snd style);

      (* Check for errors from C library. *)
      pr "  if (r == %s) {\n" error_code;
      pr "    PyErr_SetString (PyExc_RuntimeError,\n";
      pr "                     strerror (errno));\n";
      pr "    return NULL;\n";
      pr "  }\n";
      pr "\n";

      (* Convert return value to Python. *)
      (match fst style with
       | RErr
       | RErrDispose ->
           pr "  Py_INCREF (Py_None);\n";
           pr "  py_r = Py_None;\n"
       | RHive ->
           pr "  py_r = put_handle (r);\n"
       | RSize
       | RNode ->
           pr "  py_r = PyLong_FromLongLong (r);\n"
       | RNodeNotFound ->
           pr "  if (r)\n";
           pr "    py_r = PyLong_FromLongLong (r);\n";
           pr "  else {\n";
           pr "    Py_INCREF (Py_None);\n";
           pr "    py_r = Py_None;\n";
           pr "  }\n";
       | RNodeList
       | RValueList ->
           pr "  py_r = put_node_list (r);\n";
           pr "  free (r);\n"
       | RValue ->
           pr "  py_r = PyLong_FromLongLong (r);\n"
       | RString ->
           if f_len_exists name then
             pr "  size_t sz = hivex_%s_len (%s);\n"
               name (String.concat ", " c_params);
           if f_len_exists name then
             pr "  py_r = PyUnicode_DecodeUTF8 (r, sz, NULL);\n"
           else
             pr "  py_r = PyUnicode_DecodeUTF8 (r, strlen (r), NULL);\n";
           pr "  free (r);"
       | RStringList ->
           pr "  py_r = put_string_list (r);\n";
           pr "  free_strings (r);\n"
       | RLenType ->
           pr "  py_r = put_len_type (len, t);\n"
       | RLenValue ->
           pr "  py_r = put_len_val (len, r);\n"
       | RLenTypeVal ->
           pr "  py_r = put_val_type (r, len, t);\n";
           pr "  free (r);\n"
       | RInt32 ->
           pr "  py_r = PyLong_FromLong ((long) r);\n"
       | RInt64 ->
           pr "  py_r = PyLong_FromLongLong (r);\n"
      );
      pr "  return py_r;\n";
      pr "}\n";
      pr "\n"
  ) functions;

  (* Table of functions. *)
  pr "static PyMethodDef methods[] = {\n";
  List.iter (
    fun (name, _, _, _) ->
      pr "  { (char *) \"%s\", py_hivex_%s, METH_VARARGS, NULL },\n"
        name name
  ) functions;
  pr "  { NULL, NULL, 0, NULL }\n";
  pr "};\n";
  pr "\n";

  (* Init function. *)
  pr "\
#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef moduledef = {
  PyModuleDef_HEAD_INIT,
  \"libhivexmod\",       /* m_name */
  \"hivex module\",      /* m_doc */
  -1,                    /* m_size */
  methods,               /* m_methods */
  NULL,                  /* m_reload */
  NULL,                  /* m_traverse */
  NULL,                  /* m_clear */
  NULL,                  /* m_free */
};
#endif

static PyObject *
moduleinit (void)
{
  PyObject *m;

#if PY_MAJOR_VERSION >= 3
  m = PyModule_Create (&moduledef);
#else
  m = Py_InitModule ((char *) \"libhivexmod\", methods);
#endif

  if (m) {
    PyModule_AddStringConstant (m, \"__version__\", PACKAGE_VERSION);
  }

  return m; /* m might be NULL if module init failed */
}

#if PY_MAJOR_VERSION >= 3
PyMODINIT_FUNC
PyInit_libhivexmod (void)
{
  return moduleinit ();
}
#else
void
initlibhivexmod (void)
{
  (void) moduleinit ();
}
#endif
"

and generate_python_py () =
  generate_header HashStyle LGPLv2plus;

  pr "\
\"\"\"Python bindings for hivex

import hivex
h = hivex.Hivex (filename)

The hivex module provides Python bindings to the hivex API for
examining and modifying Windows Registry 'hive' files.

Read the hivex(3) man page to find out how to use the API.
\"\"\"

import libhivexmod

__version__ = libhivexmod.__version__

class Hivex(object):
    \"\"\"Instances of this class are hivex API handles.\"\"\"

    def __init__ (self, filename";

  List.iter (
    fun (_, flag, _) -> pr ", %s = False" (String.lowercase_ascii flag)
  ) open_flags;

  pr "):
        \"\"\"Create a new hivex handle.\"\"\"
        flags = 0
";

  List.iter (
    fun (n, flag, description) ->
      pr "        # %s\n" description;
      pr "        if %s: flags += %d\n" (String.lowercase_ascii flag) n
  ) open_flags;

  pr "        self._o = libhivexmod.open (filename, flags)

    def __del__ (self):
        libhivexmod.close (self._o)

";

  List.iter (
    fun (name, style, shortdesc, _) ->
      (* The close and open calls are handled specially above. *)
      if fst style <> RErrDispose && List.hd (snd style) = AHive then (
        let args = List.tl (snd style) in
        let args = List.filter (
          function AOpenFlags | AUnusedFlags -> false
          | _ -> true
        ) args in

        pr "    def %s (self" name;
        List.iter (fun arg -> pr ", %s" (name_of_argt arg)) args;
        pr "):\n";
        pr "        \"\"\"%s\"\"\"\n" shortdesc;
        pr "        return libhivexmod.%s (self._o" name;
        List.iter (
          fun arg ->
            pr ", ";
            match arg with
            | AHive -> assert false
            | ANode n | AValue n
            | AString n | AStringNullable n -> pr "%s" n
            | AOpenFlags
            | AUnusedFlags -> assert false
            | ASetValues -> pr "values"
            | ASetValue -> pr "val"
        ) args;
        pr ")\n";
        pr "\n"
      )
  ) functions

and generate_python_hive_types_py () =
  generate_header HashStyle LGPLv2plus;

  pr "\
\"\"\"Define integer constants for hive type

The names correspond with the hive_type enum type of the C API, but without
'hive_t_' prefix.
\"\"\"

";
  List.iter (
    fun (t, _, new_style, description) ->
      pr "# %s\n" description;
      pr "REG_%s = %d\n" new_style t
  ) hive_types;

and generate_ruby_c () =
  generate_header CStyle LGPLv2plus;

  pr "\
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <ruby.h>
#ifdef HAVE_RUBY_ENCODING_H
#include <ruby/encoding.h>
#endif

#include \"hivex.h\"

#include \"extconf.h\"

/* For Ruby < 1.9 */
#ifndef RARRAY_LEN
#define RARRAY_LEN(r) (RARRAY((r))->len)
#endif

#ifndef RSTRING_LEN
#define RSTRING_LEN(r) (RSTRING((r))->len)
#endif

#ifndef RSTRING_PTR
#define RSTRING_PTR(r) (RSTRING((r))->ptr)
#endif

static VALUE m_hivex;                   /* hivex module */
static VALUE c_hivex;                   /* hive_h handle */
static VALUE e_Error;                   /* used for all errors */

static void
ruby_hivex_free (void *hvp)
{
  hive_h *h = hvp;

  if (h)
    hivex_close (h);
}

static void
get_value (VALUE valv, hive_set_value *val)
{
  VALUE key = rb_hash_lookup (valv, ID2SYM (rb_intern (\"key\")));
  VALUE type = rb_hash_lookup (valv, ID2SYM (rb_intern (\"type\")));
  VALUE value = rb_hash_lookup (valv, ID2SYM (rb_intern (\"value\")));

  val->key = StringValueCStr (key);
  val->t = NUM2ULL (type);
  val->len = RSTRING_LEN (value);
  val->value = RSTRING_PTR (value);
}

static hive_set_value *
get_values (VALUE valuesv, size_t *nr_values)
{
  size_t i;
  hive_set_value *ret;

  *nr_values = RARRAY_LEN (valuesv);
  ret = malloc (sizeof (*ret) * *nr_values);
  if (ret == NULL)
    abort ();

  for (i = 0; i < *nr_values; ++i) {
    VALUE v = rb_ary_entry (valuesv, i);
    get_value (v, &ret[i]);
  }

  return ret;
}

";

  List.iter (
    fun (name, (ret, args), shortdesc, longdesc) ->
      let () =
        (* Generate rdoc. *)
        let doc = replace_str longdesc "C<hivex_" "C<h." in
        let doc = pod2text ~width:60 name doc in
        let doc = String.concat "\n * " doc in
        let doc = trim doc in

        let call, args =
          match args with
          | AHive :: args -> "h." ^ name, args
          | args -> "Hivex::" ^ name, args in
        let args = filter_map (
          function
          | AUnusedFlags -> None
          | args -> Some (name_of_argt args)
        ) args in
        let args = String.concat ", " args in

        let ret =
          match ret with
          | RErr | RErrDispose -> "nil"
          | RHive -> "Hivex::Hivex"
          | RSize | RNode | RNodeNotFound -> "integer"
          | RNodeList -> "list"
          | RValue -> "integer"
          | RValueList -> "list"
          | RString -> "string"
          | RStringList -> "list"
          | RLenType -> "hash"
          | RLenValue -> "integer"
          | RLenTypeVal -> "hash"
          | RInt32 -> "integer"
          | RInt64 -> "integer" in

        pr "\
/*
 * call-seq:
 *   %s(%s) -> %s
 *
 * %s
 *
 * %s
 *
 * (For the C API documentation for this function, see
 * +hivex_%s+[http://libguestfs.org/hivex.3.html#hivex_%s]).
 */
" call args ret shortdesc doc name name in

      (* Generate the function. *)
      pr "static VALUE\n";
      pr "ruby_hivex_%s (" name;

      let () =
        (* If the first argument is not AHive, then this is a module-level
         * function, and Ruby passes an implicit module argument which we
         * must ignore.  Otherwise the first argument is the hive handle.
         *)
        let args =
          match args with
          | AHive :: args -> pr "VALUE hv"; args
          | args -> pr "VALUE modulev"; args in
        List.iter (
          function
          | AUnusedFlags -> ()
          | arg ->
            pr ", VALUE %sv" (name_of_argt arg)
        ) args;
        pr ")\n" in

      pr "{\n";

      List.iter (
        function
        | AHive ->
          pr "  hive_h *h;\n";
          pr "  Data_Get_Struct (hv, hive_h, h);\n";
          pr "  if (!h)\n";
          pr "    rb_raise (rb_eArgError, \"%%s: used handle after closing it\",\n";
          pr "              \"%s\");\n" name;
        | ANode n ->
          pr "  hive_node_h %s = NUM2ULL (%sv);\n" n n
        | AValue n ->
          pr "  hive_value_h %s = NUM2ULL (%sv);\n" n n
        | AString n ->
          pr "  const char *%s = StringValueCStr (%sv);\n" n n;
        | AStringNullable n ->
          pr "  const char *%s =\n" n;
          pr "    !NIL_P (%sv) ? StringValueCStr (%sv) : NULL;\n" n n
        | AOpenFlags ->
          pr "  int flags = 0;\n";
          List.iter (
            fun (n, flag, _) ->
              pr "  if (RTEST (rb_hash_lookup (flagsv, ID2SYM (rb_intern (\"%s\")))))\n"
                (String.lowercase_ascii flag);
              pr "    flags += %d;\n" n
          ) open_flags
        | AUnusedFlags -> ()
        | ASetValues ->
          pr "  size_t nr_values;\n";
          pr "  hive_set_value *values;\n";
          pr "  values = get_values (valuesv, &nr_values);\n"
        | ASetValue ->
          pr "  hive_set_value val;\n";
          pr "  get_value (valv, &val);\n"
      ) args;
      pr "\n";

      let error_code =
        match ret with
        | RErr -> pr "  int r;\n"; "-1"
        | RErrDispose -> pr "  int r;\n"; "-1"
        | RHive -> pr "  hive_h *r;\n"; "NULL"
        | RSize -> pr "  size_t r;\n"; "0"
        | RNode -> pr "  hive_node_h r;\n"; "0"
        | RNodeNotFound ->
            pr "  errno = 0;\n";
            pr "  hive_node_h r;\n";
            "0 && errno != 0"
        | RNodeList -> pr "  hive_node_h *r;\n"; "NULL"
        | RValue -> pr "  hive_value_h r;\n"; "0"
        | RValueList -> pr "  hive_value_h *r;\n"; "NULL"
        | RString -> pr "  char *r;\n"; "NULL"
        | RStringList -> pr "  char **r;\n"; "NULL"
        | RLenType ->
            pr "  int r;\n";
            pr "  size_t len;\n";
            pr "  hive_type t;\n";
            "-1"
        | RLenValue ->
            pr "  errno = 0;\n";
            pr "  hive_value_h r;\n";
            pr "  size_t len;\n";
            "0 && errno != 0"
        | RLenTypeVal ->
            pr "  char *r;\n";
            pr "  size_t len;\n";
            pr "  hive_type t;\n";
            "NULL"
        | RInt32 ->
            pr "  errno = 0;\n";
            pr "  int32_t r;\n";
            "-1 && errno != 0"
        | RInt64 ->
            pr "  errno = 0;\n";
            pr "  int64_t r;\n";
            "-1 && errno != 0" in
      pr "\n";

      let c_params =
        List.map (function
                  | ASetValues -> ["nr_values"; "values"]
                  | ASetValue -> ["&val"]
                  | AUnusedFlags -> ["0"]
                  | arg -> [name_of_argt arg]) args in
      let c_params =
        match ret with
        | RLenType | RLenTypeVal -> c_params @ [["&t"; "&len"]]
        | RLenValue -> c_params @ [["&len"]]
        | _ -> c_params in
      let c_params = List.concat c_params in

      pr "  r = hivex_%s (%s" name (List.hd c_params);
      List.iter (pr ", %s") (List.tl c_params);
      pr ");\n";
      pr "\n";

      (* Dispose of the hive handle (even if hivex_close returns error). *)
      (match ret with
       | RErrDispose ->
           pr "  /* So we don't double-free in the finalizer. */\n";
           pr "  DATA_PTR (hv) = NULL;\n";
           pr "\n";
       | _ -> ()
      );

      List.iter (
        function
        | AHive
        | ANode _
        | AValue _
        | AString _
        | AStringNullable _
        | AOpenFlags
        | AUnusedFlags -> ()
        | ASetValues ->
          pr "  free (values);\n"
        | ASetValue -> ()
      ) args;

      (* Check for errors from C library. *)
      pr "  if (r == %s)\n" error_code;
      pr "    rb_raise (e_Error, \"%%s\", strerror (errno));\n";
      pr "\n";

      (match ret with
      | RErr | RErrDispose ->
        pr "  return Qnil;\n"
      | RHive ->
        pr "  return Data_Wrap_Struct (c_hivex, NULL, ruby_hivex_free, r);\n"
      | RSize
      | RNode
      | RValue
      | RInt64 ->
        pr "  return ULL2NUM (r);\n"
      | RInt32 ->
        pr "  return INT2NUM (r);\n"
      | RNodeNotFound ->
        pr "  if (r)\n";
        pr "    return ULL2NUM (r);\n";
        pr "  else\n";
        pr "    return Qnil;\n"
      | RNodeList
      | RValueList ->
        pr "  size_t i, len = 0;\n";
        pr "  for (i = 0; r[i] != 0; ++i) len++;\n";
        pr "  VALUE rv = rb_ary_new2 (len);\n";
        pr "  for (i = 0; r[i] != 0; ++i)\n";
        pr "    rb_ary_push (rv, ULL2NUM (r[i]));\n";
        pr "  free (r);\n";
        pr "  return rv;\n"
      | RString ->
        if f_len_exists name then (
          pr "  size_t sz = hivex_%s_len (%s);\n" name (String.concat ", " c_params);
          pr "  VALUE rv = rb_str_new (r, sz);\n";
          pr "#ifdef HAVE_RUBY_ENCODING_H\n";
          pr "  rb_enc_associate (rv, rb_utf8_encoding ());\n";
          pr "#endif\n";
        ) else
          pr "  VALUE rv = rb_str_new2 (r);\n";
        pr "  free (r);\n";
        pr "  return rv;\n"
      | RStringList ->
        pr "  size_t i, len = 0;\n";
        pr "  for (i = 0; r[i] != NULL; ++i) len++;\n";
        pr "  VALUE rv = rb_ary_new2 (len);\n";
        pr "  for (i = 0; r[i] != NULL; ++i) {\n";
        pr "    rb_ary_push (rv, rb_str_new2 (r[i]));\n";
        pr "    free (r[i]);\n";
        pr "  }\n";
        pr "  free (r);\n";
        pr "  return rv;\n"
      | RLenType ->
        pr "  VALUE rv = rb_hash_new ();\n";
        pr "  rb_hash_aset (rv, ID2SYM (rb_intern (\"len\")), INT2NUM (len));\n";
        pr "  rb_hash_aset (rv, ID2SYM (rb_intern (\"type\")), INT2NUM (t));\n";
        pr "  return rv;\n"
      | RLenValue ->
        pr "  VALUE rv = rb_hash_new ();\n";
        pr "  rb_hash_aset (rv, ID2SYM (rb_intern (\"len\")), INT2NUM (len));\n";
        pr "  rb_hash_aset (rv, ID2SYM (rb_intern (\"off\")), ULL2NUM (r));\n";
        pr "  return rv;\n"
      | RLenTypeVal ->
        pr "  VALUE rv = rb_hash_new ();\n";
        pr "  rb_hash_aset (rv, ID2SYM (rb_intern (\"len\")), INT2NUM (len));\n";
        pr "  rb_hash_aset (rv, ID2SYM (rb_intern (\"type\")), INT2NUM (t));\n";
        pr "  rb_hash_aset (rv, ID2SYM (rb_intern (\"value\")), rb_str_new (r, len));\n";
        pr "  free (r);\n";
        pr "  return rv;\n"
      );

      pr "}\n";
      pr "\n"
  ) functions;

  pr "\
/* Initialize the module. */
void Init__hivex ()
{
  m_hivex = rb_define_module (\"Hivex\");
  c_hivex = rb_define_class_under (m_hivex, \"Hivex\", rb_cObject);
  e_Error = rb_define_class_under (m_hivex, \"Error\", rb_eStandardError);

  /* XXX How to pass arguments? */
#if 0
#ifdef HAVE_RB_DEFINE_ALLOC_FUNC
  rb_define_alloc_func (c_hivex, ruby_hivex_open);
#endif
#endif

";

  (* Methods. *)
  List.iter (
    fun (name, (_, args), _, _) ->
      let args = List.filter (
        function
        | AUnusedFlags -> false
        | _ -> true
      ) args in
      let nr_args = List.length args in
      match args with
      | AHive :: _ ->
        pr "  rb_define_method (c_hivex, \"%s\",\n" name;
        pr "                    ruby_hivex_%s, %d);\n" name (nr_args-1)
      | args -> (* class function *)
        pr "  rb_define_module_function (m_hivex, \"%s\",\n" name;
        pr "                             ruby_hivex_%s, %d);\n" name nr_args
  ) functions;

  pr "}\n"

let output_to filename k =
  let filename_new = filename ^ ".new" in
  chan := open_out filename_new;
  k ();
  close_out !chan;
  chan := Stdlib.stdout;

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

  output_to "include/hivex.h" generate_c_header;
  output_to "lib/hivex.pod" generate_c_pod;

  output_to "lib/hivex.syms" generate_linker_script;

  output_to "ocaml/hivex.mli" generate_ocaml_interface;
  output_to "ocaml/hivex.ml" generate_ocaml_implementation;
  output_to "ocaml/hivex_c.c" generate_ocaml_c;

  output_to "perl/lib/Win/Hivex.pm" generate_perl_pm;
  output_to "perl/Hivex.xs" generate_perl_xs;

  (try Unix.mkdir "python/hivex" 0o755 with Unix_error _ -> ());
  output_to "python/hivex/__init__.py" generate_python_py;
  output_to "python/hivex/hive_types.py" generate_python_hive_types_py;
  output_to "python/hivex-py.c" generate_python_c;

  output_to "ruby/ext/hivex/_hivex.c" generate_ruby_c;

  (* Always generate this file last, and unconditionally.  It's used
   * by the Makefile to know when we must re-run the generator.
   *)
  let chan = open_out "generator/stamp-generator" in
  fprintf chan "1\n";
  close_out chan;

  printf "generated %d lines of code\n" !lines
