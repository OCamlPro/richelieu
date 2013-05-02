(*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2012-2013 - OCAMLPRO INRIA - Fabrice LE FESSANT
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 *)

open ScilabSymbol

exception ErrorUndefinedVariable of string
exception CannotResumeFromToplevelScope

val clear_all_local_scopes : context -> unit
val getInstance : unit -> context
val put : context -> symbol -> t -> unit
val get : context -> symbol -> t
val global : context -> symbol -> unit
val resume : context -> symbol -> t -> unit
val begin_scope : context -> unit
val end_scope : context -> unit
val current_scope : context -> scope
val restore_scope : context -> scope -> unit

val to_string : unit -> string

val set_context_create_empty_value : context -> (unit -> t) -> unit
val set_context_string_of_value : context -> (t -> string) -> unit
val set_context_refcount_setters :
  context -> (t -> t) -> (t -> unit) -> unit

