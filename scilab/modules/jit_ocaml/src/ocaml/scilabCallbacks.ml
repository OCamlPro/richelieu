(*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2012-2012 - OCAMLPRO INRIA - Fabrice LE FESSANT
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 *)

let debug = false
let _ =
  Printf.fprintf stderr "OCaml JIT in !\n%!"

let print_ast =
  try
    ignore (Sys.getenv "SCILAB_AST_DEBUG"); true
  with _ -> false

let exec_ast =
  try
    ignore (Sys.getenv "OCAML_EXEC_SCILAB"); true
  with _ -> false

external jit_ocaml_register_callback_ml :
  (string -> int -> int ->
   ScilabContext.t array option) -> unit = "scicaml_register_callback_c"

type action =
  ActionExec
| ActionStep
| ActionTimed
| ActionAnalyse

let _ =
  jit_ocaml_register_callback_ml
    (fun s_c action expected ->
      try
        (* 1. convert the string into an AST *)
        let ast = ScilabString2Ast.ast_of_string s_c in

        (* 2. verify that the conversion is idempotent *)
        let s2 = ScilabAst2String.string_of_ast ast in
        if debug then begin
          let s1 = ScilabString2Ast.copy_string s_c in
          ScilabString2Ast.diff_strings s1 s2;
          print_string (ScilabAstPrinter.to_string ast);
          print_newline ()
        end else
          if print_ast then begin
            print_string (ScilabAstPrinter.to_string ast);
            print_newline ()
          end;

        (* 3. execute the code *)
        (*
          Printf.fprintf stderr "Context before:\n%s%!"
          (ScilabContext.to_string ());
        *)
        try
          let t0 = Unix.gettimeofday () in
          let res = ScilabInterp.interp ast expected in
          let list = Array.to_list res in
          let list = List.map ScilabInternalType.to_string list in
          Printf.fprintf stderr "ocamlvalue= %s\n%!"
            (String.concat "," list);
          let t1 = Unix.gettimeofday () in
          Printf.fprintf stderr "timing : %.3fs\n%!" (t1 -. t0);
          Some res
        with e ->
  (* TODO: set a Scilab error in case of ... error ! *)
          Printf.fprintf stderr "ocamlvalue= exception %S\n%!"
            (Printexc.to_string e);
          None
      (*
        Printf.fprintf stderr "Context after:\n%s%!"
        (ScilabContext.to_string ());
      *)

      with e ->
        Printf.fprintf stderr "jit_ocaml_register_callback_ml: exception %S\n%!"
          (Printexc.to_string e);
        None
    )

let main () = ()
