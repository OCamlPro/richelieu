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

let verbose =
  try
    ignore (Sys.getenv "OCAML_JIT_VERBOSE"); true
  with _ -> false

let exec_ast =
  try
    ignore (Sys.getenv "OCAML_EXEC_SCILAB"); true
  with _ -> false

type action =
  ActionExec
| ActionStep
| ActionTimed
| ActionAnalyse

type return =
  ReturnNotExec (* not executed *)
| ReturnError of string
| ReturnValues of ScilabContext.t array

external jit_ocaml_register_callback_ml :
  (ScilabString2Ast.wstring -> action -> int -> return) -> unit =
  "scicaml_register_callback_c"

let oc = ref None
let oc() =
  match !oc with
  | Some oc -> oc
  | None ->
    let occ = open_out "log.sci" in
    oc := Some occ;
    occ

let _ =
  jit_ocaml_register_callback_ml
    (fun s_c action expected ->
      try
        (* 1. convert the string into an AST *)
        let ast = ScilabString2Ast.ast_of_wstring s_c in

        (* 2. verify that the conversion is idempotent *)
        let s2 = ScilabAst2String.string_of_ast ast in
        if debug then begin
          let s1 = ScilabString2Ast.string_of_wstring s_c in
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
        if exec_ast then
          try
            Printf.fprintf (oc()) "%s\n%!"
              (ScilabAstPrinter.to_string ast);
            let res = ScilabInterp.interp ast expected in
            ReturnValues res
          with
          | ScilabInterp.FallbackToCPP ->
            Printf.fprintf stderr "jit_ocaml: fallback to CPP\n%!";
            ReturnNotExec
          | e ->
            let s = Printexc.to_string e in
          (* TODO: set a Scilab error in case of ... error ! *)
            Printf.fprintf stderr "ocamlvalue= exception %S\n%!" s;
            Printf.fprintf (oc()) "ERROR: exception %S\n%!" s;
            ReturnError (Printf.sprintf
                           "jit_ocaml - exception during exec: %s" s)
      (*
        Printf.fprintf stderr "Context after:\n%s%!"
        (ScilabContext.to_string ());
      *)
        else begin
          if verbose then
            Printf.fprintf stderr "jit_ocaml: don't exec\n%!";
          ReturnNotExec
        end
      with e ->
        let s = Printexc.to_string e in
        Printf.fprintf stderr "jit_ocaml_register_callback_ml: exception %S\n%!"
          s;
        ReturnError (Printf.sprintf
                       "jit_ocaml - exception before exec: %s" s)
    )

let main () = ()
