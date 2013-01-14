let file = ref ""
let test_flag = ref false
let args = [("-t", Arg.Unit (fun () -> test_flag := true), ": run tests")]
let usage = "Usage: ./main -t [fichier]  (stdin par default)"

(* let test_parser ast = *)
(*   (\* ast -> binary format *\) *)
(*   let s = ScilabAst2String.string_of_ast ast in *)
(*   print_endline s *)
(*   (\* read binary file generates by scilab's parser *\) *)

let list_ext = [".sci"; ".sce"; ".tst"; (* ".dia.ref" *)]

let print_exn_infos =
  Printf.printf " -> Exception at token : %s (line %i, character %i) \n\n"

let run_test file =
  (* let regexp = Str.regexp ".*bug.*.tst" in *)
  let ch = if file = "" then stdin else open_in file in
  Printf.printf "Testing %s :" file;
  let lexbuf = Lexing.from_channel ch in
  ScilabLexer.init_lexer_var ();
  try
    let ast = ScilabParser.program ScilabLexer.token lexbuf in
    begin
      match ast with
        | ScilabAst.Exp exp -> (* test_parser exp; *) print_endline (* (ScilabAstPrinter.to_string exp) *) " -> OK\n"
        | _ -> ()
    end;
    flush stdout;
    close_in ch
  with Parsing.Parse_error ->
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol - 1 in
    let tok = Lexing.lexeme lexbuf in
    print_exn_infos tok line cnum;
    close_in ch
    | ScilabLexer.Heterous_str ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol - 1 in
        let tok = Lexing.lexeme lexbuf in
        print_endline !ScilabLexer.str_err;
        print_exn_infos tok line cnum;
        close_in ch
    | _ as err -> raise err 

let run_tests dirname =
  let files = Sys.readdir dirname in
  Printf.printf "# tests to run in %s : %i\n\n" dirname (Array.length files);
  Array.iter (fun file ->
    let file = Filename.concat dirname file in
    if List.exists (Filename.check_suffix file) list_ext then
      run_test file
  ) files

let _ =
  Arg.parse args (fun s ->  run_test s) usage;
  if !test_flag
  then
    begin
      let dir_tests =
        [ (* My tests *)
          "test/";
         
         (* Scilab 5' tests *)
          "/home/michael/scilab.5/scilab-5.4.0/modules/action_binding/tests/nonreg_tests";
          
          "/home/michael/scilab.5/scilab-5.4.0/modules/api_scilab/tests/unit_tests";
          "/home/michael/scilab.5/scilab-5.4.0/modules/api_scilab/tests/nonreg_tests";
          
           "/home/michael/scilab.5/scilab-5.4.0/modules/arnoldi/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/arnoldi/tests/nonreg_tests";
           
           "/home/michael/scilab.5/scilab-5.4.0/modules/atoms/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/atoms/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/boolean/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/boolean/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/cacsd/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/cacsd/tests/nonreg_tests";

           (* C code ?! *)
           (* "/home/michael/scilab.5/scilab-5.4.0/modules/call_scilab/tests/nonreg_tests"; *)

           "/home/michael/scilab.5/scilab-5.4.0/modules/compatibility_functions/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/compatibility_functions/tests/nonreg_tests";
           
           "/home/michael/scilab.5/scilab-5.4.0/modules/completion/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/completion/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/console/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/console/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/core/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/core/tests/nonreg_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/core/tests/benchmarks";

           
(*            
compatibility_functions/  elementary_functions/  graphic_objects/  io/               m2sci/            operations/     preferences/          sound/              tclsci/
completion/               external_objects/      graphics/         javasci/          Makefile          optimization/   randlib/              sparse/             threads/
console/                  fftw/                  gui/              jit_ocaml/        Makefile.am       output_stream/  renderer/             special_functions/  time/
core/                     fileio/                hdf5/             jvm/              Makefile.in       overloading/    scicos/               spreadsheet/        types/
data_structures/          functions/             helptools/        libscilab-cli/    matio/            parallel/       scicos_blocks/        statistics/         ui_data/
demo_tools/               functions_manager/     history_browser/  libscilab-cli.la  memory_manager/   parameters/     scinotes/             string/             umfpack/
development_tools/        genetic_algorithms/    history_manager/  libscilab.la      metanet/          parse/          scipad/               symbol/             windows_tools/
differential_equations/   graph/                 integer/          linear_algebra/   mexlib/           polynomials/    signal_processing/    symbolic/           xcos/
dynamic_link/             graphic_export/        interpolation/    localization/     modules_manager/  prebuildjava/   simulated_annealing/  system_env/         xml/
*)
          

         (* Scilab 6' tests *)
         (* "../../test/good/"; *)
         (* "../../test/control/"; *)
         (* "../../test/exec/OpExp/"; *)
         (* "../../test/goodDelayed/"; *)
         (* "../../test/syntax/"; *)
         (* "../../test/bad/"; *)
          
         (* Richelieu' tests *)
         "/home/michael/dev_sci/richelieu/code_samples/declarations/";
         "/home/michael/dev_sci/richelieu/code_samples/scoping/";
         "/home/michael/dev_sci/richelieu/code_samples/slow_code/";
         "/home/michael/dev_sci/richelieu/code_samples/trick/";
         ] in
      List.iter (run_tests) dir_tests
    end







