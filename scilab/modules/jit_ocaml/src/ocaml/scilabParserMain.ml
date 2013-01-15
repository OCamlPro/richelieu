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
  Printf.printf "Error at token : %s (line %i, character %i) \n\n"

let print_lex_infos =
  Printf.printf "at token : %s (line %i, character %i) \n\n"

let run_test file =
  (* let regexp = Str.regexp ".*bug.*.tst" in *)
  let ch = if file = "" then stdin else open_in file in
  Printf.printf "Testing %s : " file;
  let lexbuf = Lexing.from_channel ch in
  ScilabLexer.init_lexer_var ();
  try
    let ast = ScilabParser.program ScilabLexer.token lexbuf in
    begin
      match ast with
        | ScilabAst.Exp exp -> (* test_parser exp; *) print_endline (* (ScilabAstPrinter.to_string exp) *) "-> OK\n"
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
    | ScilabLexer.Err_str str_err ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol - 1 in
        let tok = Lexing.lexeme lexbuf in
        print_string str_err;
        print_lex_infos tok line cnum;
        close_in ch
    | ScilabLexer.Lex_err str_lex ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol - 1 in
        let tok = Lexing.lexeme lexbuf in
        print_string str_lex;
        print_lex_infos tok line cnum;
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

           "/home/michael/scilab.5/scilab-5.4.0/modules/data_structures/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/data_structures/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/demo_tools/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/demo_tools/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/development_tools/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/development_tools/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/differential_equations/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/differential_equations/tests/nonreg_tests";
           
           "/home/michael/scilab.5/scilab-5.4.0/modules/dynamic_link/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/dynamic_link/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/elementary_functions/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/elementary_functions/tests/nonreg_tests";  

           "/home/michael/scilab.5/scilab-5.4.0/modules/fftw/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/fftw/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/fileio/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/fileio/tests/nonreg_tests";
           
           "/home/michael/scilab.5/scilab-5.4.0/modules/functions/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/functions/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/genetic_algorithms/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/genetic_algorithms/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/graphic_export/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/graphic_export/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/graphic_objects/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/graphic_objects/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/graphics/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/graphics/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/gui/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/gui/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/hdf5/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/hdf5/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/helptools/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/helptools/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/history_manager/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/history_manager/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/integer/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/integer/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/interpolation/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/interpolation/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/io/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/io/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/jvm/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/jvm/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/linear_algebra/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/linear_algebra/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/localization/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/localization/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/m2sci/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/m2sci/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/matio/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/matio/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/optimization/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/optimization/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/output_stream/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/output_stream/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/overloading/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/overloading/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/parallel/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/parallel/tests/nonreg_tests";

           "/home/michael/scilab.5/scilab-5.4.0/modules/parameters/tests/unit_tests";
           "/home/michael/scilab.5/scilab-5.4.0/modules/parameters/tests/nonreg_tests";

(*            
operations/     preferences/          sound/              tclsci/
optimization/   randlib/              sparse/             threads/
output_stream/  renderer/             special_functions/  time/
overloading/    scicos/               spreadsheet/        types/
parallel/       scicos_blocks/        statistics/         ui_data/
parameters/     scinotes/             string/             umfpack/
parse/          scipad/               symbol/             windows_tools/
polynomials/    signal_processing/    symbolic/           xcos/
prebuildjava/   simulated_annealing/  system_env/         xml/
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







