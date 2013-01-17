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

let scilab5_modules_path = "/home/michael/scilab.5/scilab-5.4.0/modules/"

let scilab6_test_path = "/home/michael/scilab.6/scilab/test/"

let richelieu_test_path = "/home/michael/dev_sci/richelieu/"

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
        | ScilabAst.Exp exp -> 
            (* print_endline (ScilabAstPrinter.to_string exp); *)
            (* let n = !ScilabAstStats.node_visited in *)
            ScilabAstStats.analyze_ast exp;
            (* ScilabAstStats.print_stats(); *)
            (* let curr = lexbuf.Lexing.lex_curr_p in *)
            (* Printf.printf "node = %i; ligne = %i; rapport = %f" (!ScilabAstStats.node_visited - n) curr.Lexing.pos_lnum ((float (!ScilabAstStats.node_visited - n))/.(float curr.Lexing.pos_lnum)); *)
            print_endline "-> OK\n"
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
          
          (* scilab 5' tests *)
          scilab5_modules_path ^ "action_binding/tests/nonreg_tests";
          
          scilab5_modules_path ^ "api_scilab/tests/unit_tests";
          scilab5_modules_path ^ "api_scilab/tests/nonreg_tests";
          
           scilab5_modules_path ^ "arnoldi/tests/unit_tests";
           scilab5_modules_path ^ "arnoldi/tests/nonreg_tests";
           
           scilab5_modules_path ^ "atoms/tests/unit_tests";
           scilab5_modules_path ^ "atoms/tests/nonreg_tests";

           scilab5_modules_path ^ "boolean/tests/unit_tests";
           scilab5_modules_path ^ "boolean/tests/nonreg_tests";

           scilab5_modules_path ^ "cacsd/tests/unit_tests";
           scilab5_modules_path ^ "cacsd/tests/nonreg_tests";

           (* C code ?! *)
           (* scilab5_modules_path ^ "call_scilab/tests/nonreg_tests"; *)

           scilab5_modules_path ^ "compatibility_functions/tests/unit_tests";
           scilab5_modules_path ^ "compatibility_functions/tests/nonreg_tests";
           
           scilab5_modules_path ^ "completion/tests/unit_tests";
           scilab5_modules_path ^ "completion/tests/nonreg_tests";

           scilab5_modules_path ^ "console/tests/unit_tests";
           scilab5_modules_path ^ "console/tests/nonreg_tests";

           scilab5_modules_path ^ "core/tests/unit_tests";
           scilab5_modules_path ^ "core/tests/nonreg_tests";
           scilab5_modules_path ^ "core/tests/benchmarks";

           scilab5_modules_path ^ "data_structures/tests/unit_tests";
           scilab5_modules_path ^ "data_structures/tests/nonreg_tests";

           scilab5_modules_path ^ "demo_tools/tests/unit_tests";
           scilab5_modules_path ^ "demo_tools/tests/nonreg_tests";

           scilab5_modules_path ^ "development_tools/tests/unit_tests";
           scilab5_modules_path ^ "development_tools/tests/nonreg_tests";

           scilab5_modules_path ^ "differential_equations/tests/unit_tests";
           scilab5_modules_path ^ "differential_equations/tests/nonreg_tests";
           
           scilab5_modules_path ^ "dynamic_link/tests/unit_tests";
           scilab5_modules_path ^ "dynamic_link/tests/nonreg_tests";

           scilab5_modules_path ^ "elementary_functions/tests/unit_tests";
           scilab5_modules_path ^ "elementary_functions/tests/nonreg_tests";

           scilab5_modules_path ^ "fftw/tests/unit_tests";
           scilab5_modules_path ^ "fftw/tests/nonreg_tests";

           scilab5_modules_path ^ "fileio/tests/unit_tests";
           scilab5_modules_path ^ "fileio/tests/nonreg_tests";
           
           scilab5_modules_path ^ "functions/tests/unit_tests";
           scilab5_modules_path ^ "functions/tests/nonreg_tests";

           scilab5_modules_path ^ "genetic_algorithms/tests/unit_tests";
           scilab5_modules_path ^ "genetic_algorithms/tests/nonreg_tests";

           scilab5_modules_path ^ "graphic_export/tests/unit_tests";
           scilab5_modules_path ^ "graphic_export/tests/nonreg_tests";

           scilab5_modules_path ^ "graphic_objects/tests/unit_tests";
           scilab5_modules_path ^ "graphic_objects/tests/nonreg_tests";

           scilab5_modules_path ^ "graphics/tests/unit_tests";
           scilab5_modules_path ^ "graphics/tests/nonreg_tests";

           scilab5_modules_path ^ "gui/tests/unit_tests";
           scilab5_modules_path ^ "gui/tests/nonreg_tests";

           scilab5_modules_path ^ "hdf5/tests/unit_tests";
           scilab5_modules_path ^ "hdf5/tests/nonreg_tests";

           scilab5_modules_path ^ "helptools/tests/unit_tests";
           scilab5_modules_path ^ "helptools/tests/nonreg_tests";

           scilab5_modules_path ^ "history_manager/tests/unit_tests";
           scilab5_modules_path ^ "history_manager/tests/nonreg_tests";

           scilab5_modules_path ^ "integer/tests/unit_tests";
           scilab5_modules_path ^ "integer/tests/nonreg_tests";

           scilab5_modules_path ^ "interpolation/tests/unit_tests";
           scilab5_modules_path ^ "interpolation/tests/nonreg_tests";

           scilab5_modules_path ^ "io/tests/unit_tests";
           scilab5_modules_path ^ "io/tests/nonreg_tests";

           scilab5_modules_path ^ "jvm/tests/unit_tests";
           scilab5_modules_path ^ "jvm/tests/nonreg_tests";

           scilab5_modules_path ^ "linear_algebra/tests/unit_tests";
           scilab5_modules_path ^ "linear_algebra/tests/nonreg_tests";

           scilab5_modules_path ^ "localization/tests/unit_tests";
           scilab5_modules_path ^ "localization/tests/nonreg_tests";

           scilab5_modules_path ^ "m2sci/tests/unit_tests";
           scilab5_modules_path ^ "m2sci/tests/nonreg_tests";

           scilab5_modules_path ^ "matio/tests/unit_tests";
           scilab5_modules_path ^ "matio/tests/nonreg_tests";

           scilab5_modules_path ^ "optimization/tests/unit_tests";
           scilab5_modules_path ^ "optimization/tests/nonreg_tests";

           scilab5_modules_path ^ "output_stream/tests/unit_tests";
           scilab5_modules_path ^ "output_stream/tests/nonreg_tests";

           scilab5_modules_path ^ "overloading/tests/unit_tests";
           scilab5_modules_path ^ "overloading/tests/nonreg_tests";

           scilab5_modules_path ^ "parallel/tests/unit_tests";
           scilab5_modules_path ^ "parallel/tests/nonreg_tests";

           scilab5_modules_path ^ "parameters/tests/unit_tests";
           scilab5_modules_path ^ "parameters/tests/nonreg_tests";

           scilab5_modules_path ^ "polynomials/tests/unit_tests";
           scilab5_modules_path ^ "polynomials/tests/nonreg_tests";

           scilab5_modules_path ^ "randlib/tests/unit_tests";
           scilab5_modules_path ^ "randlib/tests/nonreg_tests";

           scilab5_modules_path ^ "scicos_blocks/tests/unit_tests";
           scilab5_modules_path ^ "scicos_blocks/tests/nonreg_tests";

           scilab5_modules_path ^ "scinotes/tests/unit_tests";
           scilab5_modules_path ^ "scinotes/tests/nonreg_tests";

           scilab5_modules_path ^ "signal_processing/tests/unit_tests";
           scilab5_modules_path ^ "signal_processing/tests/nonreg_tests";

           scilab5_modules_path ^ "sound/tests/unit_tests";
           scilab5_modules_path ^ "sound/tests/nonreg_tests";

           scilab5_modules_path ^ "sparse/tests/unit_tests";
           scilab5_modules_path ^ "sparse/tests/nonreg_tests";

           scilab5_modules_path ^ "special_functions/tests/unit_tests";
           scilab5_modules_path ^ "special_functions/tests/nonreg_tests";

           scilab5_modules_path ^ "spreadsheet/tests/unit_tests";
           scilab5_modules_path ^ "spreadsheet/tests/nonreg_tests";

           scilab5_modules_path ^ "statistics/tests/unit_tests";
           scilab5_modules_path ^ "statistics/tests/nonreg_tests";

           scilab5_modules_path ^ "string/tests/unit_tests";
           scilab5_modules_path ^ "string/tests/nonreg_tests";

           scilab5_modules_path ^ "tclsci/tests/unit_tests";
           scilab5_modules_path ^ "tclsci/tests/nonreg_tests";

           scilab5_modules_path ^ "time/tests/unit_tests";
           scilab5_modules_path ^ "time/tests/nonreg_tests";


           scilab5_modules_path ^ "ui_data/tests/unit_tests";
           scilab5_modules_path ^ "ui_data/tests/nonreg_tests";

           scilab5_modules_path ^ "umfpack/tests/unit_tests";
           scilab5_modules_path ^ "umfpack/tests/nonreg_tests";

           scilab5_modules_path ^ "windows_tools/tests/unit_tests";
           scilab5_modules_path ^ "windows_tools/tests/nonreg_tests";

           scilab5_modules_path ^ "xml/tests/unit_tests";
           scilab5_modules_path ^ "xml/tests/nonreg_tests";
          

         (* Scilab 6' tests *)
           scilab6_test_path ^ "good/";
           (* scilab6_test_path ^ "/control/"; *)
           (* scilab6_test_path ^ "/exec/OpExp/"; *)
           (* scilab6_test_path ^ "/goodDelayed/"; *)
           (* scilab6_test_path ^ "/syntax/"; *)
           (* scilab6_test_path ^ "/bad/"; *)
          
           (* Richelieu' tests *)
           richelieu_test_path ^ "code_samples/declarations/";
           richelieu_test_path ^ "code_samples/scoping/";
           richelieu_test_path ^ "code_samples/slow_code/";
           richelieu_test_path ^ "code_samples/trick/";

           (* "/home/michael/git_scilab/richelieu/scilab/modules/jit_ocaml/test_stats" *)
         ] in
        List.iter (run_tests) dir_tests;
        ScilabAstStats.print_stats ()
    end







