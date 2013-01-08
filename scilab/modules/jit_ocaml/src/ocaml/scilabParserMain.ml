let file = ref ""
let test_flag = ref false
let args = [("-t", Arg.Unit (fun () -> test_flag := true), ": run tests")]
let usage = "Usage: ./main -t [fichier]  (stdin par default)"

let print_exn_infos =
  Printf.printf " -> Exception at token : %s (line %i, character %i) \n\n"

let run_test file =
  let ch = if file = "" then stdin else open_in file in
  Printf.printf "Testing %s :" file;
  let lexbuf = Lexing.from_channel ch in
  ScilabLexer.init_lexer_var ();
  try
    let ast = ScilabParser.program ScilabLexer.token lexbuf in
    begin
      match ast with
        | ScilabAst.Exp exp -> print_endline (* (ScilabAstPrinter.to_string exp) *) " -> OK\n"
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
    | _ as err -> raise err 

let run_tests dirname =
  let files = Sys.readdir dirname in
  Printf.printf "# tests to run in %s : %i\n\n" dirname (Array.length files);
  Array.iter (fun file ->
    let file = Filename.concat dirname file in
    if Filename.check_suffix file ".sci" || Filename.check_suffix file ".sce" then
      run_test file
  ) files

let _ =
  Arg.parse args (fun s ->  run_test s) usage;
  if !test_flag
  then
    begin
      let dir_tests =
        ["test/";
         (* "../../test/control/"; *)
         (* "../../test/exec/OpExp/"; *)
         (* "../../test/goodDelayed/"; *)
         (* "../../test/syntax/"; *)
         "../../test/bad/";
         (* "/home/michael/dev_sci/richelieu/code_samples/declarations/"; *)
         (* "/home/michael/dev_sci/richelieu/code_samples/scoping/"; *)
         (* "/home/michael/dev_sci/richelieu/code_samples/slow_code/"; *)
         (* "/home/michael/dev_sci/richelieu/code_samples/trick/"; *)
         (* "../../test/good/"; *)] in
      List.iter (run_tests) dir_tests
    end







