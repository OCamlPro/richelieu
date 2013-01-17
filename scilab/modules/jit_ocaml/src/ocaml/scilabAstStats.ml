open ScilabAst

module Stats = Map.Make( 
  struct
    let compare = Pervasives.compare
    type t = string
  end )

let init_stats () =
  let init_list = 
    [ ("CellExp",0);("StringExp",0);("CommentExp",0);
      ("IntExp",0);("FloatExp",0);("DoubleExp",0);("BoolExp",0);
      ("SimpleVar",0);("ColonVar",0);("DollarVar",0);("ArrayListVar",0);
      ("FieldExp",0);("IfExp",0);("TryCatchExp",0);("WhileExp",0);
      ("ForExp",0);("BreakExp",0);("ContinueExp",0);("ReturnExp",0);
      ("SelectExp",0);("CaseExp",0);("SeqExp",0);("ArrayListExp",0);
      ("AssignListExp",0);("NotExp",0);("TransposeExp",0);("VarDec",0);
      ("FunctionDec",0);("ListExp",0);("AssignExp",0);("OpExp",0);
      ("LogicalOpExp",0);("MatrixExp",0);("CallExp",0) ] in
  List.fold_left (fun st (nd,nb) -> Stats.add nd nb st) Stats.empty init_list
  
let node_visited = ref 0

let stats = ref (init_stats ())

let update_stats str =
  if Stats.mem str !stats
  then 
    let cpt = Stats.find str !stats in
    let new_stats = Stats.remove str !stats in
    incr node_visited;
    Stats.add str (cpt + 1) new_stats
  else failwith ("no [" ^ str ^ "] in the map.")

let rec analyze_ast e = match e.exp_desc with
  | AssignExp assignexp -> 
      stats := update_stats "AssignExp";
      analyze_ast assignexp.assignExp_left_exp;
      analyze_ast assignexp.assignExp_right_exp
  | CallExp callexp -> 
      stats := update_stats "CallExp";
      analyze_ast callexp.callExp_name;
      Array.iter analyze_ast callexp.callExp_args
  | CellCallExp cellcallexp -> 
      stats := update_stats "CellCallExp";
      analyze_ast cellcallexp.callExp_name;
      Array.iter analyze_ast cellcallexp.callExp_args
  | ConstExp constexp -> 
      analyze_const_exp constexp
  | ControlExp controlexp -> 
      analyze_control_exp controlexp
  | Dec dec -> 
      analyze_dec dec
  | FieldExp fieldexp -> 
      stats := update_stats "FieldExp";
      analyze_ast fieldexp.fieldExp_head;
      analyze_ast fieldexp.fieldExp_tail
  | ListExp listexp -> 
      stats := update_stats "ListExp";
      analyze_ast listexp.listExp_start;
      analyze_ast listexp.listExp_step;
      analyze_ast listexp.listExp_end
  | MathExp mathexp -> 
      analyze_mathexp mathexp
  | Var var -> 
      analyze_var var
  | SeqExp seqexp -> 
      stats := update_stats "SeqExp";
      List.iter analyze_ast seqexp
  | ArrayListExp exp_array -> 
      stats := update_stats "ArrayListExp";
      Array.iter analyze_ast exp_array
  | AssignListExp exp_array -> stats := 
      update_stats "AssignListExp";
      Array.iter analyze_ast exp_array

and analyze_const_exp e = match e with
  | BoolExp boolexp -> stats := update_stats "BoolExp"
  | CommentExp commentexp -> stats := update_stats "CommentExp"
  | DoubleExp doubleexp -> stats := update_stats "DoubleExp"
  | FloatExp floatexp -> stats := update_stats "FloatExp"
  | IntExp intexp -> stats := update_stats "IntExp"
  | NilExp -> stats := update_stats "NilExp"
  | StringExp stringexp -> stats := update_stats "StringExp"


and analyze_control_exp e = match e with
  | BreakExp -> stats := update_stats "BreakExp"
  | ContinueExp -> stats := update_stats "ContinueExp"
  | ForExp forexp -> 
      stats := update_stats "ForExp";
      analyze_var_dec forexp.forExp_vardec;
      analyze_ast forexp.forExp_body
  | IfExp ifexp -> 
      stats := update_stats "IfExp";
      analyze_ast ifexp.ifExp_test;
      analyze_ast ifexp.ifExp_then;
      begin
        match ifexp.ifExp_else with
          | Some e -> analyze_ast e
          | None -> ()
      end
  | ReturnExp returnexp -> 
      stats := update_stats "ReturnExp";
      begin
        match returnexp.returnExp_exp with
          | Some e -> analyze_ast e
          | None -> ()
      end
  | SelectExp selectexp -> 
      stats := update_stats "SelectExp";
      analyze_ast selectexp.selectExp_selectme;
      Array.iter analyze_case selectexp.selectExp_cases;
      begin 
        match selectexp.selectExp_default with
          | Some (_,e) -> List.iter analyze_ast e
          | None -> ()
      end
  | TryCatchExp trycatchexp -> 
      stats := update_stats "TryCatchExp";
      List.iter analyze_ast trycatchexp.tryCatchExp_tryme;
      List.iter analyze_ast trycatchexp.tryCatchExp_catchme
  | WhileExp whileexp -> 
      stats := update_stats "WhileExp";
      analyze_ast whileexp.whileExp_test;
      analyze_ast whileexp.whileExp_body

and analyze_var_dec e = 
  stats := update_stats "VarDec";
  analyze_ast e.varDec_init

and analyze_case e = 
  stats := update_stats "CaseExp";
  analyze_ast e.caseExp_test;
  List.iter analyze_ast e.caseExp_body

and analyze_mathexp e = match e with
  | MatrixExp matrixexp -> 
      stats := update_stats "MatrixExp";
      analyze_matrix_exp matrixexp
  | CellExp matrixexp -> 
      stats := update_stats "CellExp";
      analyze_matrix_exp matrixexp
  | NotExp notexp -> 
      stats := update_stats "NotExp";
      analyze_not_exp notexp
  | OpExp (_, args) -> 
      stats := update_stats "OpExp";
      analyze_ast args.opExp_left;
      analyze_ast args.opExp_right
  | LogicalOpExp (_, args) -> 
      stats := update_stats "LogicalOpExp";
      analyze_ast args.opExp_left;
      analyze_ast args.opExp_right
  | TransposeExp transposeexp -> 
      stats := update_stats "TransposeExp";
      analyze_ast transposeexp.transposeExp_exp

and analyze_dec d = match d with
  | FunctionDec fd -> analyze_fundec fd
  | VarDec d -> analyze_var_dec d

and analyze_fundec fd = 
  stats := update_stats "FunctionDec";
  Array.iter analyze_var fd.functionDec_args.arrayListVar_vars;
  Array.iter analyze_var fd.functionDec_returns.arrayListVar_vars;
  analyze_ast fd.functionDec_body

and analyze_matrix_exp e =
  Array.iter analyze_matrix_line_exp e.matrixExp_lines

and analyze_matrix_line_exp e =
  Array.iter analyze_ast e.matrixLineExp_columns

and analyze_not_exp e = 
  analyze_ast e.notExp_exp

and analyze_var v = match v.var_desc with
  | ColonVar -> stats := update_stats "ColonVar"
  | DollarVar -> stats := update_stats "DollarVar";
  | SimpleVar _ -> stats := update_stats "SimpleVar";
  | ArrayListVar var_array -> 
      stats := update_stats "ArrayListVar";
      Array.iter analyze_var var_array
  

let print_stats () =
  let total = ref 0. in
  Stats.iter (fun _ v -> total := !total +. (float_of_int v)) !stats;
  Printf.printf "Node visited : %.0f\n" !total;
  Stats.iter (fun k v -> 
    Printf.printf "%s : %i(%.3f%%)\n" k v (((float_of_int v)/.(!total)) *. 100.)) !stats;















