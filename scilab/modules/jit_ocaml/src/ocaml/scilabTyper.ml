open ScilabAst

(* Types *)

type base_type =
  | Type_bool
  | Type_real
  | Type_complex

type t =
  | Type_base of base_type
  | Type_matrix of t
  | Type_string
  | Type_polynome
  | Type_null
  | Type_unknow

(* Scilab primitives function *)

let string_of_base_type = function
  | Type_bool -> "Type_bool"
  | Type_real -> "Type_real"
  | Type_complex -> "Type_complex"

let rec string_of_t = function
  | Type_base bt -> string_of_base_type bt
  | Type_matrix t -> "[ " ^ string_of_t t ^ " ]"
  | Type_string -> "Type_string"
  | Type_polynome -> "Type_polynome"
  | Type_null -> "Type_null"
  | Type_unknow -> "Type_unknow"

let string_of_opExp_Oper = function
  | OpExp_plus -> " OpExp_plus "
  | OpExp_minus -> " OpExp_minus "
  | OpExp_times -> " OpExp_times "
  | OpExp_rdivide -> " OpExp_rdivide "
  | OpExp_ldivide -> " OpExp_ldivide "
  | OpExp_power -> " OpExp_power "
  | OpExp_unaryMinus -> " OpExp_unaryMinus "
  | OpExp_dottimes -> " OpExp_dottimes "
  | OpExp_dotrdivide -> " OpExp_dotrdivide "
  | OpExp_dotldivide -> " OpExp_dotldivide "
  | OpExp_dotpower -> " OpExp_dotpower "
  | OpExp_krontimes -> " OpExp_krontimes "
  | OpExp_kronrdivide -> " OpExp_kronrdivide "
  | OpExp_kronldivide -> " OpExp_kronldivide "
  | OpExp_controltimes -> " OpExp_controltimes "
  | OpExp_controlrdivide -> " OpExp_controlrdivide "
  | OpExp_controlldivide -> " OpExp_controlldivide "
  | OpExp_eq -> " OpExp_eq "
  | OpExp_ne -> " OpExp_ne "
  | OpExp_lt -> " OpExp_lt "
  | OpExp_le -> " OpExp_le "
  | OpExp_gt -> " OpExp_gt "
  | OpExp_ge -> " OpExp_ge "

let rec get_stronger_type t1 t2 = match (t1, t2) with
  | Type_base bt1, Type_base bt2  -> 
      begin 
        match bt1, bt2 with
          | Type_bool, _ -> Type_base bt2
          | Type_real, Type_bool -> Type_base bt1
          | Type_real, Type_real -> Type_base bt1
          | Type_real, Type_complex -> Type_base bt2
          | Type_complex, _ -> Type_base bt1
      end
  | Type_matrix mt1, Type_matrix mt2 -> Type_matrix (get_stronger_type mt1 mt2)
  | Type_string, Type_string -> Type_string
  | _, _ -> failwith ("Matrix' elements have different types : " ^ string_of_t t1 ^ " and " ^ string_of_t t2)

let rec type_arithmetic_op str_op t1 t2 = match (t1, t2) with
  | Type_base bt1, Type_base bt2  -> 
      begin 
        match bt1, bt2 with
          | Type_bool, _ -> Type_base bt2
          | Type_real, Type_bool -> Type_base bt1
          | Type_real, Type_real -> Type_base bt1
          | Type_real, Type_complex -> Type_base bt2
          | Type_complex, _ -> Type_base bt1
      end
  | Type_matrix mt1, Type_base bt2 -> type_arithmetic_op str_op mt1 t2
  | Type_base bt1, Type_matrix mt2 -> type_arithmetic_op str_op t1 mt2
  | Type_matrix mt1, Type_matrix mt2 -> type_arithmetic_op str_op mt1 mt2
  | Type_string, Type_string -> Type_string
  | _, _ -> failwith ("Can't type " ^ string_of_t t1 ^ str_op ^ string_of_t t2)

(* Environment *)

let print_env env = 
  print_endline "== ENV ==";
  List.iter (fun (s,t) -> print_endline (s ^ " : " ^ string_of_t t)) env;
  print_endline "=========\n"

let init_env () =
  [("%i", Type_base Type_complex);]

let add_in_env s typ env =
  try
    let nenv = List.remove_assoc s env in
    (s,typ)::nenv
  with Not_found -> (s,typ)::env

let find_in_env s env =
  try
    List.assoc s env
  with Not_found -> failwith ("No " ^ s ^ " in env")

let get_assign_ident e = match e.exp_desc with
  | Var var -> 
      begin
        match var.var_desc with
          | ColonVar  -> failwith "Can't extract ident from COLONVAR"
          | DollarVar -> failwith "Can't extract ident from DOLLARVAR"
          | SimpleVar symbol -> ScilabContext.symbol_name symbol
          | ArrayListVar arr -> failwith "Can't extract ident from ARRAYLISTVAR"
      end
  | FieldExp _ -> failwith "Can't extract ident from FielExp"
  | _ -> failwith "Can't extract ident from this"

(* Scilab AST typer *)

exception Not_null

let rec type_exp env e = match e.exp_desc with
  | SeqExp list -> 
      List.fold_left (fun (typ,env) e -> type_exp env e) (Type_null, env) list
  | ConstExp exp -> type_const env exp
  | CallExp exp -> type_call env exp
  | CellCallExp exp -> type_cell_call env exp
  | AssignExp { assignExp_left_exp; assignExp_right_exp } ->
      let (typ, new_env) = type_exp env assignExp_right_exp in
      let ident = get_assign_ident assignExp_left_exp in
      (Type_null, add_in_env ident typ new_env)
  | ControlExp controlExp -> type_cntrl env controlExp
  | Dec dec -> 
      (* Update env with new dec *)
      (Type_null, env)
  | FieldExp { fieldExp_head; fieldExp_tail } -> (Type_null, env)
  | ListExp { listExp_start; listExp_step; listExp_end } -> (Type_null, env)
  | Var var -> type_var env var
  | ArrayListExp array -> (Type_null, env)
  | AssignListExp array -> (Type_null, env)
  | MathExp mathExp -> type_math env mathExp

and type_const env = function
  | BoolExp boolExp -> (Type_base Type_bool, env)
  | CommentExp commentExp -> (Type_null, env)
  | DoubleExp doubleExp -> (Type_base Type_real, env)
  | FloatExp floatExp -> (Type_base Type_real, env)
  | IntExp intExp -> (Type_base Type_real, env)
  | NilExp -> (Type_null, env)
  | StringExp stringExp -> (Type_string, env)

and type_call env e = (Type_null, env)

and type_cell_call env e = (Type_null, env)

and type_cntrl env = function
  | BreakExp -> (Type_null, env)
  | ContinueExp -> (Type_null, env)
  | ForExp forExp -> (Type_null, env)
  | IfExp ifExp -> (Type_null, env)
  | ReturnExp returnExp -> (Type_null, env)
  | SelectExp selectExp -> (Type_null, env)
  | TryCatchExp tryCatchExp -> (Type_null, env)
  | WhileExp  whileExp -> (Type_null, env)

and type_var env v = match v.var_desc with
  | ColonVar  -> failwith "COLONVAR"
  | DollarVar -> failwith "DOLLARVAR"
  | SimpleVar symbol -> (find_in_env (ScilabContext.symbol_name symbol) env, env)
  | ArrayListVar arr -> (Type_null, env)

and type_math env = function
  | MatrixExp matrixExp -> (Type_matrix (type_matrixExp env matrixExp), env)
  | CellExp matrixExp -> (Type_null, env)
  | NotExp notExp -> (Type_null, env)
  | OpExp (opExp_Oper, opExp_args) -> type_opexp env opExp_Oper opExp_args
  | LogicalOpExp (opLogicalExp_Oper, opExp_args) -> (Type_null, env)
  | TransposeExp transposeExp -> (Type_null, env)

(* elemts in matrix can change env !! *)
and type_matrixExp env me = 
  let arr_type = 
    Array.map (fun line -> 
      Array.map (fun e ->
        let (t, _) = type_exp env e in
        match t with
          | Type_matrix ty -> ty
          | _ -> t
      ) line.matrixLineExp_columns
    ) me.matrixExp_lines in
  let fst_type = Array.get (Array.get arr_type 0) 0 in
  Array.fold_left (fun typ l ->
    Array.fold_left (fun typ_ref typ_elm ->
      get_stronger_type typ_ref typ_elm
    ) typ l
  ) fst_type arr_type
  

and type_opexp env op args = match op with
  | OpExp_plus | OpExp_minus | OpExp_times 
  | OpExp_rdivide | OpExp_ldivide | OpExp_power ->
      let (t_left, nenv) = type_exp env args.opExp_left in
      let (t_right, new_env) = type_exp nenv args.opExp_right in
      print_env new_env;
      (type_arithmetic_op (string_of_opExp_Oper op) t_left t_right, new_env)
  | _ -> failwith "OpExp TODO"

let type_ast e =
  let env = init_env () in
  let typ, env = type_exp env e in
  print_endline (string_of_t typ)

















  
