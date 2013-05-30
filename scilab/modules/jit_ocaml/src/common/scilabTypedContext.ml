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

(* Env *)

type box = {
  mutable box_value : t;
  mutable box_refcount : int;
 }


type symbol = {
  symbol_name : string;
  mutable symbol_binding : binding option;
}

and binding = {
             binding_name : string;
     mutable binding_locals : local_binding list;
     mutable binding_global : global_binding option;
}

and local_binding = {
    mutable local_box : box;
    local_scope : scope;
}

and global_binding = {
    mutable global_box : box;
    mutable global_scopes : scope list;
}

and scope = {
  scope_level : int;
  mutable scope_locals : binding list;
  mutable scope_globals : binding list;
}

and context = {
  mutable context_create_empty_value : (unit -> t);
  mutable context_string_of_value : (t -> string);
  mutable context_increase_refcount : (t -> t);
  mutable context_decrease_refcount : (t -> unit);

  mutable context_scopes : scope list; (* never [] *)
}


let new_symbol name = {
  symbol_name = name;
  symbol_binding = None;
}


let symbol_name sy = sy.symbol_name


let increase_box_refcount ctx box =
  box.box_refcount <- box.box_refcount + 1

let decrease_box_refcount ctx box =
  box.box_refcount <- box.box_refcount - 1;
  if box.box_refcount = 0 then
    ctx.context_decrease_refcount box.box_value

exception ErrorUndefinedVariable of string
exception CannotResumeFromToplevelScope

let new_scope level = {
  scope_level = level;
  scope_locals = [];
  scope_globals = [];
}

let new_binding name = {
  binding_name = name;
  binding_locals = [];
  binding_global = None;
}

let symbols = Hashtbl.create 133
let context = {
  context_scopes = [new_scope 0];
  context_create_empty_value = (fun _ ->
    failwith "context_create_empty_value not initialized");
  context_increase_refcount = (fun v -> v);
  context_decrease_refcount = (fun _ -> ());
  context_string_of_value = (fun _ -> "<abstract>");
}

let getInstance () = context

let current_scope ctx =
    match ctx.context_scopes with
      [] -> assert false
    | scope :: _ -> scope


let begin_scope ctx =
  match ctx.context_scopes with
  [] -> assert false
  | s :: _ ->
    ctx.context_scopes <- (new_scope (s.scope_level+1)) :: ctx.context_scopes

let end_scope ctx =
  match ctx.context_scopes with
    [] -> assert false
  | scope :: scopes ->
     ctx.context_scopes <- scopes;
     List.iter (fun b ->
       match b.binding_locals with
        | [] -> assert false
        | { local_box = box; local_scope = s } :: locals ->
          assert (s == scope);
          decrease_box_refcount ctx box;
          b.binding_locals <- locals
     ) scope.scope_locals;
     List.iter (fun b ->
       match b.binding_global with
        | None -> assert false
        | Some g ->
	  match g.global_scopes with
	  | [] -> assert false
          | s :: globals ->
            assert (s == scope);
            g.global_scopes <- globals
     ) scope.scope_globals

let rec restore_scope context s0 =
  match context.context_scopes with
    [] -> assert false
  | scope :: scopes ->
    if scope != s0 then begin
      end_scope context;
      restore_scope context s0
    end

let get_symbol_binding sy =
  match sy.symbol_binding with
  | Some b -> b
  | None ->
    let b = try
              Hashtbl.find symbols sy.symbol_name
      with Not_found ->
        let b = new_binding sy.symbol_name in
        Hashtbl.add symbols sy.symbol_name b;
        b
    in
    sy.symbol_binding <- Some b;
    b

let get ctx sy =
  let s0 = current_scope ctx in
  let b = get_symbol_binding sy in

  match b.binding_global with
    | Some { global_scopes = ss :: _; global_box = box }
                             when ss == s0
        -> box.box_value
    | _ ->
      match b.binding_locals with
       | l0 :: _ -> l0.local_box.box_value
       | _ -> raise (ErrorUndefinedVariable sy.symbol_name)

let new_local s box =
  { local_box = box; local_scope = s }

let set_box_value ctx box v =
  if box.box_value != v then begin
    ctx.context_decrease_refcount box.box_value;
    let v = ctx.context_increase_refcount v in
    box.box_value <- v
  end

let new_box ctx v =
  let v = ctx.context_increase_refcount v in
  {
    box_value = v;
    box_refcount = 1;
  }

let put ctx sy v =
  let s0 = current_scope ctx in
  let b = get_symbol_binding sy in
  match b.binding_global with
    | Some { global_scopes = ss :: _; global_box = box }
                             when ss == s0
        -> set_box_value ctx box v
    | _ ->
      match b.binding_locals with
       | l0 :: _ when
           l0.local_scope == s0 -> set_box_value ctx l0.local_box v
       | locals ->
           let l0 = new_local s0 (new_box ctx v) in
           b.binding_locals <- l0 :: locals;
           s0.scope_locals <- b :: s0.scope_locals

(* remove all local scopes, and keep only the global one *)
let rec clear_all_local_scopes ctx =
  match ctx.context_scopes with
    [ _ ] -> ()
  | [] -> assert false
  | s :: _ -> end_scope ctx; clear_all_local_scopes ctx

let to_string () =

  let buf = Buffer.create 1000 in
  List.iter (fun s ->
    Printf.bprintf buf "Scope %d:\n" s.scope_level;
    List.iter (fun b ->
      Printf.bprintf buf "\t%s\n" b.binding_name
    ) s.scope_locals;
    List.iter (fun b ->
      Printf.bprintf buf "\t%s\n" b.binding_name
    ) s.scope_globals;

  ) context.context_scopes;

  Hashtbl.iter (fun _ b ->
    Printf.bprintf buf "Symbol %s:\n" b.binding_name;
    List.iter (fun l ->
      Printf.bprintf buf "\t%d -> %s\n" l.local_scope.scope_level
        (context.context_string_of_value l.local_box.box_value)
    ) b.binding_locals
  ) symbols;

  Buffer.contents buf

