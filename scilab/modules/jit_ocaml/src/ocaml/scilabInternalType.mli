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

type t = ScilabContext.t

type binop =
  Unknown_oper (* first, because our C code starts with 1 for plus *)

            (* Arithmetics *)
            (* "+" *)        | Plus
            (* "-" *)        | Minus
            (* "*" *)        | Times
            (* "/" *)        | Rdivide
            (* \  *)         | Ldivide
            (* "**" or "^" *)| Power

            (*                       Element Ways     *)
            (* ".*" *)       | Dottimes
            (* "./" *)       | Dotrdivide
            (* .\ *)         | Dotldivide (* not run. used ? *)
            (* ".^" *)       | Dotpower

            (* Kroneckers *)
            (* ".*." *)      | Krontimes
            (* "./." *)      | Kronrdivide
            (* ".\." *)      | Kronldivide

            (*                       Control *)
            (* FIXME : What the hell is this ??? *)
            (* "*." *)       | Controltimes (* not run. used ? *)
            (* "/." *)       | Controlrdivide  (* not run. used ? *)
            (* "\." *)       | Controlldivide (* not run. used ? *)

            (*                       Comparison     *)
            (* "==" *)       | Eq
           (* "<>" or "~=" *)| Ne
            (* "<" *)        | Lt
            (* "<=" *)       | Le
            (* "<" *)        | Gt
            (* ">=" *)       | Ge

            (*                       Unary minus *)
            (* "-" *)        | UnaryMinus

            (*                       Logical operators *)
            (* "&" *)   | LogicalAnd
            (* "|" *)   | LogicalOr
            (* "&&" *)  | LogicalShortCutAnd
            (* "||" *)  | LogicalShortCutOr


type realType =
(* Internal Type *)
| RealInternal
            (* Generic Types *)
| RealGeneric
| RealInt8
| RealUInt8
| RealInt16
| RealUInt16
| RealInt32
| RealUInt32
| RealInt64
| RealUInt64
| RealString
| RealDouble
| RealBool
| RealFloat
| RealPoly
| RealSinglePoly
            (* Callable *)
| RealFunction
| RealMacro
| RealMacroFile
            (* Implicit List *)
| RealImplicitList
            (* Container *)
| RealContainer
| RealList
| RealTList
| RealMList
| RealSingleStruct
| RealStruct
| RealCell
            (* User *)
| RealUserType
            (*For list operation*)
| RealListOperation (* parent type *)
| RealListInsertOperation
| RealListDeleteOperation
| RealListUndefinedOperation
| RealFile
| RealColon
| RealDollar
| RealThreadId
| RealSparse
| RealSparseBool
| RealSingleHandle
| RealHandle

| RealUnknown (* Unknown Scilab type ! *)

val string_of_realType : realType -> string
val get_type : t -> realType
val bool : bool -> t
val double : float -> t
val float : float -> t
val string : string -> t
val int8 : int32 -> t
val int16 : int32 -> t
val int32 : int32 -> t
val implicitlist : t -> t -> t -> t (* start -> step -> end -> list *)

val get_implicitlist : t -> t * t * t (* start, step, end *)

val get_double : t -> int -> float
val get_string : t -> int -> string
val get_int8 : t -> int -> int32
val get_int16 : t -> int -> int32
val get_int32 : t -> int -> int32
val get_bool : t -> int -> bool

val add_int8 : int32 -> int32 -> int32
val add_int16 : int32 -> int32 -> int32
val add_int32 : int32 -> int32 -> int32

val unsafe_set_bool : t -> int -> bool -> unit
val unsafe_set_double : t -> int -> float -> unit
val unsafe_set_int8 : t -> int -> int32 -> unit
val unsafe_set_int16 : t -> int -> int32 -> unit
val unsafe_set_int32 : t -> int -> int32 -> unit

val set_double : t -> int -> float -> unit
val set_int8 : t -> int -> int32 -> unit
val set_int16 : t -> int -> int32 -> unit
val set_int32 : t -> int -> int32 -> unit
val set_bool : t -> int -> bool -> unit


val empty_double : unit -> t

val to_string : t -> string

type ocaml_function = t array -> (string * t) array -> int -> t array option
val ocamlfunction : string -> ocaml_function -> t
val call : t -> ocaml_function
val clone : t -> t
val operation : binop -> t -> t -> t

val refcount : t -> int
val incr_refcount : t -> unit
val decr_refcount : t -> unit

val is_true : t -> bool

val get_funlist : unit -> string array
val context_get : string -> t

val dollar : unit -> t
val colon : unit -> t
val isGeneric : realType -> bool
val list_get : t -> int -> t

val iterator_of_implicitlist : t -> (unit -> t option) option
val iterator_of_list : t -> (unit -> t option) option
val iterator_of_generic : t -> (unit -> t option) option

(* does something only for ImplicitList *)
val extractFullMatrix : t -> t


(* Only on Double, Bool, SparseBool *)
(* val not_exp : t -> t *)

val generic_get_rows : t -> int
val generic_get_cols : t -> int
val generic_get_size : t -> int
val generic_get_dims : t -> int array

val unsafe_get_rows : t -> int
val unsafe_get_cols : t -> int
val unsafe_get_size : t -> int
val unsafe_get_dims : t -> int array

val addElementToVariable : t option -> t -> int -> int -> t

val overload_buildName0 : string -> string
val overload_buildName1 : string -> t -> string
val overload_buildName2 : string -> t -> t -> string
val getShortTypeStr : t -> string
val overload_getNameFromOper : binop -> string

val new_bool : int array -> t

val equal : t -> t -> bool
val nequal : t -> t -> bool

val map : t -> t
val sparsebool_get : t -> int -> int -> bool
val sparsebool_set : t -> int -> int -> bool -> unit
val unsafe_sparsebool_get : t -> int -> int -> bool
val unsafe_sparsebool_set : t -> int -> int -> bool -> unit

val parse_wstring : string -> ScilabString2Ast.wstring
