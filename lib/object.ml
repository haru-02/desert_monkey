module Object = struct
  include Ast

  type obj =
    | Integer of int
    | Boolean of bool
    | String of string
    | Array of obj list
    | Return of obj
    | Function of {prms: Ast.expression list; body: Ast.statement;}
    | BuiltIn of (obj list -> obj option)
    | Quote of Ast.expression
    | Macro of {prms: Ast.expression list; body: Ast.statement}
    | Empty
    | Null
    | Error of string
    
  let type_equals a b = match (a, b) with
    | (Integer _, Integer _) -> true
    | (Boolean _, Boolean _) -> true
    | (String _, String _) -> true
    | (Array _, Array _) -> true
    | (Return _, Return _) -> true
    | (Function _, Function _) -> true
    | (BuiltIn _, BuiltIn _) -> true
    | (Macro _, Macro _) -> true
    | (Empty, Empty) -> true
    | (Null, Null) -> true
    | (Error _, Error _) -> true
    | _ -> false
  
  let equal a b = match (a, b) with
    | Integer a, Integer b -> a = b
    | a, b -> a = b
  
  let rec obj_to_string = function
    | Integer i -> "Integer (" ^ string_of_int i ^ ")"
    | Boolean i -> "Boolean (" ^ string_of_bool i ^ ")"
    | String i -> "String (" ^ i ^ ")"
    | Array i -> "Array [" ^ objs_to_string i ^ "]"
    | Return i -> "Return (" ^ obj_to_string i ^ ")"
    | Function i -> "Function (prms " ^ Ast.exps_to_string i.prms ^ ", body " ^ Ast.stm_to_string i.body ^ ")"
    | BuiltIn _ -> "Builtin"
    | Quote i -> "Quote (" ^ Ast.exp_to_string i ^ ")"
    | Macro i -> "Macro (prms " ^ Ast.exps_to_string i.prms ^ ", body " ^ Ast.stm_to_string i.body ^ ")"
    | Empty -> "Empty"
    | Null -> "Null"
    | Error i -> "Error (" ^ i ^ ")"
    
    and objs_to_string = function
      | [] -> ""
      | [h] -> obj_to_string h
      | h::t -> obj_to_string h ^ ", " ^ objs_to_string t

  let pp ppf ob = Fmt.pf ppf "Object = %s" (obj_to_string ob)

end 