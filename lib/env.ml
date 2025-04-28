module Env = struct

  include Object
  include Ast

  type env = {
    store: (string, Object.obj) Hashtbl.t;
    outer: env option;
  }

  let new_env () = {
    store = Hashtbl.create 100;
    outer = None;
  }

  let get_env = {
    store = Hashtbl.create 100;
    outer = None;
  }

  let new_enclosed_env env = {
    store = Hashtbl.create 100;
    outer = Some env;
  }

  let rec get env key = if Hashtbl.mem env.store key
    then Some (Hashtbl.find env.store key)
    else (match env.outer with
      | Some env -> get env key
      | None -> None)

  let set env key value = Hashtbl.add env.store key value; value

  let extend_function_env prms args env = let nenv = new_enclosed_env env
    in let rec refe pms ags = match pms, ags with
      | [], [] -> ()
      | (Ast.Identifier idt)::i, obj::o -> let _ = set nenv idt obj in refe i o
      | _, _ -> raise (Failure "extend env failed")
    in refe prms args; nenv

  let extend_macro_env prms args env = let nenv = new_enclosed_env env
    in let rec refe pms ags = match pms, ags with
      | [], [] -> ()
      | (Ast.Identifier idt)::i, obj::o -> let _ = set nenv idt (Object.Quote obj) in refe i o
      | _, _ -> raise (Failure "extend env failed")
    in refe prms args; nenv

  let rec dump_env env =
    Hashtbl.iter (fun k v -> Printf.printf "%s = %s\n" k (Object.obj_to_string v)) env.store;
    match env.outer with
    | Some o -> print_endline "---- Outer ----"; dump_env o
    | None -> ()
  (*This is just something simple to dump the env for debugging*)
end