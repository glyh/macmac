open Ast

type reason = 
  | TypeError of {value: datum; expected_type: string}
  | ArityError of {expected: int; actual: int}
  | Uncallable of datum
  | UndefinedVariable of string
  | SyntaxError of string

let reason_to_string r = 
  match r with
  | TypeError {value; expected_type} -> 
    "Got" ^ (as_string value) ^ "but expecting " ^ expected_type
  | ArityError {actual; expected} -> 
    Printf.sprintf "Expected %d arity got %d" expected actual
  | Uncallable v -> 
    (as_string v) ^ " is not callable"
  | UndefinedVariable s -> 
    "Symbol `" ^ s ^ "` is undefined"
  | SyntaxError sym -> 
    "Syntax form `" ^ sym ^ "` is used inproperly" 

exception RuntimeError of reason

let as_float (v: datum) : float =
  match v with
  | Float(f) -> f
  | _ -> raise (RuntimeError(TypeError { value = v; expected_type = "float" }))

let rec evlist (forms: datum list) (env: env): (datum list * env) =
  match forms with 
  | [] -> [], env
  | x :: xs -> 
    let v_out, env_out = eval x env in
    let vs_out, env_out = evlist xs env_out in
    v_out :: vs_out, env_out
and eval (form: datum) (env: env): (datum * env) = 
  match form with
  | DSym(s) -> 
    begin match lookup env s with 
    | Some(s) -> s, env
    | None -> raise (RuntimeError(UndefinedVariable s))
    end
  | DList(what, args) -> 
    (* case for function call *)
    let fn, _ = eval what env in
    begin match fn with
    | PrimFn(f) -> 
      let args, _ = evlist args env in
      let output = f args in
      output, env
    | Fn(binds, body, closure_env) -> 
        if List.length args == List.length binds 
        then
          let args, _ = evlist args env in 
          let args_bound = List.combine binds args in
          let env_body = bind_list closure_env args_bound in
            eval body env_body
        else 
          raise (RuntimeError(ArityError { expected = List.length binds ; actual = List.length args }))
    | PrimSyntax(syn_fn) -> 
        syn_fn eval args env
    | _ -> raise (RuntimeError(Uncallable fn))
    end
  | a -> a, env
