open Ast

type reason = 
  | TypeError of {value: value; expected_type: string}
  | ArityError of {expected: int; actual: int}
  | Uncallable of value
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

let as_float (v: value) : float =
  match v with
  | Float(f) -> f
  | _ -> raise (RuntimeError(TypeError { value = v; expected_type = "float" }))


let rec evlist (forms: value list) (env: env): (value list * env) =
  match forms with 
  | [] -> [], env
  | x :: xs -> 
    let v_out, env_out = eval x env in
    let vs_out, env_out = evlist xs env_out in
    v_out :: vs_out, env_out
and eval (form: value) (env: env): (value * env) = 
  match form with
  | Sym(s) -> 
    begin match lookup env s with 
    | Some(s) -> s, env
    | None -> raise (RuntimeError(UndefinedVariable s))
    end
  | List(what, args) -> 
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
