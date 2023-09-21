open Ast

type reason = 
  | TypeError of {value: value; expected_type: string}
  | ArityError of {expected: int; actual: int}
  | Uncallable of value
  | UndefinedVariable of string

exception RuntimeError of reason

let as_float (v: value) : float =
  match v with
  | Float(f) -> f
  | _ -> raise (RuntimeError(TypeError { value = v; expected_type = "float" }))

let add_vals (vs: value list) = 
  vs |> List.map as_float
  |> List.fold_left (+.) 0.0
  |> fun f -> Float f

let sub_vals (vs: value list) = 
  let fs = vs |> List.map as_float in
  match fs with
  | [only] -> Float (0.0 -. only)
  | hd :: rest ->
    Float (hd -. (List.fold_left (+.) 0.0 rest))
  | _ -> raise (RuntimeError(ArityError { expected = 2; actual = List.length vs }))

let mul_vals (vs: value list) = 
  vs |> List.map as_float
  |> List.fold_left ( *.) 1.0
  |> fun f -> Float f

let div_vals (vs: value list) = 
  let fs = vs |> List.map as_float in
  match fs with
  | [only] -> Float (1.0 /. only)
  | hd :: rest ->
    Float (hd /. (List.fold_left ( *.) 0.0 rest))
  | _ -> raise (RuntimeError(ArityError { expected = 2; actual = List.length vs }))

let repl_env = {
  bindings = Env.of_list [
    "+", PrimFn add_vals;
    "-", PrimFn sub_vals;
    "*", PrimFn mul_vals;
    "/", PrimFn div_vals;
  ];
  outer = None 
}

let rec evlist (forms: value list) (env: env): (value list * env) =
  match forms with 
  | [] -> [], env
  | x :: xs -> 
    let v_out, env_out = eval x env in
    let vs_out, env_out = evlist xs env_out in
    v_out :: vs_out, env_out
and eval (form: value) (env: env): (value * env) = 
  match form with
  | List (Sym "def!", [Sym sym; v]) ->
    let v, env_new = eval v env in
      v, bind env_new sym v
  | List (Sym "let*", List (Sym bound_sym, [bound_val]) :: inners) ->
    let v, env_eval_bound = eval bound_val env in
    let bound_env = bind env_eval_bound bound_sym v in
    let out, _ = evlist inners bound_env in
    out |> List.rev |> List.hd, env
  | Sym(s) -> 
    begin match lookup env s with 
    | Some(s) -> s, env
    | None -> raise (RuntimeError(UndefinedVariable s))
    end
  | List(Sym s, args) -> 
    let fn = begin match lookup env s with
    | Some(s) -> s
    | None -> raise (RuntimeError(UndefinedVariable s))
    end in
    begin match fn with
    | PrimFn(f) -> 
      let args, env = evlist args env in
      let output = f args in
      output, env
    | _ -> raise (RuntimeError(Uncallable fn))
    end
  | a -> a, env

let eval_multi (forms: value list) (env: env): (value * env) =
  match evlist forms env with
  | ([], env) -> (Nil, env)
  | (xs, env) -> (xs |> List.rev |> List.hd, env)
