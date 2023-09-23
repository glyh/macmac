open Ast
open Eval

(*****************************)
(* Core Macros/Special Forms *)
(*****************************)

let list_concat (v1: value) (v2: value) = 
  match v1, v2 with
  | Nil, Nil -> Nil
  | Nil, (List _ as l) -> l
  | List _ as l, Nil -> l
  | List(a, b), List(c, d) -> List(a, b @ (c::d))
  | ((List _ | Nil), v | v, _) -> raise (RuntimeError(TypeError{value= v; expected_type= "bool"}))

let list_cons (v1: value) (v2: value) = 
  match v1, v2 with
  | hd, List(hd2, tl) -> List(hd, hd2 :: tl)
  | hd, Nil -> List(hd, [])
  | _, x -> raise (RuntimeError(TypeError{value= x; expected_type= "list|nil"}))


let definition eval vals env : (value * env) = 
  match vals with
  | [Sym sym; v] -> 
    let v, env_new = eval v env in
    v, bind env_new sym v
  | _ -> raise (RuntimeError(SyntaxError "def!"))

let let_star eval vals env = 
  match vals with
  | List (Sym bound_sym, [bound_val]) :: inners -> 
    let v, env_eval_bound = eval bound_val env in
    let bound_env = bind env_eval_bound bound_sym v in
    let out, _ = evlist inners bound_env in
    out |> List.rev |> List.hd, env
  | _ -> raise (RuntimeError(SyntaxError "let*"))

let do_seq _ vals env = 
  match vals with
  | [] -> Nil, env
  | es -> 
    let vs, env = evlist es env in 
      vs |> List.rev |> List.hd, env

let if_form eval vals env = 
  match vals with 
  | [cond; _then; _else] -> 
    begin match eval cond env with
    | Bool true, _ -> 
      let out, _ = eval _then env in 
        out, env
    | Bool false , _ ->
      let out, _ = eval _else env in 
        out, env
    | v, _ -> 
      raise (RuntimeError(TypeError{value= v; expected_type= "bool"}))
    end
  | [cond; _then] -> 
    begin match eval cond env with
    | Bool true, _ -> 
      let out, _ = eval _then env in 
        out, env
    | Bool false , _ ->
        Nil, env
    | v, _ -> 
      raise (RuntimeError(TypeError{value= v; expected_type= "bool"}))
    end
  | _ -> raise (RuntimeError(SyntaxError "if"))

let sym_as_string (v: value) : string = 
  match v with
  | Sym(s) -> s
  | _ -> raise (RuntimeError(TypeError { value = v; expected_type = "symbol"}))

let fn_star _ vals env = 
  match vals with 
  | Nil :: body -> 
    Fn([], List(Sym "do", body), env), env
  | List(arg, args) :: body -> 
    Fn((arg :: args) |> List.map sym_as_string, List(Sym "do", body), env), env
  | _ -> raise (RuntimeError(SyntaxError "fn*"))

let quote _ vals env = 
  match vals with
  | [x] -> x, env
  | _ -> raise (RuntimeError(SyntaxError "quote"))


exception Unimplemented

let quasiquote eval vals env = 
  let rec quasiquote_inner eval ast env = 
    match ast with
    | List(Sym "unquote", [v]) -> eval v env
    | List(Sym "unquote", vs) -> raise (RuntimeError(ArityError { expected = 2; actual = List.length vs }))
    | List (v, vs) -> 
      quasiquote_list_acc eval (v :: vs) env, env
    | any -> any, env
  and quasiquote_list_acc eval lst env = 
    match lst with
    | List(Sym "splice-unquote", [spliced]) :: rest -> 
      let spliced, _ = eval spliced env in
      list_concat spliced (quasiquote_list_acc eval rest env)
    | any :: rest ->
      let quasiquuoted, _ = quasiquote_inner eval any env in
      list_cons quasiquuoted (quasiquote_list_acc eval rest env)
    | [] -> Nil
  in
  match vals with
  | [x] -> (quasiquote_inner eval x env)
  | _ -> raise (RuntimeError(SyntaxError "quasiquote"))

(******************)
(* Core Functions *)
(******************)

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

let prn (vs: value list) = 
  match vs with
  | [v] -> as_string v |> print_endline; Nil
  | _ -> raise (RuntimeError(ArityError { expected = 1; actual = List.length vs }))

let make_list (vs: value list) = 
  match vs with
  | [] -> Nil
  | v :: vs -> List(v, vs)

let is_list(vs: value list) = 
  match vs with
  | [List _ | Nil] -> Bool true
  | [_] -> Bool false
  | _ -> raise (RuntimeError(ArityError { expected = 1; actual = List.length vs }))

let is_empty(vs: value list) = 
  match vs with
  | [List _] -> Bool false
  | [Nil] -> Bool true
  | [v] -> raise (RuntimeError(TypeError { value = v; expected_type = "list|nil"}))
  | _ -> raise (RuntimeError(ArityError { expected = 1; actual = List.length vs }))


let count(vs: value list) = 
  match vs with
  | [List(_, rest)] -> Float(1 + List.length rest |> float_of_int)
  | [Nil] -> Float 0.0
  | _ -> raise (RuntimeError(ArityError { expected = 1; actual = List.length vs }))

let eq(vs: value list) = 
  match vs with
  | [] -> Bool true
  | i :: rest -> 
    Bool (List.for_all (fun j -> j = i) rest)

let rel (f: value -> value -> bool) (vs: value list)  =
  let rec rel_inner (vs: value list) =
    match vs with
    | [] | [_] -> true
    | v1 :: v2 :: vs -> 
      f v1 v2 && rel_inner (v2 :: vs)
  in
  vs |> rel_inner |> fun b -> Bool b

let le (vs: value list) =
  rel (fun v1 v2 -> 
    let f1 = v1 |> as_float in
    let f2 = v2 |> as_float in
    f1 <= f2) vs

let ge (vs: value list) =
  rel (fun v1 v2 -> 
    let f1 = v1 |> as_float in
    let f2 = v2 |> as_float in
    f1 >= f2) vs

let lt (vs: value list) =
  rel (fun v1 v2 -> 
    let f1 = v1 |> as_float in
    let f2 = v2 |> as_float in
    f1 < f2) vs

let gt (vs: value list) =
  rel (fun v1 v2 -> 
    let f1 = v1 |> as_float in
    let f2 = v2 |> as_float in
    f1 > f2) vs

let cons (vs: value list) = 
  match vs with
  | [a; b] -> list_cons a b
  | _ -> raise (RuntimeError(ArityError { expected = 2; actual = List.length vs }))

let rec concat (vs: value list) = 
  match vs with
  | [] -> Nil
  | [a] -> a
  | a :: rest -> list_concat a (concat rest)

let core_env = {
  bindings = Env.of_list [
    "def!", PrimSyntax definition;
    "let*", PrimSyntax let_star;
    "do", PrimSyntax do_seq;
    "if", PrimSyntax if_form; 
    "fn*", PrimSyntax fn_star;
    "quote", PrimSyntax quote;
    "quasiquote", PrimSyntax quasiquote;

    "+", PrimFn add_vals;
    "-", PrimFn sub_vals;
    "*", PrimFn mul_vals;
    "/", PrimFn div_vals;
    "prn", PrimFn prn;
    "list", PrimFn make_list;
    "list?", PrimFn is_list;
    "count", PrimFn count;
    "=", PrimFn eq;
    "<", PrimFn lt;
    "<=", PrimFn le;
    ">=", PrimFn ge;
    ">", PrimFn gt;
    "cons", PrimFn cons;
    "concat", PrimFn concat;
  ];
  outer = None 
}

