open Ast
open Eval

let add_vals (vs: value list) = 
  vs |> List.map as_float
  |> List.fold_left (+.) 0.0
  |> fun f -> Float f

let sym_as_string (v: value) : string = 
  match v with
  | Sym(s) -> s
  | _ -> raise (RuntimeError(TypeError { value = v; expected_type = "symbol"}))

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

let core_env = {
  bindings = Env.of_list [
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
  ];
  outer = None 
}

