type scope = int
type symbol = string
type uident = int

let new_scope () : scope = 
  let cnt = ref 0 in
  let ret = !cnt in
  cnt := ret + 1; 
  ret

let new_uid () : uident = 
  let cnt = ref 0 in
  let ret = !cnt in
  cnt := ret + 1; 
  ret

module ScopeOrdered = struct
  type t = scope
  let compare = compare
end

module Binding = Set.Make(ScopeOrdered)
type binding = Binding.t

type datum_t = DATUM
type syntax_object_t = SYNTAX_OBJECT

type env_key = symbol * binding

module Env = CCPersistentHashtbl.Make(struct
   type t = env_key
   let equal s t = s = t
   let hash = Hashtbl.hash
end)

type eval_t = datum -> env -> datum * env
and datum = datum_t gen_ast
and syntax_object = syntax_object_t gen_ast
and 'a gen_ast = ('a, 'a) flex_ast
and ('d, 's) flex_ast = 
  | SyntaxObject of syntax_object
  | Bool of bool
  | Float of float
  | Keyword of string
  | Str of string
  | Nil

  | DList: 'dep gen_ast * 'dep gen_ast list -> ('dep, datum_t) flex_ast
  | SList: 'dep gen_ast * 'dep gen_ast list * binding -> ('dep, syntax_object_t) flex_ast

  | SSym: symbol * binding -> ('dep, syntax_object_t) flex_ast
  | DSym: symbol -> ('dep, datum_t) flex_ast
  | UniqueSym: uident -> ('dep, datum_t) flex_ast
  | Fn: symbol list * datum * env -> ('dep, datum_t) flex_ast
  | Syntax: symbol list * datum * env -> ('dep, datum_t) flex_ast
  | PrimFn: (datum list -> datum) -> ('dep, datum_t) flex_ast
  | PrimSyntax: (eval_t -> datum list -> env -> datum * env) -> ('dep, datum_t) flex_ast

and env = {
  bindings: datum Env.t;
  outer: env option
}

let rec lookup (env: env) (sym: env_key): datum option =
  match env.outer, Env.get sym env.bindings with
  | _, Some(v) -> Some v
  | Some(outer), None -> lookup outer sym
  | None, None -> None

let is_compatible (reference: env_key) (bind: env_key) =
  let (ref_sym, ref_bind) = reference in
  let (bind_sym, bind_bind) = bind in
  ref_sym == bind_sym && Binding.subset ref_bind bind_bind
  
let rec resolve_all (env: env) (rf: env_key): env_key list =
  let cur = 
    Env.filter (fun bd _ -> is_compatible rf bd) env.bindings
    |> Env.to_list
    |> List.map (fun (k, _) -> k)
  in match env.outer with
  | None -> cur
  | Some(outer) -> cur @ (resolve_all outer rf)

let bind (env: env) (sym: env_key) (v: datum): env =
  { env with bindings = Env.add env.bindings sym v } 

let rec bind_list (env_base: env) (binds: (env_key * datum) list) : env = 
  match binds with
  | (sym, v) :: rest -> 
    bind_list (bind env_base sym v) rest
  | [] -> env_base

let rec as_string (form: datum): string = 
  match form with
  | DList(x, xs) -> "(" ^ (
    (x :: xs) |> List.map as_string |> String.concat " "
    ) ^ ")"
  | Bool(true) -> "true"
  | Bool(false) -> "false"
  | Float(f) -> string_of_float f
  | Keyword(s) -> ":" ^ s
  | Str(s) -> "\"" ^ s ^ "\"" (* not good enough, will fix *)
  | DSym(s) -> s
  | UniqueSym(id) -> "[generated " ^ (string_of_int id) ^ "]"
  | Nil -> "nil"
  | PrimFn _ -> "[primfn]"
  | PrimSyntax _ -> "[primsyntax]"
  | Fn _ -> "[function]"
  | Syntax _ -> "[syntax]"
  | SyntaxObject _ -> "[syntax object]"
