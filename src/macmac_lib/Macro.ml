(* Reference: 
   1. Binding as Sets of Scope 
   2. "Let's Build a Hygienic Macro Expander" by Matthew Flatt 
 *)

open Ast

type scope = int
type ident = string
type unique_ident = int

let new_scope () : scope = 
  let cnt = ref 0 in
  let ret = !cnt in
  cnt := ret + 1; 
  ret

let new_uid () : unique_ident = 
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

type syntax_object = 
  | SList of syntax_object * syntax_object list * binding
  | SSym of ident * binding
  | SVal of value

let rec datum_to_syntax (v: value) : syntax_object = 
  match v with
  | List(v, vs) ->
    SList(datum_to_syntax v, List.map datum_to_syntax vs, Binding.empty)
  | Sym(s) -> SSym(s, Binding.empty)
  | v -> SVal(v)

let rec syntax_to_datum (s: syntax_object) : value = 
  match s with
  | SList(v, vs, _) ->
    List(syntax_to_datum v, List.map syntax_to_datum vs)
  | SSym(s, _) -> Sym(s)
  | SVal(v) -> v

let rec adjust_scope (operation: scope -> binding -> binding) (sc: scope) (s: syntax_object): syntax_object = 
  let f = adjust_scope operation sc in
    match s with
    | SList(v, vs, bd) ->
      SList(f v, List.map f vs, operation sc bd)
    | SSym(s, bd) -> SSym(s, operation sc bd)
    | SVal(v) -> SVal(v)

let add_scope = adjust_scope Binding.add

let flip_scope = 
  let set_flip sc bind = 
    if Binding.mem sc bind then
      Binding.remove sc bind
    else 
      Binding.add sc bind in
  adjust_scope set_flip

type bkey = ident * binding
module BindEnv = CCPersistentHashtbl.Make(struct 
  type t = bkey
  let equal s t = s = t
  let hash = Hashtbl.hash
end)

type bind_env = unique_ident BindEnv.t

type reason =
  | NoMatchFor of bkey
  | TooManyResolutionFor of bkey * bkey list

exception ResolutionFailure of reason

let argmax lst f = 
  let rec argmax_helper lst f = 
  match lst with
  | [] -> []
  | x :: xs ->
    begin match argmax_helper xs f with 
    | [] -> [(x, f x)]
    | (_, f_y) :: _ as l -> 
      let f_x = f x in
        if f_x > f_y then [(x, f_x)]
        else if f_x = f_y then (x, f_x) :: l
        else l
    end
  in 
  argmax_helper lst f |>
  List.map (fun (x, _) -> x) 

let resolve (env: bind_env) (to_resolve: bkey) : unique_ident =
  let (id_x, bind_x) = to_resolve in
  let acceptable_binding =
    BindEnv.filter
      (fun (id_cur, bind_cur) _ -> id_cur == id_x && Binding.subset bind_cur bind_x)
      env
    |> BindEnv.to_list
  in 
  let resovled = argmax acceptable_binding (fun ((_, bind_x), _) -> Binding.cardinal bind_x)
  in match resovled with
  | [] -> raise (ResolutionFailure (NoMatchFor to_resolve))
  | [(_, id)] -> id
  | (many: ((bkey * unique_ident) list)) -> 
    let filtered : bkey list = List.map (fun (a, _) -> a) many in
    raise (ResolutionFailure (TooManyResolutionFor (to_resolve, filtered)))


