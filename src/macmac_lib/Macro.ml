(* Reference: 
   1. Binding as Sets of Scope 
   2. "Let's Build a Hygienic Macro Expander" by Matthew Flatt 
 *)

open Ast
open Core

module UidOrdered = struct 
  type t = uident
  let compare = compare
end

(* a set containing valid uids in current scope *)
module BindSet = Set.Make(UidOrdered)
type bind_set = BindSet.t

type reason =
  | CantConvertToSyntaxObj of datum
  | NoMatchFor of env_key
  | TooManyResolutionFor of env_key * env_key list
  | BindingOutOfScope of env_key * bind_set
  | FreeVariable of symbol

exception MacroFailure of reason

let rec datum_to_syntax (d: datum): syntax_object = 
  match d with 
  | DSym(name) -> 
    SSym(name, Binding.empty) 
  | DList(hd, rst) -> 
    SList(datum_to_syntax hd, List.map datum_to_syntax rst, Binding.empty)
  | Syntax _ | Fn _ | PrimFn _ | PrimSyntax _ | UniqueSym _ -> 
    raise (MacroFailure (CantConvertToSyntaxObj d))
  | Bool _ | Float _ | Keyword _ | Str _ | Nil as d -> d
  | SyntaxObject o -> o

let rec syntax_to_datum (s: syntax_object): datum = 
  match s with 
  | SSym(name, _) -> DSym(name)
  | SList(hd, rst, _) -> 
    DList(syntax_to_datum hd, List.map syntax_to_datum rst)
  | Bool _ | Float _ | Keyword _ | Str _ | Nil as s -> s
  | SyntaxObject o -> SyntaxObject o

let rec adjust_scope (operation: scope -> binding -> binding) (sc: scope) (s: syntax_object): syntax_object = 
  let f = adjust_scope operation sc in
    match s with 
    | Bool _ | Float _ | Keyword _ | Str _ | Nil as s -> s
    | SyntaxObject o -> adjust_scope operation sc o
    | SSym(s, bd) -> SSym(s, operation sc bd)
    | SList(v, vs, bd) -> SList(f v, List.map f vs, operation sc bd)

let add_scope = adjust_scope Binding.add

let flip_scope = 
  let set_flip sc bind = 
    if Binding.mem sc bind then
      Binding.remove sc bind
    else 
      Binding.add sc bind in
  adjust_scope set_flip

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

(* let resolve (env: env) (to_resolve: env_key) : uident = *)
(*   let (id_x, bind_x) = to_resolve in *)
(*   let acceptable_binding = *)
(*     BindEnv.filter *)
(*       (fun (id_cur, bind_cur) _ -> id_cur == id_x && Binding.subset bind_cur bind_x) *)
(*       env *)
(*     |> BindEnv.to_list *)
(*   in  *)
(*   let resolved = argmax acceptable_binding (fun ((_, bind_x), _) -> Binding.cardinal bind_x) *)
(*   in match resolved with *)
(*   | [] -> raise (MacroFailure (NoMatchFor to_resolve)) *)
(*   | [(_, id)] -> id *)
(*   | (many: ((env_key * uident) list)) ->  *)
(*     let filtered : env_key list = List.map (fun (a, _) -> a) many in *)
(*     raise (MacroFailure (TooManyResolutionFor (to_resolve, filtered))) *)
(**)
(* let core_binding_env =  *)
(*   let core_scope = new_scope () in *)
(*   let binding_to_scope_bind p =  *)
(*     let (sym, _) = p in  *)
(*     ((sym, Binding.singleton core_scope), new_uid ()) *)
(*   in  *)
(*   List.map binding_to_scope_bind binding_list *)
(*   |> BindEnv.of_list *)
(**)
(* let global_binding_env = ref core_binding_env *)
(* let core_bind_set =  *)
(*   core_binding_env  *)
(*   |> BindEnv.to_list  *)
(*   |> List.map (fun (_, v) -> v)  *)
(*   |> BindSet.of_list *)
(**)
(* (* register a binding in the global scope for the later query *) *)
(* let register_binding (key: env_key) (id: uident) = *)
(*   let new_env = BindEnv.add !global_binding_env key id in *)
(*     global_binding_env := new_env *)
(**)
(* let rec expand (env: bind_set) (s: syntax_object) = *)
(*   match s with *)
(*   | SSym(id, binding) -> expand_id env (id, binding) *)
(*   | SList(SSym(id, binding), rst, bd) -> expand_id_app env (id, binding) rst bd *)
(*   | SList(_, _, _) -> expand_app s env *)
(*   (* | SVal(v) -> v   *) *)
(*   | Bool _ | Float _ | Keyword _ | Str _ | Nil as s -> s *)
(*   | SyntaxObject o -> expand env o *)
(* and expand_id (env: bind_set) (bid: env_key) =  *)
(*   let id, binding = bid in *)
(*   try  *)
(*     let resolved = resolve !global_binding_env bid in *)
(*       if BindSet.mem resolved env then  *)
(*         SSym(id, binding) *)
(*       else *)
(*         raise (MacroFailure (BindingOutOfScope(bid, env))) *)
(*   with MacroFailure(NoMatchFor _) ->  *)
(*       raise (MacroFailure (FreeVariable(id))) *)
(**)
(* and expand_id_app (env: bind_set) (bid: env_key) (rest: syntax_object list) (bind_outer: binding) = *)
(*   let id, binding = bid in  *)
(*   try  *)
(*     let resolved = resolve !global_binding_env bid in *)
(*     match id with *)
(*     | "lambda" -> expand_lambda bid rest bind_outer *)
(*     | "let-syntax" -> expand_let_syntax bid rest bind_outer *)
(*     | "quote" | "quote-syntax" -> SyntaxObject(SList(SSym(id, binding), rest, bind_outer)) *)
(*     | _ ->  *)
(*   with MacroFailure(NoMatchFor _) ->  *)
(*       raise (MacroFailure (FreeVariable(id))) *)
(*   (* Could be macro call, function call or primitive form *) *)
(*     *)
(*     (* resolve (env: bind_env) (to_resolve) *) *)
