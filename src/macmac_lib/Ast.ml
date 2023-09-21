module Env = CCPersistentHashtbl.Make(struct
   type t = string
   let equal s t = s = t
   let hash = Hashtbl.hash
end)

type value = 
  | List of value * value list (* nonempty list *)
  | Bool of bool
  | Float of float
  | Keyword of string
  | Str of string
  | Sym of string
  | Nil
  | PrimFn of (value list -> value)
  | PrimSyntax of (value list -> env -> value * env)
and env = {
  bindings: value Env.t;
  outer: env option
}

let rec lookup (env: env) (sym: string): value option =
  match env.outer, Env.get sym env.bindings with
  | _, Some(v) -> Some v
  | Some(outer), None -> lookup outer sym
  | None, None -> None

let bind (env: env) (sym: string) (v: value): env =
  { env with bindings = Env.add env.bindings sym v } 

let rec as_string (form: value): string = 
  match form with
  | List(x, xs) -> "(" ^ (
    (x :: xs) |> List.map as_string |> String.concat " "
    ) ^ ")"
  | Bool(true) -> "true"
  | Bool(false) -> "false"
  | Float(f) -> string_of_float f
  | Keyword(s) -> ":" ^ s
  | Str(s) -> "\"" ^ s ^ "\"" (* not good enough, will fix *)
  | Sym(s) -> s
  | Nil -> "nil"
  | PrimFn _ -> "[primfn]"
  | PrimSyntax _ -> "[primsyntax]"
