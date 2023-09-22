open Macmac_lib.Utils
open Macmac_lib.Eval
open Macmac_lib.Ast
open Macmac_lib.Core

let () = 
  let env = ref core_env in
  while true do 
    try 
      let input = read_line () in
      let parsed = parse input in 
      let output, out_env = eval (List (Sym "do", parsed)) !env in
        env := out_env;
          output |> as_string |> Printf.printf "%s\n"
    with RuntimeError(r) -> 
      print_endline (reason_to_string r)
  done
