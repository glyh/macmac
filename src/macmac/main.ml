open Macmac_lib.Utils
open Macmac_lib.Eval
open Macmac_lib.Ast

let () = 
  let env = ref repl_env in
  while true do 
    let input = read_line () in
    let parsed = parse input in 
    let output, out_env = eval_multi parsed !env in
      env := out_env;
      output |> as_string |> Printf.printf "%s\n"
  done
