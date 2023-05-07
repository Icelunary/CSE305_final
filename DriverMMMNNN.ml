#use "Compile.ml";;
#use "EvalStackMMM.ml";;

let stack = new EvalStack.stack;;

exception BadInstruction;;

(* let pop = match stack#pop() with *)
(*   | None -> raise BadInstruction *)

let rec executeH x = match x with
  | SL.Const(y)::rest -> (stack#push(y)); executeH(rest)
  | SL.Var(y)::rest -> stack#push(y); executeH(rest)
  | SL.Neg::rest -> executeH(rest)
  | SL.Plus::rest -> executeH(rest)
  | SL.Minus::rest -> executeH(rest)
  | SL.Times::rest -> executeH(rest)
  | SL.Div::rest -> executeH(rest)
  | SL.Fetch::rest -> executeH(rest)
  | SL.Store::rest -> executeH(rest)
  | SL.Pop::rest -> (stack#pop); executeH(rest)
  | [] -> 0
  | _ -> raise BadInstruction

(* expects a string of code to be executed and compiled with the postfix function from Compile.ml*)
let execute x = executeH(postfix x);;

