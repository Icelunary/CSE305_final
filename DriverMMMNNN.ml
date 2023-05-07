#use "StorageNNN.ml";;
#use "EvalStackMMM.ml";;
#use "Compile.ml";;
#use "PolyOps.ml";;

let stack = new EvalStack.stack;;

exception BadInstruction;;

(* let pop = match stack#pop() with *)
(*   | None -> raise BadInstruction *)


(* expects a string of code to be executed and compiled with the postfix function from Compile.ml*)

(* based on https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)


module Driver = struct
  exception UnexpectedBug;;
  exception FileDoesNotExist;;

  class driver = object(self)
    val stack = new EvalStack.stack
    val storage = StorageNNN.storage 100

    method read_lines name : string list =
      let ic = open_in name in
      let try_read () =
        try Some (input_line ic) with End_of_file -> None in
      let rec loop acc = match try_read () with
        | Some s -> let _ = Printf.printf "Reading: %s\n" s in loop (s :: acc)
        | None -> close_in ic; List.rev acc in
      loop []

    method execute_lines lines = stack#clear; match lines with
      | line::rest -> if self#eval(line) == 0 then self#execute_lines(rest) else raise UnexpectedBug
      | [] -> 0

    method process filename = match (self#read_file filename) with
      | Some(content) -> self#execute_lines(content);
      | None -> raise FileDoesNotExist

    method read_file name = match (self#read_lines name) with
        | [] -> None
        | text -> Some text

    method stack_two_op f = match stack#peekTwo with
      | None -> false
      | Some(x, y) -> match stack#replaceTwoWithOne(f(x, y)); with
        | None -> false
        | Some(_) -> true

    method storage_fetch = match stack#peekOne with
      | None -> false
      | Some(x) -> match storage#fetch(x) with
        | None -> false
        | Some(y) -> match stack#replaceOne(y) with
          | None -> false
          | Some(_) -> true

    method storage_store = match stack#peekTwo with
      | None -> false
      | Some(x, y) -> match storage#store(x, y) with
        | None -> false
        | Some(_) -> match stack#replaceTwoWithOne(y) with
          | Some(_) -> true
          | None -> false

    method stack_pop = match stack#pop with
      | Some(_) -> true
      | None -> false


    method eval_post x = match x with
      | SL.Const(y)::rest -> (stack#push(y)); self#eval_post(rest)
      | SL.Var(y)::rest -> stack#push(y); self#eval_post(rest)
      | SL.Neg::rest -> self#eval_post(rest)
      | SL.Plus::rest -> if self#stack_two_op(PolyOps.polyPlus) then self#eval_post(rest) else 1
      | SL.Minus::rest -> if self#stack_two_op(PolyOps.polyMinus) then self#eval_post(rest) else 1
      | SL.Times::rest -> if self#stack_two_op(PolyOps.polyTimes) then self#eval_post(rest) else 1
      | SL.Div::rest -> if self#stack_two_op(PolyOps.polyDiv) then self#eval_post x else 1
      | SL.Fetch::rest -> if self#storage_fetch then self#eval_post(rest) else 1
      | SL.Store::rest -> if self#storage_store then self#eval_post(rest) else 1
      | SL.Pop::rest -> if self#stack_pop then self#eval_post(rest) else 1
      | [] -> 0
      | _ -> 1

    method eval exp = self#eval_post(postfix exp)
    (* Prof's parser.ml treats ";" as [], wondering should I change it to "pop" *)

  end;;
end;;
let driver = new Driver.driver;;
driver#process "code.txt";;
