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
  exception UnexpectedBug of string;;
  exception FileDoesNotExist;;

  class driver = object(self)
    val stack = new EvalStack.stack
    val storage = StorageNNN.storage 100
    val mutable text: string list = []

    method read_lines name : string list =
      let ic = open_in name in
      let try_read () =
        try Some (input_line ic) with End_of_file -> None in
      let rec loop acc = match try_read () with
        | Some s -> let _ = Printf.printf "Reading: %s\n" s in loop (s :: acc)
        | None -> close_in ic; List.rev acc in
      loop []

    method execute_lines lines = match lines with
      | line::rest -> self#eval(line); self#execute_lines(rest)

    method process filename = match (self#read_file filename) with
      | Some(content) -> self#execute_lines(content)
      | None -> raise FileDoesNotExist

    method getData = match text with
      | [] -> None
      | x -> Some x

    method read_file name = match (self#read_lines name) with
        | [] -> None
        | _ -> Some text

    method eval_post x = match x with
      | SL.Const(y)::rest -> (stack#push(y)); self#eval_post(rest)
      | SL.Var(y)::rest -> stack#push(y); self#eval_post(rest)
      | SL.Neg::rest -> self#eval_post(rest)
      | SL.Plus::rest -> self#eval_post(rest)
      | SL.Minus::rest -> self#eval_post(rest)
      | SL.Times::rest -> self#eval_post(rest)
      | SL.Div::rest -> self#eval_post(rest)
      | SL.Fetch::rest -> self#eval_post(rest)
      | SL.Store::rest -> self#eval_post(rest)
      | SL.Pop::rest -> (stack#pop); self#eval_post(rest)
      | [] -> 0
      | _ -> raise BadInstruction

    method eval exp = eval_post(postfix exp)
    (* Prof's parser.ml treats ";" as [], wondering should I change it to "pop" *)

  end;;
end;;
let driver = new Driver.driver;;
driver#writeText "code.txt";;
let test = "int a = a + 3;";;
(* ";" is missing during readlist *)
let a1 = let open Parser in let s = Swi.swi(test) in (readlist s);;
(* let a2 = let open Parser in groupUnops a1;; *)
let b1 = Swi.swi("int a+a+1;\n");;
