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
      | line::rest -> if self#eval(line) then self#execute_lines(rest) else false
      | [] -> true

    method process filename = match (self#read_file filename) with
      | Some(content) -> self#execute_lines(content)
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
      | SL.Plus::rest -> if self#stack_two_op(PolyOps.polyPlus) then self#eval_post(rest) else false
      | SL.Minus::rest -> if self#stack_two_op(PolyOps.polyMinus) then self#eval_post(rest) else false
      | SL.Times::rest -> if self#stack_two_op(PolyOps.polyTimes) then self#eval_post(rest) else false
      | SL.Div::rest -> if self#stack_two_op(PolyOps.polyDiv) then self#eval_post x else false
      | SL.Fetch::rest -> if self#storage_fetch then self#eval_post(rest) else false
      | SL.Store::rest -> if self#storage_store then self#eval_post(rest) else false
      | SL.Pop::rest -> if self#stack_pop then self#eval_post(rest) else false
      | [] -> true
      | _ -> false

    method eval exp = self#eval_post(postfix exp)
    (* Prof's parser.ml treats ";" as [], wondering should I change it to "pop" *)

  end;;
end;;

let driver = new Driver.driver;;
driver#process "code.txt";;
(* Essay Question
   EvalStack: a stack of string list. It can push, pop, replace, clear and peek top element from stack.
   Storage: a hash table storing the binding of val. Store can bind a value to key/varName. Fetch can get key for the value. 
            Pop will pop the value of key/varName and restore previous value. print_log will print out the history value for key
   Driver: By giving a filename to driver#process, driver starts reading the content of file and process it line by line.
          For each line, the text will be translated to tokenList and then try to execute them.
          For const and var, they will be pushed to stack. For operations, they will follow the determined action in driver#eval_post.
          If there are fetch/store/pop, driver will use storage#fetch, storage#store and storage#pop to  get/add/pop the value for the key/varName
  
  Both stack and storage do not need to know any details of the stack language commands.
  (Not sure about this one)Also, they adapt seamlessly to possible extensions in the range of C-language commands emulated.
  In this case, the key will be the address of the varName        *)