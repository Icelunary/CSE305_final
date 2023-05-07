#use "StorageNNN.ml";;
#use "EvalStackMMM.ml";;
#use "Compile.ml";;
#use "PolyOps.ml";;

(* based on https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> let _ = Printf.printf "Reading: %s\n" s in loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let code = read_lines "code.txt";;
module Driver = struct
  exception UnexpectedBug of string

  class driver = object(self)
    val stack = new EvalStack.stack
    val storage = StorageNNN.storage 100
    val mutable text: string list = []

    

    method getData = match text with 
      | [] -> None
      | x -> Some x

    method read_lines name : string list =
      let ic = open_in name in
      let try_read () =
        try Some (input_line ic) with End_of_file -> None in
      let rec loop acc = match try_read () with
        | Some s -> let _ = Printf.printf "Reading: %s\n" s in loop (s :: acc)
        | None -> close_in ic; List.rev acc in
      loop []
    
    method writeText name = text <- self#read_lines name; match text with
        | [] -> None
        | _ -> Some text

        (* read file into text *)
    method process name = let txt = self#writeText name in match txt with
        | None -> raise (UnexpectedBug "No data read")
        | Some _ -> self#processLineByLine text

    (* current has 1 issue
       1. If the var name starts with num, it will still create bind. Can use Swi.swi#getc to check the first char 
       I'm also wondering to add a "raise error" on store if user tries to bind a undeclared var like you told me*)
    (* currently considering following case for bind (want to ask prof) 
       1. second time "int a exp" -> use Hashtbl.add for a. So, if a is redeclare inside a new scope, it's dynamic binding. Not erasing old a
       2."a = exp" -> use Hashtbl.replace. So the old value binded to a is erased*)
    (* This method is expected to be called by method eval *)
    method bind var value = storage#store(var, value) 

    method processLineByLine (arr: string list) = let _ = Printf.printf "trying to process\n" in match arr with
        | [] -> Some("Finished\n")
        | "\r"::rest | "\n"::rest -> let _ = Printf.printf "Empty line\n" in self#processLineByLine rest
        | x::rest -> let a = self#processLine x in
           let _ = match a with
            | None -> Printf.printf "Empty line\n"
            | Some x -> Printf.printf "Evaluated %s\n" x
          in self#processLineByLine rest
    
    method processLine line = let _ = Printf.printf "processing line: %s\n" line in
     let tokenList = Compile.tokenList2String (postfix line) in self#eval tokenList
    (* let tokenList = postfix line *)
    
    (* method processLine line = let str = let _ = Printf.printf "processing line: %s\n" line in Swi.swi(line) in let firstItem = (str#getItem Swi.isWhitespace Swi.isAlpha) in match firstItem with
        | "Int" | "double" | "float" -> let _ = Printf.printf "case1\n" in let var = (str#getItem Swi.isWhitespace Swi.isAlpha)
           in let _ = self#bind var (self#eval(var^" "^str#getString)) true
             in Some(var)
        | _ -> let _ = Printf.printf "case2\n" in Some (self#eval line) *)
    
    (* Only test the first line of code file *)
    method testFirst = match text with
      | [] -> None
      | x::rest ->self#processLine x

    (* Get exp after "int|float|double" and evaluate it. Compile gives string list. We can iterate it to stack value and PolyOps|Storage
       once we encounter Unop|Binop|Fetch|Store. stack and storage are both inside this class*)
    method eval tokenList = Some("s")
    (* 
       "fetch"::rest -> stack#peek 1 and storage fetch it
       "store"::rest -> stack#peek 2 and self#bind it, then replace two with one
       "pop"::rest -> stack pop 1
       "PolyOps"::rest -> stack peek2, PolyOps, then stack replace two with one
       "_"::rest -> stack push*)
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