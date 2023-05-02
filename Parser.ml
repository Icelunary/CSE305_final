(** File Parse.ml, by KWR for CSE305, Spring 2023.
    Provided code for Assignment 7 mini-project.
    Parses simple statements in C/C++/Java that have infix arithmetical expressions,
    plus assignment and some other operators, using standard precedence rules.
    Module defines datatypes for parsing but not classes.

    Makes some restrictions so that the recursive-descent code is simpler to read:
    () Unary operators cannot be consecutive unless parentheses are used.
    () All unary operators have higher precedence than all binary operators.
    Gain is that users can add new binary operations just by giving their
    symbols, precedence, and associativity---without having to write
    recursive descent routines for each (new) nonterminal involved.

    Lexes into list of constants, variables, unops, and binops, then
    parses into unary-binary tree with ops as well as items put in leaves.
    You are expected to convert from there into the 'a exp datatype from 
    previous assignments.  Then you may re-use the stack translation code
    in order to feed into your main and final project portion.

    To save grief, we postpone mixing int and float and hard-wire string encoding
    of all constants here.  You might be asked as a "one more thing" to add
    support for one or more other operations.
 *)

#use "Swi.ml";;

module Parser = struct

   type lex = Op of string | Unop of string | Binop of string | Varlex of string | Constlex of string

   type gentree = 
      | EmptyGT                  (* error-catchall; not used in leaves and should never appear *)
      | Item of lex
      | Node of gentree list   (* parenthesized sub-expression *)
      | Glom of gentree list   (* like Node but absorbs terms rather than drop down; temporary *)

   type ubtree = 
      EmptyUBT
      | Leaf of lex
      | UNode of string * ubtree    (* unary node always has string operator *)
      | BNode of ubtree * string * ubtree

   exception Badform of string
   exception BadSymbol of string

   let prec(bop) = match bop with
      "=" -> 3
      | "+" -> 5
      | "-" -> 5
      | "*" -> 7
      | "/" -> 7
      | "%" -> 7
      | _ -> 0

   let isLeft(bop) = match bop with
      "=" -> false 
      | "+" -> true
      | "-" -> true
      | "*" -> true
      | "/" -> true
      | "%" -> true
      | _ -> true

   (* Presumes that Constlex and later Const have string args, not int or float (yet)
      Hence coded without a curried "a2string" function argument.
   *)
   let rec gentree2string x = match x with
      | EmptyGT -> ""
      | Item(Constlex u) -> "Item(Const " ^ u ^ ")"
      | Item(Varlex u) -> "Item(Var " ^ u ^ ")"
      | Item(Op u) -> "Item(Op " ^ u ^ ")"
      | Item(Unop u) -> "Item(Unop " ^ u ^ ")"
      | Item(Binop u) -> "Item(Binop " ^ u ^ ")"

      | Glom(ell) -> "Glom(" ^ (gentreelist2string ell) ^ ")"
      | Node(ell) -> "Node(" ^ (gentreelist2string ell) ^ ")"
   and gentreelist2string ell = match ell with
      [] -> ""
      | w::[] -> (gentree2string w)
      | v::w::rest -> (gentree2string v) ^ "::" ^ (gentreelist2string (w::rest))




(* Parse an Swi.swi object s consisting of a line of C code an an iterator on the code string.
   This stage only does grouping via parentheses in the string; it does not yet apply
   precedence or associativity, not even segregate unary and binary operators.
   
   The output is a string gentree whose root is a Node of the top-level tokens and
   where each parenthesized part is a Node with its string gentree list of children.

   Also exemplifies using "let _ = foo in bar" where foo is needed only for a side effect.
   Using wildcard _ rather than a variable name avoids an annoying unused-variable warning.
*)

let rec readtree(s) =
   if s#pastEnd then EmptyGT
   else
   let c = s#nextc in
      if Swi.isWhitespace c then readtree(s)
            else if c = '(' then 
         let ell = readlist(s) (* Leaves cursor on matching ')' *)
         in  if s#nextc = ')' then          (* Consume the ")" *)
                  Node(ell)
         else raise (Badform "In readtree inner one")
            else raise (Badform "In readtree outer one")
   (* end of if and let and if and let and innermost if *)
and
   readlist(s) =
   if s#pastEnd then []   (* leaves cursor on the ')' *)
   else let c = s#getc in
            if c = ')' || c = ';' || c = '\n' then []
            else if Swi.isWhitespace c then let _ = s#nextc in readlist(s)
            else if c = '(' then
               let t = readtree(s)
               in  t::readlist(s)
            else if Swi.isNum c then (* beginning number or . means constant *)
                  let item = s#getItem Swi.isWhitespace Swi.isNum
                  in  (Item(Constlex item))::readlist(s)
            else if Swi.isAlpha c then
                  let item = s#getItem Swi.isWhitespace Swi.isAlphanum
                  in  (Item(Varlex item))::readlist(s)
            else if Swi.isSymbol c then
                  let item = s#getItem Swi.isWhitespace Swi.isSymbol
                  in  (Item(Op item))::readlist(s)
            else raise (BadSymbol ("In readlist with c = " ^ (String.make 1 c)))
   (* end of if *)


   (** Assume that unary operators always bind tightest.  
      Transform a "gentree" into one where every unary operator is
      inside a Node holding it and exactly one other item or Node.
      Then every longer child-list of a Node has only the entries
      Item(Const x), Item(Var x), Item(Binop u), or Node(...).
   *)
   exception NotUBTree of gentree

   let rec groupUnops ell = match ell with
      [] -> []
      | [x] -> [group x]
      | [any; Item(Op y)] -> let x = if y = "++" || y = "--" then "@"^y else y
                           in [Node((group any)::Item(Unop x)::[])]
      | (Item(Op y))::any::rest -> let x = if y = "++" || y = "--" then y^"@" else y
                                 in Node(Item(Unop x)::(group any)::[])::groupUnops(rest)
      | [any1;any2] -> [group any1; group any2]
      | any1::(Item(Op y))::any2::rest ->
            if y = "++" || y = "--"
            then Node((group any1)::(Item(Unop ("@"^y)))::[])::groupUnops(any2::rest)
            else (group any1)::(Item(Binop y))::groupUnops(any2::rest)
      | _ -> raise (NotUBTree (Node(ell)))
   and group t = match t with
      EmptyGT -> EmptyGT
      | Item u -> Item u
      | Node(ell) -> Node(groupUnops ell)
      | Glom(ell) -> Glom(groupUnops ell)  


(** After applying groupUnops(...), the gentree has no more Item(Op x) subterms. 
   Collectively call a Node(...), Item(Var x), or Item(Const x) subterm an "object".
   
   type lex = Op of string | Unop of string | Binop of string | Varlex of string | Constlex of string

*)

   (** Convert gentree to ubtree.
   *)
   let rec parse t = let _ = (print_endline("Parsing " ^ (gentree2string t)))
                     in match t with
      EmptyGT -> EmptyUBT
      | Item(u) -> Leaf(u)
      | Glom(ell) -> parse (Node ell)
      | Node [] -> EmptyUBT
      | Node [Item x] -> Leaf x
      | Node(Node(any)::[]) -> parse (Node any)
      | Node(EmptyGT::rest) -> parse (Node rest)

      (* Two-child case should only be a unary operation, pre or post, on any object. *)
      | Node(Item(Unop x)::any::[]) -> UNode(x, parse any)
      | Node(any::(Item(Unop x))::[]) -> UNode(x, parse any)
      | Node(any1::any2::[]) -> raise (NotUBTree t)

      (* Three-child case should only be two objects joined by a binop. *)
      | Node(any1::(Item(Binop x))::any2::[]) -> BNode(parse any1, x, parse any2)
      | Node(any1::any2::any3::[]) -> raise (NotUBTree t)

      (* Four-element node should not occur as it would have to have one ungrouped unary op. *)
      | Node(any1::any2::any3::any4::[]) -> raise (NotUBTree t)

      (* Five-or-more-children cases must have two binops.
      *)
      | Node(any::Item(Binop x)::Item(u)::Item(Binop y)::any3::rest) ->
         if prec(x) > prec(y) || (prec(x) = prec(y) && isLeft(x)) then
            parse(Node(Node(any::Item(Binop x)::Item(u)::[])::Item(Binop y)::any3::rest))
         else
            parse(Node(any::Item(Binop x)::Glom(Item(u)::Item(Binop y)::any3::[])::rest))
      | Node(any::Item(Binop x)::Node(ell)::Item(Binop y)::any3::rest) ->
         if prec(x) > prec(y) || (prec(x) = prec(y) && isLeft(x)) then
            parse(Node(Node(any::Item(Binop x)::Node(ell)::[])::Item(Binop y)::any3::rest))
         else
            parse(Node(any::Item(Binop x)::Glom(Node(ell)::Item(Binop y)::any3::[])::rest))
      | Node(any::Item(Binop x)::Glom(ell)::Item(Binop y)::any3::rest) ->
         if prec(x) > prec(y) || (prec(x) = prec(y) && isLeft(x)) then
            parse(Node(Node(any::Item(Binop x)::Node(ell)::[])::Item(Binop y)::any3::rest))
         else
            parse(Node(any::Item(Binop x)::Glom(ell@(Item(Binop y)::any3::[]))::rest))

      | _ -> let _ = print_endline("fell through on ") in raise (NotUBTree t)


   let rec ubtree2string(ub,indent,offset) = match ub with
      EmptyUBT         -> "\n" 
      | Leaf(Constlex u) -> (String.make indent ' ')^u^(Scanf.unescaped "\n\r")
      | Leaf(Varlex u)   -> (String.make indent ' ')^u^(Scanf.unescaped "\n\r")
      | UNode(unop,t)    -> (String.make indent ' ')^unop^(Scanf.unescaped "\n\r")^
                              ubtree2string(t,indent+offset,offset) 
      | BNode(t,binop,u) -> ubtree2string(u,indent+offset,offset)^
                              (String.make indent ' ')^binop^(Scanf.unescaped "\n\r")^
                              ubtree2string(t,indent+offset,offset)
      | Leaf(Op v)
      | Leaf(Unop v)
      | Leaf(Binop v)    -> (String.make indent ' ')^v^(Scanf.unescaped "\n\r")

end;;

(** Testing code.  
*)
let a s= let open Parser in readlist s;;
let comp str = let open Parser in let s = Swi.swi(str) in 
   parse (Node(groupUnops (readlist s)));;

let pt str = print_endline("\n"^Parser.ubtree2string(comp str, 4, 4));;
 