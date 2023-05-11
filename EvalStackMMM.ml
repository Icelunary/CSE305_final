(*
   By: Anthony Schiano
   Purpose: to be used as the stack in Driver

   the stack object is what is used for the stack in driver
   it allows you to both pop the top item and the top 2 items through the methods pop and popTwo
   it also allows you to peek the first two items or replace them through the methods peekOne and peekTwo and replaceOne and replaceTwoWithOne
   it allows items to be pushed to the stack also using the method push
   and the stack can be cleared using the clear method
   it also has the get_stack and get_stack_h so the stack can be printed out at any point
*)

(**
   Refrence: https://dev.realworldocaml.org/classes.html
 **)
module EvalStack = struct

        exception InvalidStack
        class stack = object(self)
          val mutable stack = []

                method push x = stack<-(x::stack)

                method pop = match stack with
                | [] -> None
                | x::rest ->
                  stack <- rest;
                  Some(x)

                method popTwo = match stack with
                  | [] -> None
                  | _::[] -> None
                  | x::y::rest ->
                    stack <- rest;
                    Some(y, x)


                method peekOne = match stack with
                | [] -> None
                | x::rest -> Some(x)

                method peekTwo = match stack with
                | [] -> None
                | x::[] -> None
                | x::y::rest -> Some(y,x)

                method replaceOne x = match stack with
                  | [] -> None
                  | y::rest -> (stack <- x::rest); Some(y)

                method replaceTwoWithOne x = match stack with
                  | [] -> None
                  | _::[] -> None
                  | y::z::rest -> (stack <- x::rest); Some(z, y)

                method clear = stack <- []

                (* purely used for testing *)
                method get_stack = self#get_stack_h(stack, "")

                method get_stack_h (ell, a) = match ell with
                  | [] -> a
                  | x::rest -> self#get_stack_h(rest, x ^ " " ^ a)
        end;;

end;;
