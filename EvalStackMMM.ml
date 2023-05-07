(**
   Refrence: https://dev.realworldocaml.org/classes.html
 **)
module EvalStack = struct

        exception InvalidStack
        class stack = object(self)
                val mutable stack: string list = []

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

                (* purely used for testing *)
                method get_stack = self#get_stack_h(stack, "")

                method get_stack_h (ell, a) = match ell with
                  | [] -> a
                  | x::rest -> self#get_stack_h(rest, x ^ " " ^ a)

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
        end;;

end;;
