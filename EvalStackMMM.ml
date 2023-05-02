(**
   Refrence: https://dev.realworldocaml.org/classes.html
 **)
module EvalStack = struct
        class stack = object(self)
                val mutable stack: string list = []

                method push x = stack<-x::stack

                method pop = match stack with
                | [] -> None
                | x::rest -> (stack <- rest); Some(x)

                (* purely used for testing *)
                method get_stack = stack

                method peekOne = match stack with
                | [] -> None
                | x::rest -> Some(x)

                method peekTwo = match stack with
                | [] -> None
                | x::[] -> None
                | x::y::rest -> Some((x, y))

                method replaceOne x = match stack with
                  | [] -> None
                  | y::rest -> (stack <- x::rest); Some(y)

                method replaceTwoWithOne x = match stack with
                  | [] -> None
                  | _::[] -> None
                  | y::z::rest -> (stack <- x::rest); Some((y, z))
        end;;

end;;
