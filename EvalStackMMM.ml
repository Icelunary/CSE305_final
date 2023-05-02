(**
   Refrence: https://dev.realworldocaml.org/classes.html
 **)
module EvalStack = struct
        class stack = object(self)
                val mutable stack: string list = []
                method push x = stack<-[x;]@stack
                method pop = match stack with
                | [] -> None
                | x::rest -> stack <- rest; Some(x)
                method get_stack = stack

                method peekOne = match stack with
                | [] -> None
                | x::rest -> Some(x)
                method peekTwo = match stack with
                | [] -> None
                | x::[] -> None
                | x::y::rest -> Some((x, y))

                method replaceOne x = self#pop; self#push(x):
                method replaceTwoWithOne x = self#pop; self#pop; self#push(x);
        end;;

end;;
