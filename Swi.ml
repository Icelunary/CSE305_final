(** File "Swi.ml" by KWR for CSE305, Spring 2023.
    Provided code for Assignment 7 mini-project.
    String with simple integer iterator.
    For parsing infix expressions/statements in C/C++/Java/Javascript.
    Not a class because inheritance is not envisioned.

    Syntactically, the body is just a function that constructs an object,
    since "object ... end" is a legal expression as function body.
    But note that some methods mutate the function mody.

    Because we have static data and methods, we wrap this inside a module.
    The module itself guards the name of the constructor function and has a capital letter.
 *)

 module Swi = struct 

   exception PastEnd

   (* Static data---static means not dependent on any object instance as in C++/Java. 
      These could be "val" fields of the object, but then we would have multiple
      redundant copies of constant stuff.
   *)
   let symb = "+-/*<>=!@#$%^&`~|?:"
   let alph = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
   let digit = "0123456789"
   let num = digit^"."
   let alphanum = alph^num^"_'"
   let whitespace = " \t\n\r"

   (* These are like static methods *)
   let isSymbol c = String.contains_from symb 0 c
   let isAlpha c = String.contains_from alph 0 c
   let isDigit c =  String.contains_from digit 0 c
   let isNum c = String.contains_from num 0 c
   let isAlphanum c = String.contains_from alphanum 0 c
   let isWhitespace c = c = ' ' || c = '\n' || c = '\r' || c = '\t'
   let isPrintedChar c  = ' ' < c && c <= '}'

   let floatOpt str = try Some (float_of_string str) with Failure x -> None;;
   let intOpt str = try Some (int_of_string str) with Failure x -> None;;

   let endsWith(str,chr) = let n = String.length str
                           in if n = 0 then false else str.[n-1] = chr



   (** Object portion and constructor begin here.
       Iterator is considered to have not yet polled the char it is on.
       Constructor function is recursive because peek method uses it to clone.
    *)
   let rec swi gstr = object(self)
      val str = gstr
      val mutable i = 0
      val n = String.length(gstr)

      method geti = i
      method seti j = i <- j
      method pastEnd = i >= n
      method getc = if self#pastEnd then raise PastEnd else String.get str i;
      method nextc = if self#pastEnd
                     then raise PastEnd 
                     else let c = self#getc in i <- i+1; c
      
      (** Advance iterator to the next non-whitespace character (NWC). Return that char.
       *)
      method gotoFirstNWC = while isWhitespace self#getc do i <- i+1 done; self#getc

      (** Return the next non-whitespace character without advancing the iterator.
          Re-uses previous method on a cloned copy, just to show this can be done.
       *)
      (* method peekFirstNWC = let clone = swi str in clone#seti i; clone#gotoFirstNWC *)
      
      method peekFirstNWC = let clone = {< >} in clone#seti i; clone#gotoFirstNWC

      (* May not need the rest since I/O is separate from lexing now *)
      method getItem isSkipChar isGoodChar =
         while isSkipChar self#getc do i <- i+1 done;
         let rec getGoodChars acc = if self#pastEnd then acc else let c = self#getc in
            if isGoodChar c
               then (i <- i+1; getGoodChars (acc^(String.make 1 c)))
               else acc
         in getGoodChars ""

      method getString = self#getItem isWhitespace isPrintedChar
      method getLine = self#getItem isWhitespace (function c -> c != '\n')

      (* These may become lecture examples comparing with "map" and "fold",
         but it is simpler for you to use "Swi.intOpt" and "Swi.floatOpt" above.
         Note that at present the below forget to handle a leading - sign.

      method isInt = while (not self#pastEnd && isDigit self#nextc) do i <- i+1 done; self#pastEnd
      method isFloat = while ((not self#pastEnd) && isDigit self#nextc) do i <- i+1 done; 
                       if (self#pastEnd || self#nextc != '.') then false
                       else (while ((not self#pastEnd) && isDigit self#nextc) do i <- i+1 done; self#pastEnd)
      method isVarName = (not self#pastEnd) && isAlpha self#getc
      *)


   end
end;;
