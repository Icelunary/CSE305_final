(* Contributor: Howie Lin
   UBIT: haohuili
   UBIT#: 50279535
   
   Description: This module implements the variable binding by store/fetch/pop using Hashtbl build-in functionlity.
              It binds/read/pop value for key from table called tbl.
              print_log key will print out the binding history for that key*)

module StorageNNN = struct
  (* type t = (string, string list) Hashtbl.t *)
  exception Unbounded of string
  

  let storage n = object(self)
    
    val mutable tbl = Hashtbl.create n

    method store(key, value) = Hashtbl.add tbl key value; let _ = Printf.printf "Storing key: %s and value %s\n" key value in Some(value)
        
    
    method fetch(key) = 
      self#get key
    
    method pop(key) = 
      self#remove key

    method remove key =
      match self#get key with
        | None -> let _ = Printf.printf "Unbounded var: %s\n" key in None
        | Some hd -> let _ = Hashtbl.remove tbl key in Some(hd)

    method get key =
      
      let value = try Some(Hashtbl.find tbl key) with Not_found -> None in value

    method print_log key =
      match self#get key with
        | None -> Printf.printf "Unbounded value: %s\n" key
        | Some hd ->
          Printf.printf "History of %s: current is %s and history below \nCurrent<---------->Old\n" key hd;
          List.iter (fun v -> Printf.printf " %s |" v) (Hashtbl.find_all tbl key);
          Printf.printf "\n"
    
    (* Commented one is print list as string *)
    (* method printHistoryAsList key = 
      Hashtbl.find_all tbl key *)
      (* let find = try Some(self#get key) with Not_found -> None in
      match find with
        | None -> None
        | Some _ -> Some(Hashtbl.find_all tbl key) *)



    (* method add(key, value) =
      (* Printf.printf "trying to add var: %s\n" value; *)
      let _ = Hashtbl.add tbl key value in Some(value)

    (* recursively add, use for test *)
    method recurAdd = function
      | [] -> Some("Succeed")
      | (k, v)::rest -> let _= self#add(k, v) in let _ = Printf.printf "adding %s %s\n" k v in self#recurAdd rest *)

    end

end;;

(* Use for test *)
let test = [("h", "hello"); ("h", "world"); ("h", "1"); ("1", "test")];;
let store = StorageNNN.storage 100;;
(* store#recurAdd test;; *)
(* Then you can try to test other methods *)

let a = Hashtbl.create 100;;
Hashtbl.add a "h" "test1";;
Hashtbl.add a "h" "test2";;
Hashtbl.add a "h" "test3";;
Hashtbl.add a "h" "test4";;
let c = 4;;
exception Unbounded of string
let f2(key, value) = let result = try Some(Hashtbl.find a key) with Not_found -> None in
  match result with
    | None -> raise (Unbounded key)
    | Some x -> Hashtbl.replace a key value; Some(value)
;;
let f3 x = x;;
