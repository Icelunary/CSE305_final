module StorageNNN = struct
  (* type t = (string, string list) Hashtbl.t *)

  let storage n = object(self)
    val tbl = Hashtbl.create n
    (* val mutable test1 *)

    (* method create n = test <- Hashtbl.create n *)

    method add name value = 
      let history = try Hashtbl.find tbl name with Not_found -> [] in
      Hashtbl.replace tbl name (value::history)

    method get name = 
      let history = try Hashtbl.find tbl name with Not_found -> [] in
      match history with
        | [] -> None
        | hd::_ -> Some hd

    method print_log name =
      match self#get name with 
        | None -> Printf.printf "Unbounded var: %s\n" name
        | Some hd ->
          Printf.printf "History of %s: current is %s and history below \n" name hd;
          List.iter (fun v -> Printf.printf " %s |" v) (Hashtbl.find tbl name);
          Printf.printf "\n"
    
    method print_log2 name = 
      match self#get name with
        | None -> Printf.printf "Unbounded var: %s\n" name
        | Some hd -> Hashtbl.find_all tbl name

    method remove key = 
      match self#get name with
        | None -> Printf.printf "Unbounded var: %s\n" name
        | Some hd -> Hashtbl.remove tbl key
    
    end

end;;