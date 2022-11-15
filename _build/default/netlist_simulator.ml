open Netlist_ast
open Lexing

let print_only = ref false
let number_steps = ref (-1)
exception Not_a_byte
exception Type_exception
exception Invalid_adress
exception Invalid_size
exception Var_exception
exception Parse_error of string

module Memory = struct
  include Map.Make(struct
    type t = bool array
    let compare = compare
  end)end

  
let arg_to_cst a values = match a with
  |Avar(a) -> Env.find a values
  |Aconst(a) -> a
  
let bool_of_int = function
    |0 -> false
    |1 -> true
    |_ -> raise Not_a_byte

let rec read_input s vars =
  Printf.printf "%s ?" s;
    let st = read_line () in
    match (Env.find s vars) with
      |TBit -> (try VBit (bool_of_int (int_of_string st))
        with | Failure _ |Not_a_byte -> (Printf.printf "This should be 0 or 1. \n"; read_input s vars))
      |TBitArray(n) when n=1 ->(try VBit (bool_of_int (int_of_string st))
    with | Failure _ |Not_a_byte -> (Printf.printf "This should be 0 or 1. \n"; read_input s vars))
      |TBitArray(n) -> 
        if String.length st = n
        then begin
          try VBitArray(Array.of_list (String.fold_right (fun ch l -> (bool_of_int(int_of_string (String.make 1 ch)))::l) st []))
          with | Failure _ |Not_a_byte -> (Printf.printf "This should be a sequence of 0 or 1 of length %d. \n" n; read_input s vars)
        end
        else (Printf.printf "This should be a sequence of 0 or 1 of length %d. \n" n; read_input s vars)

let calculate_value values rom ram = function
  | Earg(a) -> arg_to_cst a values
  | Ereg(a) -> Env.find a values
  | Enot(a) -> (match (arg_to_cst a values) with
    |VBit(b) -> VBit (not b)
    |_ -> raise Type_exception)
  | Ebinop(Or,a,b) -> (match ((arg_to_cst a values),(arg_to_cst b values)) with
    |VBit(a),VBit(b) -> VBit (a||b)
    |_ -> raise Type_exception)
  | Ebinop(And,a,b) -> (match ((arg_to_cst a values),(arg_to_cst b values)) with
    |VBit(a),VBit(b) -> VBit (a&&b)
    |_ -> raise Type_exception)
  | Ebinop(Nand,a,b) -> (match ((arg_to_cst a values),(arg_to_cst b values)) with
    |VBit(a),VBit(b) -> VBit (not (a&&b))
    |_ -> raise Type_exception)
  | Ebinop(Xor,a,b) -> (match ((arg_to_cst a values),(arg_to_cst b values)) with
    |VBit(a),VBit(b) -> VBit ((a&& (not b))||((not a)&&b))
    |_ -> raise Type_exception)
  | Emux(c,a,b) -> (match (arg_to_cst c values) with
    |VBit(c) -> if c then (arg_to_cst b values) else (arg_to_cst a values)
    |_ -> raise Type_exception)
  | Erom(addrsz,datsz,addr) -> (match (arg_to_cst addr values) with
    |VBitArray(addr) when (Array.length addr) = addrsz && datsz <> 1-> 
      (try VBitArray(Array.sub (Memory.find addr rom) 0 (datsz))
      with Not_found -> VBitArray(Array.make datsz false))
      (*We assume that every uninitialized ROM cell is full of zeroes*)
    |VBitArray(addr) when (Array.length addr) = addrsz -> 
      (try VBit((Memory.find addr rom).(0))
      with Not_found -> VBit(false))
    |VBitArray(_) -> raise Invalid_adress
    |_ -> raise Type_exception)
  | Eram(addrsz,datsz,addr,_,_,_) -> (match (arg_to_cst addr values) with
  |VBitArray(addr) when (Array.length addr) = addrsz && datsz <> 1-> 
    (try VBitArray(Array.sub (Memory.find addr ram) 0 (datsz))
    with Not_found -> VBitArray(Array.make datsz false))
    (*We assume the RAM is initialized as full of zeroes*)
  |VBitArray(addr) when (Array.length addr) = addrsz -> 
    (try VBit((Memory.find addr ram).(0))
    with Not_found -> VBit(false))
  |VBitArray(_) -> raise Invalid_adress
  |_ -> raise Type_exception)
  | Econcat(a,b) -> (match ((arg_to_cst a values),(arg_to_cst b values)) with
    |VBitArray(a),VBitArray(b) -> if Array.length (Array.append a b) = 1 then VBit((Array.append a b).(0)) else VBitArray(Array.append a b)
    |VBitArray(a),VBit(b) -> VBitArray(Array.append a [|b|])
    |VBit(a),VBitArray(b) -> VBitArray(Array.append [|a|] b)
    |VBit(a),VBit(b) -> VBitArray(Array.append [|a|] [|b|]))
  | Eselect(i,a) -> (match ((arg_to_cst a values)) with
    |VBitArray(a) -> VBit(a.(i))
    |VBit(a) when i = 0 -> VBit(a)
    |_ -> raise Type_exception)
  | Eslice(i1,i2,a)-> (match ((arg_to_cst a values)) with
    |VBitArray(a) when i1=i2 -> VBit(a.(i1))
    |VBitArray(a) -> VBitArray(Array.sub a (i1) (i2-i1+1))
    |VBit(a) when i1 = 0 && i2 = 0 -> VBitArray([|a|])
    |_ -> raise Type_exception)
  

  
let rec initialize_rom () = 
  let rom: bool array Memory.t ref =  ref Memory.empty in
  Format.printf "Please enter the file initializing the ROM (or nothing to initialize the ROM as full of zeroes): \n";
  let s = read_line () in
  if s = "" then ()
  else begin
    try let ic = open_in s in
      let lexbuf = from_channel ic in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = s};
      try
        let key_data = Rom_parser.rom Rom_lexer.token lexbuf in
        List.iter (fun (key,data) -> rom := Memory.add (Array.of_list (String.fold_right (fun ch l -> (bool_of_int(int_of_string (String.make 1 ch)))::l) key [])) (Array.of_list (String.fold_right (fun ch l -> (bool_of_int(int_of_string (String.make 1 ch)))::l) data [])) !rom) key_data
      with
        | _ ->
            let loc = Format.sprintf "line %d, column %d"
              lexbuf.lex_curr_p.pos_lnum
              (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
            in
            raise (Parse_error ("Syntax error at "^loc))
    with | _ -> (Format.printf "File not found. \n"; rom := initialize_rom()) end;
  !rom

let string_out = function
  |VBit(true) -> "1"
  |VBit(false) -> "0"
  |VBitArray(ar) -> Array.fold_right (fun b s -> if b then "1"^s else "0"^s) ar ""

let to_array = function
    |VBit(a) -> VBitArray([|a|])
    |VBitArray(a) -> VBitArray(a)

let simulator program number_steps infinite = 
  (* A COMPLETER *)
  let values: value Env.t ref = ref Env.empty in (* Maps variables to their values *)
  Env.iter (fun key typ ->
    match typ with 
    |TBit -> values := Env.add key (VBit(false)) !values
    |TBitArray(n) when n=1 -> values := Env.add key (VBit(false)) !values
    |TBitArray(n) -> values := Env.add key (VBitArray(Array.make n false)) !values) program.p_vars;
  (*Initialize every variable to 0*)
  let ram: bool array Memory.t ref =  ref Memory.empty in (*The RAM, initialized as full of zeroes*)

  let init_rom = ref false in 
  let rom: bool array Memory.t ref =  ref Memory.empty in
  List.iter (fun (_,ex) -> match ex with
    |Erom(_) -> init_rom := true
    |_ -> ()) program.p_eqs;
  if !init_rom then rom := initialize_rom (); (*Only ask to initialize the ROM if needed*)

  let p = Scheduler.schedule program in
  let i = ref 1 in
  while infinite || !i <= number_steps do
    Printf.printf "Step %d: \n" !i;  
    i := !i +1;
    List.iter (fun s -> values := Env.add s (read_input s program.p_vars) !values) p.p_inputs;
    (*Reads the input*)
    List.iter (fun (id,ex) -> match (calculate_value !values !rom !ram ex) with (*Verifies if the value given by the operation is of the correct type*)
      |VBit(v) when Env.find id p.p_vars = TBit || Env.find id p.p_vars = TBitArray(1) -> values := Env.add id (VBit(v)) !values
      |VBitArray(v) when Env.find id p.p_vars = TBitArray(Array.length v) || (Env.find id p.p_vars = TBit && Array.length v = 1) -> values := Env.add id (VBitArray(v)) !values
      |_ -> raise Var_exception) p.p_eqs;
  (*Changes the values according to the scheduler's order. *)
    List.iter (fun (_,ex) -> match ex with
      |Eram(addrsz,datsz,_,enable,addr,dat) -> (match (to_array (arg_to_cst enable !values), to_array (arg_to_cst addr !values), to_array (arg_to_cst dat !values)) with
        |(VBit(true),VBitArray(addr),VBitArray(dat)) when (Array.length addr) = addrsz && (Array.length dat) = datsz -> ram := Memory.add addr dat !ram
        |(VBit(true),VBitArray(addr),VBitArray(_)) when (Array.length addr) = addrsz -> raise Invalid_size (*The sizes are mismatched*)
        |(VBit(true),VBitArray(_),VBitArray(_)) -> raise Invalid_adress
        |(VBit(false),_,_) -> ()
        |(_,_,_) -> raise Type_exception)
      |_ -> ()
    ) p.p_eqs;
    (*Writes in the RAM*)
    List.iter (fun s -> Printf.printf "=> %s = %s \n" s (string_out (Env.find s !values))) p.p_outputs
    (*Prints the output*)
  done



let compile filename =
  try
    let p = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let out = open_out out_name in
    let close_all () =
      close_out out
    in
    begin try
        let p = Scheduler.schedule p in
        Netlist_printer.print_program out p;
        if not !print_only then
          if !number_steps <> (-1) then
          (simulator p !number_steps false)
          else (simulator p !number_steps true) (*If no number of steps has been specified, run until user exit*)
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
        | Type_exception -> Format.eprintf "An operator expected a type, got another."
        | Invalid_adress -> Format.eprintf "Tried to read or write to an invalid adress."
        | Invalid_size -> Format.eprintf "Tried to write a word of invalid size in the RAM."
        | Var_exception -> Format.eprintf "Tried to allot a value of a different type than expected to a variable"
      end;
    close_all ();
    with
      | Netlist.Parse_error s -> Format.eprintf "An error occurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-print", Arg.Set print_only, "Only print the result of scheduling";
      "-n", Arg.Set_int number_steps, "Number of steps to simulate (Runs until user exit if unspecified)"]
    compile
    ""
;;

main ()
