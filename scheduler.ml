open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq = let (_,ex) = eq in match ex with (* Ignore registers. *)
  | Earg(Avar(a)) -> [a]
  | Enot(Avar(a)) -> [a]
  | Ebinop(_,Avar(a),Avar(b)) -> a::[b]
  | Ebinop(_,Avar(a),_) | Ebinop(_,_,Avar(a))-> [a]
  | Emux(Avar(a),Avar(b),Avar(c)) -> a::b::[c]
  | Emux(Avar(a),Avar(b),_) | Emux(Avar(a),_,Avar(b))| Emux(_,Avar(a),Avar(b))-> a::[b]
  | Emux(Avar(a),_,_) | Emux(_,Avar(a),_) | Emux(_,_,Avar(a)) -> [a]
  | Erom(_,_,Avar(a)) -> [a]
  | Eram(_,_,Avar(a),_,_,_) -> [a]
  | Econcat(Avar(a),Avar(b)) -> a::[b]
  | Econcat(Avar(a),_) | Econcat(_,Avar(a)) -> [a]
  | Eslice(_,_,Avar(a))-> [a]
  | Eselect(_,Avar(a)) -> [a]
  | _ -> []

let schedule p = 
  let g = mk_graph () in
  Env.iter (fun key _ -> add_node g key) p.p_vars;
  let add_registers eq = (* We deal with registers by computing them first : 
    as such, if e = reg(a), we assume a depends on e to make sure at the time of reading e, 
    the a value is the one from the previous step *)
    let (a,exp) = eq in
    match exp with
      |Ereg(b) -> add_edge g a b
      |_ -> () in
  List.iter add_registers p.p_eqs;
  let add_vertices eq = 
    let (a,_) = eq and l = read_exp eq in
    List.iter (fun x -> add_edge g x a) l
  in
  List.iter add_vertices p.p_eqs;
  try let l = topological g in
  let lbis = List.mapi (fun i x -> (x,i)) l in
  let leq = List.sort (fun (x,_) (y,_) -> compare (List.assoc x lbis) (List.assoc y lbis)) p.p_eqs in
  {p_eqs = leq; p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars}
  with Cycle -> raise Combinational_cycle;



