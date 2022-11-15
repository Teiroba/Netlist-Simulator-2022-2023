exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to   <- n2 :: n1.n_link_to;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found -> Format.eprintf "Tried to add an edge between non-existing nodes"; raise Not_found

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g = 
  let rec visit n =
    match n.n_mark with
    | NotVisited ->
      n.n_mark <- InProgress;
      List.iter visit n.n_link_to;
      n.n_mark <- Visited
    | InProgress -> raise Cycle
    | Visited -> ()
  in
  try
    clear_marks g;
    List.iter visit (g.g_nodes);
    false
  with Cycle -> true

let topological g = 
  let l = ref [] in
  if has_cycle g then raise Cycle;
  let rec visit n = 
    match n.n_mark with
    | NotVisited ->
      (n.n_mark <- InProgress;
      List.iter visit n.n_link_to;
      l := n.n_label :: !l;
      n.n_mark <- Visited)
    | InProgress -> raise Cycle;
    | Visited -> ()
  in
  clear_marks g;
  List.iter visit (find_roots g);
  !l

