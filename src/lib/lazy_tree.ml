type 'a inner_tree_node =
  {
    id : int;
    parent_id : int option;
    parent : 'a inner_tree_node Lazy.t option;
    pos : int;
    children : 'a inner_tree_node list;
    depth : int;
    data : 'a;
  }

type 'a inner_tree = 'a inner_tree_node list

type ('a, 'b) tree_node = 'a inner_tree_node
type ('a, 'b) tree = 'a inner_tree_node list

let new_node ~id:id ~parent_id:parent_id ~pos:pos ~data:d =
  {
    id = id;
    parent_id = parent_id;
    parent = None;  (* PLACEHOLDER *)
    children = [];  (* PLACEHOLDER *)
    depth = 0;      (* PLACEHOLDER *)
    pos = pos;
    data = d;
  }

let id node = node.id

let parent_id node = node.parent_id

let pos node = node.pos

let data node = node.data

let parent node =
  match node.parent with
  | None -> None
  | Some lp -> Some (Lazy.force lp)

let children node = node.children

let depth node = node.depth

let build_tree (raw_data : 'a inner_tree_node list) : 'a inner_tree =
  let rec aux (raw_data : 'a inner_tree_node list)
              (depth : int)
              (parent : 'a inner_tree_node Lazy.t option)
              (parent_id : int option) =
    let children, rest =
      List.partition
        (fun el -> if el.parent_id = parent_id then true else false)
        raw_data in

    let children, rest =
      List.fold_left
        (fun (new_children, rest) node ->
          let rec new_node = lazy (
            let complete_children, rest = aux rest (depth + 1) (Some new_node) (Some node.id) in
              { node with
                  parent = parent;
                  children = List.sort (fun x y -> compare x.pos y.pos) complete_children;
                  depth = depth;
              }
            ) in
          (Lazy.force new_node)::new_children, rest)
        ([], rest)
        children in
    List.rev children, rest in (* END of aux *)
  fst (aux raw_data 0 None None)
;;

let rec flat_tree (tree : 'a inner_tree) : 'a inner_tree_node list =
  List.fold_left (fun acc node -> acc @ [node] @ (flat_tree node.children)) [] tree
;;

let tree_fold ~before ~after tree acc =
  let rec aux ~before ~after node acc =
    let acc' = before node acc in
    let acc'' =
      List.fold_left
        (fun acc el -> aux ~before ~after el acc)
        acc'
        node.children in
    after node acc'' in
  List.fold_left (fun acc node -> aux ~before ~after node acc) acc tree
;;

