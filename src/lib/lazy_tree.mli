type ('a, +'b) tree_node
type ('a, +'b) tree = ('a, 'b) tree_node list

val new_node : id:int -> parent_id:int option -> pos:int -> data:'a -> ('a, [> `incomplete]) tree_node

val id : ('a, 'b) tree_node -> int
val parent_id : ('a, 'b) tree_node -> int option
val pos : ('a, 'b) tree_node -> int
val data : ('a, 'b) tree_node -> 'a

val parent : ('a, [< `complete]) tree_node -> ('a, [> `complete]) tree_node option
val children : ('a, [< `complete]) tree_node -> ('a, [> `complete]) tree_node list
val depth : ('a, [< `complete]) tree_node -> int

val build_tree : ('a, 'b) tree_node list -> ('a, [> `complete]) tree
val flat_tree : ('a, [< `complete]) tree -> ('a, [> `complete]) tree_node list

val tree_fold :
  before:(('a, [< `complete]) tree_node -> 'acc -> 'acc) ->
  after:(('a, [< `complete]) tree_node -> 'acc -> 'acc) ->
    (('a, [< `complete]) tree) -> 'acc -> 'acc

