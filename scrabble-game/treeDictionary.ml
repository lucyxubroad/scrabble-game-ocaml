open Dictionary

module Make
  = functor (K : KeySig) -> functor (V : ValueSig) ->
  struct
    module Key = K
    module Value = V
    type key = K.t
    type value = V.t

    (* Color type of the nodes in red-black tree. *)
    type color = Red | Black | BlackBlack
    type 'a rbtree =
        Node of color * 'a * 'a rbtree * 'a rbtree | Leaf | BBLeaf


    (* AF: The red black tree of pairs (k1,v1), (k2,v2), ... represents the 
        dictionary that maps the keys k1..kn to values v1..vn.
     * RI: There are no two adjacent red nodes along any path.
       Every path from the root to a leaf has the same number of black nodes. *)
    type t = (key*value) rbtree

    let debug = false

    (* [no_adj_red t] checks if the red-black tree [t] satisfies the invariant
       that there are no two adjacent red nodes. If it satisfies the invariant,
       return true, otherwise return false.  *)
    let rec no_adj_red t = 
      match t with 
      | Node(c, _, Node (c2, y2, l2, r2), Leaf) -> 
        if (c = Red && c2 = Red) then failwith "no_adj_red 1"
        else no_adj_red (Node (c2, y2, l2, r2))
      (* | Node(c, _, Node (c2, y2, l2, r2), BBLeaf) -> 
         if (c = Red && c2 = Red) then failwith "no_adj_red 2"
         else no_adj_red (Node (c2, y2, l2, r2)) *)
      | Node(c, _, Leaf, Node (c2, y2, l2, r2)) -> 
        if (c = Red && c2 = Red) then failwith "no_adj_red 3" 
        else no_adj_red (Node (c2, y2, l2, r2))
      (* | Node(c, _, BBLeaf, Node (c2, y2, l2, r2)) -> 
         if (c = Red && c2 = Red) then failwith "no_adj_red 4" 
         else no_adj_red (Node (c2, y2, l2, r2)) *)
      | Node(c, _, Node (c2, y2, l2, r2), Node(c3, y3, l3, r3)) -> 
        if (c = Red && c2 = Red) then failwith "no_adj_red 5 left"
        else if (c = Red && c3 = Red) then failwith "no_adj_red 5 right"
        else no_adj_red (Node (c2, y2, l2, r2)) && no_adj_red (Node(c3, y3, l3, r3)) 
      | Node(c, _, Leaf, Leaf) -> true
      (* | Node(c, _, BBLeaf, BBLeaf) -> true *)
      | Leaf -> true
      (* | BBLeaf -> true *)
      | _ -> failwith "Invariant 1 broken!"

    (* [compare_k k1 k2] is the order of type [order] between the 
            keys [k1] and [k2].   *)
    let compare_k k1 k2 =
      match K.compare k1 k2 with
      | LT -> -1
      | GT -> 1
      | EQ -> 0

    (* [tree_in_order t] checks whether the red-black tree [t] has a valid 
       order of nodes as a binary search tree. If it is valid, return true,
       otherwise return false. *)
    let rec tree_in_order t = 
      match t with
      | Node (c1, (k1, v1), Node (c2, (k2, v2), l2, r2), Node (c3, (k3, v3), l3, r3)) -> 
        if ((compare_k k2 k1 = -1) && (compare_k k3 k1 = 1)) then tree_in_order (Node (c2, (k2, v2), l2, r2)) 
                                                                  && tree_in_order (Node (c3, (k3, v3), l3, r3)) else failwith "tree_in_order 1" 
      | Node (c1, (k1, v1), Node (c2, (k2, v2), l2, r2), Leaf) -> 
        if (compare_k k2 k1 = -1) then tree_in_order (Node (c2, (k2, v2), l2, r2)) else failwith "tree_in_order 2" 
      | Node (c1, (k1, v1), Node (c2, (k2, v2), l2, r2), BBLeaf) -> 
        if (compare_k k2 k1 = -1) then tree_in_order (Node (c2, (k2, v2), l2, r2)) else failwith "tree_in_order 3" 
      | Node (c1, (k1, v1), Leaf, Node (c2, (k2, v2), l2, r2)) -> 
        if (compare_k k2 k1 = 1) then tree_in_order (Node (c2, (k2, v2), l2, r2)) else failwith "tree_in_order 4" 
      | Node (c1, (k1, v1), BBLeaf, Node (c2, (k2, v2), l2, r2)) -> 
        if (compare_k k2 k1 = 1) then tree_in_order (Node (c2, (k2, v2), l2, r2)) else failwith "tree_in_order 5" 
      | Node (c1, (k1, v1), Leaf, Leaf) -> true
      | Node (c1, (k1, v1), BBLeaf, BBLeaf) -> true
      | Leaf -> true
      | BBLeaf -> true
      | _ -> true

    (* [get_bh acc t] is the black height [acc] of the red-black tree [t]. *)
    let rec get_bh acc t =
      match t with 
      | Node (c, _, l, _) -> if c = Black then get_bh (acc+1) l else get_bh (acc) l
      | Leaf -> acc
      | BBLeaf -> acc

    (* 【check_bh dec t] checks whether the black height of the red-black tree
       [t] is matches the expected black height [dec]. If it matches the 
       expectation, return true, otherwise return false. *)
    let rec check_bh dec t = 
      match t with 
      | Node (c, _, l, r) -> if c = Black 
        then (check_bh (dec-1) l && check_bh (dec-1) r) 
        else (check_bh (dec) l && check_bh (dec) r)
      | Leaf -> dec = 0
      | BBLeaf -> dec = 0

    let rep_ok d =
      if debug then (
        let no_red_adj = no_adj_red d in
        let in_order = tree_in_order d in
        let black_height = get_bh 0 d in
        let same_bh = check_bh black_height d in 
        if same_bh && no_red_adj && in_order then d
        else failwith "Invariant broken!")
      else d

    let empty = Leaf

    let is_empty d =
      d = Leaf

    let rec size d =
      match d with 
      | Leaf -> 0
      | BBLeaf -> 0
      | Node(_,_,l,r) -> 1 + size l + size r

    (* [balance] rebalances the tree o preserve the red-black invariant when a
       node is inserted or removed. *)
    let balance = function
      | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
      | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
      | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
      | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | BlackBlack, z, Node (Red, x, a, Node (Red, y, b, c)), d 
      | BlackBlack, x, a, Node (Red, z, Node (Red, y, b, c), d) ->
        Node (Black, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | a, b, c, d -> Node (a, b, c, d)

    let insert k v d =
      let ok_d = rep_ok d in
      let rec ins d = match d with
        | BBLeaf 
        | Leaf -> Node (Red, (k,v), Leaf, Leaf)
        | Node (color, (k1,v1), l, r) ->
          let comparison = compare_k k k1 in
          if (comparison < 0) then balance (color, (k1,v1), ins l, r)
          else if  (comparison > 0) then balance (color, (k1,v1), l, ins r)
          else Node (color, (k,v), l, r) in
      match ins ok_d with 
      | Node (_, y, l, r) -> Node (Black, y, l, r)
      | BBLeaf
      | Leaf -> Leaf

    (* [rotate tree2] compensates function [balance] to preserve the red-black
       invariants, rectifing the red-red violation that [balance] does not cover.  *)
    let rotate tree2 =
      match tree2 with 
      | Node (Red, (k1, v1), Node (BlackBlack, (k2, v2), l1, r1), Node (Black, (k3, v3), l2, r2)) -> 
        balance (Black, (k3, v3), Node (Red, (k1, v1), Node (Black, (k2, v2), l1, r1), l2), r2)
      | Node (Red, (k1, v1), BBLeaf, Node (Black, (k3, v3), l2, r2)) -> 
        balance (Black, (k3, v3), Node (Red, (k1, v1), Leaf, l2), r2)
      | Node (Red, (k1, v1), Node (Black, (k2, v2), l1, r1), Node (BlackBlack, (k3, v3), l2, r2)) ->
        balance (Black, (k2, v2), l1, Node (Red, (k1, v1), r1, Node (Black, (k3, v3), l2, r2)))
      | Node (Red, (k1, v1), Node (Black, (k2, v2), l1, r1), BBLeaf) ->
        balance (Black, (k2, v2), l1, Node (Red, (k1, v1), r1, Leaf))
      | Node (Black, (k1, v1), Node (BlackBlack, (k2, v2), l1, r1), Node (Black, (k3, v3), l2, r2)) ->
        balance (BlackBlack, (k3, v3), Node (Red, (k1, v1), Node (Black, (k2, v2), l1, r1), l2), r2)
      | Node (Black, (k1, v1), BBLeaf, Node (Black, (k3, v3), l2, r2)) ->
        balance (BlackBlack, (k3, v3), Node (Red, (k1, v1), Leaf, l2), r2)
      | Node (Black, (k1, v1), Node (Black, (k2, v2), l1, r1), Node (BlackBlack, (k3, v3), l2, r2)) ->
        balance (BlackBlack, (k2, v2), l1, Node (Red, (k1, v1), r1, Node (Black, (k3, v3), l2, r2)))
      | Node (Black, (k1, v1), Node (Black, (k2, v2), l1, r1), BBLeaf) ->
        balance (BlackBlack, (k2, v2), l1, Node (Red, (k1, v1), r1, Leaf))
      | Node (Black, (k1, v1), Node (BlackBlack, (k2, v2), l1, r1), Node (Red, (k3, v3), Node (Black, (k4, v4), l2, r2), r3)) ->
        Node (Black, (k3, v3), balance (Black, (k4, v4), Node (Red, (k1, v1), Node (Black, (k2, v2), l1, r1), l2), r2), r3)
      | Node (Black, (k1, v1), BBLeaf, Node (Red, (k3, v3), Node (Black, (k4, v4), l2, r2), r3)) ->
        Node (Black, (k3, v3), balance (Black, (k4, v4), Node (Red, (k1, v1), Leaf, l2), r2), r3)
      | Node (Black, (k1, v1), Node (Red, (k2, v2), l1, Node (Black, (k3, v3), l2, r2)), Node (BlackBlack, (k4, v4), l3, r3)) ->
        Node (Black, (k2, v2), l1, balance (Black, (k3, v3), l2, Node (Red, (k1, v1), r2, Node (Black, (k4, v4), l3, r3))))
      | Node (Black, (k1, v1), Node (Red, (k2, v2), l1, Node (Black, (k3, v3), l2, r2)), BBLeaf) ->
        Node (Black, (k2, v2), l1, balance (Black, (k3, v3), l2, Node (Red, (k1, v1), r2, Leaf)))
      | Node (a, b, c, d) -> 
        Node (a, b, c, d)
      | Leaf -> 
        tree2
      | BBLeaf -> 
        tree2

    (* [min_delete tree4] extracts and deletes the minimum node of the 
       red-black tree [tree4].  *)
    let rec min_delete tree4 =
      match tree4 with
      | BBLeaf 
      | Leaf -> failwith "Empty tree!"
      | Node (Red, (k1,v1), Leaf, Leaf) -> ((k1,v1), Leaf)
      | Node (Black, (k1,v1), Leaf, Leaf) -> ((k1,v1), BBLeaf)
      | Node (Black, (k1, v1), Leaf, Node(Red, (k2, v2), l, r)) -> ((k1,v1), Node(Black, (k2, v2), l, r))
      | Node (c, (k1, v1), l, r) -> let (k'', l'') = (min_delete l) in (k'', rotate (Node(c, (k1, v1), l'', r)))

    (* [redden tree3] reddens the black nodes before the deletion to preserve
       the red-black invariants.  *)
    let redden tree3 = 
      match tree3 with 
      | Node (Black, (k1, v1), Leaf, Leaf) -> Node (Red, (k1, v1), Leaf, Leaf)
      (* | Node (Black, (k1, v1), Node (Black, (k2, v2), l2, r2), Leaf) -> Node (Red, (k1, v1), Node (Black, (k2, v2), l2, r2), Leaf)
         | Node (Black, (k1, v1), Leaf, Node (Black, (k2, v2), l2, r2)) -> Node (Red, (k1, v1), Leaf, Node (Black, (k2, v2), l2, r2)) *)
      | Node (Black, (k1, v1), Node (Black, (k2, v2), l2, r2), Node (Black, (k3, v3), l3, r3)) -> Node (Red, (k1, v1), Node (Black, (k2, v2), l2, r2), Node (Black, (k3, v3), l3, r3))
      | Node (a, b, c, d) -> Node (a, b, c, d)
      | _ -> tree3

    let rec find k d =
      match d with 
      | BBLeaf
      | Leaf -> None
      | Node (_, (k1,v1), l, r) ->
        let comparison = compare_k k k1 in
        if comparison = 0 then Some v1 
        else if comparison < 0 then find k l
        else find k r

    let remove k d =
      let ok_d = rep_ok d in
      let rec del key tree1 = 
        match tree1 with 
        | BBLeaf
        | Leaf -> Leaf
        | Node (Red, (k1, v1), Leaf, Leaf) when (compare_k key k1 = 0) -> Leaf  
        | Node (Black, (k1, v1), Node(Red, (k2, v2), l, r), Leaf) when (compare_k key k1 = 0) -> Node (Black, (k2, v2), l, r)
        | Node (Black, (k1, v1), Leaf, Leaf) when (compare_k key k1 = 0) -> BBLeaf
        | Node (c, (k1,v1), l, r) ->
          let comparison = compare_k key k1 in
          if (comparison < 0) then rotate (Node (c, (k1,v1), del key l, r))
          else if  (comparison > 0) then rotate (Node (c, (k1,v1), l, del key r))
          else let (k', r') = min_delete r in rotate (Node (c, k', l, r'))
      in del k (redden ok_d)

    let rec member k d =
      match d with
      | BBLeaf
      | Leaf -> false
      | Node (_, (k1,v1), l, r) ->
        let comparison = compare_k k k1 in
        comparison = 0 || (comparison < 0 && member k l) 
        || (comparison > 0 && member k r)

    let choose d =
      match d with 
      | BBLeaf 
      | Leaf -> None
      | Node (_, (k,v), l, r) -> Some (k,v)

    let to_list d =
      let rec sort_rtl acc d = 
        match d with 
        | BBLeaf
        | Leaf -> acc
        | Node (_,(k,v),l,r) -> sort_rtl ((k,v)::(sort_rtl acc r)) l in
      sort_rtl [] d

    let rec fold f acc (d: t) =
      match d with 
      | BBLeaf
      | Leaf -> acc
      | Node (_,(k1,v1),l,r) -> f k1 v1 (fold f (fold f acc r) l) 

  end