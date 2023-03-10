open Tree_diff_lib.Tree23
(* let _ = diff "" "" false *)
let%expect_test _ =
  (* test 1 *)
  diff "(Node3 (Leaf t) (Node2 (Leaf u) (Leaf v)) (Node2 (Leaf w) (Leaf x)))" "(Node3 (Leaf t) (Node2 (Leaf v) (Leaf u)) (Node2 (Leaf w') (Leaf x)))" true;
  [%expect{|
  Tree1
  (Node3 (Leaf t) (Node2 (Leaf u) (Leaf v)) (Node2 (Leaf w) (Leaf x)))
  Tree2
  (Node3 (Leaf t) (Node2 (Leaf v) (Leaf u)) (Node2 (Leaf w') (Leaf x)))
  Patch
  (Node3 (Leaf t) (Hole ((Node2 (Hole 1) (Hole 0)) (Node2 (Hole 0) (Hole 1))))
   (Node2 (Hole ((Leaf w) (Leaf w'))) (Leaf x)))
  Hole 0
  (Leaf v)
  Hole 1
  (Leaf u) |}];
  (* test 2 *)
  diff "(Node2 (Leaf t) (Leaf v))" "(Node3 (Leaf t) (Node2 (Leaf v) (Leaf u)) (Node2 (Leaf w') (Leaf x)))" true;
  [%expect{|
  Tree1
  (Node2 (Leaf t) (Leaf v))
  Tree2
  (Node3 (Leaf t) (Node2 (Leaf v) (Leaf u)) (Node2 (Leaf w') (Leaf x)))
  Patch
  (Hole
   ((Node2 (Hole 1) (Hole 0))
    (Node3 (Hole 1) (Node2 (Hole 0) (Leaf u)) (Node2 (Leaf w') (Leaf x)))))
  Hole 0
  (Leaf v)
  Hole 1
  (Leaf t) |}];
  (* test 3 *)
  diff "(Node3 (Leaf t) (Node3 (Leaf t) (Node2 (Leaf u) (Leaf v)) (Node2 (Leaf v) (Leaf u))) (Node2 (Leaf w) (Leaf x)))" "(Node3 (Leaf t) (Node2 (Leaf v) (Leaf u)) (Node2 (Leaf w') (Leaf x)))" true;
  [%expect{|
  Tree1
  (Node3 (Leaf t)
   (Node3 (Leaf t) (Node2 (Leaf u) (Leaf v)) (Node2 (Leaf v) (Leaf u)))
   (Node2 (Leaf w) (Leaf x)))
  Tree2
  (Node3 (Leaf t) (Node2 (Leaf v) (Leaf u)) (Node2 (Leaf w') (Leaf x)))
  Patch
  (Hole
   ((Node3 (Hole 2) (Node3 (Hole 2) (Node2 (Leaf u) (Leaf v)) (Hole 1))
     (Node2 (Leaf w) (Hole 0)))
    (Node3 (Hole 2) (Hole 1) (Node2 (Leaf w') (Hole 0)))))
  Hole 0
  (Leaf x)
  Hole 1
  (Node2 (Leaf v) (Leaf u))
  Hole 2
  (Leaf t) |}];
  (* test 4 *)
  diff "(Node3 (Leaf t) (Node3 (Leaf u) (Node2 (Leaf u) (Leaf v)) (Node2 (Leaf v) (Leaf u))) (Node2 (Leaf w) (Leaf x)))" "(Node3 (Leaf t) (Node2 (Leaf v) (Leaf u)) (Node2 (Leaf w') (Leaf x)))" true;
  [%expect{|
  Tree1
  (Node3 (Leaf t)
   (Node3 (Leaf u) (Node2 (Leaf u) (Leaf v)) (Node2 (Leaf v) (Leaf u)))
   (Node2 (Leaf w) (Leaf x)))
  Tree2
  (Node3 (Leaf t) (Node2 (Leaf v) (Leaf u)) (Node2 (Leaf w') (Leaf x)))
  Patch
  (Node3 (Leaf t)
   (Hole ((Node3 (Leaf u) (Node2 (Leaf u) (Leaf v)) (Hole 0)) (Hole 0)))
   (Node2 (Hole ((Leaf w) (Leaf w'))) (Leaf x)))
  Hole 0
  (Node2 (Leaf v) (Leaf u)) |}];
  (** Inter vs union in closure
  After removing the identicial substitutions, get common subterms by "get_source" from
  Node2 (
    Hole (
      (Node2 (a Hole0))
      (Node2 (Hole0 Hole1)))
    c)
  However, source does not contain Hole1 and causes error in reordering in the future. *)
  diff "(Node2 (Node2 (Leaf a) (Leaf b)) (Leaf c))" "(Node2 (Node2 (Leaf b) (Leaf c)) (Leaf c))" true;
  [%expect{|
  Tree1
  (Node2 (Node2 (Leaf a) (Leaf b)) (Leaf c))
  Tree2
  (Node2 (Node2 (Leaf b) (Leaf c)) (Leaf c))
  Patch
  (Hole
   ((Node2 (Node2 (Leaf a) (Hole 1)) (Hole 0))
    (Node2 (Node2 (Hole 1) (Hole 0)) (Hole 0))))
  Hole 0
  (Leaf c)
  Hole 1
  (Leaf b) |}];