Simplest possible Cram test
  $ cd /home/zhangliao/tree_diff1
  $ _build/default/bin/tree23_main.exe -file data/tree23.dat -context
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
  (Node2 (Leaf v) (Leaf u))
  
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
  (Leaf t)
  
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
  (Leaf t)
  
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
  (Leaf u)
  
