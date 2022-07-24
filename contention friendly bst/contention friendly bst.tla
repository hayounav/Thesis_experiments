-------------------------------- MODULE foo --------------------------------
EXTENDS Naturals, TLC, Randomization, FiniteSets

CONSTANTS System, Insert, Delete, Contains, Addresses, Keys, infty, ninfty, NO_LOCK

VARIABLES nodes, left, right, key, pc, nd, nxt, k, locked, removed, deleted, prt, lft, r, rl, l, child

vars == << nodes, left, right, key, pc, nd, nxt, k, locked, removed, deleted, prt, lft, r, rl, l, child >>

scheduling_lines == {"m0"}
contains_lines == {"c1", "c2"}
delete_lines == {"d1", "d2lock", "d2"}
insert_lines == {"i1", "i2lock", "i2", "i3lock", "i3"}
rotate_lines == {"f6", "f7", "f8", "f9"}
remove_lines == {"v5", "v6", "v7", "v8"}
system_lines == rotate_lines \cup remove_lines
lines == scheduling_lines \cup contains_lines \cup delete_lines \cup insert_lines \cup system_lines

min_k == CHOOSE j \in Keys : (\A x \in Keys: x = j \/ j < x) 

root == CHOOSE a \in Addresses : TRUE
bottom == CHOOSE a \in Addresses : a /= root

ProcSet == {Insert, Delete, Contains, System}

TypeOK == 
    /\ lft \in {TRUE, FALSE}
    /\ nodes \in SUBSET Addresses
    /\ root \in nodes
    /\ bottom \in nodes
    /\ key \in [nodes -> Keys \cup {infty, ninfty}]
    /\ key[root] = infty
    /\ key[bottom] = ninfty
    /\ left \in [nodes -> nodes]
    /\ left[bottom] = bottom
    /\ right \in [nodes -> nodes]
    /\ right[bottom] = bottom
    /\ right[root] = root
    /\ prt \in nodes
    /\ r \in nodes
    /\ l \in nodes
    /\ rl \in nodes
    /\ child \in nodes
    /\ locked \in [nodes -> ProcSet \cup {NO_LOCK}]
    /\ removed \in SUBSET nodes
    /\ deleted \in SUBSET nodes
    /\ k \in [ProcSet -> Keys]
    /\ nd \in [ProcSet -> nodes]
    /\ nxt \in [ProcSet -> nodes]
    /\ pc \in [ProcSet -> lines]
        
Init ==
        /\ nodes = {root, bottom}
        /\ key = (root :> infty @@ bottom :> ninfty)
        /\ left = [n \in nodes |-> bottom]
        /\ right = (root :> root @@ bottom :> bottom)
        /\ pc = [p \in ProcSet |-> "m0"]
        /\ nd = [p \in ProcSet |-> root]
        /\ nxt = [p \in ProcSet |-> root]
        /\ k = [p \in ProcSet |-> min_k] 
        /\ removed = {}
        /\ deleted = {}
        /\ locked = [n \in nodes |-> NO_LOCK]
        /\ r = root
        /\ prt = root
        /\ rl = root
        /\ l = root
        /\ child = root
        /\ lft = TRUE
        /\ TypeOK
        
        
Unlock(ns, p) == 
    /\ \A n \in ns : locked[n] = p
    /\ locked' = [x \in ns |-> NO_LOCK] @@ locked
    

Lock(ns, p) ==
    /\ \A n \in ns : locked[n] \in {p, NO_LOCK}
    /\ locked' = [n \in ns |-> p] @@ locked
    

LR(n, kv) == IF kv < key[n] THEN left[n] ELSE right[n]


c1 == 
    /\ pc[Contains] = "c1" 
    /\ IF nxt[Contains] = bottom 
        THEN 
            /\ pc' = [pc EXCEPT ![Contains] = "m0"]
            /\ nxt' = nxt 
            /\ nd' = nd
        ELSE
            /\ nd' = [nd EXCEPT ![Contains] = nxt[Contains]]
            /\ IF k[Contains] = key[nd'[Contains]] 
                THEN 
                    /\ pc' = [pc EXCEPT ![Contains] = "c2"]
                    /\ nxt' = nxt
                ELSE 
                    /\ nxt' = [nxt EXCEPT ![Contains] = LR(nd[Contains], k[Contains])]
                    /\ pc' = [pc EXCEPT ![Contains] = "c1"]
    /\ UNCHANGED<< nodes, left, right, key, k, removed, deleted, locked, lft, prt, l, r, rl, child >>
    
c2 == 
    /\ pc[Contains] = "c2"
    /\ pc' = [pc EXCEPT ![Contains] = "m0"]
    /\ UNCHANGED<< nd, nxt, nodes, left, right, key, k, removed, deleted, locked, lft, prt, l, r, rl, child >>

contains == c1 \/ c2



d1 == 
    /\ pc[Delete] = "d1" 
    /\ IF nxt[Delete] = bottom 
        THEN 
            /\ pc' = [pc EXCEPT ![Delete] = "m0"]
            /\ nxt' = nxt 
            /\ nd' = nd
        ELSE
            /\ nd' = [nd EXCEPT ![Delete] = nxt[Delete]]
            /\ IF k[Delete] = key[nd'[Delete]] 
                THEN 
                    /\ pc' = [pc EXCEPT ![Delete] = "d2lock"]
                    /\ nxt' = nxt
                ELSE 
                    /\ nxt' = [nxt EXCEPT ![Delete] = LR(nd[Delete], k[Delete])]
                    /\ pc' = [pc EXCEPT ![Delete] = "d1"]
    /\ UNCHANGED<< nodes, left, right, key, k, removed, deleted, locked, lft, prt, l, r, rl, child >>

d2lock ==
    /\ pc[Delete] = "d2lock"
    /\ Lock({nd[Delete]}, Delete)
    /\ pc' = [pc EXCEPT ![Delete] = "d2"]
    /\ UNCHANGED<< nodes, left, right, key, nd, nxt, k, removed, deleted, prt, lft, r, rl, l, child >>
    
d2 == 
    /\ pc[Delete] = "d2"
    /\ locked[nd[Delete]] = Delete
    /\ IF nd[Delete] \in removed THEN
        /\ nd' = [nd EXCEPT ![Delete] = right[nd[Delete]]]
        /\ pc' = [pc EXCEPT ![Delete] = "d1"]
       ELSE
        /\ pc' = [pc EXCEPT ![Delete] = "m0"]
        /\ nd' = nd
    /\ Unlock({nd[Delete]}, Delete)
    /\ UNCHANGED<< nodes, left, right, key, nxt, k, removed, deleted, lft, prt, l, r, rl, child >>

delete == d1 \/ d2lock \/ d2


i1 == 
    /\ pc[Insert] = "i1"
    /\ IF nxt[Insert] = bottom 
        THEN 
            /\ pc' = [pc EXCEPT ![Insert] = "i3lock"]
            /\ nxt' = nxt 
            /\ nd' = nd
        ELSE
            /\ nd' = [nd EXCEPT ![Insert] = nxt[Insert]]
            /\ IF k[Insert] = key[nd'[Insert]] 
                THEN 
                    /\ pc' = [pc EXCEPT ![Insert] = "i2lock"]
                    /\ nxt' = nxt
                ELSE 
                    /\ nxt' = [nxt EXCEPT ![Insert] = LR(nd[Insert], k[Insert])]
                    /\ pc' = [pc EXCEPT ![Insert] = "i1"]
    /\ UNCHANGED<< nodes, left, right, key, k, removed, deleted, locked, lft, prt, l, r, rl, child >>

i2lock ==
    /\ pc[Insert] = "i2lock"
    /\ Lock({nd[Insert]}, Insert)
    /\ pc' = [pc EXCEPT ![Insert] = "i2"]
    /\ UNCHANGED<< nodes, left, right, key, nd, nxt, k, removed, deleted, prt, lft, r, rl, l, child >>
    
i2 == 
    /\ pc[Insert] = "i2"
    /\ locked[nd[Insert]] = Insert
    /\ IF nd[Insert] \in removed THEN
        /\ nd' = [nd EXCEPT ![Insert] = right[nd[Insert]]]
        /\ pc' = [pc EXCEPT ![Insert] = "i1"]
       ELSE
        /\ pc' = [pc EXCEPT ![Insert] = "m0"]
        /\ nd' = nd
    /\ Unlock({nd[Insert]}, Insert)
    /\ UNCHANGED<< nodes, left, right, key, nxt, k, removed, deleted, lft, prt, l, r, rl, child >>

i3lock ==
    /\ pc[Insert] = "i3lock"
    /\ Lock({nd[Insert]}, Insert)
    /\ pc' = [pc EXCEPT ![Insert] = "i3"]
    /\ UNCHANGED<< nodes, left, right, key, nd, nxt, k, removed, deleted, prt, lft, r, rl, l, child >>
          
i3 == 
    /\ pc[Insert] = "i3"
    /\ locked[nd[Insert]] = Insert
    /\ IF nd[Insert] \in removed \/ bottom /= (IF k[Insert] < key[nd[Insert]] THEN left[nd[Insert]] ELSE right[nd[Insert]]) THEN
          /\ pc' = [pc EXCEPT ![Insert] = "i1"]
          /\ left' = left
          /\ right' = right
          /\ key' = key
          /\ nodes' = nodes
          /\ Unlock({nd[Insert]}, Insert)
       ELSE
          \E a \in Addresses :
             /\ a \notin nodes
             /\ nodes' = nodes \cup {a}
             /\ key' = (a :> k[Insert]) @@ key
             \* have to manually unlock here becuase the new node must be added 
             \* to the `locked` function domain as well
             /\ locked' = (a :> NO_LOCK) @@ (nd[Insert] :> NO_LOCK) @@ locked 
             /\ pc' = [pc EXCEPT ![Insert] = "m0"]
             /\ IF k[Insert] < key[nd[Insert]] THEN
                   /\ left' = (a :> bottom) @@ [left EXCEPT ![nd[Insert]] = a] 
                   /\ right' = (a :> bottom) @@ right
                ELSE
                   /\ left' = (a :> bottom) @@ left 
                   /\ right' = (a :> bottom) @@ [right EXCEPT ![nd[Insert]] = a]
    /\ UNCHANGED<<nd, nxt, k, removed, deleted, lft, prt, r, rl, l, child >>

insert == i1 \/ i2lock \/ i2 \/ i3lock \/ i3



f6 == 
    /\ pc[System] = "f6"
    /\ locked[prt] = System
    /\ locked[nd[System]] = System
    /\ locked[r] = System
    /\ \E a \in Addresses : 
        /\ a \notin nodes
        /\ nodes' = nodes \cup {a}
        /\ left' = (a :> l) @@ (r :> a) @@ left
        /\ right' = (a :> rl) @@ right
        /\ key' = (a :> key[nd[System]]) @@ key
        /\ locked' = (a :> NO_LOCK) @@ locked
    /\ pc' = [pc EXCEPT ![System] = "f7"]
    /\ UNCHANGED<< nd, nxt, k, removed, deleted, lft, prt, l, rl, r, child >>

f7 == 
    /\ pc[System] = "f7"
    /\ left' = [left EXCEPT ![nd[System]] = r]
    /\ pc' = [pc EXCEPT ![System] = "f8"]
    /\ UNCHANGED<< nodes, right, key, nd, nxt, k, removed, deleted, locked, lft, prt, l, rl, r, child >>

f8 == 
    /\ pc[System] = "f8"
    /\ IF lft THEN
            /\ left' = [left EXCEPT ![prt] = r]
            /\ right' = right
       ELSE
            /\ left' = left
            /\ right' = [right EXCEPT ![prt] = r]
    /\ pc' = [pc EXCEPT ![System] = "f9"]
    /\ UNCHANGED<< nodes, key, nd, nxt, k, removed, deleted, locked, lft, prt, l, rl, r, child >>
    
f9 == 
    /\ pc[System] = "f9"    
    /\ pc' = [pc EXCEPT ![System] = "m0"]
    /\ removed' = removed \cup {nd[System]}
    /\ Unlock({prt, nd[System], r}, System)
    /\ UNCHANGED<< nodes, left, right, key, nd, nxt, k, lft, deleted, prt, l, rl, r, child >>

rotateLeft == f6 \/ f7 \/ f8 \/ f9



v5 == 
    /\ pc[System] = "v5"
    /\ locked[prt] = System
    /\ locked[nd[System]] = System
    /\ IF lft THEN 
        /\ left' = [left EXCEPT ![prt] = child]
        /\ right' = right
       ELSE
        /\ left' = left
        /\ right' = [right EXCEPT ![prt] = child]
    /\ pc' = [pc EXCEPT ![System] = "v6"]
    /\ UNCHANGED<< nodes, key, nd, nxt, k, lft, removed, deleted, locked, prt, l, rl, r, child >>

v6 == 
    /\ pc[System] = "v6"
    /\ IF left[nd[System]] = bottom THEN
        /\ left' = [left EXCEPT ![nd[System]] = prt]
        /\ right' = right
       ELSE
        /\ left' = left
        /\ right' = [right EXCEPT ![nd[System]] = prt]
    /\ pc' = [pc EXCEPT ![System] = "v7"]
    /\ UNCHANGED<< nodes, key, nd, nxt, k, lft, removed, deleted, locked, prt, l, rl, r, child >>

v7 == 
    /\ pc[System] = "v7"
    /\ IF left[nd[System]] = prt THEN
        /\ left' = left
        /\ right' = [right EXCEPT ![nd[System]] = prt]
       ELSE
        /\ left' = [left EXCEPT ![nd[System]] = prt]
        /\ right' = right
    /\ pc' = [pc EXCEPT ![System] = "v8"]
    /\ UNCHANGED<< nodes, key, nd, nxt, k, lft, removed, deleted, locked, prt, l, rl, r, child >>

v8 == 
    /\ pc[System] = "v8"    
    /\ pc' = [pc EXCEPT ![System] = "m0"]
    /\ removed' = removed \cup {nd[System]}
    /\ Unlock({prt, nd[System], r}, System)
    /\ UNCHANGED<< nodes, left, right, key, nd, nxt, k, lft, deleted, prt, l, rl, r, child >> 

remove == v5 \/ v6 \/ v7 \/ v8



system == rotateLeft \/ remove


FirstInstruction(p) == 
    CASE p = Contains -> "c1"
      [] p = Delete -> "d1"
      [] p = Insert -> "i1"

m0 == 
    /\ \E p \in ProcSet : 
        /\ pc[p] = "m0"
        /\ CASE p \in {Contains, Delete, Insert} -> 
                /\ pc' = [pc EXCEPT ![p] = FirstInstruction(p)]
                /\ nd' = [nd EXCEPT ![p] = root]
                /\ nxt' = [nxt EXCEPT ![p] = root]
                /\ \E m \in Keys : k' = [k EXCEPT ![p] = m]
                /\ lft' = lft
                /\ prt' = prt
                /\ l' = l
                /\ r' = r
                /\ rl' = rl
                /\ child' = child
                /\ locked' = locked 
           [] p = System ->
                /\ \E inst \in {"f6", "v5"} : 
                    /\ pc' = [pc EXCEPT ![p] = inst]
                    /\ k' = k
                    /\ \E b \in {TRUE, FALSE} : lft' = b
                    /\ \E n \in (nodes \ ({bottom} \cup removed)) : 
                        /\ prt' = n 
                        /\ nd' = [nd EXCEPT ![System] = (IF lft' THEN left[n] ELSE right[n])]
                        /\ nd'[System] \notin {root, bottom}
                        /\ IF inst = "f6" THEN
                            /\ r' = right[nd'[System]]
                            /\ r' /= bottom
                            /\ l' = left[nd'[System]]
                            /\ rl' = left[r']   
                            /\ child' = child
                            /\ Lock({prt', nd'[System], r'}, System)
                           ELSE
                            /\ child' = IF left[nd'[System]] = bottom THEN right[nd'[System]] ELSE left[nd'[System]]
                            /\ bottom \in {left[nd'[System]], right[nd'[System]]}
                            /\ l' = l
                            /\ r' = r
                            /\ rl' = rl
                            /\ Lock({prt', nd'[System]}, System)
                        /\ nxt' = nxt
    /\ UNCHANGED<< nodes, left, right, key, removed, deleted >>
                   

schedule == m0

Next ==  contains \/ delete \/ insert \/ system \/ schedule

Spec == Init /\ [][Next]_vars



--------------------------------------------------------------------------------
(* Sanity check invariant *)
Inv_processes_perform_single_job ==
    /\ pc[System] \in system_lines \cup scheduling_lines
    /\ pc[Insert] \in insert_lines \cup scheduling_lines
    /\ pc[Delete] \in delete_lines \cup scheduling_lines
    /\ pc[Contains] \in contains_lines \cup scheduling_lines


----------------------------------------------------------------------------
(* Contains Control-dependant invariants *)
CD_contains_1_2 == pc[Contains] \in contains_lines => nd[Contains] /= bottom
CD_contains_1 == pc[Contains] = "c1" => key[nd[Contains]] /= k[Contains]
CD_contains_2 == pc[Contains] = "c2" => key[nd[Contains]] = k[Contains]
CD_contains == CD_contains_1_2 /\ CD_contains_1 /\ CD_contains_2

(* Delete Control-dependant invariants *)
CD_delete_1_2 == pc[Delete] \in delete_lines => nd[Delete] /= bottom
CD_delete_1 == pc[Delete] = "d1" => (key[nd[Delete]] = k[Delete] => nd[Delete] \in removed)
CD_delete_2 == pc[Delete] = "d2" => locked[nd[Delete]] = Delete /\ key[nd[Delete]] = k[Delete]
CD_delete == CD_delete_1_2 /\ CD_delete_1 /\ CD_delete_2

(* Insert Control-dependant invariants *)
CD_insert_1_3 == pc[Insert] \in insert_lines => nd[Insert] /= bottom
CD_insert_1 == pc[Insert] = "i1" => (key[nd[Insert]] = k[Insert] => nd[Insert] \in removed)
CD_insert_2 == pc[Insert] = "i2" => locked[nd[Insert]] = Insert /\ key[nd[Insert]] = k[Insert]
CD_insert_3 == pc[Insert] = "i3" => locked[nd[Insert]] = Insert /\ key[nd[Insert]] /= k[Insert] /\ nxt[Insert] = bottom
CD_insert == CD_insert_1_3 /\ CD_insert_1 /\ CD_insert_2 /\ CD_insert_3

CD_worker == CD_contains /\ CD_delete /\ CD_insert


----------------------------------------------------------------------------
(* RotateLeft Control-dependant invariants *)
CD_rotate_6_9 == pc[System] \in rotate_lines => 
                    /\ prt /= bottom /\ prt \notin removed
                    /\ nd[System] /= bottom /\ nd[System] \notin removed
                    /\ r = right[nd[System]] /\ r /= bottom /\ r \notin removed
                    /\ locked[nd[System]] = System /\ locked[prt] = System /\ locked[r] = System
                    /\ nd[System] /= prt 
CD_rotate_6_8 == pc[System] \in (rotate_lines \ {"f9"}) => IF lft THEN nd[System] = left[prt] ELSE nd[System] = right[prt]
CD_rotate_6 == pc[System] = "f6" => rl = left[r] /\ l = left[nd[System]]
CD_rotate_7_9 == pc[System] \in (rotate_lines \ {"f6"}) => 
                    LET new == left[r] IN
                        /\ key[new] = key[nd[System]]
                        /\ new /= nd[System]
                        /\ new \in deleted <=> nd[System] \in deleted
                        /\ left[new] = l /\ right[new] = rl 
CD_rotate_7 == pc[System] = "f7" => left[nd[System]] = l 
CD_rotate_8_9 == pc[System] \in {"f8", "f9"} => left[nd[System]] = r
CD_rotate_9 == pc[System] = "f9" =>  IF lft THEN r = left[prt] ELSE r = right[prt]
CD_rotate == CD_rotate_6_9 /\ CD_rotate_6_8 /\ CD_rotate_6 /\ CD_rotate_7_9 /\ CD_rotate_7 /\ CD_rotate_8_9 /\ CD_rotate_9


(* Remove Control-dependant invariants *)
CD_remove_5_8 == pc[System] \in remove_lines =>
                    /\ prt /= bottom /\ prt \notin removed
                    /\ nd[System] /= bottom /\ nd[System] \notin removed
                    /\ locked[nd[System]] = System /\ locked[prt] = System
                    /\ nd[System] /= prt
CD_remove_5 == pc[System] = "v5" => 
                    /\ IF lft THEN nd[System] = left[prt] ELSE nd[System] = right[prt]
                    /\ child \in {left[nd[System]], right[nd[System]]}
                    /\ bottom \in {left[nd[System]], right[nd[System]]}
CD_remove_5_6 == pc[System] \in {"v5", "v6"} => 
                    /\ (left[nd[System]] = bottom => right[nd[System]] = child)
                    /\ (left[nd[System]] /= bottom => left[nd[System]] = child)
CD_remove_6_7 == pc[System] \in {"v6", "v7"} =>
                    /\ child \in {left[nd[System]], right[nd[System]]}
                    /\ child \in {left[prt], right[prt]}
                    /\ nd[System] \notin {left[prt], right[prt]}
CD_remove_7_8 == pc[System] \in {"v7", "v8"} => prt \in {left[nd[System]], right[nd[System]]}
CD_remove == CD_remove_5_8 /\ CD_remove_5 /\ CD_remove_5_6 /\ CD_remove_6_7 /\ CD_remove_7_8

CD_system == CD_rotate /\ CD_remove


----------------------------------------------------------------------------
(* Step-property 4.7 *)
SP_4_7 == 
    [][\E x,y,z\in Addresses : 
        (/\ x /= y /\ y /= z /\ x /= z
         /\ \/ left[x] = y /\ left'[x] = z
            \/ right[x] = y /\ right'[x] = z) =>
        (/\ pc[Insert] = "i3" /\ pc[Insert] = "m0"
         /\ y = bottom /\ x = nd[Insert] /\ x\notin removed
         /\ z\notin nodes /\ z\in nodes'
         /\ left'[z]=bottom /\ right'[z]=bottom)]_<<right,left,nodes>>
              

=============================================================================
\* Modification History
\* Last modified Sun Jul 24 18:23:55 IDT 2022 by hayounav
\* Created Thu Sep 02 13:15:57 IDT 2021 by hayounav
