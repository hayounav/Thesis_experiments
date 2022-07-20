-------------------------------- MODULE foo --------------------------------
EXTENDS Naturals

CONSTANTS System, Insert, Delete, Contains, Addresses, root, bottom, Keys, infty, undef

VARIABLES Nodes, left, right, key, pc, locks, input

vars == << Nodes, left, right, key, pc, locks, input >>

ProcSet == {Contains} \cup {Delete} \cup {Insert} \cup {System}

TypeOK == 
        /\ infty \in Keys /\ (\A k \in Keys : k < infty) 
        /\ Nodes \subseteq Addresses
        /\ key \in [Nodes -> Keys]
        /\ left \in [Nodes -> Nodes]
        /\ right \in [Nodes -> Nodes]
        /\ root \in Nodes
        /\ locks \in [Nodes -> {"free"} \cup ProcSet]
        /\ input \in [ProcSet -> Keys \cup {undef}]
        
Init ==
        /\ Nodes = {root}
        /\ key = [root |-> infty]
        /\ left = [root |-> bottom]
        /\ right = [root |-> bottom]
        /\ pc = [p \in ProcSet |-> "m0"]
        /\ locks = [nd \in Nodes |-> "free"]
        /\ input = [p \in ProcSet |-> undef]
        

c1 == pc[Contains] = "c1"
contains == c1

d1 == pc[Delete] = "d1"
d2 == pc[Delete] = "d2"
d3 == pc[Delete] = "d3"
delete == d1 \/ d2 \/ d3

i1 == pc[Delete] = "i1"
i2 == pc[Delete] = "i2"
i3 == pc[Delete] = "i3"
insert == i1 \/ i2 \/ i3

v1 == pc[System] = "v1"
v2 == pc[System] = "v2"
v3 == pc[System] = "v3"
v4 == pc[System] = "v4"
v5 == pc[System] = "v5"
v6 == pc[System] = "v6"
remove == v1 \/ v2 \/ v3 \/ v4 \/ v5 \/ v6

f1 == pc[System] = "f1"
f2 == pc[System] = "f2"
f3 == pc[System] = "f3"
f4 == pc[System] = "f4"
f5 == pc[System] = "f5"
f6 == pc[System] = "f6"
rotateLeft == f1 \/ f2 \/ f3 \/ f4 \/ f5 \/ f6

r1 == pc[System] = "r1"
r2 == pc[System] = "r2"
r3 == pc[System] = "r3"
r4 == pc[System] = "r4"
r5 == pc[System] = "r5"
r6 == pc[System] = "r6"
rotateRight == r1 \/ r2 \/ r3 \/ r4 \/ r5 \/ r6

system == remove \/ rotateLeft \/ rotateRight

m0 == /\ LET p == CHOOSE f \in ProcSet : pc[f] = "m0"
             k == CHOOSE l \in Keys : TRUE
         IN
            CASE p = Insert -> pc' = [pc EXCEPT ![p] = "i1"] /\ input' = [input EXCEPT ![p] = k]
            [] p = Delete -> pc' = [pc EXCEPT ![p] = "d1"] /\ input' = [input EXCEPT ![p] = k]
            [] p = Contains -> pc' = [pc EXCEPT ![p] = "c1"] /\ input' = [input EXCEPT ![p] = k]
            [] p = System -> LET nxt == CHOOSE f \in {"r1", "f1", "v1"} : TRUE IN
                                 /\ pc' = [pc EXCEPT ![p] = nxt] /\ UNCHANGED input
            [] OTHER -> FALSE
      /\  UNCHANGED << Nodes, left, right, key, locks >>

AnyAction == ((system \/ insert \/ delete)  /\ UNCHANGED vars) \/ m0
     

Next == AnyAction

Spec == Init /\ [][Next]_vars
=============================================================================
\* Modification History
\* Last modified Thu Sep 02 15:01:49 IDT 2021 by hayounav
\* Created Thu Sep 02 13:15:57 IDT 2021 by hayounav
