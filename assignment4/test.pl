
segment(A,L):-prefix(P,L), suffix(S,L), append(P,A,W), append(W,S,Z), Z = L.