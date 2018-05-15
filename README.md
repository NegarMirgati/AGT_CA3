# Algorithmic Graph Theory Project 3
# Undirected Minimum Dominatng Set
Output of greedy algorithm for minimum dominating set of queens : 7
# Directed Minimum Dominating Set
rules :  
1) If an unobserved vertex i has no predecessor in the current digraph D, it is added to set Γ and become occupied All the previously unobserved successors of i then become observed.
2) If an unobserved vertex j has only a single unoccupied predecessor (say vertex k) and no unobserved successor in the current digraph D, vertex k is added to set Γ and become occupied. All the previously unobserved successors of k (including j) then become observed.
3) If an unoccupied but observed vertex l has only a single unobserved successor (say m) in the current digraph D, occupying l is not better than occupying m, therefore the arc (l, m) is deleted from D. We emphasize that vertex m is still unobserved after this arc deletion. (Rule 3 is specific to the dominating set problem and it is absent in the conventional leaf-removal process

** The above-mentioned microscopic rules only involve the local structure of the digraph, they are simple to implement. we can prove that if all the vertices are observed after the GLR process, the constructed vertex set Γ must be a MDS for the original digraph D. If some vertices remain to be unobserved after the GLR process, this set of remaining vertices is unique and is independent of the particular order of the GLR process.


# Vertex Coloring
integer programming tutorial :  
    http://wwwhome.math.utwente.nl/~uetzm/do/IP-FKS.pdf  
    https://ocw.mit.edu/courses/sloan-school-of-management/15-053-optimization-methods-in-management-science-spring-2013/lecture-notes/MIT15_053S13_lec11.pdf
