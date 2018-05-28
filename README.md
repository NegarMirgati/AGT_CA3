# Algorithmic Graph Theory Project 3
In graph theory, a dominating set for a graph G = (V, E) is a subset D of V such that every vertex not in D is adjacent to at least one member of D. The domination number γ(G) is the number of vertices in a smallest dominating set for G. The dominating set problem concerns testing whether γ(G) ≤ K for a given graph G and input K; it is a classical NP-complete decision problem in computational complexity theory (Garey & Johnson 1979). Therefore it is believed that there may be no efficient algorithm that finds a smallest dominating set for all graphs, although there are efficient approximation algorithms, as well as both efficient and exact algorithms for certain graph classes.  
![dominating sets](https://upload.wikimedia.org/wikipedia/commons/e/e1/Dominating-set.svg)  
  
    
A minimum dominating set of an n-vertex graph can be found in time O(2<sup>n</sup>n) by inspecting all vertex subsets. Fomin, Grandoni & Kratsch (2009) show how to find a minimum dominating set in time O(1.5137<sup>n</sup>) and exponential space, and in time O(1.5264<sup>n</sup>) and polynomial space. A faster algorithm, using O(1.5048<sup>n</sup>) time was found by van Rooij, Nederlof & van Dijk (2009), who also show that the number of minimum dominating sets can be computed in this time. The number of minimal dominating sets is at most 1.7159<sup>n</sup> and all such sets can be listed in time O(1.7159<sup>n</sup>) (Fomin et al. 2008).  

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
