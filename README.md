# coding-practices

this repository includes implementations of data structures and algorithms from scratch in scala

## data structure
* linked list
* doubly linked list
  * with O(1) time complexity deletion when the address of the value is known
* binary tree
* hash map
  * automatically double the lookup table size when occupancy exceeds a loading factor of 0.7
  * each cell in the lookup table is filled with a doubly linked list, allowing O(k) time complexity insertion and O(1) time complexity deletion where k is the number of collisions for the same hashed key
* binary heap
  * use hash map as a lookup table instead of array, allowing dynamically scaled lookup table size
* disjoint set
  * with path compression and union by rank
* segment tree
  * get range sum or range min in O(log(n)) time complexity and update value in O(log(n)) time complexity in integer array where n is the array size
  * segment tree is constructed in 2n-1 time complexity

## algorithm
* merge sort
* egg drop problem
  * solve for the minimum number of experiments required for determining the pivotal floor, with both bottom up and top down approach
* dijkstra algorithm
  * solve for a path from source to any given vertex with the shortest distance, restricted to problems where distance between any vertices is non-negative 
  * implement with priority queue where we give priority to the shortest distance from source to the given vertex, allowing O(E * log(V)) build time where E is the number of edges and V is the number of vertices

## cache
* least recently used cache
  * use doubly linked list to store the key value pair
  * use hash map to store the key and the address of the key value pair in doubly linked list
  * with above, allowing updating the most recently used element in O(1) time complexity by appending the element to the tail of doubly linked list
  * whenever the cache has cached elements exceeding the configured limit, evicting the least recently used element in O(1) time complexity by popping the element in the front of doubly linked list
  
## cipher
* aes
  * expose only encrypt/decrypt method for arbitrary string
  * the parameters for building the cipher is made configurable from application.conf
