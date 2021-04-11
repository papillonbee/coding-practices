# coding-practices

this repository includes implementations of data structures and algorithms from scratch in scala

## data structure
* linked list
* doubly linked list
  * with O(1) deletion when the address of the value is known
* binary tree
* hash map
  * automatically double the lookup table size when occupancy exceeds a loading factor of 0.7
  * each cell in the lookup table is filled with a doubly linked list, allowing O(k) insertion and O(1) deletion where k is the number of collisions for the same hashed key
* binary heap
  * use hash map as a lookup table instead of array, allowing dynamically scaled lookup table size

## algorithm
* merge sort

## cache
* least recently used cache
  * use doubly linked list to store the key value pair
  * use hash map to store the key and the address of the key value pair in doubly linked list
  * with above, allowing O(1) update of appending the most recently used to the tail of doubly linked list
  * whenever the cache has cached elements exceeding the configured limit, popping the least recently used element in O(1)
  
