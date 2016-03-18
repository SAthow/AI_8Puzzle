#| 
 | Program: 8-Puzzle
 | Author: Stephanie Athow
 | Class: CSC 447/547 (Artificial Intelligence)
 | Instructor: Dr. John Weiss
 | Due Date: 27 March 2016
 | Description:
 | 		Contains the function definition for the Breadth-First-Search and 
 | 		supporting functions
 |#

; create a structure to hold a state and it's parent
( defstruct node state parent )

; check goal state
( defun is-goal? ( node1 )
	( equal (node-state node1) ( *goal* ) )
)

; Breadth First Search - uses a queue implementation of (state parent) nodes 
; 	for the OPEN list
 


; check to see if state has already been generated
( defun state-created? ( state lst ) 
	( dolist ( node lst )
		( if ( equal-states? state node-state ) (return T) )
	)
)

; test to see if two nodes are equal
( defun equal-states? ( node1 node2 ) 
	( equal ( node-state node1 ) ( node-state node2 ) ) 
)


