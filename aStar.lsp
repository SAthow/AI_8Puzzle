

; Node structure: stores state and parent.
	(defstruct node state parent fN gN hN)
    
;goal state (needs to be initialized depending on the size of the puzzle being solved)
    (defvar *GOAL* (make-node :state (1 2 3 8 0 4 7 6 5) :parent nil )

; Test if two nodes have the same state.
	(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
;------------------------------------------------------------------------------
(defun build-solution (node node-list)
	(do
		((path (list (node-state node))))        ; local loop var
		((null (node-parent node)) path)         ; termination condition

		; find the parent of the current node
		(setf node (member-state (node-parent node) node-list))

		; add it to the path
		(setf path (cons (node-state node) path))
	)
)

(defun fN node OPEN

)



(defun aStar (start) 
    (let fN  ; fN = gN + hN  MIGHT NOT NEED THIS HERE
        (currNode (make-node :state start :parent nil :gN 0 :) )  ;create node for start state
	    (OPEN (list currNode))     ; put start node on OPEN list
	    (CLOSED nil)  ;initialize CLOSED list
             

        (loop while ( > (length OPEN ) 0 )     ; loop until open list is empty
            (setf currNode (BestSuccessor (node-state OPEN ) )); grab the best node from the successors of the open list
            
            (delete currNode 'OPEN ) ;take currNode off of OPEN list
            
            (setf (car CLOSED) currNode ); put currNode onto CLOSED list
            
            ;if the current state is a goal state, return success (likely return a list of states or something (not T))
            (when (equal-states  (node-state currNode ), (node-state GOAL) ) (build-solution currNode CLOSED) ); 
            
            (dolist (child (gen_successors (node-state currNode )))  ;for each successor of currNode
               
               (setf (node-parent child ) (node-state currNode ) ); set the parent of the child node
               
                ;if the child is not in the open or closed list
                ( cond ((and (not (find child 'OPEN ) ) (not (find 'child 'CLOSED ) ) ) 
                       (setf (car OPEN ) child ) ;add the child to the start of open list
                       (1+ *distinctNodes* )  ; increment the distinct node counter 
                       )
                )
                
                (when (find child 'OPEN ) ; if the child is on the open list
                    ;update F' of child and parent of child
                )
                
                (when (find child 'CLOSED ) ; if the child is on the closed list
                    ;update F' of child and parent of child and move child from closed to open
                )
            
            )
        )
        nil ; return nil if success not returned prior
    )
)