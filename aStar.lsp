;goal state (needs to be initialized depending on the size of the puzzle being solved
(defvar *GOAL* )
; Node structure: stores state and parent.
	(defstruct node state parent)

; Test if two nodes have the same state.
	(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))
;------------------------------------------------------------------------------

(defun aStar (start) 
    (let fN
        (currNode (make-node :state start :parent nil))  ; create node for start state
	    (OPEN (list currNode))                           ; put start node on OPEN list
	    (CLOSED nil)                                     ;initialize CLOSED list

        (loop while ( > (length OPEN ) 0 )     ; loop until open list is empty
            (setf currNode (BestSuccessor OPEN ) ); grab the best node from the successors of the open list
            
            ;TAKE currNode OFF OF OPEN LIST
            
            (setf (car CLOSED) currNode ); put currNode onto CLOSED list
            
            (when equal-states ( currNode, GOAL ) 'T ); if the current state is a goal state, return success (likely return a list of states or something (not T))
            
            (loop for i in (gen_successors currNode ) do  ;for each successor of currNode
               
                ; if the child is not in the open or closed list
                (when (and (not (find 'i 'OPEN ) ) (not (find 'i 'CLOSED ) ) ) 
                    ( setf (car OPEN ) i ); add the child to the start of open list
                )
                
                (when (find i 'OPEN ) ; if the child is on the open list
                    ;update F' of currNode and parent of currNode
                )
                
                (when (find i 'CLOSED ) ; if the child is on the closed list
                    ;update F' of currNode and parent of currNode and move currNode from closed to open
                )
            
            )
        )
        'nil ; return nil if success not returned prior
    )
)