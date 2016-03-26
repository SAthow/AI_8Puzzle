

; Node structure: stores state and parent.
	(defstruct node state parent fN gN hN)
    
;goal state (needs to be initialized depending on the size of the puzzle being solved)
    (defvar *GOAL* (make-node :state (1 2 3 8 0 4 7 6 5) :parent nil )

; Test if two nodes have the same state.
;	(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))
;------------------------------------------------------------------------------



;------------------------------------------------------------------------------
; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
;------------------------------------------------------------------------------
;(defun build-solution (node node-list)
;	(do
;		((path (list (node-state node))))        ; local loop var
;		((null (node-parent node)) path)         ; termination condition

		; find the parent of the current node
;		(setf node (member-state (node-parent node) node-list))

		; add it to the path
;		(setf path (cons (node-state node) path))
;	)
;)

(load "search.lsp" )
(load "AI_8Puzzle.lsp" )


(defun BestSuccessor (OPEN)
    (let minf minfpos
        (setf minf -1)
        (setf minfpos -1)
        (dolist (node OPEN) 
            ;if the current node has a smaller fN value than the currently tracked fN
            (cond ((< (node-fN node) minf) 
                  ;set the new minimum fN value to be tracked
                  (setf minf (node-fN node ) )
                  ;track the position of the node with the "best" fN value
                  (setf minfpos (position 0 state :test #'equal))
                )
            )  
        )   
        (nth minfpos OPEN) ; return the state a minfpos in the OPEN list as the best
    )
)



(defun aStar (start) 
    (let hN  ; fN = gN + hN  MIGHT NOT NEED THIS HERE
        
        (currNode (make-node :state start :parent nil :gN 0 :) )  ;create node for start state
        
        (setf (node-hN currNode) (heuristic (node-state currNode) *GOAL* 3 ) ) ; set the hN value for current node, n = 3 for now
        
        (setf (node-fN currNode) (+ (node-hN currNode) (node-gN) ) ) ; set fN value for currNode
        
	    (OPEN (list currNode))  ; put start node on OPEN list
        
	    (CLOSED nil)  ;initialize CLOSED list
             

        (loop while ( > (length OPEN ) 0 )     ; loop until open list is empty
            (setf currNode (BestSuccessor (node-state OPEN ) )); grab the best node from the successors of the open list
            
            (delete currNode 'OPEN ) ;take currNode off of OPEN list
            
            (setf (car CLOSED) currNode ); put currNode onto CLOSED list
            
            ;if the current state is a goal state, return success (likely return a list of states or something (not T))
            (when (equal-states  (node-state currNode ), (node-state GOAL) ) (build-solution currNode CLOSED) ); 
            
            (dolist (child (gen_successors (node-state currNode )))  ;for each successor of currNode
                ;initialize some of the child node
               (setf child (make-node :state child :parent (node-state currNode) :gN (1+ (node-gN currNode ))))
               (setf (node-hN child) (heuristic (node-state child) OPEN n ) ) ;set child hN
               (setf (node-fN child) (+ (node-gN child) (node-hN child) ) ) ;set child fN
               (setf (node-parent child ) (node-state currNode ) ); set the parent of the child node
               
                ;if the child is not in the open or closed list
                (cond ((and (not (find child 'OPEN ) ) (not (find 'child 'CLOSED ) ) ) 
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