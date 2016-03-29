

; Node structure: stores state and parent.
	(defstruct starNode state parent fN gN hN)

    
;goal state (needs to be initialized depending on the size of the puzzle being solved)
    (defvar *GOALSTATE* (make-starNode :state '(1 2 3 8 0 4 7 6 5) :parent nil :fN nil :gN nil :hN nil ) )

    (defvar *START* (make-starNode :state '(1 3 4 8 6 2 7 0 5) :parent nil :fN nil :gN nil :hN nil) )

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

;(load "search.lsp" )
(load "AI_8Puzzle.lsp" )

; Member-state looks for a node on the node-list with the same state.
(defun mem-state (state anode-list)
	(dolist (anode anode-list)
	(when (equal state (starNode-state anode)) (return anode))
	)
)

;------------------------------------------------------------------------------
; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
;------------------------------------------------------------------------------
(defun solution-path(anode anode-list)
    (print anode-list )
    
	(do
		((path (list (starNode-state anode))))        ; local loop var
		((null (starNode-parent anode)) path)         ; termination condition

		; find the parent of the current node
		(setf node (mem-state (starNode-parent anode) anode-list))

		; add it to the path
		(setf path (cons (starNode-state anode) path))
	)
    
)

(defun matching-states (n1 n2)
    (equal (starNode-state n1) (starNode-state n2) ) 
)


(defun BestSuccessor (OPEN) ;
    (let (minf minfpos) ; declare local variables
        (setf minf 1000) ; initialze minf value to something large
        (setf minfpos 0)  ; initialze minfpos value to something
        (dolist (node OPEN)
            ;if the current node has a smaller fN value than the currently tracked fN
            (cond ((> minf(starNode-fN node) )
                  (print "min fN")
                   (print minf)
                  ;set the new minimum fN value to be tracked
                  (setf minf (starNode-fN node ) )
                  (print minf)
                  ;track the position of the node with the "best" fN value
                  (setf minfpos (position node OPEN :test #'equal))
                  (print "min fN position")
                  (print minfpos)
                )
                 
            )  
        )   
        (nth minfpos OPEN) ; return the state a minfpos in the OPEN list as the best
    )
)


(defun aStar (start) 
    (let 
        (   ; local variables
            (n 3) ; TODO: get from input, not constant 3
            (currNode (make-starNode :state start :parent nil :fN 0 :gN 0 :hN 0 ) )
            (OPEN nil)
            (CLOSED nil)
        )

        (setf (starNode-hN currNode) (ad1 (starNode-state currNode) start)) ; set the hN value for current node, n = 3 for now

        (setf (starNode-fN currNode) (+ (starNode-hN currNode) (starNode-gN currNode) ) ) ; set fN value for currNode
        (setf OPEN (list currNode )) ; put first node on the open list
        ;(print currNode)
        (loop while ( > (length OPEN ) 0) do   ; loop until open list is empty
            ;(print (length OPEN) )
            ;(print "OPEN LIST before: " )
            ;(print OPEN)
            (setf currNode  (BestSuccessor OPEN ) ) ; grab the best node from the successors of the open list
            ;(print (starNode-state currNode))
            (setf OPEN (delete currNode OPEN)) ;take currNode off of OPEN list
            ;(print "OPEN LIST after delete: " )
            ;(print OPEN)
            ; put currNode onto CLOSED list
            
            ;(if 
                ;(= (length CLOSED) 0 ) ; if the closed list is empty
                ;(setf CLOSED (cons currNode nil) ) ; must be a cons cell to become a "proper list"
                ;(setf (car CLOSED) currNode ) ; else add to front of closed list
            ;)
            (setf CLOSED (cons currNode CLOSED) )
            ;(print "CLOSED LIST:")

            ;if the current state is a goal state, return the solution path
            (when (matching-states currNode *GOALSTATE*) (solution-path currNode CLOSED)) 

            (dolist (child (generate-successors (starNode-state currNode )))  ;for each successor of currNode
                ;initialize the child node
               (setf child (make-starNode :state child :parent (starNode-state currNode) :gN (1+ (starNode-gN currNode ))))
               (setf (starNode-hN child) (ad1 (starNode-state child) OPEN n ) ) ;set child hN
               (setf (starNode-fN child) (+ (starNode-gN child) (starNode-hN child) ) ) ;set child fN
               (setf (starNode-parent child ) (starNode-state currNode ) ); set the parent of the child node

                ;if the child is not in the open or closed list
               (cond 
                   ((and (not (member child OPEN :test #'matching-states)) ; if the child is not on either OPEN or CLOSED
                    (not (member child CLOSED :test #'matching-states)))
                        ;(setf (car OPEN ) child ) ;add the child to the start of open list
                        (setf OPEN (cons child OPEN) )
                        (setf *distinctNodes* (1+ *distinctNodes* ))  ; increment the distinct node counter 
                    )
                      
                    ;(when (find child OPEN ) ; if the child is on the open list
                   (;(find child OPEN)
                    (member child OPEN :test #'matching-states)
                         ;(print "Child was found on OPEN list")
                        ;update F' of child and parent of child
                        ;(setf (starNode-gN child) (1+ (starNode-gN currNode ) ) )
                        (setf (starNode-hN child) (ad1 (starNode-state child ) (starNode-state *GOALSTATE*) n ) )
                        (setf (starNode-fN child) (+ (starNode-gN child) (starNode-hN child ) ) ) ; fN = gN + hN
                        (setf (starNode-parent child) (starNode-state currNode ) )
                    )
                     
                   (;(find child CLOSED) ; if the child is on the closed list
                    (member child CLOSED :test #'matching-states)
                        
                        ;update F' of child and parent of child
                        ;(setf (starNode-gN child) (1+ (starNode-gN currNode ) ) )
                        (setf (starNode-hN child) (ad1 (starNode-state child ) (starNode-state *GOALSTATE*) n ) )
                        (setf (starNode-fN child) (+ (starNode-gN child) (starNode-hN child ) ) ) ; fN = fN + hN
                        (setf (starNode-parent child) (starNode-state currNode ) )
                        
                        (setf CLOSED (delete child CLOSED));take currNode off of CLOSED list
                        (setf OPEN (cons child OPEN) ); put currNode onto OPEN list
                    )
                    ;)
                )
            )
        )
        nil ; return nil if success not returned prior
    )
)

;(aStar *START*)