
#|**************************** ASTAR.LSP *************************************

 A* function that utilizes 2 admissable and 1 inadmissable heuristics, located
 in AI_8Puzzle.lsp

 Author: Luke Meyer
 Written Spring 2016 

 
*****************************************************************************|#

(load "AI_8Puzzle.lsp" )

#|*****************************************************************************
  Function: mem-state

  Author: Dr. John Weiss, slightly modified by Luke Meyer

  Description: Looks for a node on the node-list with the same state.

  Args:
	state:		potential state generated
	anode-list:	list of nodes, each node contains a state, parent of state and depth generated
*****************************************************************************|#
(defun mem-state (state anode-list)
	(dolist (anode anode-list)
	(when (equal state (starNode-state anode)) (return anode))
	)
)

#|*****************************************************************************
  Function: solution-path

  Author: Dr. John Weiss, slightly modified by Luke Meyer

  Description: solution-path takes a state and a list of (state parent) pairs
	and constructs the list of states that led to the current state
	by tracing back through the parents to the start node (nil parent).

  Args:
	anode:		goal node with parent field pointing to successor
	anode-list:	list of expanded nodes
*****************************************************************************|#
(defun solution-path(anode anode-list)
    ;(print anode-list )
    
    ;T
    
    (let (path_passed)
    
	(do
		((path (list (starNode-state anode))))        ; local loop var
		((null (starNode-parent anode)) path)         ; termination condition

		; find the parent of the current node
		(setf anode (mem-state (starNode-parent anode) anode-list))
        

		; add it to the path
		(setf path (cons (starNode-state anode) path))
        
        (setf *moveCount* (1-(length path)))
        

        (setf path_passed path)
    
	)
    (format_output path_passed)
    
    ;(print path)
    T
    )
)

#|*****************************************************************************
  Function: matching-states

  Author: Dr. John Weiss, slighly modified by Luke Meyer

  Description:  Test if two nodes have the same state.

  Args:
	n1:	node 1
	n2: node 2
*****************************************************************************|#
(defun matching-states (n1 n2)
    (equal (starNode-state n1) (starNode-state n2) ) 
)


#|*****************************************************************************
  Function: BestSuccessor

  Author: Luke Meyer

  Description:  Returns the "best" successor in the OPEN list, based on the current heuristic

  Args:
	OPEN:	list of un-expanded nodes
*****************************************************************************|#
(defun BestSuccessor (OPEN) ;
    (let ((minf 100000)
         (minfpos -1)) ; declare local variables
        ;(setf minf 1000000) ; initialze minf value to something large
        ;(setf minfpos -1)  ; initialze minfpos value to something
        (dolist (node OPEN)
            ;if the current node has a smaller fN value than the currently tracked fN
            (cond ((> minf(starNode-fN node) )
                  ;(print "min fN")
                  ;set the new minimum fN value to be tracked
                  (setf minf (starNode-fN node ) )
                  ;(print minf)
                  ;track the position of the node with the "best" fN value
                  (setf minfpos (position node OPEN :test #'equal))
                  ;(print "min fN position")
                  ;(print minfpos)
                )
                 
            ) 
        )   
        (nth minfpos OPEN) ; return the state a minfpos in the OPEN list as the best
    )
)


#|*****************************************************************************
  Function: BestSuccessor

  Author: Luke Meyer

  Description:  Returns the "best" successor in the OPEN list, based on the current heuristic

  Args:
	start:	start state of puzzle
    heuristic: flag to tell what heuristic is being applied to the a* run
*****************************************************************************|#
(defun aStar (start heuristic)
    (setf *distinctNodes* 0)
    (setf *generatedCount* 0)
    (setf *expandedCount* 0)
    (let 
        (   ; local variables
            (n 3) 
            (currNode (make-starNode :state start :parent nil :fN 0 :gN 0 :hN 0 ) )
            (OPEN nil)
            (CLOSED nil)
            (tempNode (make-starNode :state nil :parent nil :fN 0 :gN 0 :hN 0 ) )
        )
        

        (cond   ; set the hN value for current node
            ((eq heuristic 'ad1)
                (setf (starNode-hN currNode) (ad1 (starNode-state currNode) ))
            )
            
            ((eq heuristic 'ad2)
                (setf (starNode-hN currNode) (ad2 (starNode-state currNode) ))
            )
            
            ((eq heuristic 'inad1)
                (setf (starNode-hN currNode) (inad1 (starNode-state currNode) )) 
            )   
        )

        (setf (starNode-fN currNode) (+ (starNode-hN currNode) (starNode-gN currNode) ) ) ; set fN value for currNode
        (setf OPEN (list currNode )) ; put first node on the open list

        (loop while ( > (length OPEN ) 0) do   ; loop until open list is empty
          
            (setf currNode  (BestSuccessor OPEN ) ) ; grab the best node from the successors of the open list
            ;(print (starNode-state currNode))
            (setf OPEN (delete currNode OPEN)) ;take currNode off of OPEN list
            
            ; put currNode onto CLOSED list
            (setf CLOSED (cons currNode CLOSED) )
            (setf *expandedCount* (1+ *expandedCount*))

            ;if the current state is a goal state, return the solution path
            (when (matching-states currNode *GOALSTATE*)  (return-from aStar(solution-path currNode CLOSED)))

            (dolist (child (generate-successors (starNode-state currNode )))  ;for each successor of currNode

                ;initialize the child node
               (setf child (make-starNode :state child :parent (starNode-state currNode) :gN (1+ (starNode-gN currNode ))))
                ;set child hN
               (cond   
                 ((eq heuristic 'ad1)
                    (setf (starNode-hN child) (ad1 (starNode-state child) )) 
                 )
            
                 ((eq heuristic 'ad2)
                    (setf (starNode-hN child) (ad2 (starNode-state child) )) 
                 )
            
                 ((eq heuristic 'inad1)
                    (setf (starNode-hN child) (inad1 (starNode-state child) )) 
                 )   
                )

               (setf (starNode-fN child) (+ (starNode-gN child) (starNode-hN child) ) ) ;set child fN

               (setf (starNode-parent child ) (starNode-state currNode ) ); set the parent of the child node
                
                ;if the child is not in the open or closed list
               (cond     
                    ; if the child is on the open list
                    ((member child OPEN :test #'matching-states)
                        (setf tempNode (car (member child OPEN :test #'matching-states))); get the old node
                        (when (< (starNode-fN child) (starNode-fN tempNode))
                           (setf OPEN (delete tempNode OPEN))  ; discard old node
                           (setf OPEN (cons child OPEN) ) ; add new node
                        
                        )   
                    )
                     
                        ; if the child is on the closed list
                    ((member child CLOSED :test #'matching-states)
                        ; get the old node value
                        (setf tempNode (car (member child CLOSED :test #'matching-states)))

                        (when (< (starNode-fN child) (starNode-fN tempNode))
                            ;   (print 5)
                           (setf CLOSED (delete tempNode CLOSED))  ; discard old node
                           
                           (setf OPEN (cons child OPEN) ); put child onto OPEN list
                           ;    (print 6)
                        )
                        
                    )
                    
                    ((and (not (member child OPEN :test #'matching-states)) ; if the child is not on either OPEN or CLOSED
                     (not (member child CLOSED :test #'matching-states)))
                        ;add the child to the start of open list
                        (setf OPEN (cons child OPEN) )
                        (setf *distinctNodes* (1+ *distinctNodes* ))  ; increment the distinct node counter 

                    )
                 )   
                 
                 
              )

            )
            nil ; return nil if success not returned prior
        )      
    
)

