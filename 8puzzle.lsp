#|**************************************************************************

 Author: Stephanie Athow, Luke Meyer, Alex Nienhueser


 Description: This program is design to solve an 8-puzzle using BFS 
   (breadth first search), DFID (depth first iterated deepening), and A*.
    The start position may be specified in a puzzle file, or via prompts.
    After solving the puzzle with each search algorithm, a nicely 
    formatted list of positions, leading from the start state to the goal 
    state will be printed, as well as the number of moves required to reach
    the goal state, and the number of nodes generated and expanded 
    
    Bugs: none known


*****************************************************************************|#
#|*****************************************************************************
  Structure: starNode

  Authors: Luke Meyer

  Description: Stores state, parent, and relevant a* stuff
*****************************************************************************|#
(defstruct starNode state parent fN gN hN)

#|*****************************************************************************
  Structure: Node

  Authors: Dr. John Weiss, Stephanie Athow

  Description: Stores state, parent, and depth the state was generated
*****************************************************************************|#
(defstruct node state parent depth)

;global variable to track moves
(defvar *moveCount* 0)
;global variable to track nodes generated
(defvar *generatedCount* 0)
;global variable to track nodes expanded
(defvar *expandedCount* 0)
;global variable to track distinct nodes generated
(defvar *distinctNodes* 0)
;global variable to track duplicate nodes generated
(defvar *duplicateNodes* 0)
;global variable to store the start state
(defvar *start* nil)
;global variable to store the goal state for a*
(defvar *GOALSTATE* (make-starNode :state '(1 2 3 8 0 4 7 6 5) :parent nil :fN nil :gN nil :hN nil ) )
;global variable to store the goal state for bfs and dfid
(defvar *GOAL* '(1 2 3 8 0 4 7 6 5) )
;global variable to store the puzzle dimension
(defvar *n* 3)

(load "AI_8Puzzle.lsp")
(load "aStar.lsp")
(load "getPuzzle.lsp")


#|*****************************************************************************
  Function: 8puzzle

  Author: Luke Meyer

  Description:  main driver function for the program

  Args:
	args:	optional parameters
*****************************************************************************|#
(defun 8puzzle ( &optional (args nil) )

    ;(when ( equal args nil) (return-from 8puzzle nil ))
    
    (getPuzzle args)      ; get them puzzlin states and set start state
    
    (format t "~%~% BFS graph Search ~%")
    (search_bfs_dfs *start* 'bfs) ;run bfs
    
    
    (format t "~%~% DFID graph Search ~%")
    (search_dfid *start*)  ;run depth first search with iterated deepening
    

    (format t "~%~% A* graph Search (heuristic: number out of place) ~%")    
    (aStar *start* 'ad1)   ; run a* with "number out of place" heuristic


    (format t "~%~% A* graph Search (heuristic: Manhattan Distance ) ~%")    
    (aStar *start* 'ad2)   ; run a* with "manhattan distance" heuristic


    (format t "~%~% A* graph Search (heuristic:  Nilsson's Sequence Score) ~%")    
    (aStar *start* 'inad1)   ; run a* with inadmissable heuristic


)

(defun cmdline_8puz ()
    (if (not (null *args*) )
        (8puzzle *args*)
    )
)
(print *args*)

;(8puzzle *args*)
(cmdline_8puz)