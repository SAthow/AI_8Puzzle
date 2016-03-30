#|**************************************************************************

 Author: Stephanie Athow, Luke Meyer, Alex Nienhueser


 Description: This program is design to solve an 8-puzzle using BFS 
   (breadth first search), DFID (depth first iterated deepening), and A*.
    The start position may be specified in a puzzle file, or via prompts.
    After solving the puzzle with each search algorithm, a nicely 
    formatted list of positions, leading from the start state to the goal 
    state will be printed, as well as the number of moves required to reach
    the goal state, and the number of nodes generated and expanded 


*****************************************************************************|#

(defvar *moveCount* 0)
(defvar *generatedCount* 0)
(defvar *expandedCount* 0)
(defvar *distinctNodes* 0)
(defvar *duplicateNodes* 0)
(defvar *start* nil)
(defvar *goalstate* nil)
(defvar *n* 3)

(load "AI_8Puzzle.lsp")
(load "aStar.lsp")
(load "getPuzzle.lsp")


(defun 8puzzle ()
    
    (getPuzzle *args*)      ; get them puzzlin' shitz and set start state
    
    (search_bfs_dfs *start* 'bfs) ;run bfs
    
    (search_dfid *start*)  ;run depth first search with iterated deepening
    
    (aStar *start* ad1)   ; run a* with "number out of place" heuristic
    
    (aStar *start* ad2)   ; run a* with "manhattan distance" heuristic
    
    (aStar *start* inad1)   ; run a* with inadmissable heuristic

)