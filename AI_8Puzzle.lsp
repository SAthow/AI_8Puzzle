#|**************************************************************************

 Author: Stephanie Athow, Luke Meyer, Alex Nienhueser


 Description: This program is design to solve an 8-puzzle using BFS 
   (breadth first search), DFID (depth first iterated deepening), and A*.
    The start position may be specified in a puzzle file, or via promts.
    After solving the puzzle with each search algorithm, a nicely 
    formatted list of positions, leading from the start state to the goal 
    state will be printed. As well as the number of moves required to reach
    the goal state, and the number of nodes generated and expanded 
    (i.e., the number placed on the OPEN and CLOSED lists, respectively). 
    This will provide a rough metric for search algorithm efficiency.


*****************************************************************************|#

;(defvar *start* '())
;(defvar *goal* '(1 2 3 8 0 4 7 6 5))
;(defvar *open* '())
;(defvar *close* '())
;(defvar *dimension* 0)
;(defvar *moveCount* 0)
;(defvar *generatedCount* 0)
;(defvar *expandedCount* 0)
;(defvar *distinctNodes* 0)
;(defvar *duplicateNodes* 0)
;(defvar *n* 3) ;size of puzzle

(defvar *testSol* '(1 2 3 8 0 4 7 6 5))

(load "search.lsp")


;(defun main 



#|**************************************************************************
 Function: Swp

 Author: Alex Nienhueser

 Description: This function takes a position and an offeset and swap the given
    position with its given offeset in the passed list.

 Args:
    lst:        List of the elements to be swapped.
    pos:        The position to be swapped.
    offset:        The offset of the pos to be swapped with.

*****************************************************************************|#

(defun Swp (lst pos offset)
	(let (tempList)
		(setq tempList (copy-list lst) )
		(setf temp (nth (+ pos offset) tempList))
		(setf (nth (+ pos offset) tempList) 0)
		(setf (nth pos tempList) temp)
		tempList
	)
)

#|**************************************************************************
Function: generate-successors
 
 Author: Stephanie Athow, Alex Nienhueser

 Description: This function takes a node state and will generate all possible 
    successors from it (ex: Up,  Down, Left, Right). It will the return a 
    list of lists containing those successors.

 Args:
   state:        The current node state to have its childern generated.

*****************************************************************************|#

(defun generate-successors (state )
	(let (pos lst left right up down)
		(setf pos (position 0 state :test #'equal)) 


        ;Determine if the left sucessor can be generated
		(when (and (>= (mod pos *n*) 0)
			 (not (eq pos 0) ))
			(setf left (Swp state pos -1) )
			(setf *generatedCount* (1+ *generatedCount*) )
		)

		;Determine if the right sucessor can be generated
		(when (< (mod pos *n*) (- *n* 1))
			(setf right (Swp state pos 1 ) )
			(setf *generatedCount* (1+ *generatedCount*) )
		)

		;Determine if the up sucessor can be generated 
		(when (>= (/ pos (float *n*)) 1)
			(setf up (Swp state pos (- 0 *n* ) ) )
			(setf *generatedCount* (1+ *generatedCount*) ) 
		)

		;Determine if the down sucessor can be generated
		(when (< (/ pos (float *n*)) (- *n* 1))
			(setf down (Swp state pos *n* ) )
			(setf *generatedCount* (1+ *generatedCount*) )
		)
		
		(setf successors (list left right up down) )
		(setf successors (remove nil successors))
		(setf state successors)
	)
	state
)

#|**************************************************************************
 Function: ad1
 
 Author: Alex Nienhueser
 
 Description: This function calculates an admissable h' for a given node.
    The fuction checks the amount of value that incorrectly placed and returns
    the sum of missplaced values.

 Args:
    state:        The current node state to have it's huerstic calculated
 
*****************************************************************************|#

(defun ad1 (state)
	(let (total_diff)
		(setf total_diff 0)
		(dotimes (x (* *n* *n*) 0)
		
			(if
				(not(eq (nth x state) (nth x (starNode-state *goalstate*))))
				(setf total_diff(+ total_diff 1))	
			)
		)
		
		total_diff
	)
)

#|**************************************************************************
 Function: ad2
 
 Author: Alex Nienhueser
 
 Description: This function calculates an admissable h' for a given node.
    The fuction checks the amount of value that incorrectly placed and returns
    the sum of the distance they are from the their correct position.
 
 Args:
    state:        The current node state to have it's huerstic calculated
	
*****************************************************************************|#

(defun ad2 (state)
	(let (total test_pos test_vert test_val curr_pos curr_hor curr_vert)
		(setf total 0)
		
		(dotimes (x (* *n* *n*) 0)
		
			;Obtain the goal states variables
			(setf test_val (nth x (starNode-state *goalstate*)))
			(setf test_vert (floor (/ x *n*)))
			(setf test_hor (mod x *n*))	
			
			(setf curr_pos (position test_val state :test #'equal))
			(setf curr_vert (floor(/ curr_pos *n*)))
			(setf curr_hor (mod curr_pos *n*))
			(setf total (+ total (+ (abs (- curr_hor test_hor)) (abs (- curr_vert test_vert)))))
			
		)
		
		total
	)
)

#|**************************************************************************
 Function: inad1
 
 Author: Alex Nienhueser
 
 Description: This function calculates an inadmissable h' for a given node,
	this method is know as Nilsson's Sequence Score 
	(https://heuristicswiki.wikispaces.com/Nilsson%27s+Sequence+Score). 
	The fuction will check each position in a given puzzle. If the position's
	value is zero and its following value doesnt match goals we will increment
	one. If the position's value is non-zero and its following value doesnt match
	goals we will increment two. Once all the positions have been checked we then
	add the incremented total to our second admissable funtion.
 
 Args:
    state:        The current node state to have it's huerstic calculated
 
*****************************************************************************|#
(defun inad1 (state)
	(let (pos total)
		(setf total 0) 
		(dotimes (indexX (* *n* *n*) 0)
			(setf pos (position (nth indexX state) (starNode-state *goalstate*) :test #'equal))
			(when (not(eq (nth (+ indexX 1) state) (nth (+ pos 1) state)))
				(if (eq (nth indexX state) 0)
				(setf total (+ total 1))
				(setf total (+ total 2))
			
				)
			)
		)
		(+ (ad2 state) (* total 3))
	)
)


#|**************************************************************************
 Function: get_Row
 
 Author: Alex Nienhueser
 
 Description: This function ouputs the path and infomation from a given 
	search algorithm.
 
 Args:
   lst:		The list of puzzle paths
   row: 	The current row to be printed of the puzzle
   amount: 	Amount of lists to be printed
   arrow: 	Determines if an arrow needs to be printed for the fourth list
 
*****************************************************************************|#
(defun format_output (lst)
	(let (len temp1 temp2 temp3 temp4)
		(setf len (length lst))
		
		;(format t "~%~% *BFS* graph Search ~% ----------------")
		(format t "----------------~%")
		(format t " Solution found ~D moves ~%" *moveCount*)
		(format t " ~D nodes generated (~D distinct nodes), ~D nodes expanded~%~%" *generatedCount* *distinctNodes* *expandedCount*)
		
		(do 
			((count 0 (setf count (+ count 4))))
			((>= count len))
			(setf temp1 (nth count lst))
			(setf temp2 (nth (+ count 1) lst))
			(setf temp3 (nth (+ count 2) lst))
			(setf temp4 (nth (+ count 3) lst))

			
			
			(when
				(> (- len count) 4)
				(get_Row temp1 temp2 temp3 temp4 0 4 1)
			)
			
			(when
				(<= (- len count) 4)
				(get_Row temp1 temp2 temp3 temp4 0 (- len count) 0)
			)
			(format t "~%")	
			
			
		)
	   
	)

)

#|**************************************************************************
 Function: get_Row
 
 Author: Alex Nienhueser
 
 Description: This function takes up to four lists and formats output.
 
 Args:
   lst1: 	First Puzzle
   lst2: 	Second Puzzle
   lst3: 	Thrid Puzzle
   lst4: 	Puzzle
   row: 	The current row to be printed of the puzzle
   amount: 	Amount of lists to be printed
   arrow: 	Determines if an arrow needs to be printed for the fourth list
 
*****************************************************************************|#
(defun get_Row (lst1 lst2 lst3 lst4 row amount arrow)
	
		;If we are done printing return
		(if
			(>= row (* *n* *n*))
			(return-from get_Row())
		)
	
		(format t "  " )
		
		;Print a row of the first list 
		(when
			(>= amount 1)
		
			(do 
				((indexX row (setf indexX (+ indexX 1))))
				((>= indexX (+ row *n*)))
				(format t " ~D " (nth indexX lst1))
			
			)
		)	
		
		;Print a row of the second list 
		(when
			(>= amount 2)
			;Print an arrow/Space
			(if
				(eq (floor(/ row *n*)) (floor(/ *n* 2)))
				(format t "   ->    " )
				(format t "         " )
			)
		
			(do 
				((indexX row (setf indexX (+ indexX 1))))
				((>= indexX (+ row *n*)))
				(format t " ~D " (nth indexX lst2))
			
			)
		)

		;Print a row of the third list 
		(when
			(>= amount 3)
			;Print an arrow/Space
			(if
				(eq (floor(/ row *n*)) (floor(/ *n* 2)))
				(format t "    ->    " )
				(format t "          " )
			)
			
			(do 
				((indexX row (setf indexX (+ indexX 1))))
				((>= indexX (+ row *n*)))
				(format t " ~D " (nth indexX lst3))
			
			)	
		)
		
		;Print a row of the fourth list 
		(when
			;Print an arrow/Space
			(>= amount 4)
			(if
				(= (floor(/ row *n*)) (floor(/ *n* 2)))
				(format t "    ->    " )
				(format t "          " )
			)
		
			(do 
				((indexX row (setf indexX (+ indexX 1))))
				((>= indexX (+ row *n*)))
				(format t " ~D " (nth indexX lst4))
			
			)
		)
			;Determine if an arrow needs to be printed
			(if
				(and (= arrow 1) (= (floor(/ row *n*)) (floor(/ *n* 2))))
				(format t "    ->    ")
				(format t "          ")
			)
		
		(format t "~%")	
		
		(get_Row lst1 lst2 lst3 lst4 (+ row *n*) amount arrow)
)
