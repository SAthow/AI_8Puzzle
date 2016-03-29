#|
        ***** AI_8Puzzle.lsp *****

This program

Author: John M. Weiss, Ph.D.
Posted Spring 2016 for SDSM&T CSC447/547 Artificial Intelligence.
|#
(defvar *start* '())
(defvar *goal* '(1 2 3 8 0 4 7 6 5))
(defvar *open* '())
(defvar *close* '())
(defvar *dimension* 0)
(defvar *moveCount* 0)
(defvar *generatedCount* 0)
(defvar *expandedCount* 0)
(defvar *distinctNodes* 0)
(defvar *duplicateNodes* 0)
(defvar *n* 3) ;size of puzzle

(defvar *testSol* '(1 2 3 8 0 4 7 6 5))

(load "search.lsp")

(defun Swp (lst pos offset)
	(let (tempList)
		(setq tempList (copy-list lst) )
		(setf temp (nth (+ pos offset) tempList))
		(setf (nth (+ pos offset) tempList) 0)
		(setf (nth pos tempList) temp)
		tempList
	)
)

(defun generate-successors (state )
	(let (pos lst left right up down)
		(setf pos (position 0 state :test #'equal)) 

		;left
		(when (and (>= (mod pos  *n*) 0)
			(not (eq pos 0) ))
			(setf left (Swp state pos -1) )
			(1+ *generatedCount*)
		)

		;right
		(when (< (mod pos  *n*) (- *n* 1))
			(setf right (Swp state pos 1 ) )
			(1+ *generatedCount*)
		)

		; up 
		(when (>= (/ pos (float *n*)) 1)
			(setf up (Swp state pos (- 0 *n* ) ) )
			(1+ *generatedCount*) 
		)

		; down
		(when (< (/ pos (float *n*)) (- *n* 1))
			(setf down (Swp state pos *n* ) )
			(1+ *generatedCount*)
		)
		(setf successors (list left right up down) )
		(setf successors (remove nil successors))
		(setf state successors)
	)
;	(print state)
	state
)

(defun ad1 (state goal n)
	(let (total_diff)
		(setf total_diff 0)
		(dotimes (x (* n n) nil)
		
			(if
				(not(eq (nth x state) (nth x goal))) 
				(setf total_diff(+ total_diff 1))	
			)
		)
		
		total_diff
	)
)

(defun ad2 (state goal n)
	(let (total test_pos test_vert test_val curr_pos curr_hor curr_vert)
		(setf total 0)
		
		(dotimes (x (* n n) nil)
		
			;Obtain the goal states variables
			(setf test_val (nth x goal))
			(setf test_vert (floor (/ x n)))
			(setf test_hor (mod x n))	
			
			(setf curr_pos (position test_val state :test #'equal))
			(print curr_pos)
			(setf curr_vert (floor(/ curr_pos n)))
			(setf curr_hor (mod curr_pos n))
			(setf total (+ total (+ (abs (- curr_hor test_hor)) (abs (- curr_vert test_vert)))))
			
		)
		
		total
	)
)

(defun inad1 (state goal n)
	(- (* n n)(ad1 state goal n))
)
