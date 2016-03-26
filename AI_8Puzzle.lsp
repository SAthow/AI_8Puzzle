#|
        ***** AI_8Puzzle.lsp *****

This program

Author: John M. Weiss, Ph.D.
Posted Spring 2016 for SDSM&T CSC447/547 Artificial Intelligence.
|#
(defvar *start* '())
(defvar *goal* '())
(defvar *open* '())
(defvar *close* '())
(defvar *dimension* 0)
(defvar *moveCount* 0)
(defvar *generatedCount* 0)
(defvar *expandedCount* 0)
(defvar *distinctNodes* 0)
(defvar *duplicateNodes* 0)


;(load "search.lsp")

(defun Swp (lst pos offset)
	(let (tempList)
		(setq tempList (copy-list lst) )
		(setf temp (nth (+ pos offset) tempList))
		(setf (nth (+ pos offset) tempList) 0)
		(setf (nth pos tempList) temp)
		tempList
	)
)

(defun generate-successors (state n)
	(let (pos lst)
		(setf lst nil)
		(setf pos (position 0 state :test #'equal)) 

		;left
		(when (>= (mod pos (float n)) 0)

;			(setf lst( cons lst (cons (Swp state pos -1) nil ) ) )
			(1+ *generatedCount*) 
			(print 'left)
		)

		;right
		(when (<= (mod pos (float n)) (- n 1))
			(print 'right)

;			(setf lst( cons lst (cons (Swp state pos 1) nil) ) )
			(1+ *generatedCount*)
		)

		; up 
		(when (>= (/ pos (float n)) 1)
			(print 'up)
			(setf lst (cons lst (Swp state pos (- 0 n) ) ) )
;			(setf lst (cons lst (cons (Swp state pos (- 0 n) ) nil ) ) )
			(1+ *generatedCount*) 
		)

		; down
		(when (< (/ pos (float n)) (- n 1))
			(print 'down)
			(setf lst (cons lst (Swp state pos n) ) )  
;			(setf lst (cons lst (cons (Swp state pos n) nil ) ) ) 
			(1+ *generatedCount*)
		)
		(setf state lst)
		(print lst)
	)
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