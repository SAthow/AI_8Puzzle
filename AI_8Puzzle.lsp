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
(devar *moveCount* 0)
(devar *generatedCount* 0)
(devar *expandedCount* 0)
(devar *distinctNodes* 0)
(devar *duplicateNodes* 0)

(defun Moves (state n)
	(let (pos lst)
		(setf lst nil)
		(setf pos (position 0 state :test #'equal)) 
		(if (not(eq (mod pos n) 0)) (append lst(Swp state pos -1)))
		(if (not(eq (mod pos n) (- n 1)))(append lst(Swp state pos 1)))	
		(if (> (/ pos n) 1)(append lst(Swp state pos n)))
		(if (< (/ pos n) (- n 1))(append lst(Swp state pos (- 0 n))))
	)
)

(defun Swp (lst pos offset)
	(let
		(setf temp (nth (+ pos offset) lst))
		(setf (nth (+ pos offset) lst) 0)
		(setf (nth pos lst) temp)
	)
	lst
)

(defun hueristic (state goal n)
	(let total_diff
		(setf total_diff 0)
		(dotimes x (* n n)
		
			(if(not(eq (nth x state) (nth x goal))) (setf total_diff(+ total_diff 1)))
		)
	)
)