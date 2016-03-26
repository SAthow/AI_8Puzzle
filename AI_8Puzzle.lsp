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
			(setf lst (append lst(Swp state pos -1)))
			(1+ *generatedCount*) 
			(write 'left )
		)
		;right
		(when (<= (mod pos (float n)) (- n 1))
			(write 'right)
			(nconc lst(Swp state pos 1)) 
			(1+ *generatedCount*)
		)
		; up 
		(when (>= (/ pos (float n)) 1)
			(write 'up)
			(nconc lst(Swp state pos (- 0 n))) 
			(1+ *generatedCount*) 
		)
		; down
		(when (< (/ pos (float n)) (- n 1))
			(write 'down)
			(nconc lst(Swp state pos  n))
			(1+ *generatedCount*)
		)
		(setf state lst)
		(write lst)
	)
	state
)

(defun hueristic (state goal n)
	(let (total_diff)
		(setf total_diff 0)
		(dotimes (x (* n n) nil)
		
			(if(not(eq (nth x state) (nth x goal))) (setf total_diff(+ total_diff 1)))
		)
	)
)
