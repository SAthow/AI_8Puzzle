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
;		(print 'state)
;		(print state)
;		(print 'lst)
;		(print lst)

		;left
		(when (and (>= (mod pos *n*) 0)
			 (not (eq pos 0) ))
;			(print 'left)
;			(setf lst( list lst (Swp state pos -1) ) )
;			(print lst)
;			(print pos)
			(setf left (Swp state pos -1) )
			(1+ *generatedCount*)
		)

		;right
		(when (< (mod pos *n*) (- *n* 1))
;			(print 'right)
			(setf right (Swp state pos 1 ) )
;			(setf lst( list lst (Swp state pos 1) ) ) 
;			(print lst)
			(1+ *generatedCount*)
		)

		; up 
		(when (>= (/ pos (float *n*)) 1)
;			(print 'up)
			(setf up (Swp state pos (- 0 *n* ) ) )
;			(setf lst (list lst (Swp state pos (- 0 *n*) ) ) )
;			(print lst)
			(1+ *generatedCount*) 
		)

		; down
		(when (< (/ pos (float *n*)) (- *n* 1))
;			(print 'down)
			(setf down (Swp state pos *n* ) )
;			(setf lst (list lst (Swp state pos *n*) ) )
;			(print lst)
			(1+ *generatedCount*)
		)
;		(setf state lst)
;		(setf lst( remove nil lst) )
		(setf successors (list left right up down) )
		(setf successors (remove nil successors))
;		(setf successors ( cons left (cons right (cons up down ) ) ) )
;		(print 'successors)
;		(print successors)
;		(print lst)
		(setf state successors)
	)
	state
)

(defun ad1 (state)
	(let (total_diff)
		(setf total_diff 0)
		(dotimes (x (* *n* *n*) nil)
		
			(if
				(not(eq (nth x state) (nth x *goal*)))
				(setf total_diff(+ total_diff 1))	
			)
		)
		
		total_diff
	)
)

(defun ad2 (state)
	(let (total test_pos test_vert test_val curr_pos curr_hor curr_vert)
		(setf total 0)
		
		(dotimes (x (* *n* *n*) nil)
		
			;Obtain the goal states variables
			(setf test_val (nth x *goal*))
			(setf test_vert (floor (/ x *n*)))
			(setf test_hor (mod x *n*))	
			
			(setf curr_pos (position test_val state :test #'equal))
			(print curr_pos)
			(setf curr_vert (floor(/ curr_pos *n*)))
			(setf curr_hor (mod curr_pos *n*))
			(setf total (+ total (+ (abs (- curr_hor test_hor)) (abs (- curr_vert test_vert)))))
			
		)
		
		total
	)
)

(defun inad1 (state)
	(let (pos total)
		(setf total 0) 
		(dotimes (indexX (* *n* *n*) 0)
			(setf pos (position (nth indexX state) *goal* :test #'equal))
		(print "YAY")
			(when (not(eq (nth (+ indexX 1) state) (nth (+ pos 1) state)))
				(if (eq (nth indexX state) 0)
				(setf total (+ total 1))
				(setf total (+ total 2))
			
				)
			)
		)
		total
	)
)

(defun output_Test (lst n search_name)
	(let (len temp1 temp2 temp3 temp4)
		(setf len (length lst))
		
		(format t "~%~% *BFS* graph Search ~% ----------------")
		(format t "----------------~%")
		(format t " Soultion found ~D moves ~%" 7)
		(format t " ~D nodes generated (~D), ~D nodes expanded~%~%" 350 219 125)
		
		(do 
			((count 0 (setf count (+ count 4))))
			((>= count len))
			(setf temp1 (nth count lst))
			(setf temp2 (nth (+ count 1) lst))
			(setf temp3 (nth (+ count 2) lst))
			(setf temp4 (nth (+ count 3) lst))

			
			
			(when
				(> (- len count) 4)
				(get_Row temp1 temp2 temp3 temp4 n 0 4 1)
			)
			
			(when
				(<= (- len count) 4)
				(get_Row temp1 temp2 temp3 temp4 n 0 (- len count) 0)
			)
			(format t "~%")	
			
			
		)
	   
	)

)

(defun get_Row (lst1 lst2 lst3 lst4 n row amount arrow)
	
		(if
			(>= row (* n n))
			(return-from get_Row())
		)
	
		(format t "  " )
		
		(when
			(>= amount 1)
		
			(do 
				((indexX row (setf indexX (+ indexX 1))))
				((>= indexX (+ row n)))
				(format t " ~D " (nth indexX lst1))
			
			)
		)	
		
		(when
			(>= amount 2)
			(if
				(eq (floor(/ row n)) (floor(/ n 2)))
				(format t "   ->    " )
				(format t "         " )
			)
		
			(do 
				((indexX row (setf indexX (+ indexX 1))))
				((>= indexX (+ row n)))
				(format t " ~D " (nth indexX lst2))
			
			)
		)

		(when
			(>= amount 3)
			
			(if
				(eq (floor(/ row n)) (floor(/ n 2)))
				(format t "    ->    " )
				(format t "          " )
			)
			
			(do 
				((indexX row (setf indexX (+ indexX 1))))
				((>= indexX (+ row n)))
				(format t " ~D " (nth indexX lst3))
			
			)	
		)
		
		(when
			(>= amount 4)
			(if
				(= (floor(/ row n)) (floor(/ n 2)))
				(format t "    ->    " )
				(format t "          " )
			)
		
			(do 
				((indexX row (setf indexX (+ indexX 1))))
				((>= indexX (+ row n)))
				(format t " ~D " (nth indexX lst4))
			
			)
		)
			(if
				(and (= arrow 1) (= (floor(/ row n)) (floor(/ n 2))))
				(format t "    ->    ")
				(format t "          ")
			)
		
		(format t "~%")	
		
		(get_Row lst1 lst2 lst3 lst4 n (+ row n) amount arrow)
)
