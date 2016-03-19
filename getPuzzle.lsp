;( defvar *Start* )    ;start state of 8-tile puzzle
;(print 1)
(defun readPuzzleFile 'file T )

#|
                  ***** SOLVABLE.LSP *****

The SOLVABLE function returns T if a given 8-puzzle position is solvable,
NIL otherwise.

Usage:    (solvable L)
          where L is a 9-element list such as (1 2 3 8 0 4 7 6 5)

Reference:  "Mathematical Games and Pastimes", p.79-85,
             A.P.Domoryad, Macmillan, 1964.

Written 03/88 by John M. Weiss, Ph.D.

Modifications:
|#

(defvar *flag*)

(defun solvable (L)
    (setf *flag* nil)                               ;global *flag*
    (mapcar #'(lambda (elem) (disorder elem L)) L)
    (eq *flag* (evenp (position 0 L)))
)

(defun disorder (elem L)
    (cond
        ((eq (car L) elem))
        ((> (car L) elem)
            (setf *flag* (not *flag*))
            (disorder elem (cdr L))
        )
        (t (disorder elem (cdr L)))
    )
)



(setf args (list 1 (list 1 3 4 8 6 2 7 0 5) ) )
(print (length args ) )
(print 2)
(defun getPuz(args)
	(let (file puzzle)
		(cond
			; usage case: (load '8puzzle) 
			;             (8puzzle)
			((= (length args ) 1 ) ;if the user wants to interactively specify the puzzle to solve
                (print "args was 1" )
                
				(format t "Please enter a permutation of the digits 0-8, separating each by a space; 0 denotes the blank tile: ")
                
				(setf puzzle (list (read) (read) (read) (read) (read) (read) (read) (read) (read) ) )  ; do nine reads to get user's input
                ;(print (car puzzle) )
                ;(print (cdr puzzle) )
                
				(if ( = (length puzzle) 9) 
                    ; validate puzzle and put into *Start*
                    ;(format t "The cdr of the puzzle is: ~a" (cdr puzzle))
                    (if (solvable puzzle )
                        (print "Puzzle is solvable")
                        
                        ;else
                        (print "Puzzle is not solvable")   
                    ) 
					   
					;ELSE make recursive call to allow the user to re-enter their puzzle 
                    (getPuz args)
				)
				 	
			)
			; usage case: (load '8puzzle) 
			;             (8puzzle (puzzlelist)) or (8puzzle file_name )
			((= (length args) 2)  ;if the user specified a puzzle file or entered a list as a command line argument
			
	           (cond 
		
                    ((listp (cadr args ))  ; if a list is the second command line argument
                        ;(print "The cadr of args is a list!")
						(setf puzzle (cadr args))
                        ;(print (car puzzle) )
                        ;(print (cdr puzzle) ) 
                        
                    )
						;NEED TO VALIDATE PUZZLE
					;( (not (listp (cadr args)) )  
                    ((atomp (cdr args ))  ;if an atom is present in the cdr of args, assume it is a file name
						(setf file (caddr args))
						(setf puzzle (readPuzzleFile file))
                        (print "cadr was not a list!")
                        (if (solvable puzzle )
                            ;put into *Start*
                            (print "Puzzle is solvable")
                            ;else tell user that the puzzle is not solvable
                            (print "Puzzle is not solvable")
                        )                        
                    )  
                )
                (print "args was 2" )
			)				
                	
		
			  ; usage case: clisp 8puzzle.lsp puzzlefile 
			((= (length args) 3)	
				(setf file (caddr args))
				(setf puzzle (readPuzzleFile file))
                
                (print "args was 3" )
			)							
			
				;print usage for any other form of user input
			(t   (printUsage) )
)))

(getPuz args)
(print 6