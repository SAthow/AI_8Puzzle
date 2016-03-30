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



(setf args (list 1 (list 1 3 4 8 6 2 7 0 5) "easy.puz" ) )
;(print (length args ) )
;(print 2)
(defun getPuzzle (args)
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
                        ;(print "Puzzle is solvable")
                        (setf *start* (list puzzle))
                        
                        ;else
                        (print "Puzzle is not solvable")   
                    ) 
					   
					;ELSE make recursive call to allow the user to re-enter their puzzle 
                    (getPuzzle args )
				)
				 	
			)
			; usage case: (load '8puzzle) 
			;             (8puzzle (puzzlelist)) or (8puzzle file_name )
			((= (length args) 2)  ;if the user specified a puzzle file or entered a list as a command line argument
                (print "args was 2" )
	           (cond 
		
                    ((listp (cadr args ))  ; if a list is the second command line argument
						(setf puzzle (cadr args))
                        
                        (when (solvable puzzle)
                            (setf *start* (list puzzle))
                            (print *start*)
                        )
                        
                        (when (not (solvable puzzle) )
                            (print "Puzzle is not solvable")
                            nil ; return nil?
                        )
                            
  
                    ((atomp (cdr args ))  ;if an atom is present in the cdr of args, assume it is a file name
						;(setf file (caddr args))
                        (setf file (cadr args))
						(setf puzzle (read_Puzzle file))
                        ;(print "cadr was not a list!")
                        (when (solvable puzzle )
                            ;put into *Start*
                            (setf *start* (list puzzle))
                            (print *start*)
                        )
                        
                        (when (not (solvable puzzle) )
                            ;else tell user that the puzzle is not solvable
                            (print "Puzzle is not solvable")
                            nil
                        )
                                                
                    )  
                )

                  ; usage case: clisp 8puzzle.lsp puzzlefile 
                    ((= (length args) 3)
                        (print "args was 3" )
                        (setf file (caddr *args*))
                        (setf puzzle (read_Puzzle file))
                        
                        (when (solvable puzzle )
                            ;put into *Start*
                            (setf *start* (list puzzle))
                            (print *start*)
                        )
                        
                        (when (not (solvable puzzle) )
                            ;else tell user that the puzzle is not solvable
                            (print "Puzzle is not solvable")
                            nil
                        )
                        
                    )							
                    
                        ;print usage for any other form of user input
                    (t   (print "DIDN'T WORK"));(printUsage) 
                )
)))

(defun read_Puzzle ( filename )
	(setf lst '())
	
    ; check for correct usage
    (when (null filename) 
		(return-from fileio "Usage: fileio.lsp filename"))

    ; read through file using open
    (format t "~%Opening file ~a using open~%" filename)
    (setf fin (open filename :if-does-not-exist nil))   ; open file, returning NIL on error
    (when (null fin) (return-from fileio (format nil "Error: cannot open file ~a" filename)))
    (do ((data (read fin nil) (read fin nil)))          ; read entire file, returning NIL at EOF
        ((null data) (close fin))                       ; exit when file is read
		(setf lst (append lst (list data)))
    )
	lst
)

(defun read_Goal ()
	(setf *Goal* '())
	
    ; read through file using open
    (setf fin (open "goal.puz" :if-does-not-exist nil))   ; open file, returning NIL on error
    (when (null fin) (return-from fileio (format nil "Error: cannot open file ~a" "goal.puz")))
    (do ((data (read fin nil) (read fin nil)))          ; read entire file, returning NIL at EOF
        ((null data) (close fin))                       ; exit when file is read
		(setf *Goal* (append *Goal* (list data)))
    )
)

(defun generate_Goal ()
	
	(setf *Goal* ())
	(dotimes (indexX (* *n* *n*) nil)
		(setf *Goal* (append *Goal* (list indexX)))
	)
)

;(getPuz args)
;(print 6)