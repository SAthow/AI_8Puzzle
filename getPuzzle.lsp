#|**************************** GETPUZZLE.LSP *************************************

 The functions found within this file are responsible for getting input from the user

 Author: Luke Meyer, Alex Nienhueser
 Written Spring 2016 
 
*****************************************************************************|#

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



#|*****************************************************************************
  Function: getPuzzle

  Author: Luke Meyer

  Description:  gets puzzle from user, either from file or list

  Args:
	args:	user parameters
*****************************************************************************|#
(defun getPuzzle (args) 
	(let (file puzzle)
       
        (when (= args 0 )

            (print "Please enter a permutation of the digits 0-8, separating each by a space; 0 denotes the blank tile: ")
                
				(setf puzzle (list (read) (read) (read) (read) (read) (read) (read) (read) (read) ) )  ; do nine reads to get user's input
                
				(if ( = (length puzzle) 9) 
                    ; validate puzzle and put into *Start*
                    (if (solvable puzzle )
                        (setf *start* puzzle)
                        
                        ;else
                        (print "Puzzle is not solvable")   
                    ) 
					   
					;ELSE make recursive call to allow the user to re-enter their puzzle 
                    (getPuzzle args )
				)
        
        )
        

        (when (listp args)
                (setf puzzle args)
                (when (solvable puzzle)
                        (setf *start* puzzle)
                )
                
                (when (not (solvable puzzle) )
                        (print "Puzzle is not solvable")
                        nil ; return nil?
                )
        )
            
            (when (stringp args)
                (setf file args)
                
                (setf puzzle (read_Puzzle file))
                    (when (solvable puzzle )
                        ;put into *Start*
                        (setf *start* puzzle)
                    )
                    
                    (when (not (solvable puzzle) )
                        ;else tell user that the puzzle is not solvable
                        (print "Puzzle is not solvable")
                        nil
                    )
            
            
            ) 
            
            
        
        
    )
)


#|*****************************************************************************
  Function: read_Puzzle

  Author: Alex Nienhueser

  Description:  gets puzzle from user, either from file or list

  Args:
	filename:	name of file to be read
*****************************************************************************|#
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

#|*****************************************************************************
  Function: read_Goal

  Author: Alex Nienhueser

  Description:  reads goal file. not currently used

  Args:
	none
*****************************************************************************|#
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

#|*****************************************************************************
  Function: generate_Goal

  Author: Alex Nienhueser

  Description:  generates a goal state based on puzzle dimension. not used currently

  Args:
	none
*****************************************************************************|#
(defun generate_Goal ()
	
	(setf *Goal* ())
	(dotimes (indexX (* *n* *n*) nil)
		(setf *Goal* (append *Goal* (list indexX)))
	)
)

