( defvar *Start* )    ;start state of 8-tile puzzle

(defun readPuzzleFile file




)

(defun getPuz(args)
	(let (file puzzle)
		(cond
		
			; usage case: (load '8puzzle) 
			;             (8puzzle)
			((= (length args) 1) ;if the user wants to interactively specify the puzzle to solve
				(fromat t "Please enter a permutation of the digits 0-8, each digit seperated by whitespace; 0 denotes the blank tile.")
				(dotimes( i 9 ) (setf puzzle (read)) )  ; do nine reads to get user's input
				(if ( not ( = (length puzzle) 9))
					(getPuz args)  ; make recursive call to allow the user to re-enter their puzzle
					; ELSE validate puzzle and put into *Start*
				)
				 	
			)

			; usage case: (load '8puzzle) 
			;             (8puzzle (puzzlelist))
			((= (length args) 2)  ; if the user specified a puzzle file or entered a list as a command line argument
			
	            (cond 
				
					((listp (cadr args ))  ; if a list is the second command line argument
					
						(setf puzzle (cadr args) ) ) 		
						; NEED TO VALIDATE PUZZLE
					
					( (not (listp (cadr args) ) )  ;if a list wasn't given, assume it was a file name
						(setf file (caddr args))
						(setf puzzle (readPuzzleFile file))	)
						;NEED TO VALIDATE PUZZLe and put into *Start*
				)				
				
			)
			  ; usage case: clisp 8puzzle.lsp puzzlefile 
			((= (length args) 3)	
				(setf file (caddr args))
				(setf puzzle (readPuzzleFile file))
			)							
			
				;print usage for any other form of user input
			(t   (printUsage) )
)))
