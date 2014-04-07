;************************************************************************
; Author: Hafiza Farzami
; Professor: Dr. John Weiss
; Course: Programming Languages, CSC - 461
; 
; Description: The following function takes in a file name, opens it, 
; reads it in a variable called database. Towards the end, it loads the
; "functions.lsp" file.
;
; Parameters:
;	filename - the name of the input file
;
; Returns: 
;	true - if no file name is provided
;************************************************************************

(defun main ( filename )
	(cond
		((not (equal filename nil))
			(setf inputFile (open filename))
			(do 
				(
					(*database* (read inputFile nil)
		       			(read inputFile nil))
				)
       				(null *database*)
      				(print *database*)
			)
			(close inputfile)
    		)
		( 
			t
			(format t "Usage: famtree.lsp filename.dat")
			(return-from main t)
		)
	)

	(load "function.lsp")   
)
