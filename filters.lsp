;**********************************************************
; File Name: filters.lsp
; Brief Description: This file contains the code with
;   filters, or often-used code blocks in this program
;   for list processing.
; Authors: Julian Brackins and Hafiza Farzami
; Course: Programming Languages, CSC - 461
; Due Date: April 16, 2014
; Professor: Dr. John Weiss
;**********************************************************

;Given a list of structs and a gender, it filters out by
;gender
(defun sexFilter (temp sex)
	(setf lst nil)
	(dolist (child temp)
		(setf person (findPerson child))
		(if (null person) nil 

			(when (equal (person-sex person) sex)
				(push (person-name person) lst)
			)
		)	
	)
	(nreverse lst)
)

(defun findPerson (name)	
	;Loop through the database to find the children
	(dolist (x *database*)
		;Add person's children to the list 
		(when (equal name (person-name x))
			(return-from findPerson x)
		)		
	)
)

;Take a list of lists, and collapse it into a singular list
;Also removes the parameter "name" from the list, if present
(defun collapseList (name lst)
    (setf lst (loop for outer in lst
      nconcing (loop for inner in outer collecting inner)))
    (remove name (remove-duplicates lst))
)