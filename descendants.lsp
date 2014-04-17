;**********************************************************
; File Name: descendants.lsp
; Brief Description: This file contains the code necessary
;	for querying the descendants of a given person. 
; Author: Hafiza Farzami
; Course: Programming Languages, CSC - 461
; Due Date: April 16, 2014
; Professor: Dr. John Weiss
;**********************************************************

;**********************************************************
;		   Descendants Functions		  			   
;**********************************************************

(defun children (name)
;Return a given person's child list
    (person-children (findPerson name))
)

(defun daughters (name)
	(sexFilter (children name) 'female)
)

(defun sons (name)
	(sexFilter (children name) 'male)
)

(defun grandchildren (name)
;Grandchildren = Children of Children
	(let ((child (children name)) (grands nil))
		(dolist (item child)
			(dolist (x (children item))
				(push x grands)
			)		
		)
		(nreverse grands)
	)
)

(defun granddaughters (name)
	(sexFilter (grandchildren name) 'female)
)

(defun grandsons (name)
	(sexFilter (grandchildren name) 'male)
)

(defun descendants (name)
;Recursively add each child to a list, then return a list of their children
	(cond 
		((null name) nil)
		((atom name) (append (children name) (descendants (children name))))
		(T (append (descendants (car name)) (descendants (cdr name))))
	)	
)

(defun female-descendants (name)
	(sexFilter (descendants name) 'female)
)

(defun male-descendants (name)
	(sexFilter (descendants name) 'male)
)
