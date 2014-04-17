;**********************************************************
; File Name: descendants.lsp
; Brief Description: This file contains the code necessary
;	for querying the descendants of a given person. This
;   file focuses on queries that involve traversing through 
;   a family tree with decreasing seniority.
; Author: Hafiza Farzami
; Course: Programming Languages, CSC - 461
; Due Date: April 16, 2014
; Professor: Dr. John Weiss
;**********************************************************

;**********************************************************
;		   Descendants Functions		  			   
;**********************************************************

;Returns the list of children
(defun children (name)
;Return a given person's child list
    (person-children (findPerson name))
)

;filter children to get just daughters
(defun daughters (name)
	(sexFilter (children name) 'female)
)

;filter children to get just sons
(defun sons (name)
	(sexFilter (children name) 'male)
)

;Returns the list of grandchildren
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

;filter grandchildren to get just granddaughters
(defun granddaughters (name)
	(sexFilter (grandchildren name) 'female)
)

;filter grandchildren to get just grandsons
(defun grandsons (name)
	(sexFilter (grandchildren name) 'male)
)

;Returns the list of descendants
(defun descendants (name)
;Recursively add each child to a list, then return a list of their children
	(cond 
		((null name) nil)
		((atom name) (append (children name) (descendants (children name))))
		(T (append (descendants (car name)) (descendants (cdr name))))
	)	
)

;filter descendants to get just female-descendants
(defun female-descendants (name)
	(sexFilter (descendants name) 'female)
)

;filter descendants to get just male-descendants
(defun male-descendants (name)
	(sexFilter (descendants name) 'male)
)