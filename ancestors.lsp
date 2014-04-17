;**********************************************************
; File Name: ancestors.lsp
; Brief Description: This file contains the code necessary
;	for querying the ancestors of a given person. 
; Author: Julian Brackins
; Course: Programming Languages, CSC - 461
; Due Date: April 16, 2014
; Professor: Dr. John Weiss
;**********************************************************


;**********************************************************
;		   Ancestor Functions		  			   
;**********************************************************
(defun parents (name)
	(if (null (findPerson name)) nil (person-parents (findPerson name)))
)

(defun mothers (name)
    (sexFilter (parents name) 'female)
)

(defun fathers (name)
    (sexFilter (parents name) 'male)
)

(defun grandparents (name)
	(let ((parent (parents name)) (grands nil))
		(dolist (item parent)
			(dolist (x (parents item))
				(push x grands)
			)		
		)
		(nreverse grands)
	)
)

(defun grandmothers (name)
    (sexFilter (grandparents name) 'female)
)

(defun grandfathers (name)
    (sexFilter (grandparents name) 'male)
)

(defun ancestors (name)
    (cond 
		((null name) nil)
		((atom name) (append (parents name) (ancestors (parents name))))
		(T (append (ancestors (car name)) (ancestors (cdr name))))
	)	
)

(defun female-ancestors (name)
	(sexFilter (ancestors name) 'female)
)

(defun male-ancestors (name)
	(sexFilter (ancestors name) 'male)
)