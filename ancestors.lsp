;**********************************************************
; File Name: ancestors.lsp
; Brief Description: This file contains the code necessary
;	for querying the ancestors of a given person. This file
;   focuses on queries that involve traversing through a 
;   family tree with increasing seniority.
; Author: Julian Brackins
; Course: Programming Languages, CSC - 461
; Due Date: April 16, 2014
; Professor: Dr. John Weiss
;**********************************************************


;**********************************************************
;		   Ancestor Functions		  			   
;**********************************************************

;Returns the list of parents
(defun parents (name)
    ;verify person's name is in family tree, then return their parents
	(if (null (findPerson name)) nil (person-parents (findPerson name)))
)

;filter parents to get just mothers
(defun mothers (name)
    (sexFilter (parents name) 'female)
)

;filter parents to get just fathers
(defun fathers (name)
    (sexFilter (parents name) 'male)
)

;Returns the list of grandparents
(defun grandparents (name)
;Grandparents = Parents of Parents
	(let ((parent (parents name)) (grands nil))
		(dolist (item parent)
			(dolist (x (parents item))
				(push x grands)
			)		
		)
		(nreverse grands)
	)
)

;filter grandparents to get just grandmothers
(defun grandmothers (name)
    (sexFilter (grandparents name) 'female)
)

;filter grandparents to get just grandfathers
(defun grandfathers (name)
    (sexFilter (grandparents name) 'male)
)

;Returns the list of ancestors
(defun ancestors (name)
;recursively add each person's parents, then find who their parents are.
    (cond 
		((null name) nil)
		((atom name) (append (parents name) (ancestors (parents name))))
		(T (append (ancestors (car name)) (ancestors (cdr name))))
	)	
)

;filter ancestors to get just female-ancestors
(defun female-ancestors (name)
	(sexFilter (ancestors name) 'female)
)

;filter ancestors to get just male-ancestors
(defun male-ancestors (name)
	(sexFilter (ancestors name) 'male)
)