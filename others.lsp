;**********************************************************
; File Name: descendants.lsp
; Brief Description: This file contains the code necessary
;	for querying the list of a given person's siblings,
;	aunts and uncles, nieces and nephews. This file
;   focuses on queries that involve traversing through a 
;   family tree using a combination of queries created for
;   descendants.lsp and ancestors.lsp
; Authors: Julian Brackins and Hafiza Farzami
; Course: Programming Languages, CSC - 461
; Due Date: April 16, 2014
; Professor: Dr. John Weiss
;**********************************************************

;**********************************************************
;		    Other Functions		  			   
;**********************************************************

;Return the names of all a given parent's children
;except for the person who's name was entered
(defun siblings (name)
;Siblings = Children of Parents - Oneself
    (setf sibs nil)
    (dolist (x (parents name))
        (push (children x) sibs)
    )
    ;collapse multiple lists into singular list!
    (collapseList name sibs)
)

;filter siblings to get just sisters
(defun sisters (name)
	(sexFilter (siblings name) 'female)
)

;filter siblings to get just brothers
(defun brothers (name)
    (sexFilter (siblings name) 'male)
)

;Returns the list of aunts and uncles
(defun aunts-and-uncles (name)
;Aunts & Uncles = Siblings of Parents
	(let (sibs)
		(dolist (parent (parents name))
			(dolist (x (siblings parent))
				(push x sibs)
			)
		)
		(return-from aunts-and-uncles sibs)
	)
)

;filter aunts-and-uncles to get just aunts
(defun aunts (name)
	(sexFilter (aunts-and-uncles name) 'female)
)

;filter aunts-and-uncles to get just uncles
(defun uncles (name)
	(sexFilter (aunts-and-uncles name) 'male)
)

;Returns the list of nieces and nephews
(defun nieces-and-nephews (name)
;Nieces & Nephews = Children of Siblings
    (setf niecenephew nil)
    (dolist (x (siblings name))
        (push (children x) niecenephew)
    )
    ;collapse multiple lists into singular list!
    (collapseList name niecenephew)
)

;filter nieces-and-nephews to get just nieces
(defun nieces (name)
    (sexFilter (nieces-and-nephews name) 'female)
)

;filter nieces-and-nephews to get just nephews
(defun nephews (name)
    (sexFilter (nieces-and-nephews name) 'male)
)

;Returns the list of cousins
(defun cousins (name)
;Cousins = Children of Aunts & Uncles
    (setf couslist nil)
    (dolist (x (aunts-and-uncles name))
        (push (children x) couslist)
    )
    ;collapse multiple lists into singular list!
    (collapseList name couslist)
)

;filter cousins to get just female-cousins
(defun female-cousins (name)
    (sexFilter (cousins name) 'female)
)

;filter cousins to get just male-cousins
(defun male-cousins (name)
    (sexFilter (cousins name) 'male)
)
