;**********************************************************
; File Name: descendants.lsp
; Brief Description: This file contains the code necessary
;	for querying the list of a given person's siblings,
;	aunts and uncles, nieces and nephews. 
; Authors: Julian Brackins and Hafiza Farzami
; Course: Programming Languages, CSC - 461
; Due Date: April 16, 2014
; Professor: Dr. John Weiss
;**********************************************************

;**********************************************************
;		    Other Functions		  			   
;**********************************************************

(defun siblings (name)
    (setf sibs nil)
    (dolist (x (parents name))
        (push (children x) sibs)
    )
    ;collapse multiple lists into singular list!
    (collapseList name sibs)
)


(defun brothers (name)
    (sexFilter (siblings name) 'male)
)

(defun sisters (name)
	(sexFilter (siblings name) 'female)
)

;Returns the list of aunts and uncles
(defun aunts-and-uncles (name)
	(let (sibs)
		(dolist (parent (parents name))
			(dolist (x (siblings parent))
				(push x sibs)
			)
		)
		(return-from aunts-and-uncles sibs)
	)
)

;Returns the list of aunts
(defun aunts (name)
	(sexFilter (aunts-and-uncles name) 'female)
)

;Returns the list of uncles
(defun uncles (name)
	(sexFilter (aunts-and-uncles name) 'male)
)

(defun nieces-and-nephews (name)
    (setf niecenephew nil)
    (dolist (x (siblings name))
        (push (children x) niecenephew)
    )
    ;collapse multiple lists into singular list!
    (collapseList name niecenephew)
)

(defun nieces (name)
    (sexFilter (nieces-and-nephews name) 'female)
)

(defun nephews (name)
    (sexFilter (nieces-and-nephews name) 'male)
)

(defun cousins (name)
    (setf couslist nil)
    (dolist (x (aunts-and-uncles name))
        (push (children x) couslist)
    )
    ;collapse multiple lists into singular list!
    (collapseList name couslist)
)

(defun female-cousins (name)
    (sexFilter (cousins name) 'female)
)

(defun male-cousins (name)
    (sexFilter (cousins name) 'male)
)
