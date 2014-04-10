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
;			Functions
;**********************************************************

;Given a list of structs and a gender, it filters out by
;gender
(defun sexFilter (temp sex)
	(setf lst nil)
	(dolist (child temp)
		(when (equal (person-sex child) sex)
			(push (person-name child) lst)
		)
	)
	lst
)

;This loop is used by several different function
(defun helperOne (temp)
	(setf lst nil)
	(dolist (child temp)
		(push (person-name child) lst)
	)	
	(print lst)
)

;Another helper loop function
(defun helperTwo (temp)
	(setf lst nil)
	(dolist (child temp)
		(dolist (entry *database*)
			(if (equal child (person-name entry))
				(push entry lst)
			)
		)
	)
	lst
)

(defun listOfChildren (name)
	(setf temp nil)
	
	;Loop through the database to find the children
	(dolist (x *database*)
		;Add person's children to the list 
		(when (equal name (person-name x))
			(dolist (y (person-children x))
				(push y temp)
			)
		)		
	)
	
	(setf childList (helperTwo temp))

	(nreverse childList)
)

(defun listOfGrandChildren (name)
	(setf lst (listOfChildren name))
	(setf temp nil)

	(dolist (child lst)
		(dolist (x (person-children child))
			(push x temp)
		)	
	)
	(setf gcList (helperTwo temp))
	(nreverse gcList)	
)

(defun children (name)
	(setf child (listOfChildren name))
	(helperOne child)
)

(defun daughters (name)
	(setf daugList (listOfChildren name))
	(print (sexFilter daugList 'female))
)

(defun sons (name)
	(setf sonsList (listOfChildren name))
	(print (sexFilter sonsList 'male))
)

(defun grandchildren (name)
	(setf grandChild (listOfGrandChildren name))
	(helperOne grandChild)
)

(defun granddaughters (name)
	(setf gdList (listOfGrandChildren name))
	(print (sexFilter gdList 'female))
)

(defun grandsons (name)
	(setf gsList (listOfGrandChildren name))
	(print (sexFilter gsList 'male))
)
