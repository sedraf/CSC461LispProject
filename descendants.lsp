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
(defun listOfChildren (name)
	;List of all children
	(setf childList nil)
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

	(dolist (child temp)
		(dolist (entry *database*)
			(if (equal child (person-name entry))
				(push entry childList)
			)
		)
	)

	(return-from listOfChildren (nreverse childList))
)


(defun children (name)
	(setf temp (listOfChildren name))
	(setf lst nil)

	(dolist (child temp)
		(push (person-name child) lst)
	)	
	(prin1 lst)
)

(defun daughters (name)
	(setf temp (listOfChildren name))
	(setf daugList nil)

	(dolist (child temp)
		(when (equal (person-sex child) 'female)
			(push (person-name child) daugList)
		)
	)

	(nreverse daugList)
	(print daugList)
)

(defun sons (name)
	(setf temp (listOfChildren name))
	(setf sonList nil)

	(dolist (child temp)
		(when (equal (person-sex child) 'male)
			(push (person-name child) sonList)
		)
	)

	(nreverse sonList)
	(print sonList)
)
