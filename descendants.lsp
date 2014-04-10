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
;**********************************************************
; Author: Hafiza Farzami 
;
; Description: The following function returns the list of 
; 	descendants of a given name.
;
; Parameters:
;	name - the name of the person whose descend-
;		      ants are being queried
;
; Returns:	
;	descList - list of a given person's descendants
;	nil - if the person does not have descendants		
;**********************************************************

;Still working on this function
(defun findDescendants (name)
	;List of all descendants
	(setf descList nil)
	
	;Loop through the database to find the descendants
	(dolist (x *database*)
		;Add person's children to the list 
		(when (equal name (person-name x))
			(append (person-children x) descList)
		)		
	)
	
	(return-from descendants descList)
)

;Print the descendants
(defun descendants (name)
	(setf lst (findDescendants name))
	(print lst)
)


(defun listOfChildren (name)
	;List of all children
	(setf childList nil)
	(setf temp nil)
	
	;Loop through the database to find the children
	(dolist (x *database*)
		;Add person's children to the list 
		(when (equal name (person-name x))
			(append (person-children x) temp)
		)		
	)
	
	(dolist (child temp)
		(dolist (entry *database*)
			(
				(if (equal child (person-name entry))
					(push entry childList)					
				)
			)
		)	
	)

	(return-from listOfChildren (nreverse childList))
)

(defun children (name)
	(setf temp (listOfChildren name))

	(dolist (child temp)
		(print (person-name child))
	)	
)

(defun duaghters (name)
	(setf temp (listOfChildren name))
	(setf daugList nil)

	(dolist (child temp)
		(when (equal (person-sex child) 'female)
			(push (person name child) daugList)
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
			(push (person name child) sonList)
		)
	)

	(nreverse sonList)
	(print sonList)
)

(defun memberOf (itm lst)
	(dolist (x lst)
		(if (equal itm x)
			(return-from memberOf t)		
		)
	)
	(return-from memberOf nil)
)
