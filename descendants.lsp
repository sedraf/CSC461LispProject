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
(defun descendants (name)
	;List of all descendants
	(setf descList nil)

	;Loop through the database to find the descendants
	(dolist (x *database*)
		;Check if the person has children
		(when (equal nil person-children)
			(format t "No descendants.")
			(return-from descendants nil)		
		)
		;Add person's children to the list 
		(when (equal name person-name)
			(dolist (child person-children)
				(append child descList)
			)
		)		
	)
	
	(print descList) ;for debugging, will get rid of it
	(return-from descendants descList)
)
