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
;			Functions
;**********************************************************
;**********************************************************
; Author: Julian Brackins
;
; Description: The following function returns the list of 
; 	ancestors of a given name.
;
; Parameters:
;	name - the name of the person whose ancestors
;		       are being queried
;
; Returns:	
;	ancList - list of a given person's ancestors
;	nil - if the person does not have ancestors		
;**********************************************************
(defun ancestors (name)
	;List of all ancestors
	(setq ancList nil)

    
	;Loop through the database to find the ancestors
	(dolist (x *database*)
		;Check if the person has parents
		(when (equal nil (person-parents x))
			;(format t "No ancestors.")
			;(return-from ancestors nil)		
		)
		;Add person's parents to the list 
		(when (equal name (person-name x))
            (setq temp (person-parents x))
            (print temp)
            (setq ancList temp)
            (print ancList)
			;(dolist (person-parents x)
				(ancestors (car (person-parents x)))
			;)
		)		
	)
	
	;(print ancList) ;for debugging, will get rid of it
	(return-from ancestors ancList)
)