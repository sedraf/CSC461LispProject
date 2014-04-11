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

(defun listOfParents (name)
	;List of all parents
	(setf ParentList nil)
	(setf temp nil)
	
	;Loop through the database to find the parents
	(dolist (x *database*)
		;Add person's parents to the list 
		(when (equal name (person-name x))
			(dolist (y (person-parents x))
				(push y temp)
			)
		)		
	)

	(dolist (parent temp)
		(dolist (entry *database*)
			(if (equal parent (person-name entry))
				(push entry ParentList)
			)
		)
	)
	(return-from listOfParents (nreverse ParentList))
)


(defun parents (name)
	(setf temp (listOfParents name))
	(setf lst nil)

	(dolist (parent temp)
		(push (person-name parent) lst)
	)	
	(prin1 lst)
)

(defun mothers (name)
	(setf temp (listOfParents name))
	(setf momList nil)

	(dolist (parent temp)
		(when (equal (person-sex parent) 'female)
			(push (person-name parent) momList)
		)
	)
	(print momList)
)

(defun fathers (name)
	(setf temp (listOfParents name))
	(setf dadList nil)

	(dolist (parent temp)
		(when (equal (person-sex parent) 'male)
			(push (person-name parent) dadList)
		)
	)
	(print dadList)
)

(defun listOfGrandparents (name)
	;List of all parents
	(setf GrandParentList nil)
	(setf temp nil)
    
	
	;Loop through the database to find the parents
	(dolist (x *database*)
		;Add person's parents to the list 
		(when (equal name (person-name x))
			(dolist (y (person-parents x))
				(push y temp)
			)
		)		
	)

	(dolist (parent temp)
        (dolist (entry *database*)
            (when (equal parent (person-name entry))
                ;Find the parents of these parents
                (push (listOfParents (person-name entry)) GrandParentList)
            )
        )
        
    )
    ;(print GrandParentList)
    (return-from listOfGrandparents GrandParentList)
)

(defun grandparents (name)
	(setf temp (listOfGrandparents name))
	(setf grandparentList nil)
	
    (dolist (grandparent temp)
        (dolist (x grandparent)
            (push (person-name x) grandparentList)
        )
	)	
	(return-from grandparents grandparentList)
)

(defun grandmothers (name)
	(setf temp (listOfGrandparents name))
	(setf grandmomList nil)
    
    (dolist (grandparent temp)
        (dolist (x grandparent)
            (when (equal (person-sex x) 'female)
                (push (person-name x) grandmomList)
            )
        )
	)	
	(return-from grandmothers grandmomList)
)

(defun grandfathers (name)
	(setf temp (listOfGrandparents name))
	(setf granddadList nil)
    
    (dolist (grandparent temp)
        (dolist (x grandparent)
            (when (equal (person-sex x) 'male)
                (push (person-name x) granddadList)
            )
        )
	)	
	(return-from grandfathers granddadList)
)


(defun listOfSiblings (name)
	;List of all parents
	(setf SiblingList nil)
	(setf temp nil)
	
	;Loop through the database to find the parents
	(dolist (x *database*)
		;Add person's parents to the list 
		(when (equal name (person-name x))
			(dolist (y (person-parents x))
				(push y temp)
			)
		)		
	)

	(dolist (parent temp)
		(dolist (entry *database*)
			(if (equal parent (person-name entry))
                ;find the children of these parents
				(push (listOfChildren (person-name entry)) SiblingList)
			)
		)
	)
	(return-from listOfSiblings (nreverse SiblingList))
)


(defun siblings (name)
	(setf temp (listOfSiblings name))
	(setf siblingList nil)
    
    (dolist (sibling temp)
        (dolist (x sibling)
            (if (equal (person-name x) name)
                ()
                (setf checkdup (person-name x) )
            )
            (if (member checkdup siblingList)
                ()
                (push checkdup siblingList)
            )
        )
	)	
	(return-from siblings siblingList)
)


(defun brothers (name)
	(setf temp (listOfSiblings name))
	(setf broList nil)
    
    (dolist (sibling temp)
        (dolist (x sibling)
            (when (equal (person-sex x) 'male)
                (if (equal (person-name x) name)
                    ()
                    (setf checkdup (person-name x) )
                )
                (if (member checkdup broList)
                    ()
                    (push checkdup broList)
                )
            )
        )
	)	
	(return-from brothers broList)
)

(defun sisters (name)
	(setf temp (listOfSiblings name))
	(setf sisList nil)
    
    (dolist (sibling temp)
        (dolist (x sibling)
            (when (equal (person-sex x) 'female)
                (if (equal (person-name x) name)
                    ()
                    (setf checkdup (person-name x) )
                )
                (if (member checkdup sisList)
                    ()
                    (push checkdup sisList)
                )
            )
        )
	)	
	(return-from sisters sisList)
)