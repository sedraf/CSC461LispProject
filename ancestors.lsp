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
	(setf parentList nil)

	(dolist (parent temp)
		(push (person-name parent) parentList)
	)	
	(return-from parents parentList)
)

(defun mothers (name)
    (setf momList (listOfParents name))
	(setf mom (sexFilter momList 'female))
)

(defun fathers (name)
    (setf dadList (listOfParents name))
    (setf dad (sexFilter dadList 'male))
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

(defun listOfAncestors (name)
	;List of all parents
	
    (setf temp nil)
	(setf mommies nil)
    (setf daddies nil)
	
	;Loop through the database to find the parents
	(dolist (x *database*)
		;Add person's parents to the list 
		(when (equal name (person-name x))
            (push x *AncList*)
            ;(format t "Processing ~a...~%" (person-name x))
            (if (equal nil (person-parents x))
                ()
                (dolist (y (person-parents x))
                    (push y temp)
                )
            )
		)		
	)
    
	(dolist (parent temp)
		(dolist (entry *database*)
			(when (equal parent (person-name entry))
                (if (equal (person-sex entry) 'male)
                    (push entry daddies)
                    (push entry mommies)
                )
			)
		)
	)

    (dolist (xy daddies)
        (dolist (xx mommies)
            (listOfAncestors (person-name xx))
        )
        (listOfAncestors (person-name xy))
    )

	(return-from listOfAncestors (nreverse *AncList*))
)


(defun ancestors (name)
    (setf *AncList* nil) 
    (setf ancestorList nil)
	(setf temp (listOfAncestors name))
    
	(dolist (ancestor temp)
        (if (equal (person-name ancestor) name)
            ()
            (push (person-name ancestor) ancestorList)
        )
	)	
	(return-from ancestors ancestorList)
)

(defun ancestors-female (name)
    (setf *AncList* nil) 
    (setf temp (listOfAncestors name))
    (setf ancestorList nil)
    (dolist (ancestor temp)
        (if (equal (person-name ancestor) name)
            ()
            (push ancestor ancestorList)
        )
	)	
	(setf ancestorList (sexFilter ancestorList 'female))
)

(defun ancestors-male (name)
    (setf *AncList* nil) 
    (setf temp (listOfAncestors name))
    (setf ancestorList nil)
    (dolist (ancestor temp)
        (if (equal (person-name ancestor) name)
            ()
            (push ancestor ancestorList)
        )
	)	
	(setf ancestorList (sexFilter ancestorList 'male))
)