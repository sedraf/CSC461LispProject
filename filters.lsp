;Given a list of structs and a gender, it filters out by
;gender
(defun sexFilter (temp sex)
	(setf lst nil)
	(dolist (child temp)
		(setf person (findPerson child))
		(if (null person) nil 

			(when (equal (person-sex person) sex)
				(push (person-name person) lst)
			)
		)	
	)
	(nreverse lst)
)

(defun findPerson (name)	
	;Loop through the database to find the children
	(dolist (x *database*)
		;Add person's children to the list 
		(when (equal name (person-name x))
			(return-from findPerson x)
		)		
	)
)