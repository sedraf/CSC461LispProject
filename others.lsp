(defun siblings (name)
    (setf sibs nil)
    (dolist (x (parents name))
        (push (children x) sibs)
    )
    
    ;collapse multiple lists into singular list!
    (setf sibs (loop for outer in sibs
      nconcing (loop for inner in outer collecting inner)))
    (remove name (remove-duplicates sibs))
)


(defun brothers (name)
    (sexFilter (siblings name) 'male)
)

(defun sisters (name)
	(sexFilter (siblings name) 'female)
)

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

(defun aunts (name)
	(sexFilter (aunts-and-uncles name) 'female)
)

(defun uncles (name)
	(sexFilter (aunts-and-uncles name) 'male)
)

(defun nieces-and-nephews (name)
    (setf nieneph nil)
    (dolist (x (siblings name))
        (push (children x) nieneph)
    )
    
    ;collapse multiple lists into singular list!
    (setf nieneph (loop for outer in nieneph
      nconcing (loop for inner in outer collecting inner)))
    (remove name (remove-duplicates nieneph))
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
    (setf couslist (loop for outer in couslist
      nconcing (loop for inner in outer collecting inner)))
    (remove name (remove-duplicates couslist))
)

(defun female-cousins (name)
    (sexFilter (cousins name) 'female)
)

(defun male-cousins (name)
    (sexFilter (cousins name) 'male)
)