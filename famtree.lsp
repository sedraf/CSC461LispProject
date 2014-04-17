#|
        ***** famtree.lsp *****

Emulate a main function in Lisp, with command-line arguments.

Authors: John M. Weiss, Ph.D. and Hafiza Farzami
|#

; main function
(defun main ( filename )
    "(main args): emulate a main function, called with command-line args"

    (when (null filename) (return-from main (format nil "Usage: fileio.lsp filename")))

    (format t "~%Opening file ~a using open~%" filename)

    (setf *database* 'nil)
    (defstruct person name sex children parents)

    (setf fin (open filename :if-does-not-exist nil))   ; open file, returning NIL on error
    (when (null fin) (return-from main (format nil "Error: cannot open file ~a" filename)))
    (do ((data (read fin nil) (read fin nil)))          ; read entire file, returning NIL at EOF
        ((null data) (close fin))                       ; exit when file is read
	(setf individual (make-person :name (first data) :sex (second data) :children (third data) :parents (fourth data)))
	(push individual *database*)
    )
    (nreverse *database*)

    (load "descendants.lsp")
    (load "ancestors.lsp")
    (load "others.lsp")
)

(main (car *args*))
