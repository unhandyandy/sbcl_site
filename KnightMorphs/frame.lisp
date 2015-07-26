
(defpackage :gerrymander-hex_frame
  (:use :common-lisp 
	#+(or :cmu :scl) :ext
	#+:sbcl :sb-ext
	:mydefs 
	:hexutils 
	:gerrymander-hex_engine
	:gameframe
	)
  (:export start
;;	   initBdTab
;;	   hexsIZE
   )
)


(in-package :gerrymander-hex_frame)


;;(load "mydefs")
;;(load "hexutils01")



(defvar gAMEnAME 
  (concatenate 'string 
	       "GerrymanderHex"
	       (write-to-string hexsize)
  )
)

;;(defvar gameframe:initBdTab)

(funcall gameframe:initBdTabAux
	 :new (let ((bd (hexArray hexsIZE))
		    (fun #'(lambda (v)
			     (let ((i (elt v 0))
				   (j (elt v 1))
				   )
			       (list "     
     " (list i j) :bg "darkgray")
			       )
			     )
		      )
		    )	    
		(map-at-level fun bd 2)))

(funcall gameframe:buttonTabAux :new (copy-tree (gameframe:initbdtab)))


;;(load "gerrymanderhexengine01")

;;(load "gameframe01")

(defun start ()
  (makegameframe (gameframe:initBdTab))
  1
)


