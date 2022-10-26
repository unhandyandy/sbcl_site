(in-package :gameframe)


(defvar initBdTabAux (makeregister))

(defun initBdTab () (funcall initBdTabAux))

(defvar buttonTabAux (makeregister))

(defun buttonTab () (funcall buttonTabAux))


;;(load "/home/dabrowsa/lisp/ltk-0.91/ltk")


;;(require 'ltk)

;(with-ltk ()

;;(defvar buttontab)

;;(setf buttontab (copy-tree initbdtab))

(push :tk84 *features*)

(defvar mainwindow 
;  (make-instance 'frame)
)




(defun makepanel (vctmat)
  (mapc #'makerow vctmat)
)


(defun textwin (n)
  (concatenate 'string 
	       "Player " 
	       (write-to-string n)
	       " wins!"))


(defvar controls)

(defvar controlstab)

(defun launchframe (file)
  (let ((fileloc 
	 (concatenate 'string
		      "/home/dabrowsa/lisp/"  file  ".lisp")))
    (load fileloc)
    (makegameframe (initbdtab))))

;(defvar messagetext "messages") 

(defvar messagewidget) 

;;(load "gamesengine02.lisp")
;;(require 'gamesengine)

;; (defun makerow (vctlst)
;;   (let ((f (make-instance 'frame 
;; 			  :master mainwindow)))
;;     (dolist (btls vctlst)
;;       (if (equal (second btls) nil)
;; 	  (pack (apply #'makebutton 
;; 		       btls)
;; 		:side 'left)
;; 	  (pack (apply #'makebutton 
;; 		       (append (subseq btls 0 2)  
;; 			       (list #'buttonfn f)
;; 			       (subseq btls 2)
;; 		       )
;; 		)
;; 		:side 'left)
;;       )
;;     (pack f)
;;     )
;;   )
;; )

(defun whichButton (btls f)
  (if (equal (second btls) nil)
		 (apply #'makebutton 
			btls)		 
		 (apply #'makebutton 
			(append (subseq btls 0 2)  
				(list #'buttonfn f)
				(subseq btls 2)))))

(defun makerow (vctlst)
  (let ((f (make-instance 'frame 
			  :master mainwindow)))
    (dolist (btls vctlst)
      (let ((bt (whichButton btls f)))
	(pack bt :side 'left)
	)
      (pack f))))


(defun makebutton (text data func master 
		   &key (bg "white") (fg "black") (width 9))
  (let ((btn (make-instance 'button 
		       :text text 
		       ;;:font "-*-BPmono-Bold-Regular-*--*-140-*"
		       :master master
		       :command #'(lambda ()
				  (funcall func data))
		       ;:foreground fg
		       ;:background bg
		       :width width)))
;;    (format t (write-to-string btn))
    (if (not (equal data nil))
	(treesetl (buttontab) btn data))
    btn))

(defun setButtonProps (name txt &optional (fgc nil) (bgc nil)) 
   
      (If (not (equal txt nil))
;	  (configure (apply #'treeref (buttontab) name) :text txt)
	  (setf (text (treerefl (buttontab) name)) txt)
      )
      (If (not (equal fgc nil))
	  (configure (treerefl (buttontab) name) :foreground fgc)
     )
      (If (not (equal bgc nil))
;	  (configure (apply #'treeref (buttontab) name) :background bgc)
	  (configure (treerefl (buttontab) name) :background bgc)
      )
)


;; (setf postMessage 
;; (lambda (str)
;; ;      (format t "posting message...")
;; ;      (configure messagewidget :text str)
;;       (setf (text messagewidget) str)
;; ))

(funcall gamesengine:postmessageaux 
	 :new (lambda (str) (setf (text messagewidget) str)))


(defun makegameframe (bdtab)
  (with-ltk ()
;  (start-wish)

;;    (setf mainwindow (make-instance 'frame :background "black"))
    (setf mainwindow (make-instance 'frame))
    (setf messagewidget 
      (make-instance 'label
		     :text "messages" 
;		     :state 'disabled
;		     :justify 'center 
		     :master mainwindow
      )
    )
    (setf controls 
	  (make-instance 'frame 
			 :master mainwindow
          )
    )
    (setf controlstab
	  (list 
		     (list "Undo"
			   nil
			   #'undoFn
			   controls)
		     (list "New Game"
			   nil
			   #'initgame
			   controls))
    )
    (pack messagewidget  :expand t :fill 'x)
    (makepanel bdtab)
    (makerow controlstab)
    (pack controls :side 'bottom)
    (pack mainwindow)

    (initEngine)
;;    (format t (write-to-string depthTable))
;(mainloop)
  )
)

(defun dispMove (mov)
   (let (dsp fun)
     (setq dsp (Transpose mov))
     (setf (first dsp)
	   (multi-subst (first dsp)
			'("a" 0) '("b" 1)
			'("c" 2) '("d" 3)
			'("e" 4) '("f" 5)
			'("g" 6) '("h" 7)
           )
     )      
     (setf (second dsp)
	   (map 'list #'write-To-String 
		   (vector+ #(1 1) 
			    (second dsp)
		   )
	   )
     )
     (setq fun #'(lambda (m n)
		   (concatenate 'string
				(first m)
				(second m)
				"-" 
				(first n)
				(second n)
                   )
                 )
     )
     (apply fun (transpose dsp))
  )
)

;)
