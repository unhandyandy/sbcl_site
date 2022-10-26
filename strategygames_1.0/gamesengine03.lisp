
#|-----------------------------------------------------------------------------
Artificial Intelligence, Second Edition
Elaine Rich and Kevin Knight
McGraw Hill, 1991

This code may be freely copied and used for educational or research purposes.
All software written by Kevin Knight.
Comments, bugs, improvements to knight@cs.cmu.edu
----------------------------------------------------------------------------|#

#|----------------------------------------------------------------------------
		             MINIMAX SEARCH
			    "minimax.lisp"
----------------------------------------------------------------------------|#

#|-----------------------------------------------------------------------------

 Minimax game playing.

 This file contains functions for doing game-playing search.  This 
 program will play any game for which the following functions and
 variables are defined:

	(print-board b)
	(movegen pos player)
	(opposite player)
	(static pos player)
	(won? pos player)
	(drawn? pos)
	(deep-enough pos depth)
	(make-move pos player move)
	*start*

 These functions are implemented for tic-tac-toe in the file 
 "tictactoe.lisp".

----------------------------------------------------------------------------|#

(in-package :gamesengine)


(defvar comp 1)

(defvar nUMcHOICES)

(defvar gamename (lambda () ""))

(defun getgamename ()
    (funcall gamename))


;(load "memoize-1.6/memoize")

;;(asdf:operate 'asdf:load-op 'memoization)

;(load "/usr/share/common-lisp/systems/memoization.asd")

;;(require 'memoization)

;; (defvar MEMOIZATION::*Source-File-Extension* ".lisp")

;; (defvar MEMOIZATION::*Compiled-File-Extension* 
;;   #+:sbcl ".fasl"
;;   #+:ecl ".fas")


(defun movegen (pos player)
  (newFromPos pos player)
)

(defun opposite (player)
  (- 3 player)
)

(defun static (pos player)
  (evalposuncert pos player)
)

(defun win? (pos player)
  (winq pos player)
)

(defun loss? (pos player)
  (lossq pos player)
)

(defun drawn? (pos)
  (drawq pos status)
)

(defvar desireddepth)

(defun deep-enough (pos dp)
  (>= dp desireddepth)
)



(defvar poscur)
(defvar history)
(defvar status 1)
(defvar histbutt nil)


;(defparameter *start* posinit)


;; Function MINIMAX performs minimax search from a given position (pos), 
;; to a given search ply (depth), for a given player (player).  It returns
;; a list of board positions representing what it sees as the best moves for 
;; both players.  The first element of the list is the value of the board 
;; position after the proposed move.

(defun make-str (a b) (list a b))

(defun move-value (str) (car str))

(defun path (str) (cadr str))

(defun next-move (str) (car (path str)))



;; Function MINIMAX-A-B performs minimax search with alpha-beta pruning. 
;; It is far more efficient than MINIMAX.

;; (if (not (probe-file "minimax-a-b.lisp"))
;;     (with-open-file (out "minimax-a-b.lisp" 
;; 			 :direction :output
;; ;			 :if-exists :supersede
;; 			 :if-does-not-exist :create
;; 			 )))


(defvar depthTable)

(defun defineDepthTable ()
  (let ((fnm (concatenate 'string (getgamename) "-depthTable.fasl")))
    (if (probe-file fnm)
	(load fnm)
	(setq depthTable (make-hash-table :test #'equal)))))

(defineDepthTable)

(defvar minimaxABcache)

(defun defineMinimaxCache ()
  (let ((fnm (concatenate 'string (getgamename) "-minimaxAB.fasl")))
    (if (probe-file fnm)
	(load fnm)
	(setq minimaxABcache (make-hash-table :test #'equal)))))

(defineMinimaxCache)


(defparameter *maxval* 100000)

(defun minimax-a-b (pos depth player)
  (let* ((newdep (max depth (getDepth pos player)))
	 (hashkeymnmx (list pos player))
	 ;;(hashkeydptb (list pos player))
	 (cchval (gethash hashkeymnmx minimaxABcache)))
    ;;(hiya cchval)
    (if (null cchval)
	(let ((res (minimax-a-b-1 pos newdep player *maxval* (- *maxval*))))
	  (setf (gethash hashkeymnmx minimaxABcache) res)
	  (setf (gethash hashkeymnmx depthTable) newdep)
	  (return-from minimax-a-b res))
	(return-from minimax-a-b cchval))))


(defun minimax-a-b-1 (pos depth player use-thresh pass-thresh)
  (let ((deptab (getDepth pos player)))
    (cond ((>= deptab depth)
	   (return-from minimax-a-b-1 (minimax-a-b pos deptab player)))
	  ((= depth 0)
	   (return-from minimax-a-b-1 (make-str (static pos player) nil)))
	  (t 
	   (let* ((newmvs (movesfrompos pos player))
		  (successors (if (>
				   (length newmvs)
				   numChoices
				   )
				  (subseq newmvs 0 numChoices)
				  newmvs
				  )
		    )
		  (best-path (list (first successors)))
		  )
	     ;;(if (< (length successors) 2) (hiya successors))
	     (cond ((null successors) (make-str (static pos player) nil))
		   (t
		    (do ((s successors (cdr s)) (quit nil))
			((or quit (null s)))
		      (let* ((succ (car s))
			     (newplayer (opposite player))
			     (newpos (positionfrommove succ pos player))
			     (new-value (cond ((win? newpos newplayer)
					       (make-str *maxval* nil))
					      ((loss? newpos newplayer)
					       (return-from minimax-a-b-1 (make-str *maxval* (list succ))))
					      (t
					       (minimax-a-b-1 newpos
							      (1- depth)
							      newplayer
							      (- pass-thresh)
							      (- use-thresh))))))
			(let ((newmovval (- (move-value new-value))))
			  (when (> newmovval pass-thresh)
			    (setq pass-thresh newmovval)
			    (setq best-path 
				  (cons succ
					(path new-value))))
			  (when (>= pass-thresh use-thresh) (setq quit t)))))
		    (make-str pass-thresh best-path))))))))
  

;; Function PLAY allows you to play a game against the computer.  Call (play) 
;; if you want to move first, or (play t) to let the computer move first.


;; (defun play (&optional machine-first?)
;;   (let ((b *start*))
;;     (when machine-first? 
;;       (let ((m1 (minimax-a-b b 0 'o)))
;;         (setq b (next-move m1))))
;;     (do ()
;; 	((or (won? b 'x) (won? b 'o) (drawn? b))
;; 	 (format t "Final position: ~%")
;; 	 (print-board b)
;; 	 (when (won? b 'o) (format t "I win.~%"))
;; 	 (when (won? b 'x) (format t "You win.~%"))
;; 	 (when (drawn? b)  (format t "Drawn.~%")))
;;       (print-board b)
;;       (format t "Your move: ")
;;       (let ((m (- (read) 1)))
;; 	(setq b (make-move b 'x m))
;; 	(when (not (drawn? b))
;; 	  (print-board b)
;; 	  (let ((m1 (minimax-a-b b 0 'o)))
;; 	    (setq b (next-move m1))
;; 	    (if (and (not (drawn? b))
;; 		     (not (won? b 'o))) 
;; 		(format t "My move: ~%"))))))))



(defun compTurn ()
  (let ((mvs (movesFromPos pOScUR sTATUS)))
    (postMessage "thinking...")

;; (mydefs:hiya "(get 'minimax-a-b :Memo-Table)")
;; (hiya "(equal (get 'minimax-a-b :Memo-Table) nil)")

;;   (if (equal (get 'minimax-a-b :Memo-Table) nil)
;;       (setf (get 'minimax-a-b :Memo-Table) 
;; 	    (make-hash-table)))

;; (hiya "(get 'minimax-a-b :Memo-Table)")

    (If (equal mvs nil)
	(postPosition pOScUR)
	(let* ((dep (max desireddepth (getDepth poscur status)))
	       (new (minimax-a-b 
		     poscur 
		     dep
		     status))
	       (mov (next-move new))
              )
;	  (format t (write-to-string poscur))
;	  (format t (write-to-string mov))
	  (updatePosCur mov)
	)
    )
    (If (not pAUSE)
       (postMessage "Your move")
    )
  )
)




;;(load "/home/dabrowsa/lisp/mydefs.lisp")

(defvar pause)

(defun setpause (&optional (val t))
  (setq pause val)
)


(defun updateposcur (new) 
  (setf (first history) (cons new (first history)))
  (setf (second history) (cons poscur (second history)))
  (setf pOScUR (positionFromMove new pOScUR sTATUS))
  (setf hISTbUTT nil)
  (postPosition pOScUR)
  (process-events)
  (setf sTATUS (- 3 sTATUS))

  (cond
    ((winQ pOScUR sTATUS) 
     (postMessage (gameframe:textWin sTATUS))
     (setPause)
     (postMortemCheck (- 3 sTATUS)))
 
    ((lossQ pOScUR sTATUS)
     (postMessage (gameframe:textWin (- 3 sTATUS)))
     (setPause)
     (postMortemCheck sTATUS))
 
    ((drawQ pOScUR sTATUS)
     (postMessage "Game Drawn")
     (setPause))
  )
)



;; (defvar poscurToDisplay (makeregister))
;; ;;(funcall poscurToDisplay :new (lambda (pos) nil))


;; (defmacro postPosition (pos)
;;   `(let ((bdtab (funcall (funcall poscurToDisplay) ,pos)))
;;     (postPositionCol bdtab)))

(defun postPosition (pos)
  (let ((bdtab (poscurToDisplay pos)))
    (postPositionCol bdtab)))


(defun postPositionCol (rows &optional (rownum 0))
   (If (equal rows nil) 
       (Return-from postPositionCol nil)
       (progn
	        (postPositionRow (First rows) rownum)
		(postPositionCol (rest rows) (+ rownum 1))
       )
   )
)



(defun postPositionRow (row rownum &optional (colnum 0)) 

   (If (equal row nil)
       (Return-from postPositionRow nil)
      (progn
       (apply #'gameframe:setButtonProps (list rownum colnum) (First row))
       (postPositionRow (rest row) rownum (+ colnum 1))
    ))
)


(defun clearAllCache ()
  (setq minimaxABcache (make-hash-table :test #'equal))
  (setq depthTable (make-hash-table :test #'equal))
  ;; also wipe saved files!
  (let ((fnmlsp (concatenate 'string (getgamename) "-minimaxAB.lisp"))
	(fnmfsl (concatenate 'string (getgamename) "-minimaxAB.fasl")))
    (if (probe-file fnmlsp)
	(delete-file fnmlsp)
	)
    (if (probe-file fnmfsl)
	(delete-file fnmfsl)
	)
    )
  (let ((fnmlsp (concatenate 'string (getgamename) "-depthTable.lisp"))
	(fnmfsl (concatenate 'string (getgamename) "-depthTable.fasl")))
    (if (probe-file fnmlsp)
	(delete-file fnmlsp)
	)
    (if (probe-file fnmfsl)
	(delete-file fnmfsl)
	))
  )

(defvar posinit 37)


(defun setup ()

  (setPause nil)
  (setf sTATUS 1)
  (setf hISTORY (list nil nil))
  (setf hISTbUTT nil)
   
  (setf pOScUR pOSiNIT)
;;  (format t (write-to-string poscurtodisplay))
  (postPosition pOScUR)

  (setf cOMP (- 3 cOMP))
   
  (If (equal cOMP  sTATUS)
    (compTurn)))
;;  (format t (write-to-string depthTable))



;; (defvar save-path 
;;   (make-pathname :directory (list :absolute
;; 				  "home" "dabrowsa" "lisp")
;; 		 :name (concatenate 'string "evalDeep-" name)
;; 		 :type "lisp"
;;   )
;; )

;;(defvar newMessageText "")

(defvar postMessageAux
  (let ((setfun (lambda (str) nil)))
    (lambda (&key (str "") (new nil))
      (if (equal new nil)
	  (funcall setfun str)
	  (setf setfun new)))))

(defun postMessage (str)
  (funcall postmessageaux :str str))


(defun initGame (args)
  (postMessage "New Game")
;;  (format t (write-to-string (probe-file "minimax-a-b.lisp")))
  (if (probe-file "minimax-a-b.lisp")
      (delete-file "minimax-a-b.lisp")
  )
  (if (probe-file "minimax-a-b.fasl")
      (delete-file "minimax-a-b.fasl")
  )

  ;; (if (equal (get 'minimax-a-b :Memo-Table) nil)
  ;;     (setf (get 'minimax-a-b :Memo-Table) 
  ;; 	    (make-hash-table)))

  ;; (format t (write-to-string 
  ;; 	     (get 'minimax-a-b :Memo-Table)))

;;  (save-memo-table 'minimax-a-b)

;;  (format t (write-to-string depthTable))
(let ((dtnm (concatenate 'string (getgamename) "-depthTable.lisp")))
  (with-open-file 
      (out dtnm
	   :direction :output
	   :if-exists :supersede
	   :if-does-not-exist :create
      )
    (format out "(setf gamesengine:depthTable '#.gamesengine:depthTable)")

;    (multiple-value-bind (cf if) (make-load-form depthTable)
;      (write if :stream out)
;    )
   )
  (compile-file dtnm))

(let ((mnnm (concatenate 'string (getgamename) "-minimaxAB.lisp")))
  (with-open-file 
      (out mnnm
	   :direction :output
	   :if-exists :supersede
	   :if-does-not-exist :create
      )
    (format out "(setf gamesengine:minimaxABcache '#.gamesengine:minimaxABcache)")

;    (multiple-value-bind (cf if) (make-load-form depthTable)
;      (write if :stream out)
;    )
   )
  (compile-file mnnm))


  (setup))

(defun initEngine ()
  (defineDepthTable)
)

(defun buttonFn (arg)
  (let* ((hst hISTbUTT)
	 (nwmv (Append hst (list arg)))
	 (mvVct (map-apply #'vector nwmv))
        )
;    (format t (write-to-string arg))
;    (format t (write-to-string nwmv))
;    (format t (write-to-string mvVct))
;(format t (write-to-string (movesFromPos pOScUR sTATUS)))
    
    (postMessage (concatenate 'string "New move: " 
			       (write-To-String nwmv)
		 )
    )
    (If (and (equal sTATUS (- 3 cOMP)) 
	     (not pAUSE))

	(If (Member mvVct 
		    (movesFromPos pOScUR sTATUS)
		    :test #'equalp
            )
       
	    (progn
	     (updatePosCur mvVct)
   	     (If (and (equal cOMP sTATUS) (not pAUSE))
        	(compTurn)
             )
            )
	    (setf hISTbUTT nwmv)
	)
    )
  )
)


(defun newFromPos (pos plyr)
  (mapcar #'(lambda (mv) 
	      (positionFromMove mv pos plyr))
	  (movesFromPos pos plyr)
  )
)

(defun undoFn (arg)
  (let ((move hISTbUTT)
	(movls (first hISTORY))
	(posls (second hISTORY))
       )
    (If (not (equal sTATUS cOMP))
	(progn 
	  (If (and (equal move nil)
		 (> (Length movls) 1))
	    (progn
	      (setf pOScUR (second posls))
	      (postPosition pOScUR)
	      (setf (second hISTORY) (cddr posls))
	      (setf (first hISTORY) (cddr movls))
            )
           )
	  (setf hISTbUTT nil)
	  (setPause nil)
	  (postMessage "Undo!")
        )
    )
   )
)



(defun repetitionQaux (pos plyr hist stt)
  (let ((hsls)
	(split (unzip hist))
       )
    (setq hsls 
	  (If (equal plyr stt)
	      (second split)
	      (first split)
          )
    )
    (Member pos hsls :test #'equal)
  )
)
   
(defun repetitionQ (pos plyr)
  (repetitionQaux pos plyr (second hISTORY) sTATUS)
)


(defun postmortemcheck (plyr)
  (If (= cOMP plyr)
      (postMortem hISTORY plyr)
  )
)

(defvar cUToFF 99999)

(defun truncEval (x)
  (If (<= (Abs x)  cUToFF)
   x
   (* (Signum x) 99999)
  )
)

(defun postMortem (hist plyr)
  (let ((fct 1))
    (setf numchoices (* fct numchoices))
    (let (pscur pslst hstrmn dep hsttot)
      (postMessage "Performing post-mortem...")
      
      (setq hsttot (second hist))
      (setf hsttot (cons poscur hsttot))
      
      (If (= sTATUS plyr)
	  (setf hsttot (rest hsttot))
	  )
      (setq pslst (First hsttot))
      (setq pscur (second hsttot))
      
      (setq hstrmn (subseq hsttot 2))
      (setq dep desireddepth)
      
      (do () ((reEval pscur pslst plyr dep))
	
	(when (< (Length hstrmn) 2)
	  (setf hstrmn hsttot)
	  (setf dep (1+ dep))
	  )
	(setf pslst (First hstrmn))
	(setf pscur (second hstrmn))
	(setf hstrmn (subseq hstrmn 2))
	)
      )
    (setf numchoices (/ numchoices fct))
    (postMessage "Performing post-mortem...done!")
    ))


(defun reEval (pscur pslst  plyr dep)
  ;;(hiya dep)
  (let* ((deptab (let ((trial (getDepth pscur plyr)))
		   (if (< trial 0)
		       desiredDepth
		       trial)))
	 (curdep (max dep deptab))
	 (newval (minimax-a-b-1 pscur curdep plyr (- *maxval*) *maxval*))
	 (newpos (positionfrommove (next-move newval) pscur plyr)))

    (If (or (< (move-value newval) 0)
	    (equal newpos pslst)   ;; but when are positions equal??
	    )
	(Return-from reEval nil)
	(progn
	  (setf (gethash (list pscur plyr) 
			 depthTable)
		curdep
	  )
	  (setf (gethash (list pscur plyr) 
			 minimaxABcache)
		newval
	  )
	  (Return-from reEval t)
	)
    )
  )
)



(defun getDepth (pos plyr)
  (let ((res (gethash (list pos plyr) depthTable))
       )
    (if res
	res
	-1
    )
  )
)
