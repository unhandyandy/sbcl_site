
(defpackage :knightMorphs_engine
  (:use :common-lisp 
	#+(or :cmu :scl) :ext
	#+:sbcl :sb-ext
	:mydefs :hexutils
	)
  (:export dispMove
;;	   nUMcHOICES
	   nbrOccN
	   movNbrOccN
	   sortMoves
	   rowLen
	   colorFn
;;	   posinit
	   valLoc
	   aveList
	   aveList
	   normalize-votes
	   aveCell
	   valCell
;;	   evalPosUncert
	   evalScore
;;	   cUToFF
	   nbrsLow
	   nbrsHigh
	   nbrs
	   nbrsAll
	   emptyLocQ
	   posAll
	   gamesOver
;;	   winQ
;;	   lossQ
;;	   drawQ
	   stitchMove
;;	   movesFromPos
;;	   positionFromMove
	   dispFunc
;;	   poscurToDisplay
;;	   desireddepth
	   comp
	   hexsIZE
	   playersgn
	   groupOf
;;	   moveThinN
   )
)


(in-package :knightMorphs_engine)

(setq gamesengine:gamename (lambda () "knightMorphs"))

;;(require 'mydefs)

;;(load "hexutils01")

(defvar sIZE 6)

(defstruct (numberPiece
	     (:constructor
	      create-piece (plyr value)))
  plyr value)

(defstruct arithPos tab plyr goalRows (white t) (moves nil) )

(defvar *allLocs*
  (let ((l (number-sequence :to (1- sIZE))))
    (cartesianProduct l l)))


						 
(defun nbrOccN (loc pos)
  (let (fun)
    (setq fun #'(lambda (nbr)
		  (emptylocq 
		   (boardref 
		    pos 
		    nbr)
		  )
		)
    )
    (count nil (mapcar fun (nbrsall loc)))
  )
)

(defun occFlt (ls pos)
  (let ((fun #'(lambda (nbr)
		  (emptylocq 
		   (boardref 
		    pos 
		    nbr)))))
    (remove-if fun ls)))


(defun occN (ls pos)
  (list-length (occFlt ls pos)))

(defun onBoardQ (loc) (checkOnBoardQ loc hexsIZE))

(defun mutualsN (mv pos)
  (occN (remove-if-not #'onBoardQ (mutuals mv hEXsIZE)) pos))

(defun movThinN (mv pos)
  (let* ((allnbrs (mapcar
		   (lambda (ls) (occFlt ls pos))
		   (mapcar #'nbrsall mv)))
	 (allcnns (apply #'append (mapcar (lambda (lst) 
					    (let ((ls (car lst))
						  (el (cadr lst)))
					      (mapcar (lambda (x) (list x el)) ls)))
					    (transpose (list allnbrs mv)))))
	 (cnns (remove mv allcnns :test #'equalp)))
    (if (or (= (list-length cnns) 1)
	    (and (= (list-length cnns) 3)
		 (= (list-length (apply #'setunion cnns)) 3)))
	'(10)
	(mapcar (lambda (mv) (mutualsN mv pos)) cnns))))
	  


(defun sortOrder (l1 l2)
  (let ((n1 (apply #'min (car l1)))
	(n2 (apply #'min (car l2))))
    (or (< n1 n2)
	(and (= n1 n2)
	     (>= (count n1 l1) (count n1 l2))))))
    



(defun sortMoves (mvs pos)
  (if (null mvs) 
      (return-from sortMoves nil))
  (let* ((fun #'(lambda (mv)
		  (movThinN mv pos)
                )
         )
	 (occs (mapcar fun mvs))
	 (srt (Sort 
	       (Transpose (list occs mvs))
	       #'sortOrder
	      )
	 )
       )
    ;; (if (< (length srt) 5)
    ;; 	(hiya (second (transpose srt))))
    (second (transpose srt))
   )
)


(defun rowLen (r)
  (- (- (* 2 hexSize) 1) 
     (Abs (- hexSize (+ r 1))))
)

(defun colorFn (r c)
  (let ((rng (- 
	      (Min (1+ r)
		   (1+ c) 
		   (- (rowLen r) c) 
		   (- (* 2 hexSize) (1+ r))
	      )
	      1
	     )
       ))
    (if (> (- (rowlen r) rng) (1+ c))
	(if (evenp (+ r c))
	    '(0 1) 
	    '(1 0)
	)
	(if (evenp (+ r hexSize 1))
	    '(0 1)
	    '(1 0)
	)
    )
  )
)

(defun colorFnV (pos)
  (let ((row (aref pos 0))
	(col (aref pos 1)))
    (colorFn row col)))

;;(defvar posinit nil)

(setf  gamesengine:pOSiNIT
  (let (tab)
    (setq tab (hexArray hexsIZE))
    (Map-at-level #'(lambda (vct)
		      (colorFnV vct)
		    )
		  tab 2)
  )
)
  
;;(format t (write-to-string posinit))


(defun valLoc (vts plyr)
  (let ((val (apply #'+ vts))
	(sgn (- 3 (* 2 plyr)))
       )
    (If (apply #'= vts)
	0
	(* sgn 
	   (If (apply #'> vts)
	       val
	       (- val)
	   )
	)
    )
  )
)


(defun aveList (ls)
  (let ((num (Length ls))
       )
    (If (= num 0)
	'(0 0)
	(let ((fun #'(lambda (&rest args)
		       (/ (apply #'+ args) num)
		     )
	     ))
	  (apply-mapcar fun ls)
	)
    )
  )
)


(defun normalize-votes (ls)
  (let ((sm (apply #'+ ls)))
    (mapcar #'(lambda (x)
		(/ x sm)
	      )
	    ls
    )
  )
)

(defun nbrWgt (loc pos)
  (let ((num (nbrOccN loc pos)))
    (if (= num 0)
	0
	(/ 1 num))))


(defun aveCell (loc pos)
  (If (equal (boardref pos loc)
	     '(0 0)
      )
      '(0 0)
      (let ((nbrhd (nbrsAll loc))
	    (val (apply #'+ 
			(boardref pos loc)
		 )
	    )
	    clls
	    fun
	   )
	(setq fun 
	      #'(lambda (loc)
		  (mapcar 
		   (mult-Fun (nbrWgt loc pos)) 
		   (boardref pos loc))
		)
	)
	(setq clls (mapcar fun nbrhd))
	(setf clls (cons (boardref pos loc) clls))
	(setf (car clls) (mapcar (mult-Fun 2) (car clls)))
	(setf clls (remove '(0 0) clls))
	(setf clls (apply-mapcar #'+ clls))
	(let ((diff (apply #'- clls)))
	  (cond
	    ((< diff -1) (list 0 val))
	    ((> diff  1) (list val 0))
	    (t (list 0 0)))))))



(defun valCell (loc pos plyr)
  (valLoc
   (aveCell loc  pos)
   plyr
  )
)

(defun groupOfAux (grp locs pos res)
  (let* ((news (apply #'setunion (mapcar #'nbrsall grp)))
	 (newsocc (remove-if 
		  (lambda (nbr)
		    (emptylocq 
		     (boardref 
		      pos 
		      nbr)))
		  news)))
    (if (subsetp newsocc grp :test #'equalp)
	(if (null locs)
	    (cons grp res)
	    (let* ((newgrp (list (car locs)))
		   (newlocs (remove-if (lambda (x) (position x newsocc :test #'equalp)) (cdr locs)))
		   (newres (cons grp res)))
	      (groupOfAux newgrp newlocs pos newres)))
	(let* ((newgrp (setunion grp newsocc))
	       (newlocs (remove-if (lambda (x) (position x newsocc :test #'equalp)) locs)))
	  (groupOfAux newgrp newlocs pos res)))))


(defun groupOf (pos)
  (let* ((locs (posAll))
	 (locsocc (remove-if 
		  (lambda (nbr)
		    (emptylocq 
		     (boardref 
		      pos 
		      nbr)))
		  locs)))
    (groupOfAux (list (car locsocc)) (cdr locsocc) pos nil)))

(defun grpVal (grp pos plyr)
  (let* ((num (list-length grp))
	 (vals (mapcar (lambda (loc) (boardref pos loc)) grp))
	 (valtot (apply-mapcar #'+ vals))
	 (diff (apply #'- valtot))
	 (valave (/ (apply #'+ valtot) num))
	 (cellswon (min (* 4 (abs diff)) num))
	 (valwon (* cellswon valave)))
    (cond
      ((= diff 0) 0)
      ((> diff 0) (* -1 (playerSgn plyr) valwon))
      ((< diff 0) (* (playerSgn plyr) valwon)))))

(defun gamesengine:evalPosUncert (pos plyr)
  (let ((vals (mapcar (lambda (grp) (grpVal grp pos plyr)) (groupof pos))))
    (- (apply #'+ vals) (* (playerSgn plyr) komi))))



;; (defun gamesengine:evalPosUncert (mat plyr)
;;   (let (res fun)
;;     (setq res (apply #'setunion (posAll)))
;;     (setq fun #'(lambda (loc)
;; 		  (valCell loc mat plyr)
;; 		)
;;     )
;;     (setf res (mapcar fun res))
;;     (setf res (- (apply #'+ res) (* (playerSgn plyr) komi)))
;;     (gamesengine:truncEval res)
;;   )
;; )

(defun playerSgn (plyr)
  (- (* 2 plyr) 3))

(defun evalScore (mat plyr)
  (let (res)
    (setq res (apply #'append mat))
    (setf res (mapcar 
	       #'(lambda (loc)
		   (valLoc loc plyr)
		 )
	       res
	      )
    )
    (- (apply #'+ res) (* (playerSgn plyr) komi))
  )
)
    
(defun nbrsLow (pos)
  (nbrs pos (list (hexV 0) (hexV 1) (hexV 2)))
)

(defun nbrsHigh (pos)
  (nbrs pos (list (hexV 3) (hexV 4) (hexV 5)))
)

(defun nbrsAll (pos)
  (nbrs pos)
)


(defun nbrs (pos &optional
	     (dirs (mapcar #'hexV 
			   (number-sequence :to 5)
		   )
	     )
	    )
  (let (res fnc)
    (setq res (mapcar #'(lambda (d)
			  (moveHex pos d hexSize)
			)
		      dirs
	      )
    )	  
    (setq fnc #'(lambda (loc)
		  (checkOnBoardQ loc hexsIZE)
		)
    )
    (setf res (remove-if-not fnc res))
;;     (setf fnc #'(lambda (v)
;; 		  (if (vectorp v)
;; 		      (twovectortolist v)
;; 		      v
;; 		  )
;;                 )
;;     )
;;     (mapcar fnc res)
    res
  )
)

(defun emptyLocQ (loc)
  (And (= (first loc) 0)
       (= (second loc) 0)
  )
)


(defun posAll () 
  (apply #'append (hexArray hexsIZE))
)


(defun gameOver (pos &optional
		 (rem (posAll))
		)
  (If (null rem)
      t
      (let* ((curloc (First rem))
	     (curpos (boardref pos curloc)
	    ))
	(If
	 (and (Not (emptyLocQ curpos))
	      (let ((fun
		     #'(lambda (loc)
			 (not (emptylocq 
			       (boardref 
				pos 
				loc)
			      )
			 )
		       )
		    )
		    (nbrs (nbrslow curloc))
		   )
	       (if (null nbrs)
		   nil
		   (apply #'funOr
			  (mapcar fun nbrs)
		   )
	       )
	      )
	 )
	 nil
	 (gameOver pos (rest rem))
      )
   )
  )
)

(defun gamesengine:winQ (mat plyr)
  (and (gameOver mat) 
       (> (evalScore mat plyr) 0)
  )
)

(defun gamesengine:lossQ (mat plyr)
  (gamesengine:winQ mat (- 3 plyr))
)

(defun gamesengine:drawQ (mat plyr)
  (and (gameOver mat)
       (= (evalScore mat plyr) 0)
  )
)


(defun stitchMove (m1 m2)
  (let ((v1 (first m1))
	(l1 (second m1))
	(v2 (first m2))
	(l2 (second m2))
	(res nil)
       )
    (If (<= v1 v2)
	(setf res (Append res 
			  (list (list l1 l2))
		  )
	)
    )
    (If (>= v1 v2)
	(setf res (Append res
			  (list (list l2 l1))
		  )
	)
    )
    res
  )
)


(defun gamesengine:movesFromPos (pos plyr)
  (let ((res (movesFromPosAux pos plyr)))
    (sortMoves res pos)
  )
)


(defun movesFromPosAux 
    (pos plyr &optional
     (rem (posAll))
    )
  (If (null rem)
      nil
      (let* ((curloc (First rem))
	     (curpos (apply #'+
			    (boardref 
			     pos 
			     curloc)
		     )
	    ))
	(If (= curpos 0)
	    (Return-from movesFromPosAux
	      (movesFromPosAux pos plyr 
			       (rest rem))
	    )
	    (let (res nbrs fun)
	      (setq nbrs (nbrsLow curloc))
	      (setq fun 
		    #'(lambda (loc)
			(apply #'+ 
			       (boardref 
				pos 
				loc)
		        )
	              )
	      )
	      (setq res (mapcar fun nbrs))
	      (setf nbrs (Transpose
			  (list res nbrs))
	      )
	      (setf res (remove-if-not
			 #'(lambda (x)
			     (> (first x) 0)
			   )
			 nbrs
			)
	      )
	      (setf fun #'(lambda (r)
			    (list r (list curpos curloc))
			  )
	      )
	      (setf res (mapcar fun res))
	      (setf res (map-apply #'stitchMove
				res
			)
	      )
	      (setf res (apply #'append res))
	      (Return-from movesFromPosAux
		(append res
			(movesFromPosAux pos plyr 
					 (rest rem)
			)
		)
	      )
	    )
))))


(defun gamesengine:positionFromMove (mov pos plyr)
  (let* ((st (First mov))
	 (fn (second mov))
	 (new (mapcar #'+
		      (boardref pos st)
		      (boardref pos fn)
	      )
	 )
	 (res (copy-tree pos))
	)
;    (format t "Hello?")
    (boardset res st '(0 0))
    (boardset res fn new)
    res
  )
)

(defun dispFunc (m n)
  (concatenate 'string
	       
	       (make-string 
		m 
		:initial-element #\u25ce)
	       
	       "
"
	       
	       (make-string 
		n 
		:initial-element #\u254b)
	       
  )
)

(defun gamesengine:poscurToDisplay (pos)
  (MapAtLevel
   #'(lambda (val loc)
       (list (apply #'dispFunc val) nil (cellBG loc))
       )
   2
   pos
   (hexArray hexSize)))

(defun cellMvFun (loc)
  (let ((res (find loc (car gamesengine:history) :key #'car :test #'equalp)))
    (if (null res) 
	res
	(cadr res))))

(defun mapCellDest (loc)
  (let ((res (cellMvFun loc)))
    (if (null res)
	loc
	(mapCellDest res))))

(defun cellBG (loc)
  (let* ((dest (mapCellDest loc))
	 (val (apply #'- (boardref gamesengine:poscur dest))))
    (cond ((< val 0) "#8af")
	  ((> val 0) "#fa2")
	  ((= val 0) "darkgray"))))


(setf gamesengine:numchoices 12)

(setf gamesengine:cUToFF 10)

(setf gamesengine:desireddepth 4)

(setf gamesengine:comp 1)

