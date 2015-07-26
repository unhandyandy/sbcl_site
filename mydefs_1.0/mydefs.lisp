(defpackage :mydefs
  (:use :common-lisp 
	#+(or :cmu :scl) :ext
	#+:sbcl :sb-ext
	)
  (:export memoize-init
	   treeref
	   treerefl
	   twovectortolist
	   treeset
	   treesetl
	   concatstring
	   unzip
	   transpose
	   outerproduct
	   number-sequence
	   setunion
	   vector+
	   vector2+
	   scalar*
	   mult-fun
	   map-at-level
	   mapAtLevel
	   multi-subst
	   map-apply
	   lexicalorder2
	   funOr
	   funAnd
	   apply-mapcar
	   memoize-init
	   makeRegister
	   hiya
	   cases
	   last1
	   left-rotate
	   right-rotate
	   remove-nth
	   makeScaledFun
	   partition
	   union1
	   toBaseN
	   toBaseTen
	   sortC
	   multiSub
	   ))

(in-package :mydefs)

(defun last1 (list)
  "Return the last element of a list."
  (first (last list)))

(defun left-rotate (list)
  "Move the first element to the end of the list."
  (append (rest list) (list (first list))))

(defun right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun cases (pred func lst)
  (let ((newlst (remove-if-not pred lst)))
    (mapcar func newlst)))

(defun makeRegister ()
  (let ((val nil))
    (lambda (&key (new nil))
      (if (not (equal new nil))
	  (setf val new))
      val)))

(defun treeref (tree n &rest rest)
  (if (equal rest nil)
      (nth n tree)
      (apply #'treeref (nth n tree) rest))
)

(defun treerefl (tree spec)
  (apply #'treeref tree spec)
)

(defun twovectortolist (v)
  (list (aref v 0) (aref v 1))
)

(defun treeset (tree val n &rest rest)
  (if (equal rest nil)
      (setf (nth n tree) val)
      (apply #'treeset (nth n tree) val rest))
)

(defun treesetl (tree val pos)
  (apply #'treeset tree val pos)
)


(defun concatstring (&rest args)
  (apply #'concatenate 'string args)
)

(defun unzip (ls &key (even nil) (odd nil))
  (if (equal ls nil)
      (list even odd)
      (unzip (rest ls) 
	     :even (append odd (list (first ls)))
	     :odd even
      )
  )
)

(defun transpose (x)
   (apply #'mapcar (cons #'list x)))

(defun outerproduct (l1 l2 &optional (fun #'list) (flat nil))
  (let ((lst (mapcar
	      #'(lambda (e1)
		  (mapcar 
		   #'(lambda (e2) (funcall fun e1 e2))
		   l2)
		  )
	      l1)))
    (if flat
	(apply #'append lst)
	lst)))

(defun number-sequence 
    (&key (from 0) to (inc 1) (incop #'+))
  (number-sequence-aux from to inc incop nil)
)
  
(defun number-sequence-aux
    (from to inc incop res)
  (if (> from to)
      (reverse res)
      (number-sequence-aux (funcall incop from inc)
			   to
			   inc
			   incop
			   (cons from res)
      )
  )
)

(defun setunion (l1 &rest args)
  (if (null args)
      l1
      (let ((head (first args))
	    (tail (rest args))
           )
	(apply #'setunion 
	       (union l1 head :test #'equalp)
	       tail
        )
      )
  )
)

(defun vector+ (v1 &rest args)
  (declare (vector v1))
  (declare (optimize (speed 3) (safety 1)))
  (if (null args)
      v1
      (let ((head (first args))
	    (tail (rest args))
           )
	(declare (vector head))
	(apply #'vector+ (map 'vector #'+ v1 head)
		 tail
        )
      )
  )
)

(defun vector2+ (v1 v2)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (declare (type (simple-array integer (2)) v1 v2))
  (let ((v10 (aref v1 0))
	(v11 (aref v1 1))
	(v20 (aref v2 0))
	(v21 (aref v2 1)))
    (declare (type fixnum v10 v11 v20 v21))
    (vector (+ v10 v20) (+ v11 v21))))
		 

(defun scalar* (s v)
  (map 'vector 
       #'(lambda (x)
	   (* s x)
	 )
       v
  )
)

(defun mult-Fun (m)
  #'(lambda (x) (* m x)))

(defun map-at-level (fun lst lev)
  (if (equal lev 0)
      (funcall fun lst)
      (mapcar #'(lambda (x)
		 (map-at-level
		  fun x (- lev 1)
		 )
		)
	      lst
      )
  )
)
(defun mapAtLevel (fun lev &rest lsts)
  (if (equal lev 0)
      (apply fun lsts)
      (mapcar #'(lambda (x)
		 (apply #'mapAtLevel
		  fun (1- lev) x))
	      (mydefs:transpose lsts))))


(defun multi-subst  (tree rule &rest args)
  (let ((sub 
	 (subst (first rule) 
		(second rule) 
		tree)
       ))
  (if (null args)
      sub
      (apply #'multi-subst sub args)
  )
  )
)

(defun map-apply (fun lst)
  (mapcar #'(lambda (l)
	      (apply fun l)
	    )
	  lst
  )
)

(defun lexicalorder2 (l1 l2)
  (cond ((null l1)
	 t)
	((null l2)
	 nil)
	(t
	 (let ((head1 (first l1))
	       (head2 (first l2))
	       (tail1 (rest l1))
	       (tail2 (rest l2))
              )
	   (if (and (numberp head1)
		    (numberp head2)
	       )
	       (or (< head1 head2)
		   (and (= head1 head2)
			(lexicalorder2 tail1 tail2)
	           )
	       )
	       (cond ((numberp head1)
		      t)
		     ((numberp head2)
		      nil)
		     (t
		      (if (equal head1 head2)
			  (lexicalorder2 tail1 tail2)
			  (lexicalorder2 head1 head2)
		      )
		     )
	       )
	   )    
      )
     )
   )
)

(defun funOr (arg1 &rest args)
  (if arg1
      t
      (if (null args)
	  nil
	  (apply #'funOr args)
      )
  )
)
(defun funAnd (arg1 &rest args)
  (if (not arg1)
      nil
      (if (null args)
	  t
	  (apply #'funAnd args)
      )
  )
)

;; (defun apply-mapcar (fun lst)
;;   (let ((trn (transpose lst))
;; 	(fun2 #'(lambda (l)
;; 		 (apply fun l)
;; 		)
;;        ))
;;     (mapcar fun2 trn)
;;   )
;; )

 (defun apply-mapcar (fun lst)
   (apply #'mapcar (cons fun lst)))

(defmacro hiya (exp)
  (let ((val exp))
    `(progn
       (format t "~%" "Hiya! ~%  ~s: ~s ~%"
	       ,exp ,val)
       ,val)))



;;(asdf:operate 'asdf:load-op :memoization)

;;(defun memoize-init ()

;;(setf MEMOIZATION::*Source-File-Extension* ".lisp"
;;)



;;;--------------------------------------------------------------------------
;;; Extension for binary files. ADD AN ENTRY IF THE APPROPRIATE ONE DOES NOT
;;; ALREADY APPEAR.

;;(setf MEMOIZATION::*Compiled-File-Extension*
;;        ".fasl"        
;;)
;;)


(defun remove-nth (i lst)
  (remove-if (lambda (x) t) lst :start i :count 1))

(defun makeScaledFun (fun instart inend &key (outstart 0) (outend 1))
  (let ((fac (/ (- outend outstart) (- inend instart))))
    (lambda (tm) (+ outstart (/ (funcall fun (* fac (- tm instart))) fac)))))

(defun partition (lst len &optional (inc len))
  (if (< (length lst) len)
      nil
      (cons (subseq lst 0 len) (partition (subseq lst inc) len inc))))

(defun makeOneArgument (fun l1 l2)
  (lambda (x) (apply fun (append l1 (list x) l2)))) 

(defun union1 (&rest args)
  (if (null args)
      nil
      (reduce #'union (append (list nil) args))))

(defun toBaseN (int n)
  (if (= int 0)
      nil
      (let ((bit (mod int n))
	    (tl (floor (/ int n))))
	(append (toBaseN tl n) (list bit)))))


(defun toBaseTen (lst n)
  (if (null lst)
      0
      (let ((hd (car lst))
	    (tl (cdr lst)))
	(+ (* hd (expt n (length tl))) (toBaseTen tl n)))))

(defun sortC (lst &key (test #'equal)) 
  (sort (copy-list lst) test))

(defun makeSubstitutionFun (old new)
  #'(lambda (x)
      (let ((pos (position x old)))
	(if pos
	    (nth pos new)
	    x))))

(defun multiSub (subs lst)
  (let* ((sublsts (transpose subs))
	  (olds (first sublsts))
	  (news (second sublsts))
	  (fun (makeSubstitutionFun olds news)))
    (mapcar fun lst)))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun cartesianProduct (l1 l2)
  (let ((f1 #'(lambda (el)
	      (mapcar #'(lambda (x) (list el x)) l2))))
    (mapcan f1 l1)))
