;; (defpackage :strategygames-system
;;   (:use :common-lisp :asdf
;;  	)
;; )

;; (in-package :strategygames-system)

(asdf:defsystem :strategyGames
  :name "Strategy Games framework"
  :components ((:file "package")
	       (:file "gamesengine03"
		      :depends-on ("package"))
	       (:file "gameframe01"
;;	       (:file "gameframeGTK"
		      ;;		      :depends-on ("package" "gamesengine03")
		      ) )
  :depends-on (;;common-lisp
	       ;;sb-ext 
	       mydefs 
	       hexutils 
	       ;;memoization
	       ltk
	       ;;cl-cffi-gtk
	       )
)
