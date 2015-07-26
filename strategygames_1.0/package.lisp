
(defpackage :gamesengine
  (:use :common-lisp 
	#+(or :cmu :scl) :ext
	#+:sbcl :sb-ext
	:mydefs 
	;;:memoization 
	:ltk
	)
  (:export movegen
	   opposite
	   static
	   won?
	   drawn?
	   deep-enough
	   poscur
	   history
	   status
	   histbutt
	   make-str
	   move-value
	   path
	   next-move
	   minimax-a-b
	   compTurn
	   pause
	   setpause
	   updateposcur
	   postPosition
	   initGame
	   setup
	   buttonFn
	   newFromPos
	   undoFn
	   repetitionQ
	   postmortemcheck
	   cUToFF
	   truncEval
	   postMortem
	   reEval
	   depthTable
	   getDepth
	   initEngine
	   posInit
	   posCurToDisplay
	   postMessageAux
	   comp
	   movesfrompos
	   desireddepth
	   numchoices
	   positionFromMove
	   evalPosUncert
	   winQ
	   lossQ
	   drawQ
	   poscurToDisplay
	   gamename
	   minimaxABcache
	   depthTable
	   clearAllCache
	   )
)

(defpackage :gameframe
  (:use :common-lisp 
	#+(or :cmu :scl) :ext
	#+:sbcl :sb-ext
	:mydefs :ltk :gamesengine
	)
  (:export makegameframe
;;	   buttontab
;;	   mainwindow
;;	   makepanel
	   textwin
;;	   controls
;;	   controlstab
	   launchframe
;;	   initBdTab
	   messagewidget
;;	   postMessage
	   initBdTabAux
	   initBdTab
	   buttonTabAux
;;	   initBdTab
	   setButtonProps
	  ))
