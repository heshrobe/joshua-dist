;;; These are used to locate various logical hosts that make 
;;; up my software environment 


;;; joshua
"joshua"     `("home;*.*" "~/joshua-dist/joshua/*.*") 
             '("**;*.*" "~/joshua-dist/joshua/**/*.*")

;;; clim-fixes
"clim-fixes"  `("source;*.*"	"~/joshua-dist/clim-fixes/*.*")
              `("**;*.*"	"~/joshua-dist/clim-fixes/**/*.*")

;;; aisl-clos
"aisl-clos" `("source;*.*"  "~/joshua-dist/clim-env/aisl-clos/*.*")
            '("**;*.*" "~/joshua-dist/clim-env/aisl-clos/**/*.*")

;;; portable-lisp-environment
"portable-lisp-environment" `("source;*.*" "~/joshua-dist/clim-env/portable-lisp-environment/*.*")
                            '("**;*.*"     "~/joshua-dist/clim-env/portable-lisp-environment/**/*.*")

;;; Clim itself
"clim" '("**;*.*" "/Applications/AllegroCL64/Contents/Resources/src/clim/**/*.*")

;;; Research Projects
"RP" '("**;*.*"	"~/research-projects/**/*.*")  
