
(asdf:defsystem Joshua
  :name "joshua"
  :description "Joshua Reasoning System"
  :long-description "..."
  :author "howard shrobe"
  :defsystem-depends-on ()
  :pathname "code"
  :components
  ((:module "core"
	    :serial t
	    :pathname #p"./code"
	    :components ((:file "packaged")
			 (:file "borrowin")
			 (:file "mapfvars")
			 (:file "mapforms")
			 (:file "readnnwr")
			 (:file "preddefs")
			 (:file "predicat")
			 (:file "unificat")
			 (:file "predprot")
			 (:file "clos-heaps")
			 (:file "predimpl" )
			 (:file "rete" )
			 (:file "matcher")
			 (:file "discrimi")
			 (:file "supplied")
			 (:file "rules")
			 (:file "ltms")
			 (:file "objectmo")
			 (:file "cfs")
			 (:file "asdf-support"))
	    )))
