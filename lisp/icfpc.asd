(defsystem "icfpc"
  :version "0.1"
  :depends-on (:alexandria :nodgui :quri :dexador)
  ;; also consider
  ;; :lparallel
  ;; :com.inuoe.jzon
  :pathname "src/"
  :components ((:file "binary-heap")
	       (:file "lambdaman" :depends-on ("binary-heap"))
	       (:file "spaceship")
	       (:file "gui")
	       (:file "util")
	       (:file "icfplang")
	       (:file "icfpc" :depends-on ("util" "icfplang" "lambdaman" "spaceship"))))

(defsystem "icfpc/executable"
  :build-operation program-op
  :build-pathname "icfpc"
  :entry-point "main::start"
  :depends-on ("icfpc")
  :components ((:file "main")))

(defsystem "icfpc/tests"
  :depends-on (:icfpc :fiveam)
  :serial t
  :components ((:module "tests"
		:serial t
		:components ((:file "test-icfp")
			     (:file "test-spaceship")
			     (:file "test-binary-heap")))))
