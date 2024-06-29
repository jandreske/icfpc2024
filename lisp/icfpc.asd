(defsystem "icfpc"
  :version "0.1"
  :depends-on (:alexandria :nodgui :quri :dexador)
  ;; also consider
  ;; :lparallel
  ;; :com.inuoe.jzon
  :pathname "src/"
  :components ((:file "binary-heap")
	       (:file "lambdaman" :depends-on ("binary-heap"))
	       (:file "gui")
	       (:file "icfplang")
	       (:file "icfpc" :depends-on ("icfplang" "lambdaman"))))

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
		:components ((:file "test-binary-heap")))))
