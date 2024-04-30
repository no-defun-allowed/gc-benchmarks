(asdf:defsystem :gc-benchmarks
  :depends-on (:alexandria :jsown :drakma)
  :serial t
  :components ((:file "package")
               (:file "harness-server")
               (:file "install")))
