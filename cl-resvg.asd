(asdf:defsystem cl-resvg
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An up-to-date bindings library for the resvg SVG rendering library"
  :homepage "https://shirakumo.github.io/cl-resvg/"
  :bug-tracker "https://github.com/shirakumo/cl-resvg/issues"
  :source-control (:git "https://github.com/shirakumo/cl-resvg.git")
  :serial T
  :components ((:file "package")
               (:file "low-level")
               (:file "wrapper")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :float-features
               :cffi))
