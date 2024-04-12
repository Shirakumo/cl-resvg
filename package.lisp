(defpackage #:org.shirakumo.fraf.resvg.cffi
  (:use #:cl)
  (:shadow
   #:error)
  (:export))

(defpackage #:org.shirakumo.fraf.resvg
  (:use #:cl)
  (:local-nicknames
   (#:resvg #:org.shirakumo.fraf.resvg.cffi))
  (:export))
