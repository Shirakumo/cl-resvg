(in-package #:org.shirakumo.fraf.resvg)

(define-condition resvg-error (error)
  ((code :initarg :code :reader code))
  (:report (lambda (c s) (format s "An error occurred: ~a" (code c)))))

(defun check-error (result)
  (unless (eq :ok result)
    (error 'resvg-error :code result)))

(defvar *options* NIL)

(defun init (&key resources-dir dpi font-size fonts languages shape-rendering-mode text-rendering-mode image-rendering-mode)
  (let ((was-init (not (null *options*))))
    (unless (cffi:foreign-library-loaded-p 'resvg:libresvg)
      (cffi:use-foreign-library resvg:libresvg))
    (unless *options*
      (setf *options* (resvg:create-options)))
    (macrolet ((%set (field)
                 `(when ,field
                    (,(intern (format NIL "~a-~a" (symbol-name '#:set) (symbol-name field))
                              '#:org.shirakumo.fraf.resvg.cffi)
                     *options* ,field))))
      (%set resources-dir)
      (%set dpi)
      (%set font-size)
      (%set languages)
      (%set shape-rendering-mode)
      (%set text-rendering-mode)
      (%set image-rendering-mode)
      (loop for (k v) on fonts by #'cddr
            do (ecase k
                 (:default (resvg:set-font-family *options* v))
                 (:serif (resvg:set-serif-family *options* v))
                 (:sans-serif (resvg:set-sans-serif-family *options* v))
                 (:cursive (resvg:set-cursive-family *options* v))
                 (:fantasy (resvg:set-fantasy-family *options* v))
                 (:monospace (resvg:set-monospace-family *options* v))))
      (unless was-init
        (resvg:load-system-fonts *options*)))))

(defun load-font (source)
  (init)
  (etypecase source
    ((simple-array (unsigned-byte 8) (*))
     (cffi:with-pointer-to-vector-data (ptr source)
       (resvg:load-font-data *options* ptr (length source))))
    (pathname
     (check-error (resvg:load-font-file *options* (pathname-utils:native-namestring source))))))

(defun shutdown ()
  (when *options*
    (resvg:destroy-options *options*)
    (setf *options* NIL)))

(defstruct (image
            (:constructor %make-image (ptr))
            (:conc-name NIL))
  (ptr (cffi:null-pointer) :type cffi:foreign-pointer))

(defmethod print-object ((image image) stream)
  (print-unreadable-object (image stream :type T :identity T)
    (if (cffi:null-pointer-p (ptr image))
        (format stream "DESTROYED")
        (destructuring-bind (w . h) (size image)
          (format stream "~dx~d" w h)))))

(defun make-image (source)
  (init)
  (cffi:with-foreign-objects ((tree :pointer))
    (check-error (etypecase source
                   ((simple-array (unsigned-byte 8) (*))
                    (cffi:with-pointer-to-vector-data (ptr source)
                      (resvg:parse-from-data ptr (length source) *options* tree)))
                   (string
                    (cffi:with-foreign-string (ptr source)
                      (resvg:parse-from-data ptr (length source) *options* tree)))
                   (pathname
                    (resvg:parse-from-file (pathname-utils:native-namestring source) *options* tree))))
    (%make-image (cffi:mem-ref tree :pointer))))

(defmethod free ((image image))
  (unless (cffi:null-pointer-p (ptr image))
    (resvg:destroy-tree (ptr image))
    (setf (ptr image) (cffi:null-pointer))))

(defun empty-p (image)
  (resvg:image-empty-p (ptr image)))

(defun viewbox (image)
  (cffi:with-foreign-objects ((bbox '(:struct resvg:rect)))
    (resvg:image-viewbox (ptr image) bbox)
    (list (resvg:rect-x bbox) (resvg:rect-y bbox)
          (resvg:rect-width bbox) (resvg:rect-height bbox))))

(defun size (image &optional node)
  (cffi:with-foreign-objects ((bbox '(:struct resvg:rect)))
    (if node
        (unless (resvg:node-bbox (ptr image) node bbox)
          (error 'resvg-error :code :no-such-node))
        (resvg:image-bbox (ptr image) bbox))
    (cons (resvg:rect-width bbox) (resvg:rect-height bbox))))

(defun bbox (image &optional node)
  (cffi:with-foreign-objects ((bbox '(:struct resvg:rect)))
    (if node
        (unless (resvg:node-bbox (ptr image) node bbox)
          (error 'resvg-error :code :no-such-node))
        (resvg:image-bbox (ptr image) bbox))
    (list (resvg:rect-x bbox) (resvg:rect-y bbox)
          (resvg:rect-width bbox) (resvg:rect-height bbox))))

(defun node-p (image node)
  (resvg:node-exists-p (ptr image) node))

(defun transform (image node &optional transform)
  (unless transform
    (setf transform (make-array 6 :element-type 'single-float)))
  (cffi:with-pointer-to-vector-data (ptr transform)
    (unless (resvg:node-transform (ptr image) node ptr)
      (error 'resvg-error :code :no-such-node)))
  transform)

(defun make-transform (&key (x-offset 0) (y-offset 0) (scale 1) (x-scale scale) (y-scale scale) (angle 0))
  (let ((arr (make-array 6 :element-type 'single-float :initial-element 0f0))
        (cos (cos (float angle 0f0)))
        (sin (sin (float angle 0f0))))
    ;; Matrix is column-major...
    (setf (aref arr 0) (* cos (float x-scale 0f0)))
    (setf (aref arr 1) (+ sin))
    (setf (aref arr 2) (- sin))
    (setf (aref arr 3) (* cos (float y-scale 0f0)))
    (setf (aref arr 4) (float x-offset 0f0))
    (setf (aref arr 5) (float y-offset 0f0))
    arr))

(defun render (image &key node output width height transform)
  ;; Default width and height
  (unless (and width height)
    (destructuring-bind (w . h) (size image node)
      (unless width (setf width (round w)))
      (unless height (setf height (round h)))))
  ;; Default the output
  (unless output
    (setf output (make-array (* 4 width height) :element-type '(unsigned-byte 8))))
  ;; Default the transform
  (unless transform
    (setf transform (load-time-value (make-transform))))
  (cffi:with-pointer-to-vector-data (tptr transform)
    (cffi:with-pointer-to-vector-data (ptr output)
      (float-features:with-float-traps-masked T
        (if node
            (unless (resvg:render-node (ptr image) node tptr width height ptr)
              (error 'resvg-error :code :no-such-node))
            (resvg:render (ptr image) tptr width height ptr)))))
  (values output width height))

(defmacro with-image ((var source &rest init-options) &body body)
  `(progn 
     (init ,@init-options)
     (let ((,var (make-image ,source)))
       (unwind-protect (let ((,var ,var))
                         ,@body)
         (free ,var)))))

(defun render-scaled (image width height &key node output)
  (destructuring-bind (image-width . image-height) (size image node)
    (cond ((not (or width height))
           (error "WIDTH or HEIGHT must be specified."))
          (width
           (setf height (round (* image-height (/ image-width width)))))
          (height
           (setf width (round (* image-width (/ image-height height))))))
    (render image :width width :height height :output output :node node
                  :transform (make-transform :x-scale (/ width image-width) :y-scale (/ height image-height)))))

#++
(defun test (in &optional (out (make-pathname :type "png" :defaults in)))
  (with-image (svg in)
    (multiple-value-bind (data w h) (render svg :transform (make-transform :scale 0.25))
      (print (list w h (viewbox svg) (bbox svg)))
      (zpng:write-png (make-instance 'zpng:png :color-type :truecolor-alpha :width w :height h :image-data data)
                      out))))
