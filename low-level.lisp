(in-package #:org.shirakumo.fraf.resvg.cffi)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* *default-pathname-defaults*))
(defvar *static* (make-pathname :name NIL :type NIL :defaults (merge-pathnames "static/" *here*)))
(pushnew *static* cffi:*foreign-library-directories*)

(cffi:define-foreign-library libresvg
  (:darwin (:or #+X86 "libresvg-mac-i686.dylib"
                #+X86-64 "libresvg-mac-amd64.dylib"
                #+ARM64 "libresvg-mac-arm64.dylib"))
  (:unix (:or #+X86 "libresvg-lin-i686.so"
              #+X86-64 "libresvg-lin-amd64.so"
              #+ARM64 "libresvg-lin-arm64.dylib"))
  (:windows (:or #+X86 "libresvg-win-i686.dll"
                 #+X86-64 "libresvg-win-amd64.dll"
                 #+ARM64 "libresvg-win-arm64.dylib"))
  (T (:or (:default "libresvg") (:default "resvg"))))

(cffi:defcenum error
  :ok
  :not-an-utf8-string
  :file-open-failed
  :malformed-gzip
  :elements-limit-reached
  :invalid-size
  :parsing-failed)

(cffi:defcenum image-rendering
  :optimize-quality
  :optimize-speed)

(cffi:defcenum shape-rendering
  :optimize-speed
  :crisp-edges
  :geometric-precision)

(cffi:defcenum text-rendering
  :optimize-speed
  :optimize-legibility
  :geometric-precision)

(cffi:defcstruct transform
  (elements :float :count 6))

(cffi:defcstruct (size :conc-name size-)
  (width :float)
  (height :float))

(cffi:defcstruct (rect :conc-name rect-)
  (x :float)
  (y :float)
  (width :float)
  (height :float))

(cffi:defcfun (init-log "resvg_init_log") :void)

(cffi:defcfun (create-options "resvg_options_create") :pointer)

(cffi:defcfun (set-resources-dir "resvg_options_set_resources_dir") :void
  (options :pointer)
  (path :string))

(cffi:defcfun (set-dpi "resvg_options_set_dpi") :void
  (options :pointer)
  (dpi :float))

(cffi:defcfun (set-font-family "resvg_options_set_font_family") :void
  (options :pointer)
  (family :string))

(cffi:defcfun (set-font-size "resvg_options_set_font_size") :void
  (options :pointer)
  (size :float))

(cffi:defcfun (set-serif-family "resvg_options_set_serif_family") :void
  (options :pointer)
  (family :string))

(cffi:defcfun (set-sans-serif-family "resvg_options_set_sans_serif_family") :void
  (options :pointer)
  (family :string))

(cffi:defcfun (set-cursive-family "resvg_options_set_cursive_family") :void
  (options :pointer)
  (family :string))

(cffi:defcfun (set-fantasy-family "resvg_options_set_fantasy_family") :void
  (options :pointer)
  (family :string))

(cffi:defcfun (set-monospace-family "resvg_options_set_monospace_family") :void
  (options :pointer)
  (family :string))

(cffi:defcfun (set-languages "resvg_options_set_languages") :void
  (options :pointer)
  (languages :string))

(cffi:defcfun (set-shape-rendering-mode "resvg_options_set_shape_rendering_mode") :void
  (options :pointer)
  (mode shape-rendering))

(cffi:defcfun (set-text-rendering-mode "resvg_options_set_text_rendering_mode") :void
  (options :pointer)
  (mode text-rendering))

(cffi:defcfun (set-image-rendering-mode "resvg_options_set_image_rendering_mode") :void
  (options :pointer)
  (mode image-rendering))

(cffi:defcfun (load-font-data "resvg_options_load_font_data") :void
  (options :pointer)
  (data :pointer)
  (length :uintptr))

(cffi:defcfun (load-font-file "resvg_options_load_font_file") error
  (options :pointer)
  (file :string))

(cffi:defcfun (load-system-fonts "resvg_options_load_system_fonts") :void
  (options :pointer))

(cffi:defcfun (destroy-options "resvg_options_destroy") :void
  (options :pointer))

(cffi:defcfun (parse-from-file "resvg_parse_tree_from_file") error
  (path :pointer)
  (options :pointer)
  (tree :pointer))

(cffi:defcfun (parse-from-data "resvg_parse_tree_from_data") error
  (data :pointer)
  (length :uintptr)
  (options :pointer)
  (tree :pointer))

(cffi:defcfun (image-empty-p "resvg_is_image_empty") :bool
  (tree :pointer))

(cffi:defcfun (image-size "resvg_get_image_size") :void
  (tree :pointer)
  (size :pointer))

(cffi:defcfun (image-viewbox "resvg_get_image_viewbox") :void
  (tree :pointer)
  (rect :pointer))

(cffi:defcfun (image-bbox "resvg_get_image_bbox") :bool
  (tree :pointer)
  (bbox :pointer))

(cffi:defcfun (node-exists-p "resvg_node_exists") :bool
  (tree :pointer)
  (id :string))

(cffi:defcfun (node-transform "resvg_get_node_transform") :bool
  (tree :pointer)
  (id :string)
  (transform :pointer))

(cffi:defcfun (node-bbox "resvg_get_node_bbox") :bool
  (tree :pointer)
  (id :string)
  (bbox :pointer))

(cffi:defcfun (node-stroke-bbox "resvg_get_node_stroke_bbox") :bool
  (tree :pointer)
  (id :string)
  (bbox :pointer))

(cffi:defcfun (destroy-tree "resvg_tree_destroy") :void
  (tree :pointer))

(cffi:defcfun (render "resvg_render") :void
  (tree :pointer)
  (transform :pointer)
  (width :uint32)
  (height :uint32)
  (pixmap :pointer))

(cffi:defcfun (render-node "resvg_render_node") :void
  (tree :pointer)
  (id :pointer)
  (transform :pointer)
  (width :uint32)
  (height :uint32)
  (pixmap :pointer))
