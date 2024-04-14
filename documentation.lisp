(in-package #:org.shirakumo.fraf.resvg)

(docs:define-docs
  (type resvg-error
    "Error signalled when something goes wrong in the underlying library.

See CODE")
  
  (function code
    "Returns the error code signalled by the underlying library.

See RESVG-ERROR")
  
  (function init
    "Initializes the library.

It is safe to call this multiple times, especially to set different
options.
Should be called before other functions are called.

The options are as follows:
  resources-dir         --- The directory to locate relative paths from
  dpi                   --- Sets the target DPI (default: 96)
  font-size             --- Sets the default font size (defalut: 12)
  fonts                 --- Sets the default fonts. Should be a plist
                            with the following keys, and the values
                            being the name of the font-family to use.
    :DEFAULT
    :SERIF
    :SANS-SERIF
    :CURSIVE
    :FANTASY
    :MONOSPACE
  languages             --- Sets the system languages. Should be a
                            comma-separated string
  shape-rendering-mode  --- Sets the rendering mode for shapes. Can be
                            one of:
    :OPTIMIZE-SPEED
    :CRISP-EDGES
    :GEOMETRIC-PRECISION
  text-rendering-mode   --- Sets the rendering mode for text: Can be
                            one of:
    :OPTIMIZE-SPEED
    :OPTIMIZE-LEGIBILITY
    :GEOMETRIC-PRECISION
  image-rendering-mode  --- Sets the rendering mode for images: Can be
                            one of:
    :OPTIMIZE-QUALITY
    :OPTIMIZE-SPEED

See LOAD-FONT
See SHUTDOWN")
  
  (function load-font
    "Load a font from a file or UB8 vector.

After loading, the font will be available to render in SVG images.
Calls INIT if it wasn't yet.

See INIT")
  
  (function shutdown
    "Uninitializes the library.

After calling this function you make not call any other functions
except for INIT.
It is safe to call this multiple times.

See INIT")
  
  (type image
    "Representation of an SVG image.

See MAKE-IMAGE
See FREE
See EMPTY-P
See VIEWBOX
See SIZE
See BBOX
See NODE-P
See TRANSFORM
See RENDER")
  
  (function make-image
    "Create a new SVG image from a data source.

SOURCE can either be a STRING or an UB8 vector for in-memory loading,
or a PATHNAME to load from a file.

Calls INIT if it wasn't yet.

Signals a RESVG-ERROR if the file couldn't be parsed.

See INIT
See IMAGE (type)")
  
  (function free
    "Frees the image.

The object may not be used anymore after this.
It is safe to call this multiple times.

See IMAGE (type)")
  
  (function empty-p
    "Returns true if the image has no content.

See IMAGE (type)")
  
  (function viewbox
    "Returns the view box as an (X Y W H) list.

See IMAGE (type)")
  
  (function size
    "Returns the size box as a (X . Y) cons.

If NODE is given, it should be the ID of a node in the SVG tree.
If no such node exists, an error of type RESVG-ERROR is signalled.

See IMAGE (type)")
  
  (function bbox
    "Returns the bounding box as an (X Y W H) list.

If NODE is given, it should be the ID of a node in the SVG tree.
If no such node exists, an error of type RESVG-ERROR is signalled.

See IMAGE (type)")
  
  (function node-p
    "Returns true if a node with the given ID exists.

See IMAGE (type)")
  
  (function transform
    "Returns the transform matrix of the given node.

If no such node exists, an error of type RESVG-ERROR is signalled.
The transform is a 3x2 single-float matrix. If no simple-array is
passed, one is created and returned for you.

See MAKE-TRANSFORM
See IMAGE (type)")
  
  (function render
    "Render the image to a pixel buffer.

If NODE is given, it should be the ID of a node in the SVG tree.
If no such node exists, an error of type RESVG-ERROR is signalled.
If WIDTH or HEIGHT are not given, they are set to the dimensions of
the image.
If OUTPUT is not given, an array of the appropriate size is created
and returned for you.
If the TRANSFORM is given, it should be a 3x2 single-float
simple-array vector, by which the image is transformed before output.

Returns three values:
  The output pixel array
  The width in pixels
  The height in pixels

See IMAGE (type)
See MAKE-TRANSFORM
See RENDER-SCALED
See SIZE")

  (function with-image
    "Convenience macro to lexically bind an image.

Calls INIT with the supplied options, then binds IMAGE to the result
of MAKE-IMAGE on SOURCE. Upon exit of the BODY, calls FREE on the
image.

See INIT
See IMAGE (type)
See MAKE-IMAGE
See FREE")

  (function make-transform
    "Create a transform matrix.

X-OFFSET and Y-OFFSET represent a translation.
X-SCALE and Y-SCALE represent a scaling.
ANGLE represents a rotation.

See RENDER")

  (function render-scaled
    "Render the image to a scaled output.

Either WIDTH or HEIGHT must be specified, setting the output image's
width or height. If one of the parameters is omitted, the other is
computed based on the image's aspect ratio and treating the specified
size as a uniform scaling factor.

For the other parameters, see RENDER

See RENDER"))

