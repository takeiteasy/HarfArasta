;;;; types.lisp
;;;; Data structures for signed distance field generation

(in-package #:harfarasta)

;;; Vector types

(defstruct (vec2 (:constructor make-vec2 (x y))
                 (:constructor vec2 (x y)))
  "2D vector with single-float components."
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(defstruct (vec3 (:constructor make-vec3 (x y z))
                 (:constructor vec3 (x y z)))
  "3D vector with single-float components (used for RGB)."
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

;;; Distance calculation

(defstruct (signed-distance (:constructor make-signed-distance (dist d))
                            (:constructor signed-distance (dist d)))
  "Signed distance with tie-breaker for equidistant points.
   DIST is the actual signed distance.
   D is a secondary value for comparing equidistant edges (0 means on curve)."
  (dist 0.0d0 :type double-float)
  (d 1.0d0 :type double-float))

;;; Edge colors for MSDF
;;; Colors are bit flags: R=1, G=2, B=4

(defconstant +edge-color-black+ 0)
(defconstant +edge-color-red+ 1)
(defconstant +edge-color-green+ 2)
(defconstant +edge-color-yellow+ 3)  ; R + G
(defconstant +edge-color-blue+ 4)
(defconstant +edge-color-magenta+ 5) ; R + B
(defconstant +edge-color-cyan+ 6)    ; G + B
(defconstant +edge-color-white+ 7)   ; R + G + B

;;; Edge types

(defconstant +edge-type-linear+ 0)
(defconstant +edge-type-quadratic+ 1)
(defconstant +edge-type-cubic+ 2)

;;; Edge segment for MSDF

(defstruct (edge-segment (:constructor %make-edge-segment))
  "A segment of an edge (linear, quadratic, or cubic Bezier).
   P0-P3 are control points (P2, P3 unused for linear; P3 unused for quadratic)."
  (edge-type +edge-type-linear+ :type fixnum)
  (color +edge-color-white+ :type fixnum)
  (p0 nil :type (or null vec2))
  (p1 nil :type (or null vec2))
  (p2 nil :type (or null vec2))
  (p3 nil :type (or null vec2)))

(defun make-edge-segment (edge-type p0 p1 &optional p2 p3)
  "Create an edge segment of the given type with control points."
  (%make-edge-segment :edge-type edge-type
                      :color +edge-color-white+
                      :p0 p0 :p1 p1 :p2 p2 :p3 p3))

;;; Contour (closed path of edges)

(defstruct (contour (:constructor %make-contour))
  "A closed contour made up of edge segments."
  (edges nil :type list)
  (winding 0 :type fixnum))

(defun make-contour (&rest edges)
  "Create a contour from a list of edges."
  (%make-contour :edges (copy-list edges)))

;;; Shape (collection of contours)

(defstruct (shape (:constructor %make-shape))
  "A shape made up of contours."
  (contours nil :type list))

(defun make-shape (&rest contours)
  "Create a shape from a list of contours."
  (%make-shape :contours (copy-list contours)))

;;; Bitmap output

(defstruct (bitmap (:constructor %make-bitmap))
  "A bitmap with float data. Channels can be 1 (grayscale) or 3 (RGB).
   Data is stored in row-major order, bottom-to-top, with channels interleaved."
  (data nil :type (or null (simple-array single-float (*))))
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (channels 1 :type fixnum))

(defun make-bitmap (width height &optional (channels 1))
  "Create a new bitmap with the given dimensions and channel count."
  (%make-bitmap :data (make-array (* width height channels)
                                  :element-type 'single-float
                                  :initial-element 0.0)
                :width width
                :height height
                :channels channels))

;;; Grayscale image (input for raster SDF)

(defstruct (grayscale-image (:constructor %make-grayscale-image))
  "A grayscale image with byte data for input to SDF generation.
   Values 0=background, 255=foreground, intermediate=antialiased edge."
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defun make-grayscale-image (width height &optional initial-data)
  "Create a grayscale image. If INITIAL-DATA is provided, it should be
   a (simple-array (unsigned-byte 8) (*)) of length width*height."
  (if initial-data
      (%make-grayscale-image :data initial-data :width width :height height)
      (%make-grayscale-image :data (make-array (* width height)
                                               :element-type '(unsigned-byte 8)
                                               :initial-element 0)
                             :width width
                             :height height)))

;;; Edge point (for tracking nearest edge during MSDF generation)

(defstruct (edge-point (:constructor make-edge-point ()))
  "Tracks the nearest edge and distance for a color channel."
  (min-distance nil :type (or null signed-distance))
  (near-edge nil :type (or null edge-segment))
  (near-param 0.0d0 :type double-float))

;;; Multi-distance (for storing per-contour distances)

(defstruct (multi-distance (:constructor make-multi-distance ())
                           (:constructor multi-distance (r g b med)))
  "Distance values for R, G, B channels plus median."
  (r 0.0d0 :type double-float)
  (g 0.0d0 :type double-float)
  (b 0.0d0 :type double-float)
  (med 0.0d0 :type double-float))

;;; Path builder state

(defstruct (path-builder (:constructor make-path-builder ()))
  "State for building shapes using SVG-like path commands."
  (contours nil :type list)        ; completed contours
  (current-edges nil :type list)   ; edges in current contour
  (start-point nil :type (or null vec2))  ; first point of current contour
  (current-point nil :type (or null vec2))) ; current point
