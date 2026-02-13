;;;; inflate.lisp — Portable Common Lisp DEFLATE decompressor
;;;;
;;;; Based on the DEFLATE algorithm (RFC 1951).
;;;; Provides inflate-octets: decompress a DEFLATE byte vector to an octet vector.

(in-package #:harfarasta)

;;; ——— Bit reader over an octet vector ———

(defstruct (bit-reader (:constructor %make-bit-reader))
  (data (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum)
  (bit-buf 0 :type (unsigned-byte 32))
  (bit-count 0 :type (integer 0 32)))

(defun make-bit-reader (data)
  (%make-bit-reader :data data :pos 0 :bit-buf 0 :bit-count 0))

(declaim (inline read-bits))
(defun read-bits (br count)
  "Read COUNT bits (0–16) from the bit reader, LSB first."
  (declare (optimize (speed 3) (safety 1))
           (type bit-reader br)
           (type (integer 0 16) count))
  (when (zerop count) (return-from read-bits 0))
  (loop while (< (bit-reader-bit-count br) count)
        do (when (>= (bit-reader-pos br)
                     (length (bit-reader-data br)))
             (error "Unexpected end of DEFLATE data"))
           (setf (bit-reader-bit-buf br)
                 (logior (bit-reader-bit-buf br)
                         (ash (aref (bit-reader-data br)
                                    (bit-reader-pos br))
                              (bit-reader-bit-count br))))
           (incf (bit-reader-pos br))
           (incf (bit-reader-bit-count br) 8))
  (let ((val (logand (bit-reader-bit-buf br) (1- (ash 1 count)))))
    (setf (bit-reader-bit-buf br)
          (ash (bit-reader-bit-buf br) (- count)))
    (decf (bit-reader-bit-count br) count)
    val))

(defun align-to-byte (br)
  "Discard remaining bits to align to byte boundary."
  (setf (bit-reader-bit-buf br) 0
        (bit-reader-bit-count br) 0))

;;; ——— Output buffer (circular, growable) ———

(defstruct (output-buffer (:constructor %make-output-buffer))
  (data (make-array 65536 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (fill 0 :type fixnum))

(defun make-output-buffer ()
  (%make-output-buffer))

(defun output-push (ob byte)
  (declare (optimize (speed 3) (safety 1))
           (type output-buffer ob)
           (type (unsigned-byte 8) byte))
  (let ((data (output-buffer-data ob))
        (fill (output-buffer-fill ob)))
    (when (>= fill (length data))
      (let ((new (make-array (* 2 (length data))
                             :element-type '(unsigned-byte 8))))
        (replace new data)
        (setf data new
              (output-buffer-data ob) new)))
    (setf (aref data fill) byte)
    (setf (output-buffer-fill ob) (1+ fill))))

(defun output-copy-back (ob distance length)
  "Copy LENGTH bytes from DISTANCE bytes back in the output."
  (declare (optimize (speed 3) (safety 1))
           (type output-buffer ob)
           (type fixnum distance length))
  (let ((fill (output-buffer-fill ob)))
    (when (> distance fill)
      (error "DEFLATE: back-reference distance ~D > output size ~D" distance fill))
    (let ((src (- fill distance)))
      (dotimes (i length)
        (output-push ob (aref (output-buffer-data ob) (+ src i)))))))

(defun output-to-vector (ob)
  (subseq (output-buffer-data ob) 0 (output-buffer-fill ob)))

;;; ——— Length/distance tables ———

(defparameter *length-base*
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31
    35 43 51 59 67 83 99 115 131 163 195 227 258))

(defparameter *length-extra*
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))

(defparameter *dist-base*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193
    257 385 513 769 1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))

(defparameter *dist-extra*
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

;;; ——— Huffman tree builder ———

;;; Tree is either a leaf (integer) or a cons (left . right).
;;; left = bit 0, right = bit 1.

(defun build-huffman-tree (code-lengths max-sym)
  "Build a Huffman tree from an array of code lengths (0 = unused).
MAX-SYM is the number of symbols."
  (let* ((max-bits (reduce #'max code-lengths :end max-sym :initial-value 0))
         (bl-count (make-array (1+ max-bits) :initial-element 0))
         (next-code (make-array (1+ max-bits) :initial-element 0)))
    ;; Count code lengths
    (loop for i below max-sym
          for len = (aref code-lengths i)
          when (> len 0) do (incf (aref bl-count len)))
    ;; Compute starting codes
    (let ((code 0))
      (loop for bits from 1 to max-bits
            do (setf code (ash (+ code (aref bl-count (1- bits))) 1))
               (setf (aref next-code bits) code)))
    ;; Build tree
    (let ((root (cons nil nil)))
      (loop for sym below max-sym
            for len = (aref code-lengths sym)
            when (> len 0)
              do (let* ((code-val (aref next-code len))
                        (node root))
                   (incf (aref next-code len))
                   ;; Walk from MSB to LSB-1, creating nodes
                   (loop for bit-pos from (1- len) downto 1
                         for bit = (ldb (byte 1 bit-pos) code-val)
                         do (if (zerop bit)
                                (progn
                                  (unless (car node) (setf (car node) (cons nil nil)))
                                  (setf node (car node)))
                                (progn
                                  (unless (cdr node) (setf (cdr node) (cons nil nil)))
                                  (setf node (cdr node)))))
                   ;; Place symbol at leaf
                   (let ((last-bit (ldb (byte 1 0) code-val)))
                     (if (zerop last-bit)
                         (setf (car node) sym)
                         (setf (cdr node) sym)))))
      root)))

(defun decode-symbol (br tree)
  "Decode one Huffman symbol from the bit reader."
  (declare (optimize (speed 3) (safety 1)))
  (let ((node tree))
    (loop
      (when (not (consp node))
        (return node))
      (if (zerop (read-bits br 1))
          (setf node (car node))
          (setf node (cdr node))))))

;;; ——— Fixed Huffman trees ———

(defvar *fixed-lit-tree* nil)
(defvar *fixed-dist-tree* nil)

(defun ensure-fixed-trees ()
  (unless *fixed-lit-tree*
    (let ((lengths (make-array 288 :initial-element 0)))
      (loop for i from 0 to 143 do (setf (aref lengths i) 8))
      (loop for i from 144 to 255 do (setf (aref lengths i) 9))
      (loop for i from 256 to 279 do (setf (aref lengths i) 7))
      (loop for i from 280 to 287 do (setf (aref lengths i) 8))
      (setf *fixed-lit-tree* (build-huffman-tree lengths 288)))
    (let ((lengths (make-array 32 :initial-element 5)))
      (setf *fixed-dist-tree* (build-huffman-tree lengths 32)))))

;;; ——— Block decoders ———

(defun decode-uncompressed-block (br ob)
  (align-to-byte br)
  (let* ((pos (bit-reader-pos br))
         (data (bit-reader-data br))
         (len (+ (aref data pos)
                 (ash (aref data (+ pos 1)) 8)))
         (nlen (+ (aref data (+ pos 2))
                  (ash (aref data (+ pos 3)) 8))))
    (unless (= len (logxor nlen #xFFFF))
      (error "DEFLATE: invalid stored block lengths"))
    (incf (bit-reader-pos br) 4)
    (loop for i from (bit-reader-pos br)
          below (+ (bit-reader-pos br) len)
          do (output-push ob (aref data i)))
    (incf (bit-reader-pos br) len)))

(defun decode-huffman-block (br ob lit-tree dist-tree)
  (loop
    (let ((sym (decode-symbol br lit-tree)))
      (cond
        ((< sym 256)
         (output-push ob sym))
        ((= sym 256)
         (return))
        (t
         (let* ((len-idx (- sym 257))
                (length (+ (svref *length-base* len-idx)
                           (read-bits br (svref *length-extra* len-idx))))
                (dist-sym (decode-symbol br dist-tree))
                (distance (+ (svref *dist-base* dist-sym)
                             (read-bits br (svref *dist-extra* dist-sym)))))
           (output-copy-back ob distance length)))))))

(defparameter *code-length-order*
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(defun decode-dynamic-huffman-block (br ob)
  (let* ((hlit (+ (read-bits br 5) 257))
         (hdist (+ (read-bits br 5) 1))
         (hclen (+ (read-bits br 4) 4))
         (cl-lengths (make-array 19 :initial-element 0)))
    ;; Read code length code lengths
    (loop for i below hclen
          do (setf (aref cl-lengths (svref *code-length-order* i))
                   (read-bits br 3)))
    (let ((cl-tree (build-huffman-tree cl-lengths 19))
          (lengths (make-array (+ hlit hdist) :initial-element 0))
          (i 0))
      ;; Read literal/length + distance code lengths
      (loop while (< i (+ hlit hdist))
            do (let ((sym (decode-symbol br cl-tree)))
                 (cond
                   ((<= sym 15)
                    (setf (aref lengths i) sym)
                    (incf i))
                   ((= sym 16)
                    (let ((prev (aref lengths (1- i)))
                          (rep (+ (read-bits br 2) 3)))
                      (loop repeat rep
                            do (setf (aref lengths i) prev)
                               (incf i))))
                   ((= sym 17)
                    (let ((rep (+ (read-bits br 3) 3)))
                      (loop repeat rep
                            do (setf (aref lengths i) 0)
                               (incf i))))
                   ((= sym 18)
                    (let ((rep (+ (read-bits br 7) 11)))
                      (loop repeat rep
                            do (setf (aref lengths i) 0)
                               (incf i)))))))
      ;; Build trees from decoded lengths
      (let ((lit-lengths (make-array hlit :initial-element 0))
            (dist-lengths (make-array hdist :initial-element 0)))
        (replace lit-lengths lengths :start2 0 :end2 hlit)
        (replace dist-lengths lengths :start2 hlit :end2 (+ hlit hdist))
        (decode-huffman-block br ob
                              (build-huffman-tree lit-lengths hlit)
                              (build-huffman-tree dist-lengths hdist))))))

;;; ——— Main entry point ———

(defun inflate-octets (compressed-data)
  "Decompress DEFLATE-compressed data (raw, no headers).
COMPRESSED-DATA is a (vector (unsigned-byte 8)).
Returns a (simple-array (unsigned-byte 8) (*))."
  (ensure-fixed-trees)
  (let ((br (make-bit-reader (coerce compressed-data
                                     '(simple-array (unsigned-byte 8) (*)))))
        (ob (make-output-buffer)))
    (loop
      (let ((bfinal (read-bits br 1))
            (btype (read-bits br 2)))
        (case btype
          (0 (decode-uncompressed-block br ob))
          (1 (decode-huffman-block br ob *fixed-lit-tree* *fixed-dist-tree*))
          (2 (decode-dynamic-huffman-block br ob))
          (otherwise (error "DEFLATE: invalid block type ~D" btype)))
        (when (= bfinal 1)
          (return (output-to-vector ob)))))))
