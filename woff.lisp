;;;; woff.lisp — Pure-Lisp WOFF1 → TTF/OTF decoder
;;;;
;;;; Parses WOFF1 container, decompresses tables via DEFLATE, and
;;;; reconstructs a valid sfnt (TTF/OTF) byte vector.

(in-package #:harfarasta)

;;; ——— Binary reading helpers ———

(declaim (inline read-u32be read-u16be))

(defun read-u32be (vec offset)
  "Read a big-endian uint32 from VEC at OFFSET."
  (declare (type (simple-array (unsigned-byte 8) (*)) vec)
           (type fixnum offset))
  (logior (ash (aref vec offset) 24)
          (ash (aref vec (+ offset 1)) 16)
          (ash (aref vec (+ offset 2)) 8)
          (aref vec (+ offset 3))))

(defun read-u16be (vec offset)
  "Read a big-endian uint16 from VEC at OFFSET."
  (declare (type (simple-array (unsigned-byte 8) (*)) vec)
           (type fixnum offset))
  (logior (ash (aref vec offset) 8)
          (aref vec (+ offset 1))))

;;; ——— Binary writing helpers ———

(declaim (inline write-u32be write-u16be))

(defun write-u32be (vec offset val)
  (declare (type (simple-array (unsigned-byte 8) (*)) vec)
           (type fixnum offset)
           (type (unsigned-byte 32) val))
  (setf (aref vec offset)       (ldb (byte 8 24) val)
        (aref vec (+ offset 1)) (ldb (byte 8 16) val)
        (aref vec (+ offset 2)) (ldb (byte 8  8) val)
        (aref vec (+ offset 3)) (ldb (byte 8  0) val)))

(defun write-u16be (vec offset val)
  (declare (type (simple-array (unsigned-byte 8) (*)) vec)
           (type fixnum offset)
           (type (unsigned-byte 16) val))
  (setf (aref vec offset)       (ldb (byte 8 8) val)
        (aref vec (+ offset 1)) (ldb (byte 8 0) val)))

;;; ——— WOFF1 table entry ———

(defstruct woff-table
  (tag 0 :type (unsigned-byte 32))
  (offset 0 :type fixnum)
  (comp-length 0 :type fixnum)
  (orig-length 0 :type fixnum)
  (orig-checksum 0 :type (unsigned-byte 32)))

;;; ——— sfnt helpers ———

(defun calc-search-range (num-tables)
  "Compute searchRange, entrySelector, rangeShift for sfnt table directory."
  (let* ((entry-selector (floor (log num-tables 2)))
         (search-range (* (expt 2 entry-selector) 16))
         (range-shift (- (* num-tables 16) search-range)))
    (values search-range entry-selector range-shift)))

(defun pad4 (n)
  "Round N up to next multiple of 4."
  (logand (+ n 3) (lognot 3)))

;;; ——— Main decoder ———

(defun woff1-decode-to-vector (woff-bytes)
  "Decode a WOFF1 byte vector to a TTF/OTF byte vector.
WOFF-BYTES is a (vector (unsigned-byte 8)). Returns a new (vector (unsigned-byte 8))."
  (let ((data (coerce woff-bytes '(simple-array (unsigned-byte 8) (*)))))
    ;; Verify signature
    (let ((sig (read-u32be data 0)))
      (unless (= sig #x774F4646)
        (error "Not a WOFF1 file (signature: #x~8,'0X)" sig)))
    (let* ((flavor (read-u32be data 4))
           (num-tables (read-u16be data 12))
           ;; Parse table directory (starts at offset 44)
           (tables (loop for i below num-tables
                         for off = (+ 44 (* i 20))
                         collect (make-woff-table
                                  :tag (read-u32be data off)
                                  :offset (read-u32be data (+ off 4))
                                  :comp-length (read-u32be data (+ off 8))
                                  :orig-length (read-u32be data (+ off 12))
                                  :orig-checksum (read-u32be data (+ off 16)))))
           ;; Compute output size: sfnt header + table directory + table data (padded)
           (header-size (+ 12 (* num-tables 16)))
           (total-data-size (reduce #'+ tables
                                    :key (lambda (tbl)
                                           (pad4 (woff-table-orig-length tbl)))))
           (output-size (+ header-size total-data-size))
           (output (make-array output-size
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))
      ;; Write sfnt header
      (write-u32be output 0 flavor)
      (write-u16be output 4 num-tables)
      (multiple-value-bind (search-range entry-selector range-shift)
          (calc-search-range num-tables)
        (write-u16be output 6 search-range)
        (write-u16be output 8 entry-selector)
        (write-u16be output 10 range-shift))
      ;; Decompress tables and write table directory + data
      (let ((data-offset header-size))
        (loop for i from 0
              for tbl in tables
              for dir-off = (+ 12 (* i 16))
              do ;; Write table directory entry
                 (write-u32be output dir-off (woff-table-tag tbl))
                 (write-u32be output (+ dir-off 4) (woff-table-orig-checksum tbl))
                 (write-u32be output (+ dir-off 8) data-offset)
                 (write-u32be output (+ dir-off 12) (woff-table-orig-length tbl))
                 ;; Decompress or copy table data
                 (let ((table-data
                         (if (= (woff-table-comp-length tbl)
                                (woff-table-orig-length tbl))
                             ;; Stored uncompressed
                             (subseq data
                                     (woff-table-offset tbl)
                                     (+ (woff-table-offset tbl)
                                        (woff-table-orig-length tbl)))
                             ;; zlib compressed: skip 2-byte header, strip 4-byte Adler-32 trailer
                             (let* ((start (+ (woff-table-offset tbl) 2))
                                    (end (+ (woff-table-offset tbl)
                                            (woff-table-comp-length tbl)
                                            -4)))
                               (inflate-octets
                                (subseq data start end))))))
                   (replace output table-data :start1 data-offset)
                   (incf data-offset (pad4 (woff-table-orig-length tbl))))))
      output)))
