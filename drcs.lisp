;; -*- Lisp -*-

(defpackage :drcs
  (:use :cl :alexandria)
  (:export
   #:upload-image-as-drcs
   #:draw-drcs-image
   #:drcs-demo))

(in-package :drcs)

(defconstant +first-drcs+ #x21)
(defconstant +last-drcs+ #x7e)
(defconstant +drcs-count+ (- +last-drcs+ +first-drcs+))

(defun analyze-image (pathname)
  (cl-gd:with-image-from-file* (pathname)
    (assert (cl-gd:true-color-p))
    (let ((colors (make-hash-table)))
      (cl-gd:do-pixels ()
        (setf (gethash (cl-gd:raw-pixel) colors) t))
      (list :ncolors (hash-table-count colors)
            :colors (hash-table-keys colors)
            :width (cl-gd:image-width)
            :height (cl-gd:image-height)))))

(defun drcs-16-count (&key width height &allow-other-keys)
  (* (ceiling width 12)
     (ceiling height 10)))

(defun map-colors (pathname)
  (destructuring-bind (&key colors &allow-other-keys) (analyze-image pathname)
    (cl-gd:with-image-from-file* (pathname)
      (let ((color-map (make-hash-table))
            (mapped-colors (make-hash-table :test #'equal)))
        (dolist (color colors)
          (destructuring-bind (red green blue alpha) (cl-gd:color-components color)
            (declare (ignore alpha))
            (setf red (ash red -4)
                  green (ash green -4)
                  blue (ash blue -4))
            (let ((mapped-color (list red green blue)))
              (when (gethash mapped-color mapped-colors)
                (warn "color ~A mapping to already mapped color ~A" color mapped-color))
              (setf (gethash color color-map) mapped-color
                    (gethash mapped-color mapped-colors) t))))
        (let ((indexed-colors (hash-table-keys mapped-colors))
              (mapping (make-hash-table)))
          (maphash (lambda (color rgb)
                     (setf (gethash color mapping) (position rgb indexed-colors :test #'equal)))
                   color-map)
          (list :indexed-colors indexed-colors
                :mapping mapping))))))

(defun color-component-cept-bits (red green blue)
  (let ((first #x40)
        (second #x40))
    (setf (ldb (byte 1 5) first) (ldb (byte 1 3) red)
          (ldb (byte 1 4) first) (ldb (byte 1 3) green)
          (ldb (byte 1 3) first) (ldb (byte 1 3) blue)
          (ldb (byte 1 2) first) (ldb (byte 1 2) red)
          (ldb (byte 1 1) first) (ldb (byte 1 2) green)
          (ldb (byte 1 0) first) (ldb (byte 1 2) blue)
          (ldb (byte 1 5) second) (ldb (byte 1 1) red)
          (ldb (byte 1 4) second) (ldb (byte 1 1) green)
          (ldb (byte 1 3) second) (ldb (byte 1 1) blue)
          (ldb (byte 1 2) second) (ldb (byte 1 0) red)
          (ldb (byte 1 1) second) (ldb (byte 1 0) green)
          (ldb (byte 1 0) second) (ldb (byte 1 0) blue))
    (list first second)))

(defun make-cept-colors (colors)
  (dotimes (i (length colors))
    (cept:write-cept #x1F #x26 #x20
                     #x1F #x26
                     (format nil "~2,'0D" (+ i 16))
                     (apply 'color-component-cept-bits (nth i colors)))))

(defun get-pixel-at (x y)
  (cl-gd:get-pixel (if (< x (cl-gd:image-width))
                       x
                       0)
                   (if (< y (cl-gd:image-height))
                       y
                       0)))

(defun ensure-image-fits (pathname)
  (cl-gd:with-image-from-file* (pathname)
    (let* ((x-cells (ceiling (cl-gd:image-width) 6))
           (y-cells (ceiling (cl-gd:image-height) 10))
           (needed (* x-cells y-cells))
           (available (floor +drcs-count+ 2)))
      (when (> needed available)
        (error "Image too large, ~D cells available ~D cells needed" available needed)))))

(defun make-drcs (pathname &optional (start-char-code +first-drcs+))
  (ensure-image-fits pathname)
  (let ((color-mapping (getf (map-colors pathname) :mapping)))
    (cept:write-cept #x1F #x23 #x20 #x4B #x44)
    (cl-gd:with-image-from-file* (pathname)
      (cept:write-cept #x1F #x23 start-char-code)
      (dotimes (row (ceiling (cl-gd:image-height) 10))
        (dotimes (col (ceiling (cl-gd:image-width) 6))
          (dotimes (bit 4)
            (cept:write-cept (+ #x30 bit))
            (dotimes (pixel-y 10)
              (let ((data #x40))
                (dotimes (pixel-x 6)
                  (let* ((x (+ (* col 6) pixel-x))
                         (y (+ (* row 10) pixel-y))
                         (color (gethash (get-pixel-at x y) color-mapping)))
                   (setf (ldb (byte 1 (- 5 pixel-x)) data) (ldb (byte 1 bit) color))))
                (cept:write-cept data)))))))))

(defun upload-image-as-drcs (pathname &key (start-char-code +first-drcs+))
  (make-cept-colors (getf (map-colors pathname) :indexed-colors))
  (cept:goto 0 0)
  (make-drcs pathname)
  (cept:goto 0 0)
  (destructuring-bind (&key width height &allow-other-keys) (analyze-image pathname)
    (list :rows (ceiling height 10)
          :cols (ceiling width 6)
          :start-char-code start-char-code)))

(defun draw-drcs-image (start-row start-col &key rows cols start-char-code)
  (let ((current-char-code start-char-code))
    ;; g3 -> drcs, g3 -> left
    (cept:write-cept #x1B #x2B #x20 #x40 #x1b #x6f)
    (dotimes (row rows)
      (cept:goto (+ start-row row) start-col)
      (dotimes (col cols)
        (cept:write-cept current-char-code)
        (incf current-char-code 2)))
    ;; g0 -> left
    (cept:write-cept #x0f)))

(defun drcs-demo (pathname)
  (cept:clear-page)
  (cept:screen-color 7)
  (destructuring-bind (&key rows cols start-char-code) (upload-image-as-drcs pathname)
    (loop for row below 24 by rows
          do (loop for col below 40 by cols
                   do (draw-drcs-image row col :rows rows :cols cols :start-char-code start-char-code))))
  (finish-output cept:*cept-stream*))

(defun scale-down (input-pathname output-pathname)
  "Scale down the image in input-pathname to half its original
dimensions by copying every other pixel to a new image, then save the
scaled-down image to output-pathname, overwriting it if it exists."
  (cl-gd:with-image-from-file (input input-pathname)
    (cl-gd:with-image (output (floor (cl-gd:image-width input) 2) (floor (cl-gd:image-height input) 2) t)
      (loop for y below (floor (cl-gd:image-height input) 2)
            do (loop for x below (floor (cl-gd:image-width input) 2)
                     do (cl-gd:set-pixel x y
                                         :color (cl-gd:get-pixel (* x 2) (* y 2) :image input)
                                         :image output)))
      (cl-gd:write-image-to-file output-pathname :image output :if-exists :supersede))))

(defun show-colors ()
  (cept:clear-page)
  (cept:screen-color 0)
  (loop for palette from 2 below 4
        do (cept:select-palette palette)
           (dotimes (color 8)
             (cept:row-color color)
             (cept:write-cept "hello hello" #\return #\linefeed))))
