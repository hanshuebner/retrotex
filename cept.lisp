;; -*- Lisp -*-

(defpackage :cept
  (:use :cl :alexandria)
  (:export #:*mode-0-conversion-table*
           #:*mode-1-conversion-table*
           #:string-to-bytes
           #:*cept-stream*
           #:write-cept
           #:reset
           #:test
           #:disable-system-line
           #:clear-page
           #:service-jump
           #:service-jump-return
           #:reset-page
           #:show-cursor
           #:hide-cursor
           #:set-scroll-region
           #:enable-scrolling
           #:disable-scrolling
           #:scroll-up
           #:scroll-down
           #:goto
           #:double-height
           #:double-width
           #:quad-size
           #:constant-input
           #:normal-size
           #:home
           #:reset-palette
           #:screen-color
           #:select-palette
           #:row-foreground-color
           #:row-background-color
           #:color-polarity
           #:foreground-color
           #:background-color))

(in-package :cept)

(defvar *cept-stream* nil)

(defun to-octets (things)
  (flex:with-output-to-sequence (s)
    (dolist (thing things)
      (etypecase thing
        (string (write-sequence (cept:string-to-bytes thing) s))
        ((array (unsigned-byte 8)) (write-sequence thing s))
        (character (write-byte (char-code thing) s))
        (number (write-byte thing s))
        (list (write-sequence (funcall #'to-octets thing) s))))))

(defun write-cept (&rest stuff)
  (let ((octets (to-octets stuff)))
    (write-sequence (make-array (length octets) :element-type '(unsigned-byte 8) :initial-contents octets)
                    *cept-stream*)))

(defun reset ()
  (write-cept 1 #x64))

(defun test ()
  (write-cept 1 #x78))

(defun disable-system-line (&optional permanentp)
  (write-cept 1 (if permanentp #x6b #x6a)))

(defun clear-page ()
  (write-cept #x0c))

(defun service-jump (&optional (line 23))
  (write-cept #x1f #x2f #x40 (+ #x41 line)))

(defun service-jump-return ()
  (write-cept #x1F #x2F #x4F))

(defun reset-page (&optional (arg #x42))
  (write-cept #x1f #x2f arg))

(defun show-cursor ()
  (write-cept #x11))

(defun hide-cursor ()
  (write-cept #x14))

(defun set-scroll-region (top bottom)
  (let ((top (1+ top))
        (bottom (1+ bottom)))
    (write-cept #x9B
                (+ #x30 (floor top 10)) (+ #x30 (mod top 10)) #x3B
                (+ #x30 (floor bottom 10)) (+ #x30 (mod bottom 10)) #x55)))

(defun enable-scrolling ()
  (write-cept #x9B #x32 #x60))

(defun disable-scrolling ()
  (write-cept #x9B #x33 #x60))

(defun scroll-up ()
  (write-cept #x9B #x30 #x60))

(defun scroll-down ()
  (write-cept #x9B #x31 #x60))

(defun goto (row col)
  (write-cept #x1f (+ #x41 row) (+ #x41 col)))

(defun home ()
  (write-cept #x1e))

(defun double-height ()
  (write-cept #x8d))

(defun double-width ()
  (write-cept #x8e))

(defun quad-size ()
  (write-cept #x8f))

(defun normal-size ()
  (write-cept #x8c))

(defun screen-color (i)
  (write-cept #x1B #x23 #x20 (+ #x50 i)))

(defun row-foreground-color (i)
  (write-cept #x1B #x23 #x21 (+ #x40 i)))

(defun row-background-color (i)
  (write-cept #x1B #x23 #x21 (+ #x50 i)))

(defun color-polarity (swap)
  (write-cept #x1b #x23 #x21 (if swap #x5d #x5c)))

(defun foreground-color (color)
  (assert (<= 0 color 7))
  (write-cept (+ #x80 color)))

(defun background-color (color)
  (assert (<= 0 color 7))
  (write-cept (+ #x90 color)))

(defun reset-basic-state (&optional (state #x41))
  (write-cept #x1f #x2f state))

(defun reset-palette ()
  (write-cept #x1F #x26 #x21))

(defun select-palette (i)
  (write-cept #x9B (+ #x30 i) #x40))

(defun constant-input ()
  (write-cept 1 #x49))

(defun g2code (mode c &optional extra)
  `(,@(if (zerop mode)
          (list #x19 (char-code c))
          (list (+ (char-code c) 128)))
     ,@(when extra
        (list (char-code extra)))))

(defun make-conversion-table-input (mode)
  (list #\¤ #\$                         ; $ and ¤ are swapped
        #\$ (g2code mode #\$)           ; $ and ¤ are swapped
        #\© (g2code mode #\S)
        #\® (g2code mode #\R)
        #\À (g2code mode #\A #\A)
        #\Á (g2code mode #\B #\A)
        #\Â (g2code mode #\C #\A)
        #\Ã (g2code mode #\D #\A)
        #\Ä (g2code mode #\H #\A)
        #\Å (g2code mode #\J #\A)
        #\Æ (g2code mode #\a)
        #\Ç (g2code mode #\K #\C)
        #\È (g2code mode #\A #\E)
        #\É (g2code mode #\B #\E)
        #\Ê (g2code mode #\C #\E)
        #\Ë (g2code mode #\H #\E)
        #\Ì (g2code mode #\A #\I)
        #\Í (g2code mode #\B #\I)
        #\Î (g2code mode #\C #\I)
        #\Ï (g2code mode #\H #\I)
        #\Ð (g2code mode #\b)
        #\Ñ (g2code mode #\D #\N)
        #\Ò (g2code mode #\A #\O)
        #\Ó (g2code mode #\B #\O)
        #\Ô (g2code mode #\C #\O)
        #\Õ (g2code mode #\D #\O)
        #\Ö (g2code mode #\H #\O)
        #\× (g2code mode #\4)
        #\Ø (g2code mode #\i)
        #\Ù (g2code mode #\A #\U)
        #\Ú (g2code mode #\B #\U)
        #\Û (g2code mode #\C #\U)
        #\Ü (g2code mode #\H #\U)
        #\Ý (g2code mode #\B #\Y)
        #\Þ (g2code mode #\l)
        #\ß (g2code mode #\{)
        #\à (g2code mode #\A #\a)
        #\á (g2code mode #\B #\a)
        #\â (g2code mode #\C #\a)
        #\ã (g2code mode #\D #\a)
        #\ä (g2code mode #\H #\a)
        #\å (g2code mode #\J #\a)
        #\æ (g2code mode #\q)
        #\ç (g2code mode #\K #\c)
        #\è (g2code mode #\A #\e)
        #\é (g2code mode #\B #\e)
        #\ê (g2code mode #\C #\e)
        #\ë (g2code mode #\H #\e)
        #\ì (g2code mode #\A #\i)
        #\í (g2code mode #\B #\i)
        #\î (g2code mode #\C #\i)
        #\ï (g2code mode #\H #\i)
        #\ð (g2code mode #\s)
        #\ñ (g2code mode #\D #\n)
        #\ò (g2code mode #\A #\o)
        #\ó (g2code mode #\B #\o)
        #\ô (g2code mode #\C #\o)
        #\õ (g2code mode #\D #\o)
        #\ö (g2code mode #\H #\o)
        #\÷ (g2code mode #\8)
        #\ø (g2code mode #\u)
        #\ù (g2code mode #\A #\u)
        #\ú (g2code mode #\B #\u)
        #\û (g2code mode #\C #\u)
        #\ü (g2code mode #\H #\u)
        #\ý (g2code mode #\A #\y)
        #\þ (g2code mode #\|)
        #\ÿ (g2code mode #\H #\y)

        ;;  arrows
        #\← (g2code mode #\,)
        #\↑ (g2code mode #\-)
        #\→ (g2code mode #\.)
        #\↓ (g2code mode #\/)

        ;;  math
        #\⋅ (g2code mode #\7)

        ;;  latin other
        #\š (g2code mode #\O #\s)
        #\Œ (g2code mode #\j)
        #\œ (g2code mode #\z)
        #\ł (g2code mode #\x)
        #\č (g2code mode #\O #\c)
        #\ć (g2code mode #\B #\c)

        ;;  greek
        #\μ (g2code mode #\5)
        #\Ω (g2code mode #\`)

        ;;  punctuation
        #\‚ (g2code mode #\))
        #\’ (g2code mode #\9)
        #\‘ (g2code mode #\9)
        #\„ (g2code mode #\*)
        #\“ (g2code mode #\:)
        #\″ (g2code mode #\:)
        #\” (g2code mode #\:)
        #\– (g2code mode #\P)

        ;;  look-alikes
        #\† (list (char-code #\+))
        #\− (list (char-code #\-))      ; MINUS SIGN
        #\⟨ (list (char-code #\<))
        #\⟩ (list (char-code #\>))
        #\∗ (list (char-code #\*))
        #\‐ (list (char-code #\-))
        #\— (list (char-code #\-))

        ;;  spaces
        #\  (list (char-code #\ ))      ; NARROW NO-BREAK SPACE
        #\  (list (char-code #\ ))      ; THIN SPACE
        #\  (list (char-code #\ ))      ; ZERO WIDTH SPACE
        #\  (list (char-code #\ ))      ; EN SPACE

        ;;  used in phonetic alphabet
        #\ˈ (list (char-code #\'))
        #\ː (list (char-code #\:))

        #\linefeed (list (char-code #\return) (char-code #\linefeed))))

(defun make-conversion-table (mode)
  (loop with result = (make-hash-table)
        for (char codes) on (make-conversion-table-input mode) by #'cddr
        do (setf (gethash char result) codes)
        finally (return result)))

(defvar *mode-0-conversion-table* (make-conversion-table 0))
(defvar *mode-1-conversion-table* (make-conversion-table 1))

(defun string-to-bytes (string &optional (conversion-table *mode-0-conversion-table*))
  (loop for c across string
        when (not (gethash c conversion-table))
          do (setf (gethash c conversion-table) (list (char-code c)))
        append (gethash c conversion-table)))
