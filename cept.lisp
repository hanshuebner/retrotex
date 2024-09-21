;; -*- Lisp -*-

(defpackage :cept
  (:use :cl :alexandria)
  (:export *mode-0-conversion-table*
           *mode-1-conversion-table*
           string-to-bytes))

(in-package :cept)

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
