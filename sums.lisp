(defun list-digits (number)
  (nreverse
   (loop :with q := number :with r := 0
      :while (plusp q)
      :do (setf (values q r) (truncate q 10))
      :collect r)))

(defun number-to-exploded-string (number)
  "returns the number with digits separated by a space"
  (format nil "~{~d~^ ~}" (list-digits number)))

(defun format-at-point (w x y fmt &rest args)
  (screen:set-window-cursor-position w x y)
  (if args
      (format w "~{~}" fmt args)
      (format w "~a" fmt)))

(defun draw-sum (a b &key ((:window w) screen:*window*) width height)
  (format-at-point w 10 10  "~d" (number-to-exploded-string a))
  (format-at-point w 12 8 "+ ~d" (number-to-exploded-string b))
  (format-at-point w 14 8 "------")
  (screen:set-window-cursor-position w 16 12))

(defun complete-sum (window width height)
  (let* ((w      window)
         (a      (+ 10 (random 89)))
         (b      (+ 10 (random 89)))
         (sum    (+ a b))
         (digits (reverse (list-digits sum))))

    (draw-sum a b :window w :width width :height height)

    (ext:with-keyboard
        (loop
           :with expected-digits := digits
           :with y-position := 12
           :for key := (read-char ext:*keyboard-input*)
           :for key-char-p := (and (not (ext:char-key key))
                                   (zerop (ext:char-bits key))
                                   (character key))
           :for key-char := (when key-char-p (character key))
           :for entered-digit := (when (and key-char
                                            (digit-char-p key-char))
                                   (parse-integer (string key-char)))

           :when (and entered-digit
                      (= entered-digit (car expected-digits)))
           :do (progn
                 (format-at-point w 16 y-position "~d" entered-digit)
                 ;; we just made progress, and we move left
                 (setf y-position      (+ y-position -2))
                 (setf expected-digits (cdr expected-digits))

                 (screen:set-window-cursor-position w 16 y-position))

           :while expected-digits

           :when (and key-char (or (char= key-char #\q)
                                   (char= key-char #\x)))
           :return (list nil nil)

           :finally (return (list a b))))))

(defun main ()
  ;; first reset the randomness of the game
  (setf *random-state* (make-random-state t))
  (screen:with-window ()
    (multiple-value-bind (width height)
        (screen:window-size screen:*window*)

      (loop :for (a b) := (complete-sum screen:*window* width height)
         :while a

         :do (progn (format-at-point screen:*window* 6 2
                                     "Bravo!  ~a + ~a = ~a."
                                     a b (+ a b))

                    ;; press any key to continue
                    (ext:with-keyboard
                        (read-char ext:*keyboard-input*))

                    (screen:clear-window screen:*window*))))))

(main)
(quit)
