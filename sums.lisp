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

(defun draw-sum (numbers &key ((:window w) screen:*window*) width height)
  (loop
     :for x-position = 4 :then (+ x-position 2)
     :for y-position = 10 :then 8
     :for number :in numbers
     :for first = t :then nil
     :do (format-at-point w x-position y-position
			  "~:[+ ~;~]~d"
			  first
			  (number-to-exploded-string number))
     :finally (format-at-point w x-position y-position "---------")))

(defun complete-sum (window width height)
  (let* ((w      window)
	 (n-list (loop :repeat 3 :collect (+ 100 (random 89))))
         (sum    (reduce #'+ n-list))
         (digits (reverse (list-digits sum)))
	 (x-ans  (+ 4 2 (* 2 (length n-list)))))

    (draw-sum n-list :window w :width width :height height)
    (screen:set-window-cursor-position w x-ans 14)

    (ext:with-keyboard
        (loop
           :with expected-digits := digits
           :with y-position := 14
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
                 (format-at-point w x-ans y-position "~d" entered-digit)
                 ;; we just made progress, and we move left
                 (setf y-position      (+ y-position -2))
                 (setf expected-digits (cdr expected-digits))

                 (screen:set-window-cursor-position w x-ans y-position))

           :while expected-digits

           :when (and key-char (or (char= key-char #\q)
                                   (char= key-char #\x)))
           :return nil

           :finally (return n-list)))))

(defun main ()
  ;; first reset the randomness of the game
  (setf *random-state* (make-random-state t))
  (screen:with-window ()
    (multiple-value-bind (width height)
        (screen:window-size screen:*window*)

      (loop :for n-list := (complete-sum screen:*window* width height)
         :while n-list

         :do (progn (format-at-point screen:*window* 2 2
                                     "Bravo!  ~{~a~^ + ~} = ~a."
                                     n-list
				     (reduce #'+ n-list))

                    ;; press any key to continue
                    (ext:with-keyboard
                        (read-char ext:*keyboard-input*))

                    (screen:clear-window screen:*window*))))))

(main)
(quit)
