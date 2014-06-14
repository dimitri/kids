#!/bin/bash
#|
exec clisp -q -q $0 $0 ${1+"$@"}
exit
|#

(defparameter *digits* 3)
(defparameter *numbers* 3)

(defun list-digits (number)
  "Return a list of NUMBER digits, including as many leading zeros as needed
   for *digits*."
  (let ((digits
         (nreverse
          (loop :with q := number :with r := 0
             :while (plusp q)
             :do (setf (values q r) (truncate q 10))
             :collect r))))
    ;; maybe preprend with leading zeroes
    (if (< (length digits) *digits*)
        (append (make-list (- *digits* (length digits)) :initial-element 0)
                digits)
        digits)))

(defun number-to-exploded-string (number)
  "returns the number with digits separated by a space"
  (let ((digits-with-leading-spaces
         (loop :for leading-zeroes = t :then (= d 0)
            :for d :in (list-digits number)
            :when (and leading-zeroes (= d 0))
            :collect #\Space
            :else
            :collect d)))
    (format nil "~{~a~^ ~}" digits-with-leading-spaces)))

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
     :finally (format-at-point w x-position y-position
                               (make-string (+ 2 (* 2 *digits*))
                                            :initial-element #\-))))

(defun complete-sum (window width height)
  (let* ((w      window)
	 (n-list (loop :repeat *numbers* :collect (random (expt 10 *digits*))))
         (sum    (reduce #'+ n-list))
         (digits (reverse (list-digits sum)))
	 (x-ans  (+ 4 2 (* 2 *numbers*)))
         (y-ans  (+ 8 (* 2 *digits*))))

    (draw-sum n-list :window w :width width :height height)
    (screen:set-window-cursor-position w x-ans y-ans)

    (ext:with-keyboard
        (loop
           :with expected-digits := digits
           :with y-position := y-ans
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

(defun parse-options (args)
  "Set *numbers* and *digits* according to command line."
  (loop :for (arg value) :on (rest args) :by #'cddr
     :when (string-equal "-n" arg)
     :do   (setf *numbers* (parse-integer value))

     :when (string-equal "-d" arg)
     :do   (setf *digits* (parse-integer value))))

(defun main ()
  ;; first reset the randomness of the game
  (setf *random-state* (make-random-state t))
  (parse-options ext:*args*)

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
