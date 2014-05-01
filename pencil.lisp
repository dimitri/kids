(defvar *pencil-list* '((:none   . nil)
			(:pencil . #\@)
			(:eraser . #\Space))
  "Alist of available pencils and if any the character it draws")

(defvar *current-pencil* :none)

(defvar *border-char* #\*)
(defvar *wall-char*   #\|)

(defun draw-board (width height)
  (loop :for x :from 0 :to width
     :do (loop :for y :from 0 :to height
	    :for border-p := (or (= x 0) (= x width)
				 (= y 0) (= y height))
	    :for wall-p := (= y (+ 1 (random (- height 1))))
	    :do
	    (screen:set-window-cursor-position screen:*window* x y)
	    (if border-p
		(format screen:*window* "~c" *border-char*)
		(when wall-p
		  (format screen:*window* "~c" *wall-char*)))
	    (unless (or border-p wall-p)
	      (screen:set-window-cursor-position screen:*window* x y)
	      (format screen:*window* " ")))))

(defun draw-head (x y)
  (screen:set-window-cursor-position screen:*window* x y)
  (destructuring-bind (pencil . char)
      (assoc *current-pencil* *pencil-list*)
    (when char
      (format screen:*window* "~c" char)
      (screen:set-window-cursor-position screen:*window* x y))))

(defun draw-legend (width height)
  (screen:set-window-cursor-position screen:*window* (+ width 2) 10)
  (format screen:*window* "=  redraw screen        space  toggle eraser")
  (screen:set-window-cursor-position screen:*window* (+ width 3) 10)
  (format screen:*window* ".  toggle pencil            q  quit"))

(defmacro with-highlighting ((on window) &body body)
  `(if ,on
       (progn
	 (screen:highlight-on ,window)
	 (unwind-protect ,@body
	   (screen:highlight-off ,window)))
       (progn ,@body)))

(defun update-legend (width height)
  (screen:set-window-cursor-position screen:*window*
				     (+ width 3)
				     (+ 10 10))

  (with-highlighting ((eq :pencil *current-pencil*) screen:*window*)
    (format screen:*window* "pencil"))
  
  (screen:set-window-cursor-position screen:*window*
				     (+ width 2)
				     (+ 10 38))

  (with-highlighting ((eq :eraser *current-pencil*) screen:*window*)
    (format screen:*window* "eraser")))

(defun toggle-pencil (width height)
  (case *current-pencil*
    (:none      (setf *current-pencil* :pencil))
    (:pencil    (setf *current-pencil* :none))
    (:eraser    (setf *current-pencil* :pencil)))
  (update-legend width height))

(defun toggle-eraser (width height)
  (case *current-pencil*
    (:none      (setf *current-pencil* :eraser))
    (:pencil    (setf *current-pencil* :eraser))
    (:eraser    (setf *current-pencil* :none)))
  (update-legend width height))

(defun main ()
  (screen:with-window ()
    (destructuring-bind (width height)
	(multiple-value-bind (w h)
	    (screen:window-size screen:*window*)
	  (list (- w 5) (- h 1)))

      (draw-board width height)
      (draw-legend width height)

     ;; on se positionne au hasard dans la grille
      (let ((x (random width))
	    (y (random height)))
	(draw-head x y)
      
	(ext:with-keyboard
	    (loop
	       :for key := (read-char ext:*keyboard-input*)
	       :for key-char-p := (and (not (ext:char-key key))
				       (zerop (ext:char-bits key))
				       (character key))
	       :for key-char := (when key-char-p (character key))
	       
	       :when key-char-p
	       :do (case (character key)
		     (#\e (when (< 1 x)            (decf x)))
		     (#\c (when (< x (- width 1))  (incf x)))
		     (#\s (when (< 1 y)            (decf y)))
		     (#\f (when (< y (- height 1)) (incf y)))

		     (#\Space (toggle-eraser width height))
		     (#\.     (toggle-pencil width height))

		     (#\= (draw-board width height)))

	       :do (case (ext:char-key key)
		     (:up    (when (< 1 x)            (decf x)))
		     (:down  (when (< x (- width 1))  (incf x)))
		     (:left  (when (< 1 y)            (decf y)))
		     (:right (when (< y (- height 1)) (incf y))))

	       :do (draw-head x y)
	       :until (and key-char (or (char= key-char #\q)
					(char= key-char #\x)))))))))

(main)
(quit)