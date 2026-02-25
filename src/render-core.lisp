(eval-when (:compile-toplevel :load-toplevel :execute)
    (unless (find-package :ql)
        (let ((quicklisp-setup
               (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname))))
            (unless (probe-file quicklisp-setup)
                (error "Quicklisp setup not found at ~a" quicklisp-setup))
            (load quicklisp-setup)))
    (let ((quickload-fn (find-symbol "QUICKLOAD" "QL")))
        (unless quickload-fn
            (error "Quicklisp loaded but QL:QUICKLOAD is unavailable"))
        (funcall quickload-fn :zpng :silent t)))

(defparameter *output-dir* "outputs/")
(defparameter *default-downsample-factor* 2)

(defun clamp-u8 (value)
    (max 0 (min 255 value)))

(defun make-blank-image (height width)
    (make-instance 'zpng:png :width width :height height :color-type :truecolor))

(defun output-path (filename)
    (concatenate 'string *output-dir* filename))

(defun normalize-downsample-factor (factor)
    (let ((n (or factor 1)))
        (if (and (integerp n) (>= n 1))
            n
            1)))

(defun image-height (image)
    (zpng:height image))

(defun image-width (image)
    (zpng:width image))

(defun in-bounds-p (image x y)
    (and (<= 0 x) (< x (image-width image))
         (<= 0 y) (< y (image-height image))))

(defun set-pixel (image x y r g b)
    (when (in-bounds-p image x y)
        (let ((data (zpng:data-array image)))
            (setf (aref data y x 0) (clamp-u8 r))
            (setf (aref data y x 1) (clamp-u8 g))
            (setf (aref data y x 2) (clamp-u8 b)))))

(defun blend-pixel (image x y r g b alpha)
    (when (in-bounds-p image x y)
        (let* ((data (zpng:data-array image))
               (a (max 0.0 (min 1.0 alpha)))
               (inv-a (- 1.0 a))
               (old-r (aref data y x 0))
               (old-g (aref data y x 1))
               (old-b (aref data y x 2)))
            (setf (aref data y x 0)
                  (clamp-u8 (round (+ (* inv-a old-r) (* a r)))))
            (setf (aref data y x 1)
                  (clamp-u8 (round (+ (* inv-a old-g) (* a g)))))
            (setf (aref data y x 2)
                  (clamp-u8 (round (+ (* inv-a old-b) (* a b))))))))

(defun write-png-file (filename image)
    (ensure-directories-exist filename)
    (zpng:write-png image filename))

(defun downsample-image (source factor)
    (let* ((f (normalize-downsample-factor factor))
           (src-h (image-height source))
           (src-w (image-width source)))
        (if (= f 1)
            source
            (let* ((dst-h (floor src-h f))
                   (dst-w (floor src-w f))
                   (dst (make-blank-image dst-h dst-w))
                   (src-data (zpng:data-array source))
                   (samples (* f f)))
                (dotimes (yy dst-h)
                    (dotimes (xx dst-w)
                        (let ((sum-r 0)
                              (sum-g 0)
                              (sum-b 0)
                              (start-y (* yy f))
                              (start-x (* xx f)))
                            (dotimes (dy f)
                                (dotimes (dx f)
                                    (let ((sy (+ start-y dy))
                                          (sx (+ start-x dx)))
                                        (incf sum-r (aref src-data sy sx 0))
                                        (incf sum-g (aref src-data sy sx 1))
                                        (incf sum-b (aref src-data sy sx 2)))))
                            (set-pixel dst
                                       xx
                                       yy
                                       (round (/ sum-r samples))
                                       (round (/ sum-g samples))
                                       (round (/ sum-b samples))))))
                dst))))

(defun draw-rect (image x y width height r g b)
    (let ((x1 (max 0 x))
          (y1 (max 0 y))
          (x2 (min (image-width image) (+ x width)))
          (y2 (min (image-height image) (+ y height))))
        (loop for yy from y1 below y2 do
            (loop for xx from x1 below x2 do
                (set-pixel image xx yy r g b))))
    image)

(defun fill-gradient (image)
    (let ((height (image-height image))
          (width (image-width image)))
        (dotimes (yy height)
            (dotimes (xx width)
                (set-pixel image
                           xx
                           yy
                           (floor (* 255 (/ xx (max 1 (- width 1)))))
                           (floor (* 255 (/ yy (max 1 (- height 1)))))
                           32))))
    image)
