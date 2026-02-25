(defvar *default-downsample-factor* 2)

(declaim (ftype function list-scenes))
(declaim (ftype function run-scene))

(defun project-root ()
    (truename "./"))

(defun scenes-dir ()
    (merge-pathnames "src/scenes/" (project-root)))

(defun scene-files ()
    (sort (directory (merge-pathnames "*.lisp" (scenes-dir)))
          #'string<
          :key #'namestring))

(defun load-runtime ()
    (load "src/render-core.lisp")
    (load "src/scene-registry.lisp")
    (dolist (file (scene-files))
        (load file))
    t)

(defun parse-int (raw fallback)
    (let ((value (and raw (parse-integer raw :junk-allowed t))))
        (if (and value (>= value 1)) value fallback)))

(defun print-scene-list ()
    (format t "Available scenes:~%")
    (dolist (entry (list-scenes))
        (format t "  ~a~@[ - ~a~]~%"
                (getf entry :name)
                (getf entry :description)))
    (format t "  all - run every discovered scene~%")
    (format t "  list - print this scene list~%")
    t)

(defun main (&optional (argv (cdr sb-ext:*posix-argv*)))
    (load-runtime)
    (let* ((scene (or (first argv) "all"))
           (downsample-factor (parse-int (second argv) *default-downsample-factor*)))
        (cond
            ((or (string= scene "list")
                 (string= scene "--list")
                 (string= scene "-l"))
             (print-scene-list))
            (t
             (run-scene scene :downsample-factor downsample-factor)
             (format t "Generated scene '~a' (downsample factor: ~a) in outputs/~%"
                     scene
                     downsample-factor)
             t))))
