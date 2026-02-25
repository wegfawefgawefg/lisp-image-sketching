(defun project-root ()
    (truename "./"))

(defun scene-files ()
    (sort (directory (merge-pathnames "src/scenes/*.lisp" (project-root)))
          #'string<
          :key #'namestring))

(let* ((root (project-root))
       (build-dir (merge-pathnames "build/" root))
       (core-files (list (merge-pathnames "src/render-core.lisp" root)
                         (merge-pathnames "src/scene-registry.lisp" root)
                         (merge-pathnames "src/main.lisp" root))))
    (ensure-directories-exist (merge-pathnames "_placeholder" build-dir))
    (dolist (file (append core-files (scene-files)))
        (let* ((out-name (concatenate 'string (pathname-name file) ".fasl"))
               (out-path (merge-pathnames out-name build-dir)))
            (compile-file file :output-file out-path))))
