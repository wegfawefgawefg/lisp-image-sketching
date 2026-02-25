(defparameter *scene-registry* (make-hash-table :test 'equal))

(defun register-scene (name fn &key description)
    (setf (gethash (string-downcase name) *scene-registry*)
          (list :name (string-downcase name)
                :fn fn
                :description description))
    t)

(defun scene-entry (name)
    (gethash (string-downcase name) *scene-registry*))

(defun list-scenes ()
    (let ((items nil))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (push v items))
                 *scene-registry*)
        (sort items #'string< :key (lambda (it) (getf it :name)))))

(defun run-scene (name &key downsample-factor)
    (let ((scene-name (string-downcase name)))
        (if (string= scene-name "all")
            (progn
                (dolist (entry (list-scenes))
                    (funcall (getf entry :fn) :downsample-factor downsample-factor))
                t)
            (let ((entry (scene-entry scene-name)))
                (unless entry
                    (error "Unknown scene '~a'. Try scene 'list' to see available scenes." name))
                (funcall (getf entry :fn) :downsample-factor downsample-factor)))))
