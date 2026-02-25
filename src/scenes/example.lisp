(defun scene-example (&key downsample-factor)
    (declare (ignore downsample-factor))
    (let ((image (fill-gradient (make-blank-image 64 64))))
        (write-png-file (output-path "example.png") image)
        t))

(register-scene "example" #'scene-example
                :description "64x64 gradient")
