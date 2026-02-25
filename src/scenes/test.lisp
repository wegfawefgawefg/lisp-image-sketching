(defun scene-test (&key downsample-factor)
    (declare (ignore downsample-factor))
    (let ((image (make-blank-image 3 3)))
        (draw-rect image 0 0 1 1 255 0 0)
        (draw-rect image 1 1 1 1 0 255 0)
        (draw-rect image 2 2 1 1 0 0 255)
        (write-png-file (output-path "test.png") image)
        t))

(register-scene "test" #'scene-test
                :description "3x3 RGB diagonal test")
