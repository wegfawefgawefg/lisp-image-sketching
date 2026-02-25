(defun scene-some-boxes (&key downsample-factor)
    (declare (ignore downsample-factor))
    (let ((image (fill-gradient (make-blank-image 128 128))))
        (draw-rect image 8 8 36 28 255 20 20)
        (draw-rect image 44 24 40 40 20 255 20)
        (draw-rect image 78 56 42 52 20 20 255)
        (write-png-file (output-path "some_boxes.png") image)
        t))

(register-scene "some-boxes" #'scene-some-boxes
                :description "128x128 gradient with colored boxes")
