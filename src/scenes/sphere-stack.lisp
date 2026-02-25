(defun draw-filled-circle (image cx cy radius r g b alpha)
    (let* ((r2 (* radius radius))
           (x0 (max 0 (- cx radius)))
           (x1 (min (1- (image-width image)) (+ cx radius)))
           (y0 (max 0 (- cy radius)))
           (y1 (min (1- (image-height image)) (+ cy radius))))
        (loop for yy from y0 to y1 do
            (let ((dy (- yy cy)))
                (loop for xx from x0 to x1 do
                    (let ((dx (- xx cx)))
                        (when (<= (+ (* dx dx) (* dy dy)) r2)
                            (blend-pixel image xx yy r g b alpha))))))))

(defun lerp-f (a b tval)
    (+ a (* (- b a) tval)))

(defun hsv-to-rgb-u8 (h s v)
    (let* ((hh (* 6.0 (mod h 1.0)))
           (i (mod (floor hh) 6))
           (f (- hh (floor hh)))
           (p (* v (- 1.0 s)))
           (q (* v (- 1.0 (* s f))))
           (tt (* v (- 1.0 (* s (- 1.0 f))))))
        (flet ((u8 (x) (clamp-u8 (round (* 255 x)))))
            (case i
                (0 (values (u8 v) (u8 tt) (u8 p)))
                (1 (values (u8 q) (u8 v) (u8 p)))
                (2 (values (u8 p) (u8 v) (u8 tt)))
                (3 (values (u8 p) (u8 q) (u8 v)))
                (4 (values (u8 tt) (u8 p) (u8 v)))
                (otherwise (values (u8 v) (u8 p) (u8 q)))))))

(defun draw-stacked-sphere (image cx cy max-radius x-bias-frac y-bias-frac
                           &key (slices 8) (base-brightness 120))
    (let ((slice-mid (/ (1- slices) 2.0)))
        (dotimes (i slices)
            (let* ((slice-t (/ i (float (max 1 (- slices 1)))))
                   (slice-index-from-mid (- i slice-mid))
                   (z (- (* 2.0 slice-t) 1.0))
                   (radius (round (* max-radius (sqrt (max 0.0 (- 1.0 (* z z)))))))
                   (alpha (+ 0.018 (* 0.055 (- 1.0 (abs z)))))
                   (brightness (+ base-brightness
                                  (round (* 120 (- 1.0 (max 0.0 z))))))
                   (r (clamp-u8 brightness))
                   (g (clamp-u8 (+ 12 brightness)))
                   (b (clamp-u8 (+ 22 brightness)))
                   (x-bias (round (* radius x-bias-frac slice-index-from-mid)))
                   (y-bias (round (* radius y-bias-frac slice-index-from-mid)))
                   (slice-cx (+ cx x-bias))
                   (slice-cy (+ cy y-bias)))
                (draw-filled-circle image slice-cx slice-cy radius r g b alpha)))))

(defun scene-sphere-stack (&key (downsample-factor *default-downsample-factor*))
    (let* ((factor (normalize-downsample-factor downsample-factor))
           (height 1080)
           (width 1920)
           (render-height (* height factor))
           (render-width (* width factor))
           (image (make-blank-image render-height render-width))
           (cx (floor render-width 2))
           (cy (floor render-height 2))
           (max-radius (round (* 0.32 (min render-width render-height))))
           (x-bias-frac-per-slice 0.15)
           (y-bias-frac-per-slice 0.08))
        (dotimes (yy render-height)
            (dotimes (xx render-width)
                (let* ((base (round (* 18 (/ yy (max 1 (- render-height 1))))))
                       (bg-r base)
                       (bg-g (+ 4 base))
                       (bg-b (+ 10 base)))
                    (set-pixel image xx yy bg-r bg-g bg-b))))
        (draw-stacked-sphere image cx cy max-radius
                             x-bias-frac-per-slice y-bias-frac-per-slice
                             :slices 8
                             :base-brightness 120)
        (write-png-file (output-path "sphere_stack_1920x1080.png")
                        (downsample-image image factor))
        t))

(register-scene "sphere" #'scene-sphere-stack
                :description "Transparent stacked circles forming a sphere")
(register-scene "sphere-stack" #'scene-sphere-stack
                :description "Alias for 'sphere'")

(defun scene-spherescape (&key (downsample-factor *default-downsample-factor*))
    (let* ((factor (normalize-downsample-factor downsample-factor))
           (height 1080)
           (width 1920)
           (render-height (* height factor))
           (render-width (* width factor))
           (image (make-blank-image render-height render-width))
           (cols 6)
           (rows 4)
           (x-margin (round (* 0.10 render-width)))
           (y-margin (round (* 0.12 render-height)))
           (x-step (/ (- render-width (* 2 x-margin)) (float (max 1 (1- cols)))))
           (y-step (/ (- render-height (* 2 y-margin)) (float (max 1 (1- rows))))))
        (dotimes (yy render-height)
            (dotimes (xx render-width)
                (let* ((ny (/ yy (float (max 1 (- render-height 1)))))
                       (nx (/ xx (float (max 1 (- render-width 1)))))
                       (base (+ 6 (round (* 18 ny))))
                       (vignette (round (* 14 (abs (- nx 0.5)))))
                       (bg-r (clamp-u8 (- base vignette)))
                       (bg-g (clamp-u8 (+ 4 (- base vignette))))
                       (bg-b (clamp-u8 (+ 16 (- base vignette))))
                       (haze (round (* 6 (sin (* 10.0 nx))))))
                    (set-pixel image xx yy bg-r (clamp-u8 (+ bg-g haze)) bg-b))))
        (dotimes (row rows)
            (dotimes (col cols)
                (let* ((cx (round (+ x-margin (* col x-step))))
                       (cy (round (+ y-margin (* row y-step))))
                       (nx (/ cx (float (max 1 (- render-width 1)))))
                       (ny (/ cy (float (max 1 (- render-height 1)))))
                       (size-mod (+ 0.55
                                    (* 0.50 (sin (+ (* 5.0 nx) (* 3.0 ny))))
                                    (* 0.35 (cos (+ (* 4.0 ny) (* 2.0 nx))))))
                       (radius (max 18 (round (* factor 62 (abs size-mod)))))
                       (x-bias-frac (+ (* 0.28 (- nx 0.5))
                                       (* 0.10 (sin (* 8.0 ny)))))
                       (y-bias-frac (+ (* 0.24 (- ny 0.5))
                                       (* 0.08 (cos (* 6.0 nx)))))
                       (brightness (+ 92 (round (* 55 ny)))))
                    (draw-stacked-sphere image cx cy radius x-bias-frac y-bias-frac
                                         :slices 8
                                         :base-brightness brightness))))
        (write-png-file (output-path "spherescape_1920x1080.png")
                        (downsample-image image factor))
        t))

(register-scene "spherescape" #'scene-spherescape
                :description "Field of offset stacked spheres with position-driven tilt")

(defun draw-stacked-sphere-tinted (image cx cy max-radius x-bias-frac y-bias-frac
                                  &key (slices 8)
                                    (base-brightness 120)
                                    (r-scale 1.00)
                                    (g-scale 0.62)
                                    (b-scale 0.20)
                                    (alpha-mult 1.0)
                                    force-alpha
                                    hue-start
                                    hue-end
                                    (hue-by-depth t)
                                    (draw-near-last t)
                                    (hue-saturation 0.95)
                                    (hue-value-base 0.82)
                                    (hue-value-range 0.18))
    (let ((slice-mid (/ (1- slices) 2.0)))
        (dotimes (i slices)
            (let* ((idx (if draw-near-last i (- (1- slices) i)))
                   (slice-t (/ idx (float (max 1 (- slices 1)))))
                   (slice-index-from-mid (- i slice-mid))
                   (z (- (* 2.0 slice-t) 1.0))
                   (depth-t (/ (+ z 1.0) 2.0))
                   (radius (round (* max-radius (sqrt (max 0.0 (- 1.0 (* z z)))))))
                   (alpha (if force-alpha
                              force-alpha
                              (min 1.0 (* alpha-mult
                                          (+ 0.020 (* 0.060 (- 1.0 (abs z))))))))
                   (brightness (+ base-brightness
                                  (round (* 110 (- 1.0 (max 0.0 z))))))
                   (energy (min 1.0 (/ brightness 255.0)))
                   (r 0)
                   (g 0)
                   (b 0)
                   (x-bias (round (* radius x-bias-frac slice-index-from-mid)))
                   (y-bias (round (* radius y-bias-frac slice-index-from-mid)))
                   (slice-cx (+ cx x-bias))
                   (slice-cy (+ cy y-bias)))
                (if (and hue-start hue-end)
                    (multiple-value-bind (hr hg hb)
                            (hsv-to-rgb-u8 (lerp-f hue-start hue-end
                                                   (if hue-by-depth depth-t slice-t))
                                           hue-saturation
                                           (min 1.0 (+ hue-value-base
                                                       (* hue-value-range
                                                          (if hue-by-depth depth-t
                                                              (- 1.0 (max 0.0 z)))))))
                        (setf r (clamp-u8 (round (* hr energy))))
                        (setf g (clamp-u8 (round (* hg energy))))
                        (setf b (clamp-u8 (round (* hb energy)))))
                    (progn
                        (setf r (clamp-u8 (round (* brightness r-scale))))
                        (setf g (clamp-u8 (round (* brightness g-scale))))
                        (setf b (clamp-u8 (round (* brightness b-scale)))))
                    )
                (draw-filled-circle image slice-cx slice-cy radius r g b alpha)))))

(defun scene-sunsphere (&key (downsample-factor *default-downsample-factor*))
    (let* ((factor (normalize-downsample-factor downsample-factor))
           (height 1080)
           (width 1920)
           (render-height (* height factor))
           (render-width (* width factor))
           (image (make-blank-image render-height render-width))
           (cx (- (floor render-width 2) (round (* 0.07 render-width))))
           (cy (+ (floor render-height 2) (round (* 0.06 render-height))))
           (center-radius (round (* factor 160)))
           ; Meta-bias: tilt up-right and toward camera.
           (meta-tilt-x 0.70)
           (meta-tilt-y -0.55)
           (meta-camera 0.65)
           (meta-shift 0.28)
           (diag 0.70710677)
           (directions (list (list  1.0  0.0)
                             (list  diag  diag)
                             (list  0.0  1.0)
                             (list (- diag)  diag)
                             (list -1.0  0.0)
                             (list (- diag) (- diag))
                             (list  0.0 -1.0)
                             (list  diag (- diag))))
           (rings (list
                   (list :dist 2.75 :rad 0.18 :bright 214)
                   (list :dist 2.15 :rad 0.30 :bright 228)
                   (list :dist 1.45 :rad 0.50 :bright 238))))
        (dotimes (yy render-height)
            (dotimes (xx render-width)
                (let* ((ny (/ yy (float (max 1 (- render-height 1)))))
                       (bands 8)
                       (band-t (/ (floor (* ny bands)) (float (max 1 (1- bands)))))
                       (bg-r (clamp-u8 (round (lerp-f 2 78 band-t))))
                       (bg-g (clamp-u8 (round (lerp-f 5 130 band-t))))
                       (bg-b (clamp-u8 (round (lerp-f 20 225 band-t)))))
                    (set-pixel image xx yy bg-r bg-g bg-b))))
        (dolist (ring rings)
            (let ((dist-scale (getf ring :dist))
                  (rad-scale (getf ring :rad))
                  (bright (getf ring :bright)))
                (dolist (dir directions)
                    (let* ((dx (first dir))
                           (dy (second dir))
                           (dist (round (* center-radius dist-scale)))
                           (depth (+ (* dx meta-tilt-x) (* dy meta-tilt-y)))
                           (perspective-scale (+ 1.0 (* depth 0.33) (* meta-camera 0.12)))
                           (sx (+ cx
                                  (round (* dx dist))
                                  (round (* meta-tilt-x dist meta-shift))))
                           (sy (+ cy
                                  (round (* dy dist))
                                  (round (* meta-tilt-y dist meta-shift))))
                           (radius (max 6 (round (* center-radius rad-scale
                                                    (max 0.60 perspective-scale)))))
                           (sphere-bright (clamp-u8
                                           (+ bright
                                              (round (* 58 depth))
                                              (round (* 40 meta-camera)))))
                           (sphere-r-scale 1.00)
                           (sphere-g-scale (max 0.34 (min 0.62 (+ 0.46 (* 0.03 depth)))))
                           (sphere-b-scale (max 0.01 (min 0.10 (- 0.05 (* 0.02 depth)))))
                           (bias-x (+ (* 0.08 dx) (* 0.17 meta-tilt-x)))
                           (bias-y (+ (* -0.04 dy) (* 0.17 meta-tilt-y))))
                        (draw-stacked-sphere-tinted image sx sy radius
                                                    bias-x bias-y
                                                    :slices 8
                                                    :base-brightness sphere-bright
                                                    :r-scale sphere-r-scale
                                                    :g-scale sphere-g-scale
                                                    :b-scale sphere-b-scale
                                                    :alpha-mult 1.45
                                                    :hue-start (+ 0.01 (* -0.01 depth))
                                                    :hue-end (+ 0.14 (* -0.01 depth))
                                                    :hue-by-depth t
                                                    :draw-near-last t
                                                    :hue-saturation 0.99
                                                    :hue-value-base 0.80
                                                    :hue-value-range 0.22
                                                    :force-alpha 1.0)))))
        (draw-stacked-sphere-tinted image cx cy center-radius
                                    (+ 0.04 (* 0.10 meta-tilt-x))
                                    (+ -0.02 (* 0.10 meta-tilt-y))
                                    :slices 10
                                    :base-brightness 255
                                    :r-scale 1.00
                                    :g-scale 0.54
                                    :b-scale 0.02
                                    :alpha-mult 1.6
                                    :hue-start 0.01
                                    :hue-end 0.15
                                    :hue-by-depth t
                                    :draw-near-last t
                                    :hue-saturation 1.00
                                    :hue-value-base 0.82
                                    :hue-value-range 0.24
                                    :force-alpha 1.0)
        (write-png-file (output-path "sunsphere_1920x1080.png")
                        (downsample-image image factor))
        t))

(register-scene "sunsphere" #'scene-sunsphere
                :description "Orange center sun sphere with 3 rings in 8 directions")
