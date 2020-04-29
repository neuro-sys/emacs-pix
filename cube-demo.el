;;; cube-demo.el --- Use pix.el to draw some animation

;; Created: 29 Apr 2020
;; Keywords: pixel, bitmap, raster graphics, drawing, paint
;; Homepage: http://github.com/neuro-sys/emacs-pix

(add-to-list 'load-path default-directory)
(require 'pix)

(defvar cube-r 0.0)

(defun make-p (x y z) (vector x y z))
(defun p-x (p) (aref p 0))
(defun p-y (p) (aref p 1))
(defun p-z (p) (aref p 2))

(defsubst clamp (a min-a max-a) (max (min a max-a) min-a))

(defun draw-rectangle (pix p0 p1 p2 p3 ink)
  (pix-draw-line pix
                 (truncate (p-x p0))
                 (truncate (p-y p0))
                 (truncate (p-x p1))
                 (truncate (p-y p1)) ink)
  (pix-draw-line pix
                 (truncate (p-x p1))
                 (truncate (p-y p1))
                 (truncate (p-x p2))
                 (truncate (p-y p2)) ink)
  (pix-draw-line pix
                 (truncate (p-x p2))
                 (truncate (p-y p2))
                 (truncate (p-x p3))
                 (truncate (p-y p3)) ink)
  (pix-draw-line pix
                 (truncate (p-x p3))
                 (truncate (p-y p3))
                 (truncate (p-x p0))
                 (truncate (p-y p0)) ink))

(defun p-rot-x (p r)
  (make-p (p-x p)
          (- (* (p-y p) (cos r))
             (* (p-z p) (sin r)))
          (+ (* (p-y p) (sin r))
             (* (p-z p) (cos r)))))

(defun p-rot-y (p r)
  (make-p (+ (* (p-z p) (sin r))
             (* (p-x p) (cos r)))
          (p-y p)
          (- (* (p-z p) (cos r))
             (* (p-x p) (sin r)))))

(defun p-rot-z (p r)
  (make-p (- (* (p-x p) (cos r))
             (* (p-y p) (sin r)))
          (+ (* (p-x p) (sin r))
             (* (p-y p) (cos r)))
          (p-z p)))

(defun p-trans (p0 p1)
  (make-p (+ (p-x p0) (p-x p1))
          (+ (p-y p0) (p-y p1))
          (+ (p-z p0) (p-z p1))))

(defun p-proj (p width height)
  (let* ((d (/ width 2.0)))
    (make-p (clamp (+ (/ width 2.0)
                      (/ (* d (p-x p)) (p-z p)))
                   0 (1- width))
            (clamp (+ (/ height 2.0)
                      (/ (* d (p-y p)) (p-z p)))
                   0 (1- height))
            0.0)))

(defun draw-rectangle-with-perspective (pix quad ink)
  (let* ((d (/ (pix-width pix) 2.0))
         (width (pix-width pix))
         (height (pix-height pix))
         (p0 (p-proj (aref quad 0) width height))
         (p1 (p-proj (aref quad 1) width height))
         (p2 (p-proj (aref quad 2) width height))
         (p3 (p-proj (aref quad 3) width height)))
    (draw-rectangle pix p0 p1 p2 p3 ink)))

(defun draw-cube (pix)
  (let* ((trans-v (make-p 0.0 0.0 -2.5))
         (vertices (list
                    (vector (make-p -0.5 0.5 -0.5)
                            (make-p -0.5 -0.5 -0.5)
                            (make-p 0.5 -0.5 -0.5)
                            (make-p 0.5 0.5 -0.5))

                    (vector (make-p -0.5 0.5 0.5)
                            (make-p -0.5 -0.5 0.5)
                            (make-p 0.5 -0.5 0.5)
                            (make-p 0.5 0.5 0.5))

                    (vector (make-p 0.5 0.5 -0.5)
                            (make-p 0.5 -0.5 -0.5)
                           (make-p 0.5 -0.5 0.5)
                            (make-p 0.5 0.5 0.5))

                    (vector (make-p -0.5 0.5 -0.5)
                            (make-p -0.5 -0.5 -0.5)
                            (make-p -0.5 -0.5 0.5)
                            (make-p -0.5 0.5 0.5))

                    (vector (make-p -0.5 0.5 0.5)
                            (make-p -0.5 0.5 -0.5)
                            (make-p 0.5 0.5 -0.5)
                            (make-p 0.5 0.5 0.5))

                    (vector (make-p -0.5 -0.5 0.5)
                            (make-p -0.5 -0.5 -0.5)
                            (make-p 0.5 -0.5 -0.5)
                            (make-p 0.5 -0.5 0.5))
                    )))
    (dolist (quad vertices)
      (dotimes (i (length quad))
        (let* ((p (aref quad i))
               (p-0 (p-rot-z (p-rot-y (p-rot-x p cube-r) cube-r) cube-r))
               (p-1 (p-trans p-0 trans-v)))
          (aset quad i p-1)))
      (draw-rectangle-with-perspective pix quad 1))
  (pix-insert pix)))

(defun cube-run ()
  (interactive)
  (defconst *width* 160)
  (defconst *height* 120)
  (defconst *pix* (pix-init *width* *height* (list '(#x00 #x00 #x00) '(#xFF #xFF #xFF)) 4.0))
  (defvar cube-timer nil)
  (if (timerp cube-timer) (cancel-timer cube-timer))
  (setq cube-timer (run-at-time nil 0.05
                                (lambda ()
                                  (with-current-buffer (get-buffer-create "cube")
                                    (erase-buffer)
                                    (pix-fill-rectangle *pix* 0 0 *width* *height* 0)
                                    (draw-cube *pix*)
                                    (setq cube-r (+ cube-r 0.1))
                                    (deactivate-mark)))))
  (add-hook 'kill-buffer-hook (lambda () (cancel-timer cube-timer)))
  (display-buffer (get-buffer-create "cube")))

(cube-run)
