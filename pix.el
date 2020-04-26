;;; pix.el --- Pixel based drawing library for Emacs

;; Created: 26 Apr 2020
;; Keywords: pixel, bitmap, raster graphics, drawing, paint
;; Homepage: http://github.com/neuro-sys/emacs-pix

;;;; Commentary:
;; This package provides a set of functions to create XPM images to
;; display bitmap graphics inside Emacs.

;;;; TODO:
;; - Add other graphics primitives: Circle, rectangle with filled options
;; - Demo for a rotating cube

(defun make-pix (width height colors scale-factor)
  "Creates an instance of pix session for a drawable canvas"
  (let* ((palette-size (pix--get-palette-size colors))
         (xpm-val-len (pix--get-xpm-val-len palette-size))
         (buffer-stride (pix--get-buffer-stride xpm-val-len width))
         (xpm-pixel-fmt (pix--get-pixel-fmt xpm-val-len))
         (buffer nil)
         (data-offset nil))
  (vector width
          height
          colors
          scale-factor
          palette-size
          xpm-val-len
          buffer-stride
          xpm-pixel-fmt
          buffer
          data-offset
          )))

(defsubst pix-width (item) (aref item 0))
(defsubst pix-height (item) (aref item 1))
(defsubst pix-colors (item) (aref item 2))
(defsubst pix-scale-factor (item) (aref item 3))
(defsubst pix--palette-size (item) (aref item 4))
(defsubst pix--xpm-val-len (item) (aref item 5))
(defsubst pix--buffer-stride (item) (aref item 6))
(defsubst pix--xpm-pixel-fmt (item) (aref item 7))
(defsubst pix--buffer (item) (aref item 8))
(defsubst pix--data-offset (item) (aref item 9))
(defsubst pix--set-buffer (item newelt) (aset item 8 newelt))
(defsubst pix--set-data-offset (item newelt) (aset item 9 newelt))
(defun pix--get-palette-size (colors) (length colors))
(defun pix--get-xpm-val-len (palette-size) (length (format "%x" palette-size)))
(defun pix--get-buffer-stride (xpm-val-len width) (+ 4 (* xpm-val-len width)))
(defun pix--get-pixel-fmt (xpm-val-len) (format "%%.%dx" xpm-val-len))

(defconst pix--xpm-header-string-format "/* XPM */\n\
static char * test_xpm[] = {\n\
\"%d %d %d %d\",\
")

(defun pix--create-xpm-color-data (colors xpm-pixel-fmt)
  "Return RGB colors in the format XPM expects"
  (let ((i -1)
        (buffer))
    (dolist (c colors)
      (setq i (1+ i))
      (let* ((col (format xpm-pixel-fmt i))
             (line (format "\"%s c #%.2X%.2X%.2X\"," col (elt c 0) (elt c 1) (elt c 2))))
        (setq buffer (concat buffer line "\n"))))
    buffer))

(defun pix--create-xpm-image-data (width height xpm-val-len)
  (let ((buf))
    (dotimes (i height)
      (setq buf (concat buf "\"" (make-string (* xpm-val-len width) ?0) "\",\n")))
    buf))

(defun pix-create-xpm-data (item)
  (let* ((header-data (format pix--xpm-header-string-format
                              (pix-width item)
                              (pix-height item)
                              (pix--palette-size item)
                              (pix--xpm-val-len item)))
         (colors-data (pix--create-xpm-color-data (pix-colors item) (pix--xpm-pixel-fmt item)))
         (image-data (pix--create-xpm-image-data
                      (pix-width item)
                      (pix-height item)
                      (pix--xpm-val-len item)))
         (header-color-data (concat header-data "\n" colors-data)))
    (pix--set-data-offset item (1+ (length header-color-data)))
    (concat header-color-data "\n" image-data "};\n")))

(defun pix-propertize (item)
  (propertize " "
              'display
              (create-image (pix--buffer item) 'xpm t :scale (pix-scale-factor item))))

(defun pix-insert (item)
  (insert
   (pix-propertize item)))

(defsubst pix-put-pixel (item x y ink)
  (let* ((offset (+ (pix--data-offset item)
                    (* (pix--buffer-stride item) y)
                    (* x (pix--xpm-val-len item))
                    1)))
    (store-substring (pix--buffer item) offset (format (pix--xpm-pixel-fmt item) ink))))

(defun pix--draw-line-low (item x0 y0 x1 y1 ink)
  (let* ((dx (- x1 x0))
         (dy1 (- y1 y0))
         (yi (if (< dy1 0) -1 1))
         (dy (if (< dy1 0) (- dy1) dy1))
         (e (- (lsh dy 1) dx))
         (y y0)
         (x x0))
    (while (< x x1)
      (pix-put-pixel item x y ink)
      (if (> e 0)
          (progn
            (setq y (+ y yi))
            (setq e (- e (lsh dx 1)))))
      (setq e (+ e (lsh dy 1)))
      (setq x (1+ x)))))

(defun pix--draw-line-high (item x0 y0 x1 y1 ink)
  (let* ((dx1 (- x1 x0))
         (dy (- y1 y0))
         (xi (if (< dx1 0) -1 1))
         (dx (if (< dx1 0) (- dx1) dx1))
         (e (- (lsh dx 1) dy))
         (x x0)
         (y y0))
    (while (< y y1)
      (pix-put-pixel item x y ink)
      (if (> e 0)
          (progn
            (setq x (+ x xi))
            (setq e (- e (lsh dy 1)))))
      (setq e (+ e (lsh dx 1)))
      (setq y (1+ y)))))

(defun pix-draw-line (item x0 y0 x1 y1 ink)
  (if (< (abs (- y1 y0)) (abs (- x1 x0)))
      (if (> x0 x1)
          (pix--draw-line-low item x1 y1 x0 y0 ink)
        (pix--draw-line-low item x0 y0 x1 y1 ink))
    (if (> y0 y1)
        (pix--draw-line-high item x1 y1 x0 y0 ink)
      (pix--draw-line-high item x0 y0 x1 y1 ink))))

(defun pix-init (width height colors scale-factor)
  (let ((item (make-pix width height colors scale-factor)))
    (pix--set-buffer item (pix-create-xpm-data item))
    item))

(provide 'pix)

;;;; Example with rotating 3D wireframe cube

(defun make-p (x y z) (vector x y z))
(defun p-x (p) (aref p 0))
(defun p-y (p) (aref p 1))
(defun p-z (p) (aref p 2))

(defsubst clamp (a min-a max-a) (max (min a max-a) min-a))

(defun draw-rectangle (item p0 p1 p2 p3 ink)
  (pix-draw-line item
                 (truncate (p-x p0))
                 (truncate (p-y p0))
                 (truncate (p-x p1))
                 (truncate (p-y p1)) ink)
  (pix-draw-line item
                 (truncate (p-x p1))
                 (truncate (p-y p1))
                 (truncate (p-x p2))
                 (truncate (p-y p2)) ink)
  (pix-draw-line item
                 (truncate (p-x p2))
                 (truncate (p-y p2))
                 (truncate (p-x p3))
                 (truncate (p-y p3)) ink)
  (pix-draw-line item
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

(defun draw-rectangle-with-perspective (item quad ink)
  (let* ((d (/ (pix-width item) 2.0))
         (width (pix-width item))
         (height (pix-height item))
         (p0 (p-proj (aref quad 0) width height))
         (p1 (p-proj (aref quad 1) width height))
         (p2 (p-proj (aref quad 2) width height))
         (p3 (p-proj (aref quad 3) width height)))
    (draw-rectangle item p0 p1 p2 p3 ink)))

(defun draw-cube ()
  (let* ((item (pix-init 320 200 (list '(#x00 #x00 #x00) '(#xFF #xFF #xFF)) 2.0))
         (trans-v (make-p 0.0 0.0 -2.5))
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
    (let ((r 0.5))
      (dolist (quad vertices)
        (dotimes (i (length quad))
          (let* ((p (aref quad i))
                 (p-0 (p-rot-z (p-rot-y (p-rot-x p r) r) r))
                 (p-1 (p-trans p-0 trans-v)))
            (aset quad i p-1)))
        (draw-rectangle-with-perspective item quad 1)))
  (pix-insert item)))

(draw-cube)
