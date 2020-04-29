;;; pix.el --- Pixel based drawing library for Emacs

;; Created: 26 Apr 2020
;; Keywords: pixel, bitmap, raster graphics, drawing, paint
;; Homepage: http://github.com/neuro-sys/emacs-pix

;;;; Commentary:
;; This package provides a set of functions to create XPM images to
;; display bitmap graphics inside Emacs.

;;;; TODO:
;; - Add other graphics primitives: Circle, rectangle with filled options

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

(defsubst pix-width (pix) (aref pix 0))
(defsubst pix-height (pix) (aref pix 1))
(defsubst pix-colors (pix) (aref pix 2))
(defsubst pix-scale-factor (pix) (aref pix 3))
(defsubst pix--palette-size (pix) (aref pix 4))
(defsubst pix--xpm-val-len (pix) (aref pix 5))
(defsubst pix--buffer-stride (pix) (aref pix 6))
(defsubst pix--xpm-pixel-fmt (pix) (aref pix 7))
(defsubst pix--buffer (pix) (aref pix 8))
(defsubst pix--data-offset (pix) (aref pix 9))
(defsubst pix--set-buffer (pix newelt) (aset pix 8 newelt))
(defsubst pix--set-data-offset (pix newelt) (aset pix 9 newelt))
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

(defun pix-create-xpm-data (pix)
  (let* ((header-data (format pix--xpm-header-string-format
                              (pix-width pix)
                              (pix-height pix)
                              (pix--palette-size pix)
                              (pix--xpm-val-len pix)))
         (colors-data (pix--create-xpm-color-data (pix-colors pix) (pix--xpm-pixel-fmt pix)))
         (image-data (pix--create-xpm-image-data
                      (pix-width pix)
                      (pix-height pix)
                      (pix--xpm-val-len pix)))
         (header-color-data (concat header-data "\n" colors-data)))
    (pix--set-data-offset pix (1+ (length header-color-data)))
    (concat header-color-data "\n" image-data "};\n")))

(defun pix-propertize (pix)
  (propertize " "
              'display
              (create-image (pix--buffer pix) 'xpm t :scale (pix-scale-factor pix))))

(defun pix-insert (pix)
  (insert
   (pix-propertize pix)))

(defun pix-fill-rectangle (pix x y width height ink)
  (let* ((x-stride (pix--xpm-val-len pix))
         (y-stride (pix--buffer-stride pix))
         (offset (+ (pix--data-offset pix)               ; offset within string buffer
                    (* y-stride y)
                    (* x-stride x)
                    1))
         (buffer (pix--buffer pix))                       ; string buffer
         (ink-char (format (pix--xpm-pixel-fmt pix) ink)) ; ink value to insert as formatted
         (ink-char-len (length ink-char))
         (yo-end (+ offset (* (pix--buffer-stride pix) height))))
    (let ((yo offset))
      (while (< yo yo-end)
        (let ((xo yo)
              (xo-end (+ yo width)))
          (while (< xo xo-end)
            (let ((i 0))
              (while (< i ink-char-len)
                (aset buffer xo (aref ink-char 0))
                (setq i (1+ i))))
            (setq xo (+ x-stride xo))))
        (setq yo (+ y-stride yo))))))
        
(defsubst pix-put-pixel (pix x y ink)
  (let* ((offset (+ (pix--data-offset pix)
                    (* (pix--buffer-stride pix) y)
                    (* x (pix--xpm-val-len pix))
                    1)))
    (aset (pix--buffer pix) offset (aref (format (pix--xpm-pixel-fmt pix) ink) 0))))

(defun pix--draw-line-low (pix x0 y0 x1 y1 ink)
  (let* ((dx (- x1 x0))
         (dy1 (- y1 y0))
         (yi (if (< dy1 0) -1 1))
         (dy (if (< dy1 0) (- dy1) dy1))
         (e (- (lsh dy 1) dx))
         (y y0)
         (x x0))
    (while (< x x1)
      (pix-put-pixel pix x y ink)
      (if (> e 0)
          (progn
            (setq y (+ y yi))
            (setq e (- e (lsh dx 1)))))
      (setq e (+ e (lsh dy 1)))
      (setq x (1+ x)))))

(defun pix--draw-line-high (pix x0 y0 x1 y1 ink)
  (let* ((dx1 (- x1 x0))
         (dy (- y1 y0))
         (xi (if (< dx1 0) -1 1))
         (dx (if (< dx1 0) (- dx1) dx1))
         (e (- (lsh dx 1) dy))
         (x x0)
         (y y0))
    (while (< y y1)
      (pix-put-pixel pix x y ink)
      (if (> e 0)
          (progn
            (setq x (+ x xi))
            (setq e (- e (lsh dy 1)))))
      (setq e (+ e (lsh dx 1)))
      (setq y (1+ y)))))

(defun pix-draw-line (pix x0 y0 x1 y1 ink)
  (if (< (abs (- y1 y0)) (abs (- x1 x0)))
      (if (> x0 x1)
          (pix--draw-line-low pix x1 y1 x0 y0 ink)
        (pix--draw-line-low pix x0 y0 x1 y1 ink))
    (if (> y0 y1)
        (pix--draw-line-high pix x1 y1 x0 y0 ink)
      (pix--draw-line-high pix x0 y0 x1 y1 ink))))

(defun pix-init (width height colors scale-factor)
  (let ((pix (make-pix width height colors scale-factor)))
    (pix--set-buffer pix (pix-create-xpm-data pix))
    pix))

(provide 'pix)
