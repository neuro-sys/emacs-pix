(add-to-list 'load-path default-directory)
(require 'pix)

(let ((pix-buf (pix-init 320 200 (list '(#x00 #x33 #x66) '(#x00 #xFF #x00)) 2.0)))
  (pix-fill-rectangle pix-buf 30 30 50 50 1)
  (pix-insert pix-buf))
