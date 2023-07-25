;;; carousel--edit.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'carousel--macros)
  (require 'ring)
  (require 'persp-mode)
  (require 'cl-lib)
  (require 'evil)
  (require 'dash)
  )

(defun carousel-edit-order ()
  (interactive)
  (with-carousel
      (let ((buffers (ring-elements wr-actual))
            (edit-buffer (get-buffer-create (format "*Carousel Buffers: %s*" (persp-parameter 'name))))
            )
        (with-current-buffer edit-buffer
          (auto-save-mode -1)
          (carousel-edit-minor-mode 1)
          (set (make-local-variable 'backup-inhibited) t)
          (erase-buffer)
          (mapc (lambda (x) (insert (format "%s\n" (buffer-name x))))
                (reverse buffers))
          )
        (display-buffer edit-buffer)
        )
    )
  )

(defun carousel-edit-commit ()
  (interactive)
  (with-carousel
      (let ((order (s-split "\n" (buffer-substring-no-properties
                                  (point-min) (point-max)) t)))
        (modify-persp-parameters `((carousel-actual . ,(make-ring 1))
                                   (carousel-focus . 0))
                                 )
        (cl-loop for name in order
                 when (get-buffer (string-trim name))
                 do
                 (carousel-add-to-head (get-buffer (string-trim name)))
                 )
        (kill-buffer-and-window)
        )
    )
  (carousel-redisplay)
  )

(setq carousel-edit-map (make-sparse-keymap))

(define-key carousel-edit-map (kbd "C-c C-c") #'carousel-edit-commit)

(define-minor-mode carousel-edit-minor-mode
  " A Minor mode to commit changes to the order of window ring buffers "
  :lighter "Carousel-Edit"
  :keymap carousel-edit-map
  )

(provide 'carousel--edit)
