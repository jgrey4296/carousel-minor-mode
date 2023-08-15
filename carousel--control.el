;;; carousel--control.el -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'carousel--macros)
  (require 'ring)
  (require 'persp-mode)
  (require 'cl-lib)
  (require 'evil)
  (require 'dash)
  )

(defun carousel-clear-ring (&optional arg)
  (interactive "p")
  (with-carousel
      (message "Clearing Window Ring")
    (modify-persp-parameters `((carousel-actual . ,(make-ring 1))
                               (carousel-focus . 0)
                               ) wr-persp)
    )
  (carousel-add-current-buffer arg)
  )

(defun carousel-pop-buffers (&optional arg)
  (interactive "p")
  (with-carousel
      ;;Pop everything from most-recent to here
      (let ((most-recent (-partial #'carousel--get wr-actual 0))
            (target (carousel--get wr-actual wr-focus))
            )
        (while (and (ring-length wr-actual)
                    (not (eq (funcall most-recent) target)))
          (ring-remove wr-actual 0))
        )
    (ring-resize wr-actual (ring-length wr-actual))
    )
  )

(defun carousel-remove-buffer (&optional buffer arg)
  " Remove the current buffer from the ring "
  (interactive "b\nP")
  (with-carousel
      (let* ((buff (if (bufferp buffer) buffer (current-buffer)))
             (index (unless (ring-empty-p wr-actual)
                      (ring-member wr-actual buff)))
             )
        (when index
          (ring-remove wr-actual index)
          (ring-resize wr-actual (ring-length wr-actual))
          )
        )
    )
  )

(defun carousel-replace-buffer(&optional buffer arg)
  " Replace the current focus buffer with the current buffer "
  (interactive "b\np")
  ;; TODO use with-other-carousel
  (with-carousel
      (let* ((buff (if (bufferp buffer) buffer (current-buffer)))
             (empty-p (ring-empty-p wr-actual))
             (index (unless empty-p (ring-member wr-actual buff)))
             (centre (unless empty-p (carousel--get wr-actual wr-focus)))
             (ring-list (unless empty-p (ring-elements wr-actual)))
             )
        (when index
          (setf (nth (cl-position centre ring-list) ring-list) buff)
          (modify-persp-parameters `((carousel-actual .
                                      ,(ring-convert-sequence-to-ring ring-list)))
                                   )
          )
        )
    )
  )

(defun carousel-add-current-buffer (&optional arg)
  (interactive "P")
  (when (and (persp-parameter 'carousel)
             (or (buffer-local-boundp 'carousel-buffer (current-buffer))
                 (funcall carousel-buffer-test-fn (current-buffer)))
             (not carousel-suppress-adding)
             )
    (carousel-add-to-head (current-buffer) t)
    )
  )

(defun carousel-add-to-head (buffer &optional arg)
  (interactive "b\nP")
  (with-carousel
      (-when-let (buff (get-buffer buffer))
        (message "Adding buffer to window ring: %s" buff)
        (ring-insert+extend wr-actual buff t)
        )
    )
  )

(defun carousel-add-to-tail (buffer &optional arg)
  (interactive "b\np")
  (with-carousel
      (-when-let (buff (get-buffer buffer))
        (ring-insert-at-beginning wr-actual buff))
    )
  )

(defun carousel-shrink-sides (amt)
  (interactive "NShrink By: ")
  (with-carousel
      (let ((curr (selected-window)))
        (walk-windows #'(lambda (wind)
                          (when (not (eq wind curr))
                            (with-selected-window wind
                              (shrink-window-horizontally amt))))
                      )))
  )

(provide 'carousel--control)
