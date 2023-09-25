;;; carousel--movement.el -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'carousel--macros)
  (require 'ring)
  (require 'persp-mode)
  (require 'cl-lib)
  (require 'evil)
  (require 'dash)
  )

(defun carousel-toggle-loop ()
  (interactive)
  (with-carousel
      (modify-persp-parameters `((carousel-loop . ,(not wr-loop))))
    )
  )

(defun carousel-move-focus (&optional arg)
  " move the focus towards the most recent addition to window ring.
 if arg is not nil, move towards oldest "
  (interactive "P")
  (cond ((persp-parameter 'carousel)
         (with-carousel
          (let* ((mv-fn (if (or (null arg) (< arg 2)) #'carousel--newer #'carousel--older))
                 (new-focus (funcall mv-fn (ring-length wr-actual) wr-focus wr-loop))
                 )
            (message "Current Focus: %s -> %s : %s %s" wr-focus new-focus mv-fn arg)
            (when new-focus
              (modify-persp-parameters `((carousel-focus . ,new-focus)))
              )
            )
          )
         )
        ((or (null arg) (< arg 2))
         (evil-window-right 1))
        (t
         (evil-window-left 1))
        )
  )

(defun carousel-move-focus-alt ()
  (interactive)
  (carousel-move-focus 2)
  )

(defun carousel-goto-newest (&optional arg)
  (interactive "p")
  (with-carousel
      (modify-persp-parameters '((carousel-focus . 0)))
    )
  )

(defun carousel-goto-oldest (&optional arg)
  (interactive "p")
  (with-carousel
      (modify-persp-parameters `((carousel-focus . ,(1- (ring-length wr-actual)))))
    )
  )

(defun carousel-move-buffer-right (&optional arg)
  (interactive "p")
  (with-carousel
      (let* ((curr-focus (ring-member wr-actual (current-buffer)))
             (curr (carousel--get wr-actual curr-focus))
             (next (carousel--get wr-actual (carousel--newer (ring-length wr-actual) curr-focus wr-loop)))
             (as-seq (ring-elements wr-actual))
             new-ring
            )
        (setq new-ring (ring-convert-sequence-to-ring (cl-loop for val in as-seq
                                                               collect (cond ((eq val curr)
                                                                              next)
                                                                             ((eq val next)
                                                                              curr)
                                                                             (t val))
                                                               )))
      (modify-persp-parameters `((carousel-actual . ,new-ring)))
      (modify-persp-parameters `((carousel-focus . ,(ring-member new-ring curr))))
      )
    (select-window (get-buffer-window (current-buffer)))
    )
  (carousel-print-order)
  )

(defun carousel-move-buffer-left (&optional arg)
  (interactive "p")
  (with-carousel
      (let* ((curr-focus (ring-member wr-actual (current-buffer)))
             (curr (carousel--get wr-actual curr-focus))
             (next (carousel--get wr-actual (carousel--older (ring-length wr-actual) curr-focus wr-loop)))
             (as-seq (ring-elements wr-actual))
             new-ring
            )
        (setq new-ring (ring-convert-sequence-to-ring (cl-loop for val in as-seq
                                                               collect (cond ((eq val curr)
                                                                              next)
                                                                             ((eq val next)
                                                                              curr)
                                                                             (t val))
                                                               )))
      (modify-persp-parameters `((carousel-actual . ,new-ring)))
      (modify-persp-parameters `((carousel-focus . ,(ring-member new-ring curr))))
      )
    (select-window (get-buffer-window (current-buffer)))
    )
  (carousel-print-order)
  )

(defun carousel-goto-choice (&optional buffer)
  (interactive "b")
  (with-carousel
   (let ((new-focus (ring-member wr-actual (get-buffer buffer))))
     (message "Carousel New focus: %s %s" buffer new-focus)
     (when new-focus
       (modify-persp-parameters `((carousel-focus . ,new-focus)))
       (carousel-redisplay)
       )
     )
   )
  )

(provide 'carousel--movement)
