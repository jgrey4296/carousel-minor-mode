;;; carousel--movement.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'carousel--macros))

(defun carousel-toggle-loop ()
  (interactive)
  (with-carousel
      (modify-persp-parameters `((carousel-loop . ,(not wr-loop))))
    )
  )

(defun carousel-move-focus (&optional arg)
  " move the focus towards the most recent addition to window ring.
 if arg is not nil, move towards oldest "
  (interactive "p")
  (cond ((persp-parameter 'carousel)
         (let* ((wr-persp      (get-current-persp))
                (wr-actual     (persp-parameter 'carousel-actual))
                (wr-grow       (persp-parameter 'carousel-grow))
                (wr-loop       (persp-parameter 'carousel-loop))
                (wr-duplicates (persp-parameter 'carousel-duplicates))
                (curr-focus    (ring-member wr-actual (current-buffer)))
                (wr-focus      (persp-parameter 'carousel-focus))
                (wr-max        (persp-parameter 'carousel-max))
                (wr-scratch    (persp-parameter 'carousel-scratch))
                (new-focus (if (< 1 arg)
                               (carousel--older (ring-length wr-actual) wr-focus wr-loop)
                            (carousel--newer (ring-length wr-actual) wr-focus wr-loop)))
                )
           (when new-focus
             (modify-persp-parameters `((carousel-focus . ,new-focus)))
             )
           )
         (carousel-redisplay)
         (carousel-print-order)
         )
        ((< 1 arg)
         (evil-window-left 1))
        (t
         (evil-window-right 1))
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
    (when arg (carousel-redisplay))
    )
  )

(defun carousel-goto-oldest (&optional arg)
  (interactive "p")
  (with-carousel
      (modify-persp-parameters `((carousel-focus . ,(1- (ring-length wr-actual)))))
    (when arg (carousel-redisplay))
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
    (when arg (carousel-redisplay))
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
    (when arg (carousel-redisplay))
    (select-window (get-buffer-window (current-buffer)))
    )
  (carousel-print-order)
  )

(provide 'carousel--movement)
