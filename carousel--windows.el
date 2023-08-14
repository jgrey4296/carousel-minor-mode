;;; carousel--windows.el -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'carousel--macros)
  (require 'ring)
  (require 'persp-mode)
  (require 'cl-lib)
  (require 'evil)
  (require 'dash)
  )

(defun carousel-reset-columns (&optional arg)
  (interactive "p")
  (carousel-setup-columns arg t)
  )

(defun carousel-setup-columns (&optional arg soft)
  " Reset windows using `carousel-column-fn`
    if SOFT then don't clear the window ring "
  (interactive "pi")
  (persp-delete-other-windows)
  (mapc (-rpartial #'carousel-claim-window t) (funcall carousel-column-fn arg soft))

  (unless soft
    ;; clear ring
    (modify-persp-parameters `((carousel-actual . ,(make-ring 1))))
    (carousel-add-current-buffer)
    )

  (when arg
    (carousel-redisplay))
  )

(defun carousel-setup-columns-default (&optional arg soft)
  ;; (arg == 1 -> one row) (else -> two rows, only use top)
  ;; Clear
  (let ((leftmost (selected-window))
        centre rightmost)
    ;; split
    (when (and (numberp arg) (< 1 arg)) ;; Split as top 3 columns
      (split-window-below))

    ;; Split as 3 columns
    (setq centre (split-window-right))
    (select-window centre)
    (setq rightmost (split-window-right))

    (when (and arg evil-auto-balance-windows)
      (balance-windows))

    (list leftmost centre rightmost)
    )
  )

(defun carousel-setup-vertical (&optional arg soft)
  (let ((top (selected-window))
        middle
        bottom)
    (setq middle (split-window-below))
    (select-window middle)
    (setq bottom (split-window-below))

    (when (and arg evil-auto-balance-windows)
      (balance-windows))
    (list top middle bottom)
    )
  )

(defun carousel-kill-persp-fn (persp)
  (with-other-carousel persp
      (message "Killing Window Ring")
    )
  )

(defun carousel-claim-window (&optional wind force)
  "Claim a window as one suitable for showing ring buffers. returns the window"
  (interactive)
  (let ((wind (or wind (selected-window))))
    (when-carousel
        (set-window-parameter wind 'carousel-claimed
                              (if force t
                                (not (window-parameter wind 'carousel-claimed))))
      )
    wind
    )
  )

(defun carousel-redisplay ()
  (interactive)
  (when-carousel
      (pcase carousel-focus-style
        ('balanced
         (carousel--redisplay-balanced))
        ('newest
        (carousel--redisplay-newest))
        ('oldest
         (carousel--redisplay-oldest))
        (_ (message "Unknown redisplay style"))
        )
    )
  )

(defun carousel--redisplay-balanced ()
  (with-carousel
      (let* ((largest  (carousel--rough-mid (window-at-side-list nil carousel-selector)))
             (ring-len (ring-length wr-actual))
             (curr-win (window-next-sibling largest))
             (index    (carousel--newer ring-len wr-focus wr-loop))
             )
        (message "Windows: Largest: %s (%s) curr: %s (%s)" largest (window-live-p largest) curr-win (window-live-p curr-win))
        (message "carousel-state: (%s) %s" ring-len wr-actual)
        (set-window-buffer largest (carousel--get wr-actual wr-focus))
        (while curr-win
          (setq index (if (carousel-set-window curr-win index) (carousel--newer ring-len index wr-loop) index)
                curr-win (window-next-sibling curr-win))
        )
        (setq index (carousel--older ring-len wr-focus wr-loop)
              curr-win (window-prev-sibling largest))
        (while curr-win
          (setq index (if (carousel-set-window curr-win index) (carousel--older ring-len index wr-loop) index)
                curr-win (window-prev-sibling curr-win))
          )
        )
    )
  )

(defun carousel--redisplay-newest ()
  (with-carousel
      (let ((windows (reverse (window-at-side-list nil carousel-selector)))
            (ring-len (ring-length wr-actual))
            (focus wr-focus)
            )
        (while windows
          (if (carousel-set-window (pop windows) focus)
              (setq focus (carousel--older ring-len focus wr-loop)))
          )
        )
    )
  )

(defun carousel--redisplay-oldest ()
  (with-carousel
      (let ((windows (window-at-side-list nil carousel-selector))
            (ring-len (ring-length wr-actual))
            (focus wr-focus)
            )
        (while windows
          (if (carousel-set-window (pop windows) focus)
              (setq focus (carousel--newer ring-len focus wr-loop)))
          )
        )
    )
  )

(defun carousel-set-window (window index)
  (with-carousel
      (unless (window-live-p window) (select-window window))
    (set-window-buffer window (if index (carousel--get wr-actual index) wr-scratch))
    (when (and index (fboundp #'solaire-mode))
      (with-current-buffer (window-buffer window)
        (solaire-mode (if (zerop (mod index 2)) 1 -1))
        )
      )
    (if (window-parameter window 'carousel-claimed)
        (message "Unclaimed Window: %s" window)
      t
      )
    )
  )

(provide 'carousel--windows)
