;;; carousel--persp.el -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'carousel--macros)
  (require 'ring)
  (require 'persp-mode)
  (require 'cl-lib)
  (require 'evil)
  (require 'dash)
  )

(defun carousel-new ()
  " create a new perspective and ring "
  (interactive)
  (message "Creating new window ring")
  (let ((ring-name (format "%s%s" (read-string "New Ring: ") carousel-name-suffix))
        (curr (current-buffer))
        )
    (with-carousel-adding
     (persp-add-new ring-name)
     (persp-switch ring-name)
     (add-hook 'find-file-hook              #'carousel-add-current-buffer)
     (add-hook 'kill-buffer-hook            #'carousel-remove-buffer)
     (add-hook 'kill-buffer-query-functions #'carousel-protect-scratch-p -50)
     (switch-to-buffer (persp-parameter 'carousel-start))
     (carousel-add-to-head curr)
     (carousel-reset-columns t)
     )
    )
  )

(defun carousel-convert ()
  " Convert a perspective to a window ring"
  (interactive)
  (unless (persp-parameter 'carousel (get-current-persp))
    (message "Converting to window ring")
    (with-carousel-adding
     (carousel-create-persp-fn (get-current-persp) nil)
     (add-hook 'find-file-hook              #'carousel-add-current-buffer)
     (add-hook 'kill-buffer-hook            #'carousel-remove-buffer)
     (add-hook 'kill-buffer-query-functions #'carousel-protect-scratch-p -50)
     (persp-rename (format "%s%s" (persp-name (get-current-persp)) carousel-name-suffix))
     (carousel-advice-for-redisplay)
     (carousel-add-to-head (current-buffer))
     (carousel-reset-columns t)
     (message "Converted")
     (carousel-print-order)
     )
    )
  )

(defun carousel-toggle ()
  (interactive)
  (pcase (persp-parameter 'carousel)
    ('nil (carousel-convert))
    ('paused
     (set-persp-parameter 'carousel t)
     (add-hook 'find-file-hook              #'carousel-add-current-buffer)
     (add-hook 'kill-buffer-hook            #'carousel-remove-buffer)
     (add-hook 'kill-buffer-query-functions #'carousel-protect-scratch-p -50)
     (carousel-advice-for-redisplay)
     (carousel-print-order)
     )
    (t
     (set-persp-parameter 'carousel 'paused)
     (remove-hook 'find-file-hook              #'carousel-add-current-buffer)
     (remove-hook 'kill-buffer-hook            #'carousel-remove-buffer)
     (remove-hook 'kill-buffer-query-functions #'carousel-protect-scratch-p)
     (carousel-remove-redisplay-advice)
     )
    )
  )

(defun carousel-deconvert ()
  (interactive)
  (when (persp-parameter 'carousel (get-current-persp))
     (remove-hook 'find-file-hook              #'carousel-add-current-buffer)
     (remove-hook 'kill-buffer-hook            #'carousel-remove-buffer)
     (remove-hook 'kill-buffer-query-functions #'carousel-protect-scratch-p)
     (mapc #'delete-persp-parameter
             '(carousel carousel-actual carousel-grow carousel-loop carousel-duplicates
               carousel-focus carousel-max carousel-backgrounds carousel-start carousel-end))
     (persp-delete-other-windows)
     (persp-rename (string-replace carousel-name-suffix "" (persp-name (get-current-persp))))
     (carousel-remove-redisplay-advice)
     )
  )

(defun carousel-create-persp-fn (persp hash)
  (message "Initializing window ring %s" carousel--adding)
  (when carousel--adding
    (modify-persp-parameters `((carousel . t)
                               (carousel-actual . ,(make-ring 1))
                               (carousel-grow . t)
                               (carousel-loop . nil)
                               (carousel-duplicates . t)
                               (carousel-focus . 0)
                               (carousel-max . -1)
                               (carousel-backgrounds . ("gray19" "gray12" "gray4"))
                               (carousel-start . (format "*%s%s*" (safe-persp-name persp) (car carousel-terminals)))
                               (carousel-end   . (format "*%s%s*" (safe-persp-name persp) (cadr carousel-terminals)))
                               )
                             persp
                             )
    (with-current-buffer (get-buffer-create (persp-parameter 'carousel-end persp))
      (when (string-empty-p (buffer-substring (point-min) (point-max)))
        (insert "Carousel Scratch End:\n")
        )
      )
    (with-current-buffer (get-buffer-create (persp-parameter 'carousel-start persp))
      (when (string-empty-p (buffer-substring (point-min) (point-max)))
        (insert "Carousel Scratch Start:\n")
        )
      )
    (run-hooks 'carousel-create-hook)
    ;; (ring-insert+extend (persp-parameter 'carousel-actual persp) (persp-parameter 'carousel-start  persp) t)
    )
  )

(defun carousel-activate-persp-fn (type)
  (when (persp-parameter 'carousel)
    (cond ('frame)
          ('window)
          )
    )
  )

(defun carousel-deactivate-persp-fn (type)
  (when (persp-parameter 'carousel)
    (cond ('frame)
          ('window)
          )
    )
  )

(defun carousel-advice-for-redisplay ()
  (dolist (fn carousel-redisplay-activators)
    (advice-add fn :after #'carousel-redisplay)
    )
  )

(defun carousel-remove-redisplay-advice ()
  (dolist (fn carousel-redisplay-activators)
    (advice-remove fn #'carousel-redisplay)
    )
  )

(provide 'carousel--persp)
