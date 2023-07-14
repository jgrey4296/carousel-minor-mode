;;; carousel--persp.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'carousel--macros))

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
     (switch-to-buffer (persp-parameter 'carousel-scratch))
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
     (carousel-add-to-head (current-buffer))
     (carousel-reset-columns t)
     (message "Converted")
     )
    )
  )

(defun carousel-deconvert ()
  (interactive)
  (when (persp-parameter 'carousel (get-current-persp))
     (remove-hook 'find-file-hook              #'carousel-add-current-buffer)
     (remove-hook 'kill-buffer-hook            #'carousel-remove-buffer)
     (remove-hook 'kill-buffer-query-functions #'carousel-protect-scratch-p -50)
     (mapc #'delete-persp-parameter
             '(carousel carousel-actual carousel-grow carousel-loop carousel-duplicates
               carousel-focus carousel-max carousel-backgrounds carousel-scratch))
     (persp-delete-other-windows)
     (persp-rename (string-replace carousel-name-suffix "" (persp-name (get-current-persp))))
     )
  )

(defun carousel-create-persp-fn (persp hash)
  (message "Initializing window ring %s" carousel--adding)
  (when carousel--adding
    (modify-persp-parameters `((carousel . t)
                               (carousel-actual . ,(make-ring 1))
                               (carousel-grow . t)
                               (carousel-loop . t)
                               (carousel-duplicates . t)
                               (carousel-focus . 0)
                               (carousel-max . -1)
                               (carousel-backgrounds . ("gray19" "gray12" "gray4"))
                               (carousel-scratch . ,(get-buffer-create (format "*%s*" (safe-persp-name persp))))
                               )
                             persp
                             )
    (ring-insert+extend (persp-parameter 'carousel-actual persp)
                        (persp-parameter 'carousel-scratch persp))
    )
  )

(defun carousel-activate-persp-fn (type)
  (when (persp-parameter 'carousel)
    (cond ('frame)
          ('window

           )
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

(provide 'carousel--persp)
