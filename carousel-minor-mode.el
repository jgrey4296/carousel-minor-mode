;;; carousel-minor-mode/carousel-minor-mode.el -*- lexical-binding: t; -*-
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows.html
;; Window Queue: Display N windows on screen, and have the be
;; views of adjacent buffers in a list
;; Be able to move to the most-recent, oldest, or along the list
;;

;; To expand to vertical:
;; Add to ring means add a ring to the ring, and add buffer there

;; Ring_prime [ Sub-Ring1[a b ] Sub-Ring2[c d ] Sub-Ring3[e f ]]

;; Control: Add to new sub-ring, or add to top of current sub-ring
;; Display: 3 columns, middle column divided in 3

;; Add-to-list most-recent/oldest

(eval-when-compile
  (require 'ring)
  (require 'persp-mode)
  (require 'cl-lib)
  (require 'evil)
  (require 'dash)
  (require 'macro-tools)
  )

(require 'carousel--macros)
(require 'carousel--util)
(require 'carousel--edit)
(require 'carousel--persp)
(require 'carousel--movement)
(require 'carousel--control)
(require 'carousel--windows)

;;-- vars

(defvar carousel--adding nil)

(defvar carousel-suppress-adding nil)

(defvar carousel-buffer-test-fn 'identity "one argument, current buffer, return non-nil to add to current ring")

(defvar carousel-column-fn #'carousel-setup-columns-default "sets up the window config, returning windows to claim")

(defvar carousel-name-suffix "-Carousel")

(defvar carousel-terminals '("-Carousel Start" "-Carousel End") "The names used to create the terminal buffers of the carousel")

(defvar carousel-focus-style 'newest "newest, oldest, balanced")

(defvar carousel-selector 'top "using window-at-side-list")

(defvar carousel-buffer-exclusions nil)

(defvar carousel-buffer--mode-exclusions (list 'dired-mode))

(defconst carousel-redisplay-activators '(carousel-add-to-head
                                          carousel-add-to-tail
                                          carousel-remove-buffer
                                          carousel-replace-buffer
                                          carousel-move-focus
                                          carousel-move-focus-alt
                                          carousel-move-buffer-left
                                          carousel-move-buffer-right
                                          carousel-goto-newest
                                          carousel-goto-oldest
                                          ))

(defvar carousel-create-hook nil)

(defvar carousel-pause-auto-redisplay nil)

;;-- end vars

;;-- mode

;;;###autoload
(define-minor-mode carousel-minor-mode
  "A Minor Mode for easy control of a 3-ple view of a ring of buffers"
  :lighter "Carousel"
  :global t
  :group 'carousel
  :keymap (make-sparse-keymap)
  (add-to-list 'persp-created-functions           #'carousel-create-persp-fn)
  (add-to-list 'persp-activated-functions         #'carousel-activate-persp-fn)
  (add-to-list 'persp-before-deactivate-functions #'carousel-deactivate-persp-fn)
  (add-to-list 'persp-before-kill-functions       #'carousel-kill-persp-fn)
  (add-hook 'find-file-hook                       #'carousel-add-current-buffer)
  (add-hook 'kill-buffer-hook                     #'carousel-remove-buffer)
  (add-hook 'kill-buffer-query-functions          #'carousel-protect-scratch-p -50)
  (message "Carousel Initalized")
  )

;;-- end mode

;;-- test predicates

(defun carousel-p (&optional arg)
  (interactive "p")
  (when (persp-parameter 'carousel)
    t)
  )

(defun carousel-empty-p (&optional arg)
  (interactive "p")
  (when (or (not (carousel-p)) (ring-empty-p (persp-parameter 'carousel-actual)))
    t
    )
  )

(defun carousel-buffer-p (&optional arg buffer)
  (interactive "p")
  (when (and (carousel-p)
             (ring-member (persp-parameter 'carousel-actual)
                          (or buffer (current-buffer))))
    t)
  )

(defun carousel-protect-scratch-p ()
  (not (with-carousel
           (or (eq (current-buffer) wr-start)
               (eq (current-buffer) wr-end))
         )
       )
  )

(defun carousel-window-claimed-p (wind)
  (window-parameter wind 'carousel-claimed)
  )

;;-- end test predicates

(provide 'carousel-minor-mode)
