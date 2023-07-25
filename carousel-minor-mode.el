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

(require 'ring)
(require 'persp-mode)
(require 'cl-lib)
(require 'evil)
(require 'dash)

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

(defvar carousel-focus-style 'newest "newest, oldest, balanced")

(defvar carousel-selector 'top "using window-at-side-list")

;;-- end vars

;;-- mode

;;;###autoload
(define-minor-mode carousel-minor-mode
  "A Minor Mode for easy control of a 3-ple view of a ring of buffers"
  :lighter "Carousel"
  :global t
  :group 'carousel
  (add-to-list 'persp-created-functions           #'carousel-create-persp-fn)
  (add-to-list 'persp-activated-functions         #'carousel-activate-persp-fn)
  (add-to-list 'persp-before-deactivate-functions #'carousel-deactivate-persp-fn)
  (add-to-list 'persp-before-kill-functions       #'carousel-kill-persp-fn)
  (add-hook 'find-file-hook                       #'carousel-add-current-buffer)
  (add-hook 'kill-buffer-hook                     #'carousel-remove-buffer)
  (add-hook 'kill-buffer-query-functions          #'carousel-protect-scratch-p -50)
  )

;;-- end mode

;;-- test predicates

(defun carousel-p (&optional arg)
  (interactive "p")
  (when (persp-parameter 'carousel)
    t)
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
           (eq (current-buffer) wr-scratch)
         )
       )
  )

(defun carousel-window-claimed-p (wind)
  (window-parameter wind 'carousel-claimed)
  )

;;-- end test predicates


(provide 'carousel-minor-mode)
