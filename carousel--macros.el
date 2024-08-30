;;; carousel--macros.el -*- lexical-binding: t; -*-

(require 'ring)
(require 'persp-mode)
(require 'cl-lib)
(require 'evil)
(require 'dash)

(defalias 'carousel--get (symbol-function 'ring-ref))
(defalias 'carousel--length (symbol-function 'ring-length))

(defmacro with-carousel (&rest body)
  `(when (persp-parameter 'carousel)
     (let ((wr-persp      (get-current-persp))
           (wr-actual     (persp-parameter 'carousel-actual))
           (wr-grow       (persp-parameter 'carousel-grow))
           (wr-loop       (persp-parameter 'carousel-loop))
           (wr-duplicates (persp-parameter 'carousel-duplicates))
           (wr-focus      (persp-parameter 'carousel-focus))
           (wr-max        (persp-parameter 'carousel-max))
           (wr-start      (persp-parameter 'carousel-start))
           (wr-end        (persp-parameter 'carousel-end))
           )
       (unless (buffer-live-p wr-start) (get-buffer-create wr-start))
       (unless (buffer-live-p wr-end) (get-buffer-create wr-end))

       ,@body
       )
     )
  )

(defmacro with-other-carousel (persp &rest body)
  `(when (persp-parameter 'carousel persp)
     (let ((wr-persp      persp)
           (wr-actual     (persp-parameter 'carousel-actual persp))
           (wr-grow       (persp-parameter 'carousel-grow persp))
           (wr-loop       (persp-parameter 'carousel-loop persp))
           (wr-duplicates (persp-parameter 'carousel-duplicates persp))
           (wr-focus      (persp-parameter 'carousel-focus persp))
           (wr-max        (persp-parameter 'carousel-max persp))
           )
       ,@body
       )
     )
  )

(defmacro when-carousel (&rest body)
  `(when (and (persp-parameter 'carousel)
              (not (ring-empty-p (persp-parameter 'carousel-actual))))
     ,@body
     )
  )

(defmacro with-carousel-adding (&rest body)
  `(progn
     (setq carousel--adding t)
     ,@body
     (setq carousel--adding nil)
     )
  )

(provide 'carousel--macros)
