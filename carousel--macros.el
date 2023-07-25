;;; carousel--macros.el -*- lexical-binding: t; -*-

(require 'ring)
(require 'persp-mode)
(require 'cl-lib)
(require 'evil)
(require 'dash)

(defalias 'carousel--get (symbol-function 'ring-ref))
(defalias 'carousel--length (symbol-function 'ring-length))

(defmacro with-carousel (&rest body)
  (declare (indent 1))
  `(when (persp-parameter 'carousel)
     (let ((wr-persp      (get-current-persp))
           (wr-actual     (persp-parameter 'carousel-actual))
           (wr-grow       (persp-parameter 'carousel-grow))
           (wr-loop       (persp-parameter 'carousel-loop))
           (wr-duplicates (persp-parameter 'carousel-duplicates))
           (wr-focus      (persp-parameter 'carousel-focus))
           (wr-max        (persp-parameter 'carousel-max))
           (wr-scratch    (persp-parameter 'carousel-scratch))
           )
       ,@body
       )
     )
  )

(defmacro with-other-carousel (persp &rest body)
  (declare (indent 1))
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
  (declare (indent 1))
  `(when (persp-parameter 'carousel)
     ,@body
     )
  )

(defmacro with-carousel-adding (&rest body)
  `(let ((carousel--adding t))
     ,@body
     )
  )

(provide 'carousel--macros)
