;;; exo-extras.el --- Extra functions -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun dump-face-attributes (face &rest props)
  "Dump selected FACE attribute of PROPS."
  (cl-loop for prop in (or props
                           (sort (mapcar #'car face-attribute-name-alist)
                                 (lambda (s1 s2)
                                   (string< (symbol-name s1) (symbol-name s2)))))
           for val = (face-attribute face prop nil t)
           unless (eq val 'unspecified)
           append (list prop val)))

(defun get-face-attribute (face prop)
  "Dump all FACE attributes of PROP."
  (plist-get (dump-face-attributes face prop) prop))

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun get-centre-width ()
  "Return a fill column that makes centring pleasant regardless of screen size."
  (let ((window-width (window-width)))
    (floor (if (<= window-width (* 1.1 fill-column))
               (* 0.9 window-width)
             (max (/ window-width 2) fill-column)))))

(defun my/root-project-dir ()
  "Return with the project's dir or current dir or default dir."
  (if (vc-root-dir)
      (vc-root-dir)
    (if buffer-file-name
        (file-name-directory (buffer-file-name))
      default-directory)))

(provide 'exo-extras)

;;; exo-extras.el ends here
