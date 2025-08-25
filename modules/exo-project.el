;;; exo-utility.el --- utility packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary: https://gist.github.com/redblobgames/eb3c36b008d12e8ab80ec3628dc8aa90
;;; Code:

(require 'project)
(require 'color)

(defun vterm-project-shell ()
  "Start an inferior vterm in the current project's root directory.
    If a buffer already exists for running a vterm in the project's root,
    switch to it. Otherwise, create a new vterm buffer.
    With \\[universal-argument] prefix arg, create a new inferior vterm buffer
    even if one already exists."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (shell-buffer-name (format "*vterm:%s*" (file-name-nondirectory (directory-file-name root)))))
    (if (and (not current-prefix-arg)
             (get-buffer shell-buffer-name))
        (switch-to-buffer shell-buffer-name)
      (let ((default-directory root))
        (vterm shell-buffer-name)))))

(advice-add 'project-shell :override #'vterm-project-shell)

(defun project-color-background ()
  "Return a background color for the project containing this directory"
  (let* ((project (project-current))
         (dirhash (sxhash (if project (project-root project) default-directory)))
         (hue (/ (mod dirhash 1000) 1000.0))
         (saturation (+ 0.3 (* 0.1 (mod (/ dirhash 1000) 3))))
         (lightness (+ 0.4 (* 0.05 (mod (/ (/ dirhash 1000) 3) 4))))
         (rgb (color-hsl-to-rgb hue saturation lightness)))
    (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 2)))

(defun exo/set-modeline-color ()
  "Set mode line color based on current buffer's project"
  (let ((color (project-color-background)))
    (face-remap-add-relative 'tab-line-tab-current :background color :foreground "white")
    (face-remap-add-relative 'mode-line-active :background color :foreground "black")
    ;;(face-remap-add-relative 'line-number-current-line :background color :foreground "black")
    ;;(face-remap-add-relative 'line-number-current-line :background 'default :foreground 'default)
    ))

(add-hook 'find-file-hook #'exo/set-modeline-color)
(add-hook 'dired-mode-hook #'exo/set-modeline-color)
(add-hook 'change-major-mode-hook #'exo/set-modeline-color)
(add-hook 'temp-buffer-setup-hook #'exo/set-modeline-color)

(provide 'exo-project)

;;; exo-project.el ends here
