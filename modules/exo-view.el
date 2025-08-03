;;; exo-view.el --- viewers packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook ((pdf-tools-enabled . pdf-view-themed-minor-mode)
         (pdf-view-mode . pdf-view-auto-slice-minor-mode)
         (pdf-view-mode . (lambda () (cua-mode 0)))
         (pdf-view-mode . pdf-view-themed-minor-mode)
         )
  :bind ((:map pdf-view-mode-map
               ("C-f" . isearch-forward)))
  :config
  (setq pdf-view-resize-factor 1.05
        pdf-annot-activate-created-annotations t
        pdf-view-use-scaling t 
        pdf-view-use-imagemagick t)
  (setq-default pdf-view-display-size 2.0)
  (with-eval-after-load 'pdf-view
    (setq pdf-view-midnight-colors '("black" . "#FDF6E3"))
    )
  )

(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename  (concat user-emacs-directory ".pdf-view-restore"))
  )

(defface exo-nov-default-face
  ;;'((t (:family "Literata" :height 1.0 :weight light)))
  ;;'((t (:family "IBM Plex Serif" :height 1.1 :weight light)))
  ;;'((t (:family "Adobe Garamond Pro" :height 1.3 :weight light)))
  ;;'((t (:family "Crimson Pro" :height 1.2 :weight light)))
  ;;'((t (:family "Crimson" :height 1.2 :weight light)))
  ;;'((t (:family "Signika" :height 1.05 :weight light)))
  ;;'((t (:family "Source Serif Variable" :height 1.1 :weight regular)))
  '((t (:family "Cantarel" :height 1.0 :weight regular)))
  "Face for the nov.")

(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (fill-column (get-centre-width))
  ;;(nov-text-width (- fill-column 2))
  (nov-text-width t)
  (visual-fill-column-center-text t)
  :hook (
         (nov-mode . (lambda () (face-remap-add-relative 'variable-pitch 'exo-nov-default-face)))
         (nov-mode . visual-line-mode)
         (nov-mode . visual-fill-column-mode)
         )
  :config
  (require 'justify-kp)
  (defun my-nov-window-configuration-change-hook ()
    (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
                 'my-nov-window-configuration-change-hook
                 t))
  (defun my-nov-post-html-render-hook ()
    (if (get-buffer-window)
        (let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
                (goto-char (line-end-position))
                (when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook
                'my-nov-window-configuration-change-hook
                nil t))
    )
  (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)
  )

(setq doc-view-resolution 144)

(provide 'exo-view)

;;; exo-view.el ends here
