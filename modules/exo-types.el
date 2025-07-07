;;; exo-types.el --- Additional file types -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  ;;(setq markdown-command "pandoc")
  )

(use-package markdown-preview-mode
  :init
  (setq  markdown-preview-auto-open (quote file))
  )

(provide 'exo-types)

;;; exo-types.el ends here
