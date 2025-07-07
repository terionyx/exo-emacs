;;; exo-vc.el --- Version control -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :bind ("M-g s" . magit-status)
  :custom
  (magit-repository-directories '(("~/Samples" . 1)))
  (magit-git-executable "/usr/bin/git")
  (magit-diff-refine-hunk t)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (with-eval-after-load 'magit
    (setq magit-format-file-function #'magit-format-file-nerd-icons))
  :custom-face
  (magit-log-author ((t (:foreground "#d04b4e"))))
  (magit-log-date ((t (:foreground "#f28735"))))
  (magit-hash ((t (:foreground "#25c192"))))
  (magit-filename ((t (:foreground "#49a6d0"))))
  (magit-branch-current ((t (:foreground "#f74e8b"))))
  :hook
  (
   (magit-status-mode-hook . hide-mode-line-mode)
   (magit-log-mode-hook . hide-mode-line-mode)
   (magit-mode-hook . garbage-collect)
   (magit-log-mode-hook . garbage-collect)
   (magit-status-mode-hook  . garbage-collect)
   (magit-popup-mode-hook  . garbage-collect)
   )
  )

(use-package git-timemachine
  :defer t
  :after magit
  :bind ("M-g t" . git-timemachine-toggle))

(use-package git-gutter
  :bind (("C-x g"     . git-gutter)
         ("C-c g ="   . git-gutter:popup-hunk)
         ("C-c g p"   . git-gutter:previous-hunk)
         ("C-c g n"   . git-gutter:next-hunk)
         ("C-c g s"   . git-gutter:stage-hunk)
         ("C-c g r"   . git-gutter:revert-hunk)
         ("C-c g SPC" . git-gutter:mark-hunk)
         )
  :hook ((text-mode . git-gutter-mode)
         (org-mode  . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :custom
  (git-gutter:modified-sign "✶")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "✗")
  :init
  (setq git-gutter:update-interval 0.2
        git-gutter:window-width    1
        git-gutter:ask-p           nil
        git-gutter:hide-gutter     t
        )
  :config
  (global-set-key (kbd "<left-margin> M-<mouse-1>") 'git-gutter:popup-hunk)
  (set-face-attribute 'git-gutter:added nil :background "#19FF44" :foreground "exo/default-bg")
  (set-face-attribute 'git-gutter:modified nil :background "#f3bf4f" :foreground "exo/default-bg")
  (set-face-attribute 'git-gutter:deleted nil :background "#FF443b" :foreground "exo/default-bg")
  )

(provide 'exo-vc)

;;; exo-vc.el ends here
