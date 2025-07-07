;;; exo-utility.el --- utility packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package bind-key
  :init
  (bind-keys*
   ("C-c C-/"  . comment-line)
   ("C-c C-?"  . comment-dwim)
   ("C-a"      . mark-whole-buffer)
   ("C-+"      . text-scale-increase)
   ("C--"      . text-scale-decrease))
  (bind-keys :map prog-mode-map
             ("C-<return>" . eval-last-sexp))
  (unbind-key "C-t")
  )

(use-package try
  :defer t)

(use-package undo-fu
  :bind (
         ("C-z"   . undo-fu-only-undo)
         ("C-/"   . undo-fu-only-undo)
         ("C-M-/" . undo-fu-only-redo)
         ("C-c u" . undo-fu-only-redo-all)))

(use-package persistent-scratch
  :commands persistent-scratch-setup-default
  :init (persistent-scratch-setup-default))

(use-package crux
  :defer 1
  :bind (("C-d"           . crux-duplicate-current-line-or-region)
         ("C-<backspace>" . crux-kill-whole-line)
         ("<home>"        . crux-move-beginning-of-line)
         ("C-<tab>"       . crux-other-window-or-switch-buffer)
         ))

(use-package move-text
  :defer 1
  :bind
  (("M-S-<up>"   . move-text-up)
   ("M-S-<down>" . move-text-down)
   ))

(use-package ultra-scroll
  ;;https://github.com/jdtsmith/ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(use-package hungry-delete
  :defer 1
  :hook
  (after-init . global-hungry-delete-mode)
  :config
  (setq hungry-delete-join-reluctantly t)
  )

(use-package expand-region
  :defer 1
  :bind
  (("C-=" . er/expand-region)))

(use-package outline-indent
  :defer 1
  :custom
  (outline-indent-ellipsis " ▼ "))

(use-package indent-bars
  :defer 1
  :custom
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-width-frac 0.1)
  ;; (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;                                     list list_comprehension
  ;;                                     dictionary dictionary_comprehension
  ;;                                     parenthesized_expression subscript)))
  )

(use-package olivetti
  :preface
  (setq exo/default-olivetti-borders-color exo/dark2-color)
  (setq exo/default-olivetti-body-width 120)
  (setq olivetti-body-width exo/default-olivetti-body-width)
  (setq olivetti-style t)
  :custom
  (olivetti-enable-visual-line-mode nil)
  :custom-face
  (olivetti-fringe ((t (:background ,exo/default-olivetti-borders-color))))
  :hook
  (
   (eww-mode     . olivetti-mode)
   (Info-mode    . (lambda ()
                     (setq-local olivetti-body-width 80)
                     (olivetti-mode)))
   (ibuffer-mode . (lambda ()
                     (setq-local olivetti-body-width 160)
                     (olivetti-mode)
                     )
                 )
   )
  )

(use-package hl-line
  :ensure nil
  :hook
  (dired-mode          . hl-line-mode)
  (org-agenda-mode     . hl-line-mode)
  (ibuffer-mode        . hl-line-mode)
  (tabulated-list-mode . hl-line-mode)
  :config (global-hl-line-mode -1))

(use-package hl-todo
  :defer 1
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(
          ("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

(use-package auto-highlight-symbol
  :defer 1
  :hook (prog-mode . auto-highlight-symbol-mode)
  :custom-face
  (ahs-plugin-default-face ((t (:background ,exo/grayer-color :foreground "gold"))))
  (ahs-plugin-default-face-unfocused ((t (:background ,exo/grayer-color :foreground "gold" :weight light))))
  (ahs-face-unfocused ((t (:background ,exo/darkless-color :foreground "gold"))))
  (ahs-face ((t (:background "gray27" :foreground "gold" :weight bold))))
  )

;; web wouser

(use-package shr
  :ensure nil
  :defer t
  :config
  (setq shr-use-fonts t
        shr-max-image-proportion 0.8
        shr-width 120
        shr-max-width 120
        shr-discard-aria-hidden t
        shr-fill-text nil
        shr-cookie-policy nil))

(use-package url-cookie
  :ensure nil
  :defer t
  :config
  (setq url-cookie-untrusted-urls '(".*")))

(use-package eww
  :ensure nil
  :hook
  (eww-mode . iscroll-mode)
  :config
  (setq eww-desktop-remove-duplicates nil)
  (setq eww-header-line-format nil)
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  (setq eww-download-directory (expand-file-name "~/Downloads"))
  ;;(set-face-attribute 'shr-text nil :family "Adobe Garamond Pro" :height 1.2 :weight 'regular) ;; it rewrites nov face
  ;;:custom
  ;;(browse-url-browser-function 'eww-browse-url)
  ;;(browse-url-browser-function 'browse-url-qutebrowser)
  )

(use-package browse-url
  :ensure nil
  :config
  ;;(setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "qutebrowser")
  (setq browse-url-qutebrowser-new-window-is-tab t)
  )

(use-package calendar
  :ensure nil
  :custom
  (calendar-week-start-day 1))

(use-package time
  :ensure nil
  :init
  (require 'calendar)
  :bind
  (:map calendar-mode-map
        ("M-«" . calendar-beginning-of-year)
        ("M-»" . calendar-end-of-year)
        ("«" . calendar-backward-month)
        ("»" . calendar-forward-month)
        ("(" . calendar-backward-week)
        (")" . calendar-forward-week)
        ("b" . calendar-backward-day)
        ("f" . calendar-forward-day)
        ("{" . calendar-backward-month)
        ("}" . calendar-forward-month)
        ("a" . calendar-beginning-of-week)
        ("e" . calendar-end-of-week))
  :config
  (setq display-time-day-and-date t ;; display date and time
        display-time-24hr-format t  ;; 24h time format
        european-calendar-style t   ;; day/month/year format for calendar
        display-time-string-forms '((if (and (not display-time-format)
                                             display-time-day-and-date)
                                        (format-time-string "%H:%M" now)))
        display-time-default-load-average nil
        )
  )

(use-package kurecolor)

(use-package posframe
  :custom
  (posframe-mouse-banish nil))

(use-package 0x0
  :defer t
  )

(use-package mpv
  :defer t)

(use-package hide-mode-line
  :defer t)

(use-package smartrep
  :config
  (smartrep-define-key
   global-map "M-g"
   '(("n" . next-line)
     ("p" . previous-line)))
  (smartrep-define-key
   global-map "C-x"
   '(("{" . shrink-window-horizontally)
     ("}" . enlarge-window-horizontally)))
  (smartrep-define-key 
      org-mode-map "C-c" '(("C-n" . (outline-next-visible-heading 1))
                           ("C-p" . (outline-previous-visible-heading 1))))
  )

(use-package show-font
  :defer t
  :bind
  (("C-c s f" . show-font-select-preview)
   ("C-c s t" . show-font-tabulated))
  :custom
  (show-font-pangram 'fox))

(provide 'exo-utility)

;;; exo-utility.el ends here
