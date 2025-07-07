;;; exo-ui.el --- ui packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; theme
(use-package zenburn-theme
  :config
  (defconst exo/darker-color   "gray16")
  (defconst exo/default-bg     "#35353A")
  (defconst exo/dark-color     "gray19")
  (defconst exo/dark2-color    "gray21")
  (defconst exo/darkless-color "gray24")
  (defconst exo/grayer-color   "gray31")
  (setq zenburn-override-colors-alist
        '(("zenburn-bg+05" . exo/darker-color)
          ("zenburn-bg"    . exo/default-bg)
          ("zenburn-bg+1"  . exo/dark-color)
          ("zenburn-bg+2"  . exo/darkless-color)
          ("zenburn-bg+3"  . exo/grayer-color)))
  (setq zenburn-use-variable-pitch t)
  (load-theme 'zenburn t)
  )

;; custom font
(set-frame-font "JetBrains Mono 11" nil t)
(set-face-attribute 'default nil :family "JetBrains Mono"
		            :height 120 :weight 'normal)
(set-face-attribute 'fixed-pitch nil
		            :family "JetBrains Mono"
		            :height 125 :weight 'normal)
(set-face-attribute 'variable-pitch nil
		            ;; :family "IBM Plex Serif" :height 1.4 :weight 'regular)
		            ;; :family "IBM Plex Sans" :height 1.2 :weight 'regular)
		            :family "Monaco Nerd Font" :height 1.1 :weight 'regular)
;; :family "Iosevka Aile" :height 1.3 :weight 'regular)
;; :family "Iosevka Etoile" :height 1.3 :weight 'regular)
;; :family "Literata" :height 1.4 :weight 'regular)
;; :family "Source Serif Pro" :height 1.4 :weight 'regular)
;; :family "Noto Serif" :height 1.4 :weight 'regular)
;; :family "Roboto Slab" :height 1.4 :weight 'regular)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-doc-face nil :slant 'italic)

;; Use specific Fontsets for Symbols
(setq use-default-font-for-symbols nil)

;; Use Symbols Nerd Font as Default Symbols Font, otherwise fall back to Symbola (or else)
(set-fontset-font t 'unicode "Symbols Nerd Font")
(set-fontset-font t '(#xF500 . #xF8FF) "Symbols Nerd Font")
(set-fontset-font t 'unicode "Symbola" nil 'append)
(set-fontset-font t 'unicode (font-spec :script 'unicode) nil 'append)

;; custom faces
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel
                ))
  (face-spec-reset-face face)
  ;; (set-face-foreground face (face-attribute 'default :background))
  (set-face-foreground face "gray15")
  )

(set-face-attribute 'region nil :background "#366060" :extend nil)
(set-face-attribute 'fill-column-indicator nil :foreground "#444444")
(set-face-background 'fringe (face-attribute 'default :background))
;; (set-face-background 'fringe "gray15")

(set-face-background 'isearch "#5F6Fa2")
(set-face-background 'lazy-highlight "#5F5F5f")
(set-face-background 'line-number exo/dark-color)

(setopt window-divider-default-right-width 1)
(setopt window-divider-default-bottom-width 0)

;; make flat header-line
(set-face-attribute 'header-line nil :box nil)
(set-face-attribute 'header-line-inactive nil :background "#383838" :box nil)

;; make flat tab-bar
(set-face-attribute 'tab-bar nil :box nil :background exo/darker-color)
(set-face-attribute 'tab-bar-tab nil :box nil :background exo/default-bg :foreground "lightgreen")
(set-face-attribute 'tab-bar-tab-inactive nil :box nil :background "gray14")
(set-face-attribute 'tab-bar-tab-highlight nil :box nil :background "gray")

;; margins
(add-hook 'after-init-hook #'(lambda ()
                               (modify-all-frames-parameters
                                '((right-divider-width . 10)
                                  (internal-border-width . 10)))))

;; icons
(use-package nerd-icons
  :if (display-graphic-p)
  :custom (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :commands (nerd-icons-dired-mode)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; rainbow-mode
(use-package rainbow-mode
  :defer 1
  :hook (emacs-lisp-mode text-mode lisp-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :defer 1
  :custom
  (rainbow-delimiters-max-face-count 5)
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; shrfaces
(use-package shrface
  :after shr
  :config
  (shrface-basic)
  (shrface-trial)
  (setq shrface-href-versatile t)

  ;; eww support
  (with-eval-after-load 'eww
    (add-hook 'eww-after-render-hook 'shrface-mode)
    (define-key eww-mode-map (kbd "<tab>") 'org-cycle)
    (define-key eww-mode-map (kbd "S-<tab>") 'org-shifttab)
    (define-key eww-mode-map (kbd "C-j") 'outline-next-visible-heading)
    (define-key eww-mode-map (kbd "C-k") 'outline-previous-visible-heading))

  ;; nov support
  (with-eval-after-load 'nov
    (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))) ; reset nov-shr-rendering-functions, in case of the list get bigger and bigger
    (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
    (add-hook 'nov-mode-hook 'shrface-mode)
    (add-hook 'nov-post-html-render-hook 'shrface-mode)
    ;; (define-key nov-mode-map (kbd "<tab>") 'org-cycle)
    (define-key nov-mode-map (kbd "S-<tab>") 'org-shifttab)
    (define-key nov-mode-map (kbd "C-j") 'outline-next-visible-heading)
    (define-key nov-mode-map (kbd "C-k") 'outline-previous-visible-heading))

  ;; mu4e support
  (with-eval-after-load 'mu4e
    (add-hook 'mu4e-view-mode-hook 'shrface-mode))
  )

(provide 'exo-ui)

;;; exo-ui.el ends here
