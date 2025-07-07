;;; exo-help.el --- Help functions -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
  "If SYM is a function, append its docstring."
  (concat
   (apply orig-fun sym r)
   (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
          (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
     (and oneline
          (stringp oneline)
          (not (string= "" oneline))
          (concat " " (propertize oneline 'face '(:slant italic :foreground "#9FC59F")))
          ))))

(use-package elisp-demos
  :vc (:url "https://github.com/xuchunyang/elisp-demos"
            :branch "master"))

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(defun showdoc (f)
  "Show function F documentation."
  (interactive (list (thing-at-point 'symbol t)))
  (message "%s: %s" f (documentation (intern f))))

(defun exo-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer."
  (interactive)
  (setq buffer-face-mode-face '(:family "Monaco Nerd Font" :height 130 :width regular))
  (buffer-face-mode))

(dolist (mode '(Info-mode-hook
                help-mode-hook
                helpful-mode-hook))
  (add-hook mode #'(lambda () (visual-line-mode)))
  (add-hook mode 'exo-buffer-face-mode-variable)  
  ;; (add-hook mode #'(lambda () (if truncate-lines
  ;;                            (toggle-truncate-lines -1))))
  )

(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-macro
             helpful-function
             helpful-command)
  :bind
  (
   ([remap describe-symbol]   . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command]  . helpful-command)
   ([remap describe-key]      . helpful-key)
   ("C-h f"   . helpful-callable)
   ("C-h F"   . helpful-function)
   ("C-h C-." . helpful-at-point)
   )
  )

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  :bind ("C-h E"  . eldoc)
  )

;;TODO test usability
(use-package eldoc-box
  :after eldoc
  ;;:hook (eglot-managed-mode . eldoc-box-hover-mode)
  )

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup)
  :hook
  (devdocs-mode . visual-line-mode))

(dolist (docs '((python-mode-hook     . ("python~3.13"))
                (java-mode-hook       . ("spring-boot" "openjdk-21"))
                (emacs-lisp-mode-hook . ("elisp"))))
  (add-hook (car docs) (lambda () (setq-local devdocs-current-docs (cdr docs))))
  )

(add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function)

(use-package rfc-mode
  :defer 1
  :config
  (setq rfc-mode-directory "/usr/share/doc/rfc/txt"))

(use-package which-key
  :ensure nil
  :init
  (setq which-key-add-column-padding 1
        which-key-dont-use-unicode nil)
  :config
  (which-key-mode 1))

(use-package keycast
  :init
  (setq keycast-mode-line-insert-after 'mode-line-misc-info
        keycast-mode-line-window-predicate 'moody-window-active-p
        keycast-mode-line-remove-tail-elements nil
        keycast-separator-width 1
        keycast-mode-line-format "%10s%k%c%R")
  :config
  (dolist (input '(self-insert-command  org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "..." "Typing…")))
  (set-face-attribute 'keycast-key nil :background "#2b2b2b" :foreground "#B2D5B2"
                      :inherit 'fixed-pitch :box nil )     ;;'(:line-width 1 :style released-button))
  (keycast-tab-bar-mode 1)
  ;;(keycast-mode-line-mode 1)
  )

(with-eval-after-load 'transient
  (transient-define-prefix exo/help-menu-transient ()
    "Help Menu"
    [["Documentation search"
      ("d" "Devdocs lookup" devdocs-lookup :transient nil)
      ("e" "Eldoc" eldoc :transient nil)
      ("r" "Rfc browse" rfc-mode-browse :transient nil)
      ]
     ["Describe"
      ("u" "Describe face" describe-face :transient nil)
      ("l" "Describe char" describe-char :transient nil)
      ("k" "Describe key" helpful-key :transient nil)
      ("s" "Describe symbol" helpful-symbol :transient nil)
      ("f" "Describe function" helpful-function :transient nil)
      ("c" "Describe callable" helpful-callable :transient nil)
      ("m" "Describe command" helpful-command :transient nil)
      ("v" "Describe variable" helpful-variable :transient nil)
      ]
     ["Utility"
      ("t" "Wrap lines" toggle-truncate-lines :transient nil)
      ]
     ])

  (global-set-key [C-f1] 'exo/help-menu-transient)
  )

(provide 'exo-help)

;;; exo-help.el ends here
