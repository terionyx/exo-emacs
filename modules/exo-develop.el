;;; exo-develop.el --- development packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure nil
  :commands (eglot
             eglot-rename
             eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :bind (("M-TAB" . completion-at-point)
         ("M-g i" . consult-imenu)
         ("C-h ." . display-local-help)
         ("M-."   . xref-find-definitions)
         ("M-,"   . xref-go-back)
         :map eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-actions-organize-imports)
         ("C-c c r" . eglot-rename)
         ("C-c c f" . eglot-format))
  :preface
  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point t)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend t))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend nil t))))
  :hook
  (((clojure-mode
     clojurec-mode
     clojurescript-mode
     java-mode
     scala-mode
     racket-mode
     zig-mode) . eglot-ensure)
   ((cider-mode eglot-managed-mode) . eglot-disable-in-cider)
   )
  :config
  ;; Optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)

  ;; (defvar complete-at-point--timer nil "Timer for triggering complete-at-point.")
  ;; (defun auto-complete-at-point (&rest _)
  ;;   "Set a time to complete the current symbol at point in 0.1 seconds"
  ;;   (when (and (not (minibufferp)))
  ;;     ;; If a user inserts a character while a timer is active, reset
  ;;     ;; the current timer
  ;;     (when (timerp complete-at-point--timer)
  ;;       (cancel-timer complete-at-point--timer))
  ;;     (setq complete-at-point--timer
  ;;           (run-at-time 0.2 nil
  ;;                        (lambda ()
  ;;                          ;; Clear out the timer and run
  ;;                          ;; completion-at-point
  ;;                          (when (timerp complete-at-point--timer)
  ;;                            (cancel-timer complete-at-point--timer))
  ;;                          (setq complete-at-point--timer nil)
  ;;                          (completion-at-point))))))
  ;; ;; Add a hook to enable auto-complete-at-point when eglot is enabled
  ;; ;; this allows use to remove the hook on 'post-self-insert-hook if
  ;; ;; eglot is disabled in the current buffer
  ;; (add-hook 'eglot-managed-mode-hook
  ;;           (lambda ()
  ;;             (if eglot--managed-mode
  ;;                 (add-hook 'post-self-insert-hook #'auto-complete-at-point nil t)
  ;;               (remove-hook 'post-self-insert-hook #'auto-complete-at-point t))))
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'flymake)
  (setq completion-category-defaults nil)
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-sync-connect nil
        eglot-ignored-server-capabilities
        '(:hoverProvider
          :documentHighlightProvider
          :documentFormattingProvider
          :documentRangeFormattingProvider
          :documentOnTypeFormattingProvider
          :colorProvider
          :foldingRangeProvider)
        ;; eglot-send-changes-idle-time 0
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-report-progress nil       ; Prevent minibuffer spam
        eglot-extend-to-xref t
        eglot-code-action-indications '(margin)
        eglot-report-progress t
        eglot-code-action-indications nil
        )
  )

(setq completion-category-overrides '((eglot (styles orderless))
                                      (eglot-capf (styles orderless))))

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

;;; https://github.com/jdtsmith/eglot-booster
(use-package eglot-booster
  :after eglot
  :config
  (setq eglot-booster-no-remote-boost t)
  (setq eglot-booster-io-only t)
  (eglot-booster-mode))

(use-package flycheck
  :hook
  ;;(lisp-data-mode . (lambda () (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)))
  (prog-mode . flycheck-mode)
  :init
  (setq-default
   flycheck-disabled-checkers
   '(emacs-lisp-checkdoc
     javascript-jshint
     haskell-stack-ghc
     haskell-ghc
     haskell-hlint))
  (setq
   flycheck-highlighting-mode 'lines
   flycheck-indication-mode 'left-fringe
   flycheck-mode-line-prefix "fly"
   flycheck-javascript-eslint-executable "eslint_d")
  :config
  (add-to-list
   'display-buffer-alist
   `(,(rx bos "*errors*" eos)
     (side . bottom)
     (reusable-frames . visible)
     (window-height . 0.33)))
  )

(use-package flycheck-indicator
  :hook (flycheck-mode . flycheck-indicator-mode))

(use-package yasnippet
  :defer nil
  :custom
  (yas-indent-line nil)
  (yas-inhibit-overlay-modification-protection t)
  :custom-face
  (yas-field-highlight-face ((t (:inherit region))))
  :bind*
  (:map yas-minor-mode-map
        ("C-j" . yas-expand)
        ("TAB" . nil)
        ("<tab>" . nil)
        :map yas-keymap
        ("TAB" . (lambda () (interactive) (company-abort) (yas-next-field)))
        ("<tab>" . (lambda () (interactive) (company-abort) (yas-next-field))))
  :hook
  (snippet-mode . (lambda () (setq-local require-final-newline nil)))
  :config
  (yas-global-mode))

(use-package yasnippet-snippets)

(use-package jarchive
  :after eglot
  :hook
  (java-mode . jarchive-mode))

;;; Geiser

(use-package geiser
  :defer 1
  :hook (
         (scheme-mode . geiser-mode)
         (racket-mode . geiser-mode)
         )
  :config
  (setq geiser-mode-start-repl-p nil
        geiser-default-implementation 'racket
        geiser-repl-history-filename (expand-file-name "geiser_history" user-emacs-directory)
        geiser-active-implementations '(guile racket mit gambit chicken chez gauche)
        geiser-autodoc-identifier-format "%s :: %s"
        geiser-repl-skip-version-check-p t)
  :custom-face
  (geiser-font-lock-autodoc-identifier  ((t (:foreground "orange" :weight bold))))
  (geiser-font-lock-autodoc-current-arg  ((t (:foreground "gold" :weight regular))))
  (geiser-font-lock-repl-prompt  ((t (:family "Terminess Nerd Font" :height 140 :foreground "violet" :weight normal))))
  )

(use-package geiser-racket
  :config
  (setq geiser-racket--prompt-regexp "<pkgs>.*> \\|\\(mzscheme\\|racket\\)@[^ ]*> ")
  :bind
  ((:map geiser-repl-mode-map
         ("C-<f12>" . switch-to-racket)))
  )
(use-package geiser-guile)
(use-package geiser-gambit)
(use-package geiser-mit)
(use-package geiser-chicken)
(use-package geiser-chez)
(use-package geiser-gauche)

(use-package racket-mode
  :mode("\\.rkt?\\'" . racket-mode)
  :hook
  (
   (racket-describe-mode . hide-mode-line-mode))
  :config
  ;; (setq racket-browse-url-function 'browse-url-chromium)
  (setq racket-browse-url-function 'eww-browse-url)
  (setq racket-documentation-search-location 'local)
  (transient-define-prefix exo/racket-transient ()
    "Racket Menu"
    [["Racket documentation search"
      ("d" "Racket describe search" racket-describe-search :transient nil)
      ("x" "Racket search external" racket-xp-documentation :transient nil)
      ]
     ])
  :bind ((:map racket-mode-map
               ("C-<f1>"  . exo/racket-transient)
               ("C-<f12>" . switch-to-racket))
         (:map racket-repl-mode-map
               ("C-<f1>"  . exo/racket-transient)))
  )

;;; Languages

;;;; Forth

;; forth-lsp is pretty buggy - crashes on save,
;; left here as an example of adding custom lsp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (with-eval-after-load 'lsp-mode                                        ;;
;;   (add-to-list 'lsp-language-id-configuration                          ;;
;;                '(forth-mode . "\\.fs\\'"))                             ;;
;;   (lsp-register-client                                                 ;;
;;    (make-lsp-client :new-connection (lsp-stdio-connection "forth-lsp") ;;
;;                     :activation-fn (lsp-activate-on "\\.fs\\'")        ;;
;;                     :server-id 'forth-lsp)))                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use tags instead

(autoload 'forth-mode "gforth.el")
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode) auto-mode-alist))
(autoload 'forth-block-mode "gforth.el")
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode) auto-mode-alist))
(add-hook 'forth-mode-hook (function (lambda ()
                                       ;; customize variables here:
                                       (setq forth-indent-level 4)
                                       (setq forth-minor-indent-level 2)
                                       (setq forth-hilight-level 3)
                                       (setq forth-jit-parser t)
                                       (define-key forth-mode-map (kbd "M-o") nil)
                                       (define-key forth-mode-map (kbd "M-RET") 'forth-send-line-and-go)
                                       )))

(defun forth-send-line-and-go ()
  "Send the current line to the inferior Forth process."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (forth-send-region (point) end))))

(provide 'exo-develop)

;;; exo-develop.el ends here
