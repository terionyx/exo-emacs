;;; exo-minibuffer.el --- minibuffer packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; preserve minibuf history
(use-package savehist
  :ensure nil
  :defer 1
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq history-length 300)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  :hook (after-init . savehist-mode))

;; vertico
(defun exo-minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name ARG delete up to parent folder,
otherwise delete a character backward."
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

(use-package vertico
  :defer t
  :hook (after-init . vertico-mode)
  :config
  (vertico-mouse-mode)
  (vertico-multiform-mode)
  :custom
  (vertico-count 22)
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-scroll-margin 2)
  :bind ((:map vertico-map
               ("<prior>"   . vertico-scroll-down)
               ("<next>"    . vertico-scroll-up)

               ;; todo recheck keybindings
               ("C-'"       . vertico-quick-exit)
               ;; Have to rebind this because C-m is translated to RET.
               ("<return>"  . exit-minibuffer)
               ("C-m"       . vertico-insert)
               ("C-c SPC"   . vertico-quick-exit)
               ("<escape>"  . minibuffer-keyboard-quit)
               ("C-M-n"     . vertico-next-group)
               ("C-M-p"     . vertico-previous-group)
               ;;              ("DEL"       . vertico-directory-delete-char)
               )
         (:map minibuffer-local-map
               ("<backspace>" . exo-minibuffer-backward-kill)
               )
         )
  )

(use-package prescient
  :defer 5
  :config
  (setq prescient-save-file (expand-file-name "prescient-save.el" user-emacs-directory))
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :defer 5
  :after vertico prescient
  :custom
  (vertico-prescient-enable-sorting t)
  (vertico-prescient-override-sorting nil)
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1)
  )

(use-package corfu-prescient
  :defer 5
  :after corfu prescient
  :custom
  (corfu-prescient-enable-sorting t)
  (corfu-prescient-override-sorting nil)
  (corfu-prescient-enable-filtering nil)
  :config
  (corfu-prescient-mode 1)
  )

(with-eval-after-load 'prescient
  ;; Have `completion-preview-mode' use prescient's sorting algorithm
  (setopt completion-preview-sort-function #'prescient-completion-sort))

;; orderless
(use-package orderless
  ;;:after corfu
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic prescient)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; marginalia
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)
  ;;:commands (marginalia-mode marginalia-cycle)
  :bind (:map minibuffer-local-map
		      ("M-A" . marginalia-cycle))
  :custom-face
  (marginalia-documentation ((t (:foreground "#959575"))))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  )

;; embark
(use-package embark
  :defer t
  :bind(("C-." . embark-act)
        ("C-;" . embark-dwim)
        ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; consult

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x R" . consult-recent-file)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("C-y" . pt/yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s R" . consult-ripgrep-symbol-at-point)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Org integration
         ;;:map org-mode-map
         ;;("C-c o o" . consult-outline)
         ;;;; Isearch integration
         ;;("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)
         ("C-s"     . consult-isearch-forward)
         ("C-r"     . consult-isearch-backward))
  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref
        completion-in-region-function  #'consult-completion-in-region
        consult-project-root-function  #'deadgrep--project-root
        register-preview-delay         0.5
        register-preview-function      #'consult-register-format)
  (setq consult-buffer-filter
        '("\\` "
          "\\`\\*Completions\\*\\'"
          "\\`\\*Flymake log\\*\\'"
          "\\`\\*Semantic SymRef\\*\\'"
          "\\`\\*vc\\*\\'"
          "\\`newsrc-dribble\\'" ;; Gnus
          "\\`\\*tramp/.*\\*\\'"
          "\\`:"))
  :config
  (defun pt/yank-pop ()
    "Consult-yank-pop."
    (interactive)
    (let ((point-before (point)))
      (consult-yank-pop)
      (indent-region point-before (point))))
  (defun consult-ripgrep-symbol-at-point ()
    "Seearch in files whose base name is the same as the current file's."
    (interactive)
    (minibuffer-with-setup-hook
        (lambda () (goto-char (1+ (minibuffer-prompt-end))))
      (consult-ripgrep (my/root-project-dir)
                       (if-let* ((sap (symbol-at-point)))
                           (format "%s -- -g *.*" sap)
                         (user-error "Buffer is not visiting a file")))))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(provide 'exo-minibuffer)

;;; exo-minibuffer.el ends here
