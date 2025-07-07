;;; exo-system.el --- system packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package async
  :init
  (dired-async-mode 1)
  :config
  (async-bytecomp-package-mode 1)
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))

(use-package gcmh
  :init
  (setq gcmh-idle-delay 15
	    gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config (gcmh-mode))

;; autocompile
(use-package compile-angel
  :demand t
  :config
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

(after-load 'compile-angel
  ;; Ensure that the value of `savehist-file` is updated before proceeding
  (with-eval-after-load "savehist"
    (push (concat "/" (file-name-nondirectory savehist-file))
          compile-angel-excluded-files))
  ;; Ensure that the value of `recentf-save-file` is updated before proceeding
  (with-eval-after-load "recentf"
    (push (concat "/" (file-name-nondirectory recentf-save-file))
          compile-angel-excluded-files))
  ;; Ensure that the value of `custom-file` is updated before proceeding
  (with-eval-after-load "cus-edit"
    (when (stringp custom-file)
      (push (concat "/" (file-name-nondirectory custom-file))
            compile-angel-excluded-files)))
  ;; Ensure that the value of `prescient-save-file` is updated before proceeding
  (with-eval-after-load "prescient"
    (push (concat "/" (file-name-nondirectory prescient-save-file))
          compile-angel-excluded-files))
  (defvar compile-angel-excluded-list '(("savehist" . savehist-file)
                                        ("recentf" .  recentf-save-file)
                                        ("cus-edit" .  custom-file)
                                        ("prescient" .  prescient-save-file)))

  
  ;; Enable mode
  (compile-angel-on-load-mode)
  )

(use-package esup :defer t)

(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user) ;; We can change interactively with `s'
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))


(provide 'exo-system)

;;; exo-system.el ends here
