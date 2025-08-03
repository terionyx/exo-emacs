;;; early-init.el --- Early init -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)

(defvar exo-emacs-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")

(setq custom-theme-directory (expand-file-name "themes/" exo-emacs-directory))

;; Additional el load paths
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("lib" "modules"))

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq exo-emacs-var-dir (expand-file-name "var/" exo-emacs-directory))
(setq package-user-dir (expand-file-name "elpa" exo-emacs-var-dir))
(setq user-emacs-directory exo-emacs-var-dir)

;;(setq custom-file null-device)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;;; Load custom file. Don't hide errors. Hide success message
(load custom-file nil t)

;; Remove the original eln-cache
(setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
;; Add the new eln-cache
(push (expand-file-name (file-name-as-directory ".cache/eln/")
                        user-emacs-directory)
      native-comp-eln-load-path)

;; alias
(eval-when-compile
  (defalias 'after-load #'with-eval-after-load))

;;; GC

(setq gc-cons-percentage 0.6)

(defvar file-name-handler-alist-backup file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-to-list 'default-frame-alist '(undecorated . t))

(add-hook 'emacs-startup-hook
	      (lambda ()
	        (setq file-name-handler-alist file-name-handler-alist-backup
		          gc-cons-threshold 100000000 ;restore GC
		          gc-cons-percentage 0.1)
            (makunbound 'file-name-handler-alist-backup)))

;; suppress GC for 1 second after the minibuffer is active
(defun defer-garbage-collection ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 100000000))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection)

;;; Immortal buffers

(defun ndw-immortal-buffers ()
  (if (or (eq (current-buffer) (get-buffer "*scratch*"))
          (eq (current-buffer) (get-buffer "*Messages*")))
      (progn (bury-buffer)
             nil)
    t))

(add-hook 'kill-buffer-query-functions 'ndw-immortal-buffers)

;;; Misc

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq default-input-method nil)

(setq load-prefer-newer t)

(setq large-file-warning-threshold 100000000)

;; don't compact font caches
(setq inhibit-compacting-font-caches t)

(setq site-run-file nil)

(unless noninteractive
  ;; Suppress redisplay and redraw during startup
  (setq-default inhibit-message t)

  ;; reset inhibit-message
  (defun reset--inhibited-vars-h ()
    (setq-default inhibit-message nil)
    (remove-hook 'post-command-hook #'reset--inhibited-vars-h))

  (add-hook 'post-command-hook #'reset--inhibited-vars-h -100)
  
  ;; avoid resize emacs to a specific column size
  (setq frame-inhibit-implied-resize t)

  (setq frame-resize-pixelwise t)
  
  ;; Reduce *Message* noise at startup
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add #'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen
  (advice-add #'display-startup-screen :override #'ignore)

  ;; Shave seconds off startup time by starting the scratch buffer in `fundamental-mode'
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  (setq-default default-major-mode 'text-mode)

  )

;;; Native compile + hide warnings
(when (native-comp-available-p)
  (setq native-comp-jit-compilation t)
  (setq package-native-compile t)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local cl-functions))

(setq warning-minimum-level :error)
(setq warning-suppress-types '((comp)))

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(setq confirm-kill-emacs 'y-or-n-p)

;;; UI

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq menu-bar-mode nil)

;; not used
(defun exo-post-forward-abbreviate (base extra-string)
  "Add directory EXTRA-STRING after BASE buffer name."
  (concat base " " (abbreviate-file-name (mapconcat #'identity extra-string "/")) "/"))

(after-load 'uniquify
  (setq uniquify-buffer-name-style nil ;;#'exo-post-forward-abbreviate
        uniquify-trailing-separator-p t
        uniquify-min-dir-content 7
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))
(setq exo-ignored-modes-list
      '(vterm-mode term-mode Info-mode help-mode helpful-mode
                   elfeed-search elfeed-show racket-repl-mode
                   pdf-view-mode ibuffer-mode eshell-mode
                   shell-mode eww-mode eww-buffers-mode dired-mode
                   dired-sidebar-mode minibuffer-mode geiser-repl-mode
                   nov-mode racket-describe-mode messages-buffer-mode
                   elfeed-search-mode elfeed-show-mode treemacs-mode))

(defun exo-ignored-status-mode ()
  "Ignored modes in modeline and title."
  (not (or
        (string= (buffer-name) "*scratch*")
        (seq-reduce
         (lambda (acc v) (or acc v))
         (mapcar 'derived-mode-p exo-ignored-modes-list) nil)))
  )

(setq-default frame-title-format
              '((:eval
                 (concat
                  (exo-custom-buffer-mode-icon)
                  " "
                  (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")
	              (if (and (buffer-modified-p) (exo-ignored-status-mode))  " ")
                  (if (and buffer-read-only (exo-ignored-status-mode)) " ")
                  )
                 )
                ))

(setq icon-title-format frame-title-format)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;;; package.el
(setq package-quickstart nil)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("stable" . 70)
                                                      ("melpa"  . 0)))

(setenv "LSP_USE_PLISTS" "true")

;;; Lsp-booster

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(provide 'early-init)

;;; early-init.el ends here
