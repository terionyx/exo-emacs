;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; vast majority of configuration params were copied from
;;; https://github.com/jamescherti/minimal-emacs.d
;;; and somwhere else :)
;;;
;;; Preconditions:
;;; user-emacs-directory should be already pointed to /var dir

;;; Code:

;; useful for error search
(setq debug-on-error t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Ensure use-package is available at compile time
(eval-when-compile
  (require 'use-package))

(setq use-package-enable-imenu-support t)
(setq package-install-upgrade-built-in t)

(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region scroll-left))

(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((lexical-binding)))

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(setq echo-keystrokes 0.01)

(setq inferior-lisp-program "/usr/bin/sbcl"
      slime-lisp-implementations '((sbcl ("sbcl" "--core" "sbcl.core-for-slime")))
      )

(setq select-enable-clipboard t)

(setq track-eol t)

;; *message* buffer max lines
(setq message-log-max 1000)

;;; UI
;; themes
(setq custom-safe-themes t)
;; Update ui delay
(setq idle-update-delay 1.0)
;; Shorter responses: "y" for yes and "n" for no.
(setq use-short-answers t)
(advice-add #'yes-or-no-p :override #'y-or-n-p)
;; Never show the hello file
(defalias #'view-hello-file #'ignore)

;; switch-to-buffer runs pop-to-buffer-same-window instead
(setq switch-to-buffer-obey-display-actions t)

;; show-paren-mode
(setq show-paren-delay 0.1
      show-paren-style 'mixed
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t
      show-paren-context-when-offscreen t)

;; whitespace-mode
(setq whitespace-line-column nil)

;; display-line-numbers-mode
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(setq-default display-line-numbers-width-start t)

;; display-fill-column-indicator-mode
(setq-default display-fill-column-indicator-column 90)
(setq-default display-fill-column-indicator-character ?\u250a)

(after-load 'comint
  (setopt comint-input-ignoredups t
          comint-prompt-read-only t
          comint-buffer-maximum-size 2048)
  (add-to-list 'comint-output-filter-functions 'comint-osc-process-output))

(after-load 'compile
  (setopt compilation-always-kill t
          compilation-ask-about-save nil
          compilation-scroll-output 'first-error
          compilation-context-lines 3)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(setq truncate-string-ellipsis "…")

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 512 1024))  ; 512kb

;; concat all possible eldoc
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

;;; Files

;; suppress warning the same file
(setq find-file-suppress-same-file-warnings t)

;; resolve symlinks
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; no confirmation for new file
(setq confirm-nonexistent-file-or-buffer nil)

(setq mouse-yank-at-point t)

;; Prefer vertical splits over horizontal ones
(setq split-width-threshold 170
      split-height-threshold nil)

;; window-divider-mode
(setq window-divider-default-bottom-width 0
      window-divider-default-places t
      window-divider-default-right-width 1)

;;; Backups

(setopt create-lockfiles nil)            ; Avoid generating lockfiles
(setopt backup-by-copying t              ; Backup by copying rather renaming
        backup-by-copying-when-linked t
        delete-old-versions t            ; Delete excess backup versions silently
        make-backup-files nil
        backup-inhibited t
        kept-new-versions 10
        kept-old-versions 10
        version-control t                ; Use version numbers for backup files
        vc-make-backup-files nil)        ; Do not backup version controlled files

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)

;;; Auto save
;; `recover-file' or `recover-session' functions to restore auto-saved data
(setq auto-save-default t)

;; Do not auto-disable auto-save after deleting large text chunks
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)

;;; Auto revert
;; automatically updates the contents of a buffer to reflect changes on disk
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

(setq auto-revert-check-vc-info t)
(setq vc-use-short-revision t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)

;;; Saveplace

;; save place in each file
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

;;; Frames and windows

;; do not resize windows pixelwise
(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

(setq-default window-combination-resize t)

(setq window-min-height 8)

(setq fit-window-to-buffer-horizontally 'only)

;;; Scrolling

;; accelerate scrolling operations
(setq fast-but-imprecise-scrolling t)

;; move point to top/bottom of buffer before signaling a scrolling error
(setq scroll-error-top-bottom t)

;; keeps screen position
(setq scroll-preserve-screen-position t)

;; Emacs spends excessive time recentering the screen when the cursor moves more
;; than N lines past the window edges (where N is the value of
;; `scroll-conservatively`). This can be particularly slow in larger files
;; during extensive scrolling. If `scroll-conservatively` is set above 100, the
;; window is never automatically recentered. The default value of 0 triggers
;; recentering too aggressively. Setting it to 10 reduces excessive recentering
;; and only recenters the window when scrolling significantly off-screen.
(setq scroll-conservatively 101)

;; scroll the window by 1 line whenever the cursor moves off the visible screen
(setq scroll-step 1)

;; Reduce cursor lag by :
;; 1. Prevent automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolve the issue of random half-screen jumps during scrolling.
(setq auto-window-vscroll nil)

;; Number of lines of margin at the top and bottom of a window.
(setq scroll-margin 1)

;; Horizontal scrolling
(setq hscroll-margin 2
      hscroll-step 1)

;; How far to scroll
(setq-default
 scroll-down-aggressively 0.01
 scroll-up-aggressively 0.01)

;; Smooth scroll

(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-interpolate-page t)

;;; Mouse Scroll

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-tilt-scroll t)

;;; Cursor

(setq-default cursor-type '(box . 2))

;; Don't blink the paren matching the one at point
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters
(setq x-stretch-cursor nil)

;; Reduce rendering/line scan by not rendering cursors in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;;; Annoyances

;; No beeping or blinking
(setopt visible-bell nil)
(setopt ring-bell-function 'ignore)

;; This controls how long Emacs will blink to show the deleted pairs with
;; `delete-pair'. A longer delay can be annoying as it causes a noticeable pause
;; after each deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

;;; Indent and formatting

(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Do not show an arrow at the top/bottomin the fringe and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Enable indentation and completion using the TAB key
(setq-default tab-always-indent 'complete)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines, which is useful for writing
;; longer comments or docstrings that span multiple lines.
(setq comment-multi-line t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Disable the obsolete practice of end-of-line spacing from the typewriter era
(setq sentence-end-double-space nil)

;; According to the POSIX, a line is defined as "a sequence of zero or
;; more non-newline characters followed by a terminating newline".
(setq-default require-final-newline t)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)

;; don’t add new line on down at end of buffer
(setq next-line-add-newlines nil)

;;; Mode line

;; Setting `display-time-default-load-average' to nil makes Emacs omit the load
;; average information from the mode line.
(setq display-time-default-load-average nil)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)

;;; Filetype

;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)

(setq sh-indent-after-continuation 'always)

(setopt dired-clean-confirm-killing-deleted-buffers nil
        dired-recursive-deletes 'top
        dired-recursive-copies  'always
        dired-create-destination-dirs 'ask
        find-ls-option '("-exec ls -ldh {} +" . "-ldh"))

;;; Font / Text scale

;; Avoid automatic frame resizing when adjusting settings.
(setq global-text-scale-adjust-resizes-frames nil)

(setq text-scale-mode-step 1.1)

(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
	  jit-lock-defer-contextually t
	  jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

;;; Ediff

;; Configure Ediff to use a single frame and split windows horizontally
(after-load 'ediff-wind
  (setq ediff-diff-options "-w"
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

(after-load 'diff-mode
  (setq diff-font-lock-prettify t))

;;; Search
(setopt xref-search-program 'ripgrep)
(setopt grep-command "rg -nS --no-heading"
        grep-find-ignored-directories
        '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

;; isearch
(setq isearch-repeat-on-direction-change t
      lazy-count-prefix-format "(%s/%s) "
      isearch-allow-scroll 'unlimited
      isearch-yank-on-move 'shift
      isearch-wrap-pause nil
      isearch-forward-thing-at-point '(region url email symbol sexp)
      isearch-allow-prefix t
      search-ring-max 16
      isearch-lazy-count t)

;;; Undo
(setopt undo-limit (* 13 160000)
        undo-strong-limit (* 13 240000)
        undo-outer-limit (* 13 24000000)
        )

;;; Help
(setq-default help-window-select t)

;;; Completion
(setopt completion-ignore-case t)
(setopt completions-detailed t)

;; Save desktop layout
(setq desktop-base-file-name      "emacs.desktop"
      ;;desktop-path `(,(expand-file-name (concat user-emacs-directory "desktop/")))
      desktop-base-lock-name      "lock"
      ;; desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "\\(^$\\|\\.*magit$\\|\\.el\\.gz$\\|\\.png$\\|\\.jpg$\\|\\.gif$\\)" ;reload tramp paths
      desktop-load-locked-desktop nil
      desktop-auto-save-timeout   30
      )

(setq desktop-modes-not-to-save '(tags-table-mode dired-mode Info-mode Info-lookup-mode geiser-repl-mode))

(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\|Ibuffer\\*\\|REPL\\*"
              "\\)$"))

(add-hook 'after-init-hook
          (lambda ()
            (setq desktop-modes-not-to-save
                  (delete-dups
                   (nconc desktop-modes-not-to-save
                          '(dired-mode Info-mode Info-lookup-mode))))))

;; doc view
(setq doc-view-continuous t)

;; dabbrev
(after-load 'dabbrev
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\|gif\\)\\'")))

;;electric
(after-load 'electric
  (setq electric-quote-context-sensitive t))

;; flyspell
(after-load 'flyspell
  (keymap-unset flyspell-mode-map "C-M-i")
  (setq flyspell-use-meta-tab nil
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil
        flyspell-consider-dash-as-word-delimiter-flag t
        flyspell-duplicate-distance 12000))

;;; Org-mode
(setq org-support-shift-select t
      org-hide-emphasis-markers t
      org-directory "~/Org/"
      org-src-window-setup 'current-window
      org-support-shift-select 'always)

;; compile
(setq load-prefer-newer t)

;; eshell
(put 'eshell 'disabled nil)

;; gpg
(after-load 'epa
  (setopt epg-pinentry-mode 'loopback))

;; gnutls
(after-load 'gnutls
  (setopt gnutls-min-prime-bits nil))

;; emacs sources
(setq source-directory "~/src/emacs/src/")

;; Mark ring
(setq global-mark-ring-max 100)

;; Project
(setq-default project-mode-line t)

;;; Modes

(desktop-save-mode 1)                   ;; add-hook not working
(tooltip-mode -1)                       ;; no tooltips
(blink-cursor-mode -1)                  ;; The blinking cursor interferes with settings
(when (display-graphic-p)
  (fringe-mode '(0 . 0)))               ;; remove fringe offset
(display-fill-column-indicator-mode -1) ;; get rid of vertical line

;; set default buffer encoding
(modify-coding-system-alist 'file "" 'utf-8)

;; A Protesilaos life savier HACK
;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
;; of the diff (if you choose `d') of what you're asked to save.
(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))


;;; Hooks

(when (and (display-graphic-p) (fboundp 'context-menu-mode))
  (add-hook 'after-init-hook #'context-menu-mode))

(add-hook 'emacs-startup-hook
          (lambda () (message "Emacs ready in %s with %d garbage collections."
                         (format "%.2f seconds"
                                 (float-time
                                  (time-subtract after-init-time before-init-time)))
                         gcs-done)))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'save-place-mode)
;;(add-hook 'after-init-hook #'pixel-scroll-precision-mode) ;; laggy scroll
(when (fboundp 'winner-mode)
  (add-hook 'after-init-hook #'winner-mode))
(add-hook 'after-init-hook #'window-divider-mode)
(add-hook 'after-init-hook #'global-prettify-symbols-mode)
(add-hook 'after-init-hook #'global-so-long-mode)
(add-hook 'after-init-hook #'delete-selection-mode)

(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'text-mode-hook #'electric-quote-mode)
(add-hook 'conf-mode-hook #'electric-quote-mode)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(dolist (mode '(prog-mode-hook
			    conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 'relative))))

;;; Modules

(require 'exo-modules)

;;; init.el ends here
