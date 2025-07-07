;;; exo-buffer.el --- buffer packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
	  (jump-to-register '_)
	(progn
	  (window-configuration-to-register '_)
	  (delete-other-windows))))
(global-set-key [M-C-return] 'toggle-maximize-buffer)

;; buffer position
(defvar parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

;;https://protesilaos.com/codelog/2024-02-08-emacs-window-rules-display-buffer-alist/
(setq display-buffer-alist
	  `(
        ("\\*\\(Occur\\)\\*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-height . fit-window-to-buffer)
         (dedicated . t)
         (body-function . (lambda (window) (select-window window)))
         ,parameters
         )

        ("^\\*\\(Help\\|info.*\\|eldoc\\)"
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (side . right)
         (window-width . fit-window-to-buffer)
         (reusable-frames . nil)
         ,parameters
 		 )
        
        ((or (derived-mode . pdf-view-mode)
             (derived-mode . nov-mode))
         (display-buffer-reuse-window
          display-buffer-in-direction)
         (direction . leftmost)
         (dedicated . t)
         (window-width . 0.4)
         (reusable-frames . nil)
         ,parameters
         )

        ((derived-mode . racket-describe-mode)
         (display-buffer-reuse-mode-window
          display-buffer-in-side-window)
         (window-height . 15)
         (side . bottom)
         (slot . -1)
         ,parameters
         )

        ("^\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
         (display-buffer-reuse-window
          display-buffer-in-side-window)
         (window-height . 15)
         (side . bottom)
         (slot . -1)
         ,parameters
         )

        ("^\\*\\(vterm\\|eshell\\)\\*"
         display-buffer-in-side-window
         (window-height . 15)
         (side . bottom)
         (slot . 0)
         (preserve-size . (nil . t))
         (body-function . (lambda (window) (set-window-text-height window 15)))
         ,parameters
         )

        ("^\\*\\(Geiser.*REPL\\)\\*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         (window-height . 15)
         (preserve-size . (nil . t))
         (dedicated . t)
         ,parameters
         )

        ("\\*Tags List\\*"
         display-buffer-in-side-window
         (side . right)
         (slot . 0)
         (window-width . fit-window-to-buffer)
         (preserve-size . (t . nil))
         ,parameters)

        ((derived-mode . calculator-mode)
         (
          display-buffer-in-side-window)
         (window-height . 10)
         (side . bottom)
         (slot . 0)
         (preserve-size . (nil . t))
         (body-function . (lambda (window) (set-window-text-height window 10)))
         ,parameters
         )
        
 	    ;;( ((derived-mode . inferior-forth-mode))
 	    ;;  (display-buffer-reuse-mode-window
 	    ;;   display-buffer-below-selected)
 	    ;;  (dedicated . t)
 	    ;;  )
	    ))

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(use-package ace-window
  :defer 1
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("M-o" . ace-window)
         ("M-O" . ace-swap-window))
  )

(with-eval-after-load 'ace-window
  (when (boundp 'aw-ignored-buffers)
    (add-to-list 'aw-ignored-buffers 'dired-sidebar-mode)
    (add-to-list 'aw-ignored-buffers 'geiser-repl-mode)
    ))

;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :hook ((ibuffer-mode . (lambda ()
                           (ibuffer-auto-mode 1)
                           ;; (ibuffer-switch-to-saved-filter-groups "Home")
                           )))
  :config
  (setq ibuffer-never-show-predicates
        '((lambda (buffer) (with-current-buffer buffer (eq major-mode 'dired-sidebar-mode))))) ;;'("*Scratch\\*" "*Message\\*"  "^\\*")
  (setq ibuffer-expert t
        ibuffer-default-sorting-mode 'alphabetic
        ibuffer-show-empty-filter-groups nil
        ibuffer-display-summary nil
        ;;ibuffer-hidden-filter-groups '("Default" "Org")
        ibuffer-saved-filter-groups
        '(("Home"
           (" Start"   (name                   . "*scratch\\*"))
           ("☣ Unsaved" (and (modified)
                             (not (starred-name))
                             (not (derived-mode . magit-mode))))
           (" Emacs"   (and (filename          . ".emacs.d/*")
		                     (visiting-file)))
           (" Dired"   (or  (derived-mode      . dired-mode)
		                     (derived-mode      . image-mode)))
           (" Org"     (or (filename           . ".+\\\\.org\\\\")
                            (mode               . org-mode)
                            (mode               . org-agenda-mode)))
           (" Java"    (filename               . ".java"))
           ("🖹 Doc"     (or (mode               . pdf-view-mode)))
           (" Mail"    (or  (derived-mode      . rmail-mode)
		                     (derived-mode      . message-mode)))
           ("🛪 Net"     (or  (mode              . eww-mode)
		                     (derived-mode      . rcirc-mode)
		                     (mode              . elpher-mode)))
           (" Music"   (name                   . "*MPC"))
           (" Proc"    (process))
           (" Magit"   (name                   . "magit\\\\*"))
           ("❆ Stars"   (starred-name))
           )
          ("Lisps"
           ("Racket" (derived-mode . racket-mode))
           ("Lisp"   (derived-mode . lisp-mode))
           ("Scheme" (derived-mode . scheme-mode))
           )
          )
        )
  (defun ibuffer--remove-column-titles-after (_format)
    "Remove column headers from Ibuffer buffer."
    (save-excursion
      (set-buffer "*Ibuffer*")
      (let ((inhibit-read-only t))
        (goto-char 1)
        (search-forward "-\n" nil t)
        (delete-region 1 (point)))
      (let ((window-min-height 1))
        ;; save a little screen estate
        (shrink-window-if-larger-than-buffer))))
  (advice-add 'ibuffer-update-title-and-summary :after #'ibuffer--remove-column-titles-after)
  )

(use-package ibuffer-vc
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq nerd-icons-ibuffer-icon-size 1.0)
  (setq nerd-icons-ibuffer-human-readable-size t)
  (setq inhibit-compacting-font-caches t)
  (setq nerd-icons-ibuffer-formats
        '((mark modified read-only locked vc-status-mini " "
                (icon 2 2) (name 18 18 :left :elide)     " "
                (size-h 9 -1 :right)                     " "
                (mode+ 16 16 :left :elide)               " "
                (vc-status 14 14  :left )                " "
                filename-and-process+)))
  )

(provide 'exo-buffer)

;;; exo-buffer.el ends here
