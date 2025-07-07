;;; exo-modeline.el --- modeline packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defface exo-readonly-buffer
  '((t :foreground "yellow2" :weight bold  :inherit my-modeline-background)) "Yellow fg face")

(defface exo-modified-buffer
  '((t :foreground "tomato" :weight bold :slant italic :inherit my-modeline-background)) "Red fg face")

(defface exo-current-buffer
  '((t :weight bold :inherit my-modeline-background)) "Default fg face")

(defface exo-inactive-buffer
  '((t :foreground "gray60" :weight normal :inherit my-modeline-background)) "Inactive fg face")

(defface my-modeline-background
  '((t :background "#383838" :inherit 'default)) "Face with custom background for use on the mode line.")

(defface exo-vc-diff-plus
  '((t :foreground "green3" :height 105 :inherit my-modeline-background)) "Plus vc diff face")

(defface exo-vc-diff-minus
  '((t :foreground "yellow3" :height 105 :inherit my-modeline-background)) "Minus vc diff face")

(defface exo-active-icon
  '((t :background "#383838" :inherit 'nerd-icons-lred)) "Active buffer icon face")

(defface exo-inactive-icon
  '((t :background "#383838" :inherit 'nerd-icons-silver)) "Inactive buffer icon face")

(defface exo-indicator-macro
  '((t :foreground "cyan" :weight bold :slant italic :inherit my-modeline-background)) "Kbd macro fg face")

(setq mode-line-percent-position '(-3 "%p"))
(setq mode-line-position-column-line-format '(" %l:%c"))
(setq mode-line-compact nil)
(setq mode-line-right-align-edge 'right-margin)
(setq-default mode-line-modified "")
(setq-default mode-line-buffer-identification '(:eval (exo-modeline--buffer-identification-with-icon)))
(setq mode-line-modes-delimiters '("" . ""))

(setq left-decoration (propertize "🭮" 'face '(:foreground "#383838")))
(setq right-decoration (propertize "🭬" 'face '(:foreground "#383838")))

(defvar-local exo-modeline-major-mode
  '(:eval
    (list
     (exo-modeline-major-mode-icon)
     " "
     (propertize (exo-modeline--major-mode-name) 'face 'bold)))
  "Mode line construct to display the major mode.")
(put 'exo-modeline-major-mode 'risky-local-variable t)

(defvar-local exo-modeline-vc
  '(:eval
    (when (mode-line-window-selected-p)
      (exo-modeline--vc-string)
      ))
  "Mode line construct to display vc.")
(put 'exo-modeline-vc 'risky-local-variable t)

(defvar-local exo-modeline-position
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-position))
  "Mode line construct to display position.")
(put 'exo-modeline-position 'risky-local-variable t)

(defvar-local exo-modeline-kmacro
  '(:eval
    (when (and (mode-line-window-selected-p) defining-kbd-macro)
      (propertize " Macro " 'face 'exo-indicator-macro)))
  "Mode line construct to display keyboard macro.")
(put 'exo-modeline-kmacro 'risky-local-variable t)

(defvar-local exo-modeline-misc-info
  '(:eval
    (when (mode-line-window-selected-p)
      mode-line-misc-info))
  "Mode line construct to display misc info.")
(put 'exo-modeline-misc-info 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                  mode-line-window-dedicated)
                 display (min-width (6.0)))
                exo-modeline-kmacro
                mode-line-frame-identification mode-line-buffer-identification "   "
                exo-modeline-position
                
                mode-line-format-right-align
                (project-mode-line project-mode-line-format)
                " "
                exo-modeline-misc-info
                exo-modeline-vc
                " "
                exo-modeline-major-mode
                mode-line-minor-modes
                " "
                ))

(setq mode-line-collapse-minor-modes t)
;; '(abbrev-mode
;;   eldoc-mode
;;   flyspell-mode
;;   smooth-scroll-mode
;;   outline-minor-mode
;;   which-key-mode
;;   git-gutter-mode
;;   auto-highlight-symbol-mode
;;   compile-angel-on-load-mode
;;   compile-angel-on-save-local-mode
;;   gcmh-mode
;;   jarchive-mode
;;   hungry-delete-mode
;;   rainbow-mode
;;   hs-minor-mode
;;   completion-preview-mode
;;   yas-minor-mode
;;   flycheck-mode))

(add-hook 'after-change-major-mode-hook #'modeline-conditional-buffer-encoding)

;; make flat modeline
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'vc-edited-state nil :weight 'bold :slant 'italic :foreground "salmon" :inherit 'my-modeline-background)
(set-face-attribute 'vc-state-base nil :inherit 'my-modeline-background)
(set-face-attribute 'vc-conflict-state nil :inherit 'my-modeline-background)
(set-face-attribute 'vc-removed-state nil :inherit 'my-modeline-background)
(set-face-attribute 'vc-missing-state nil :inherit 'my-modeline-background)
(set-face-attribute 'vc-ignored-state nil :inherit 'my-modeline-background)

;;; Fuctions:

(defun exo-modeline--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (capitalize (symbol-name major-mode)))

(defun exo-modeline-major-mode-icon ()
  "Return icon for the major mode."
  (let ((ico (cond
              ((derived-mode-p 'text-mode) "§")
              ((derived-mode-p 'prog-mode) "λ")
              ((derived-mode-p 'comint-mode) ">_")
              ((derived-mode-p 'vterm-mode) "%%>")
              (t "◦"))))
    ;;(propertize ico 'face 'shadow)
    ico
    ))

(defun exo-custom-buffer-mode-icon ()
  "Custom buffer mode icon."
  (ignore-errors
    (propertize (nerd-icons-icon-for-mode major-mode
                                          :height (/ nerd-icons-scale-factor 1.4)
                                          :v-adjust -0.03))))

(defun exo-modeline--buffer-identification-with-icon ()
  "Custom buffer identification."
  (concat
   left-decoration
   (propertize (concat " " (exo-custom-buffer-mode-icon)) 'face
               (cond ((mode-line-window-selected-p) 'exo-active-icon)
                     (t 'exo-inactive-icon)))
   (propertize " %b " 'face
  			   (cond ((not (mode-line-window-selected-p)) 'exo-inactive-buffer)
                     ((and buffer-read-only (exo-ignored-status-mode)) 'exo-readonly-buffer)
  					 ((and (buffer-modified-p) (exo-ignored-status-mode)) 'exo-modified-buffer)
  					 ((eq (current-buffer) (window-buffer (selected-window))) 'exo-current-buffer)
  					 (t '( :foreground ,(face-foreground 'default) :weight normal)))
  			   'help-echo default-directory)
   right-decoration
   ))

(defun exo-modeline--vc-string ()
  "Custom vc string."
  (if (stringp vc-mode)
      (let* ((branch-name (concat
                           (propertize "  " 'face 'my-modeline-background)
                           (replace-regexp-in-string
                            (format "^\s*%s:?-?" (vc-backend buffer-file-name))
                            ""
                            vc-mode)
                           (propertize " " 'face 'my-modeline-background)
                           ))
             (buffer-vc-state (vc-state buffer-file-name)))
        (concat left-decoration branch-name (exo-modeline--vc-diff) right-decoration))))

(defun exo-modeline--vc-diff ()
  "Modeline vc diff"
  (if (stringp vc-mode)
      (let ((plus-minus (vc-git--run-command-string buffer-file-name "diff" "--numstat" "--")))
        (when (and plus-minus (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
          (concat
           (propertize (format "+%s" (match-string 1 plus-minus)) 'face 'exo-vc-diff-plus)
           (propertize (format "-%s " (match-string 2 plus-minus)) 'face 'exo-vc-diff-minus)
           )
          )
        )))

(defun modeline-conditional-buffer-encoding ()
  "Hide \"LF UTF-8\" in modeline.
It is expected of files to be encoded with LF UTF-8, so only show
the encoding in the modeline if the encoding is worth notifying
the user."
  (setq-local mode-line-mule-info
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))

(provide 'exo-modeline)

;;; exo-modeline.el ends here
