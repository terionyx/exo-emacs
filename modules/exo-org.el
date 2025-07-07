;;; exo-org.el --- org-mode packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :hook ((org-mode                . prettify-symbols-mode)
         (org-mode                . visual-line-mode)
         (org-mode                . variable-pitch-mode)
         (org-mode                . org-indent-mode)
         (org-babel-after-execute . org-redisplay-inline-images))
  :bind (("C-c o c"   . counsel-org-capture)
         ("C-c o a"   . org-agenda)
         :map org-mode-map
         ;;("M-<left>"  . nil)
         ;;("M-<right>" . nil)
         ("C-c c"     . org-mode-insert-code)
         ("C-c a f"   . org-shifttab)
         ("C-c a S"   . zero-width))
  :custom
  (org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
  (org-adapt-indentation nil)
  (org-agenda-files (list (concat org-directory "/agenda.org")))
  (org-catch-invisible-edits 'smart)
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-ellipsis " ↩")
  (org-hide-emphasis-markers t)
  (org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (org-return-follows-link t)
  (org-src-ask-before-returning-to-edit-buffer nil "org-src is kinda needy out of the box")
  (org-src-preserve-indentation t)
  (org-startup-indented nil)
  (org-startup-with-inline-images t)

  ;; org-modern
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  :config
  (defun pt/org-mode-hook ()
    (when (s-suffix? "todo.org" (buffer-file-name (current-buffer)))
      (real-auto-save-mode)))
  (defun make-inserter (c) '(lambda () (interactive) (insert-char c)))
  (defun zero-width () (interactive) (insert "​"))
  (defun org-mode-insert-code ()
    "Like markdown-insert-code, but for org instead."
    (interactive)
    (org-emphasize ?~)))

(use-package ob-blockdiag)

(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (emacs-lisp . t)
                               (awk . t)
                               (blockdiag . t)
                               (ditaa . t)
                               (dot . t)
                               (gnuplot . t)
                               (lisp . t)
                               (plantuml . t)
                               (shell . t)
                               )
                             )

(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil :underline nil)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(require 'org-tempo)

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(let* ((variable-tuple
        (cond ;;((x-list-fonts "ETbb")            '(:font "ETbb"))
              ((x-list-fonts "Crimson Text")  '(:font "Crimson Text"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.15))))
   `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.15))))
   `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.15))))
   `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.3 :foreground "ivory"))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.35 :foreground "wheat"))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.4 :foreground "light goldenrod"))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.45 :foreground "gold"))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5 :foreground "yellow"))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "light blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind
  ("C-c n l" . #'consult-notes)
  :config
  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(provide 'exo-org)

;;; exo-org.el ends here
