;;; exo-elfeed.el --- rss/atom support -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defface news-entry '((t :foreground "#f09b9e")) "Entry for news tag")
(defface reddit-entry '((t :foreground "#f28735")) "Entry for Reddit tag")
(defface youtube-entry '((t :foreground "#f74e8b")) "Entry for YouTube tag")
(defface dev-entry '((t :foreground "green")) "Entry for dev tag")
(defface sys-entry '((t :foreground "#95c192")) "Entry for sys tag")
(defface books-entry '((t :foreground "#35f6a2")) "Entry for books tag")
(defface emacs-entry '((t :foreground "#2daaff")) "Entry for emacs tag")

(use-package elfeed
  :hook
  (
   (elfeed-show-mode     .  visual-line-mode)
   (elfeed-show-mode     .  olivetti-mode)
   (elfeed-search-update . hide-mode-line-mode)
   (elfeed-search-mode   . hide-mode-line-mode)
   (elfeed-show-mode     . hide-mode-line-mode)
   )
  :config
  (push '(unread elfeed-search-unread-title-face) elfeed-search-face-alist)  
  (push '(Books books-entry) elfeed-search-face-alist)
  (push '(Sys sys-entry) elfeed-search-face-alist)
  (push '(Dev dev-entry) elfeed-search-face-alist)
  (push '(News news-entry) elfeed-search-face-alist)
  (push '(reddit reddit-entry) elfeed-search-face-alist)
  (push '(emacs emacs-entry) elfeed-search-face-alist)
  :custom-face
  (elfeed-search-title-face ((t ( :slant italic))))
  )

(when (file-exists-p "~/.emacs.d/private/")
  (load "~/.emacs.d/private/elfeed-info"))

(use-package elfeed-protocol
  :after elfeed
  :custom
  (elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (elfeed-protocol-newsblur-maxpages 20)
  (elfeed-protocol-newsblur-fetch-tags t)
  (elfeed-protocol-newsblur-fetch-category-as-tag t)
  (elfeed-protocol-newsblur-sub-category-separator "/")
  (elfeed-curl-extra-arguments '("--cookie-jar" "/tmp/newsblur-cookie"
                                 "--cookie" "/tmp/newsblur-cookie"
                                 "--insecure" "--fail-early"
                                 "--tcp-fastopen" "--ssl-allow-beast"))
  (elfeed-protocol-feeds `((,elfeed-url :password ,elfeed-pass)))
  (elfeed-log-level 'debug)
  :config
  ;; enable elfeed-protocol
  (elfeed-protocol-enable)
  )

(use-package w3m
  :defer t
  :hook
  (w3m-mode . visual-line-mode)
  )

(defun elfeed-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-qutebrowser-open (&optional use-generic-p)
  "open with qutebrowser"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (browse-url-qutebrowser it t))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun ffap-w3m-other-window (url &optional new-session)
  "Browse url in w3m.
If current frame has only one window,
create a new window and browse the webpage"
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "Emacs-w3m URL: ")))
  (let ((w3m-pop-up-windows t))
    ;;(if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url new-session)))

(defun elfeed-w3m-open (&optional use-generic-p)
  "open with w3m"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (ffap-w3m-other-window it))
    ;;do (w3m-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(with-eval-after-load 'transient
  (transient-define-prefix exo/elfeed-menu-transient ()
    "Elfeed Menu"
    [
     ["View"
      ("e" "Open in eww" elfeed-eww-open :transient nil)
      ("b" "Open in qutebrowser" elfeed-qutebrowser-open :transient nil)
      ("w" "Open in w3m" elfeed-w3m-open :transient nil)
      ]
     ["Utility"
      ("t" "Wrap lines" toggle-truncate-lines :transient nil)
      ]
     ])
  )

(provide 'exo-elfeed)

;;; exo-elfeed.el ends here
