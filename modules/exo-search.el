;;; exo-search.el --- Search stuff -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package isearch
  :ensure nil
  :defer t
  :bind
  (:map isearch-mode-map
        ("C-o" . exo/occur-from-isearch)
        ("<f7>"  . exo/isearch-transient)
        )
  :config
  (defun exo/occur-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-update-ring isearch-string isearch-regexp)
      (let (search-nonincremental-instead)
        (ignore-errors (isearch-done t t)))
      (occur query)))
  (with-eval-after-load 'transient
    (transient-define-prefix exo/isearch-transient ()
                             "isearch Menu"
                             [["Edit Search String"
                               ("e" "Edit the search string (recursive)" isearch-edit-string
                                :transient nil)
                               ("w" "Pull next word or character word from buffer" isearch-yank-word-or-char
                                :transient nil)
                               ("s" "Pull next symbol or character from buffer" isearch-yank-symbol-or-char
                                :transient nil)
                               ("l" "Pull rest of line from buffer" isearch-yank-line
                                :transient nil)
                               ("y" "Pull string from kill ring" isearch-yank-kill
                                :transient nil)
                               ("t" "Pull thing from buffer" isearch-forward-thing-at-point
                                :transient nil)]
                              ["Replace"
                               ("q" "Start `query-replace'" isearch-query-replace
                                :if-nil buffer-read-only
                                :transient nil)
                               ("x" "Start `query-replace-regexp'" isearch-query-replace-regexp
                                :if-nil buffer-read-only
                                :transient nil)]]
                             [["Toggle"
                               ("X" "Toggle regexp searching" isearch-toggle-regexp
                                :transient nil)
                               ("S" "Toggle symbol searching" isearch-toggle-symbol
                                :transient nil)
                               ("W" "Toggle word searching" isearch-toggle-word
                                :transient nil)
                               ("F" "Toggle case fold" isearch-toggle-case-fold
                                :transient nil)
                               ("L" "Toggle lax whitespace" isearch-toggle-lax-whitespace
                                :transient nil)]
                              ["Misc"
                               ("o" "occur" isearch-occur
                                :transient nil)]])
    )
  )

(provide 'exo-search)

;;; exo-search.el ends here
