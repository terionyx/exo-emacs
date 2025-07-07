;;; exo-treesitter.el --- Tree-Sitter -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (treesit-font-lock-level 4)
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.1"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.4"))
               (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.23.2"))
               (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
               (java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.23.4"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
               (julia . ("https://github.com/tree-sitter/tree-sitter-julia" "v0.23.1"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.5"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.23.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar))
        )
      ))

  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (java-mode . java-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (julia-mode . julia-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  )

(provide 'exo-treesitter)

;;; exo-treesitter.el ends here
