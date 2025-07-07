;;; exo-modules.el --- Modules -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Extra methods
(require 'exo-extras)

;; System packages
(require 'exo-system)

;; File utils
(require 'exo-file)

;; Themes, fonts
(require 'exo-ui)

;; Auto Completion
(require 'exo-completion)

;; Version control
(require 'exo-vc)

;; Buffer
(require 'exo-buffer)

;; Modeline
(require 'exo-modeline)

;; Tab-bar
(require 'exo-tab)

;; Minibuffer
;; Savehist, vertico, prescient, orderless, marginalia, consult, embark
(require 'exo-minibuffer)

;; Search
;; isearch
(require 'exo-search)

;; Pdf viewer
(require 'exo-view)

;; Help functions
(require 'exo-help)

;; Org mode
(require 'exo-org)

;; Develop
(require 'exo-develop)

;; Treesitter
(require 'exo-treesitter)

;; Utility packages
(require 'exo-utility)

;; Additional file types
(require 'exo-types)

;; Rss support
(require 'exo-elfeed)

;; Project support
(require 'exo-project)

(provide 'exo-modules)

;;; exo-modules.el ends here
