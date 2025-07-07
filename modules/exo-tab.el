;;; exo-tab.el --- Tab-bar -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tab-bar
  :ensure nil
  :init
  (setq tab-bar-show t
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-new-tab-choice "*scratch*"
        ;;tab-bar-define-keys 'numeric
        tab-bar-tab-hints t
        tab-bar-tab-name-function 'tab-bar-tab-name-truncated
        tab-bar-tab-name-truncated-max 10
        tab-bar-format '(tab-bar-format-menu-bar
                         tab-bar-format-history
                         tab-bar-format-tabs-groups
                         tab-bar-separator
                         tab-bar-format-align-right
                         tab-bar-format-global))
  :bind
  (
   ("M-s-t"       . tab-bar-new-tab)
   ("M-s-w"       . tab-bar-close-tab)
   ("M-s-["       . tab-bar-move-tab-backward)
   ("M-s-]"       . tab-bar-move-tab)
   ("M-s-z"       . tab-bar-undo-close-tab)
   ("M-s-1"       . tab-bar-select-tab)
   ("M-s-2"       . tab-bar-select-tab)
   ("M-s-3"       . tab-bar-select-tab)
   ("M-s-4"       . tab-bar-select-tab)
   ("M-s-5"       . tab-bar-select-tab)
   ("M-s-6"       . tab-bar-select-tab)
   ("M-s-7"       . tab-bar-select-tab)
   ("M-s-8"       . tab-bar-select-tab)
   ("M-s-9"       . tab-bar-select-tab)
   )
  :config
  (tab-bar-mode 1)
  )

(with-eval-after-load 'transient
  (transient-define-prefix exo/tab-bar-menu-transient ()
    "Tab-bar Menu"
    [["Tab-bar"
      ("t" "New tab"               tab-bar-new-tab :transient nil)
      ("w" "Close tab"             tab-bar-close-tab :transient nil)
      ("f" "Move tab to new frame" tab-bar-detach-tab :transient nil)
      ("r" "Rename tab"            tab-bar-rename-tab :transient nil)
      ("d" "Duplicate tab"         tab-bar-duplicate-tab :transient nil)
      ("z" "Undo close tab"        tab-bar-undo-close-tab :transient nil)
      ]
     ])
  (global-set-key (kbd "M-s-SPC") 'exo/tab-bar-menu-transient)
)

(provide 'exo-tab)

;;; exo-tab.el ends here

