;;; exo-file.el --- file packages -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; recently opened files
(use-package recentf
  :ensure nil
  :defer 5
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup 'mode)
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "straight"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'")))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lAhp --group-directories-first")
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind
  (:map dired-mode-map
        ("C-c H" . dired-hide-details-mode)
        ("<RET>" . dired-mouse-find-alternate-file)
        ("^" . (lambda () (interactive) (find-alternate-file "..")))
        ("[mouse-2]" . dired-mouse-find-alternate-file)
        )
  )

(use-package dired-x
  :ensure nil
  :after dired
  :custom
  (dired-omit-files "^\\..*$")
  (dired-guess-shell-alist-user '(("" "xdg-open")))
  :bind
  (:map dired-mode-map
        ("C-c h" . dired-omit-mode)))

(use-package dired+
  :ensure nil
  :after dired
  :config
  (diredp-toggle-find-file-reuse-dir 1)
  :custom-face
  (diredp-dir-name ((t (:background "#3f3f3f"))))
  )

(use-package dired-narrow
  :after dired)

(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)
        ("<tab>" . dired-subtree-toggle)))

(use-package dired-rainbow
  :after dired
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

(use-package neotree
  :defer 5
  :bind (("<f12>" . neotree-project-dir))
  :hook (neo-enter . set-modeline-project-name)
  :custom
  (neo-smart-open t)
  (neo-vc-integration '(face))
  (neo-show-slash-for-folder nil)
  (neo-hide-cursor nil)
  (neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  :config
  (set-face-foreground neo-vc-edited-face "gold")
  (defun neotree-project-dir ()
    "Open NeoTree using the vc root."
    (interactive)
    (if (and (neo-global--window-exists-p)
             (eq (neo-global--get-buffer) (current-buffer)))
        (neotree-hide)
      (let ((project-dir (caddr (project-current)))
            (file-name (buffer-file-name)))
        (if project-dir
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name))
          (message "Could not find vc project root."))))
    )
  )

(provide 'exo-file)

;;; exo-file.el ends here
