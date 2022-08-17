;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

;;;;;;;;;
;; doom
;;;;;;;;;
(setq user-full-name "Di Liu"
      user-mail-address "liudi12631@email.com"

      doom-theme 'doom-one
      display-line-numbers-type 'relative

      doom-font (font-spec :family "mononoki" :size 22 :weight 'regular)
      doom-unicode-font doom-font
      )

(map! :n "C-t" nil
      :map vterm-mode-map "C-t" nil
      :map global-map "C-t" nil
      :g "C-t" :desc "toggle term" #'+vterm/toggle
      :map evil-motion-state-map "C-e" nil
      :g "C-e" :desc "toglle explore" #'+treemacs/toggle
      )
;; TODO C-/ comment

;; pdf
(setq pdf-view-midnight-colors '("#f8f8f2" . "#282a36"))
(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode)))


;; input
;; TODO emcas unable to accept input in chinese mode

;; projectile
(setq projectile-project-search-path '("~/Documents/code" "~/StudioProjects/"))


;; org
(setq org-directory "~/Documents/note/pages/org/")

;; vterm
(setq vterm-shell "/usr/bin/zsh")

;;;;;;;;;
;; lsp
;;;;;;;;;

;; TODO: (add-to-list 'lsp-file-watch-ignored-directories "\\usr\\include\\'")
;; reference: https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
(setq lsp-pylsp-plugins-pydocstyle-enabled nil
      lsp-pylsp-plugins-flake8-max-line-length 100)
(setq lsp-volar-take-over-mode t)
