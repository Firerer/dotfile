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
;; sync' after modi fying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Di Liu"
      user-mail-address "liudi12631@email.com")

;; ui
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
;; font
(setq doom-font (font-spec :family "mononoki" :size 20 :weight 'regular)
      ;; doom-big-font (font-spec :family "mononoki" :size 40 :weight 'Bold)
)

;;;;;;;;;;;;;;;;;;;;;
;;; dirs
;;;;;;;;;;;;;;;;;;;;;

;; emacs
(setq org-directory "~/Documents/org/")
(setq projectile-project-search-path "~/Documents/codes/")

;; rust
(setq racer-rust-src-path (concat (getenv "HOME") "/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library"))
