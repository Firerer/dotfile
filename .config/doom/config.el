;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Di Liu"
      user-mail-address "liudi12631@email.com")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq ;;doom-theme 'doom-one
 display-line-numbers-type 'relative

 ;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
 ;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for presentations or streaming.
 ;; - `doom-unicode-font' -- for unicode glyphs
 ;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
 ;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
 ;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
 ;; refresh your font settings. If Emacs still can't find your font, it likely
 ;; wasn't installed correctly. Font issues are rarely Doom issues!
 doom-font (font-spec ;:family "Hack Nerd Font"
            :size 22)
 ;; doom-unicode-font doom-font -- not running
 )

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings.
;; The exceptions to this rule:
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
(setq org-directory "~/Documents/org")

(after! projectile
  (setq projectile-project-search-path
        '("~/Projects"))
  )

;; set recolor-darkcolor "#f8f8f2"
;; set recolor-lightcolor "#1b1b1b"

(setq pdf-view-midnight-colors '("#f8f8f2" . "#1b1b1b"))
(add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode)))

(set-email-account!
 "liudi2631"
 '((user-full-name . "Di Liu")
   (smtpmail-smtp-user . "liudi12631")
   )
 t)

(setq +mu4e-gmail-accounts '(("liudi12631@gmail.com" . "/liudi12631")))
