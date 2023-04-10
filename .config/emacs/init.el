;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(defun install-use-package ()
  (when (not (package-installed-p 'use-package))
    (package-install 'use-package)))
(condition-case nil
    (install-use-package)
  (error
   (package-refresh-contents)
   (install-use-package)))
(require 'use-package)


(use-package magit
  :ensure t
  :config
  :bind (("C-M-g" . magit-status)))

;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; looks
(set-frame-font "Hack 12" nil t)
(load-theme 'deeper-blue t)
;(use-package doom-themes
;  :ensure t
;  :config
;  (load-theme 'doom-one t)
;  (doom-themes-visual-bell-config)
;
;  )



(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; 50MB gc threshold
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Turn off some unneeded UI elements
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil-collection evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
