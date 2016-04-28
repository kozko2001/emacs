(require 'cl)
(require 'package)

;; If first time not have melpa initialize everything to have melpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; define the packages that we need to install
(defvar my-packages
  '(evil
    company
    org
    magit
    helm
    powerline-evil
    docker
    projectile
    markdown-mode
    flycheck
    tern
    tern-auto-complete
    hlinum
    linum
    ace-window
    org-download
    android-mode
    org-bullets
    undo-tree
    avy
    expand-region))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop littering everywhere with save files, put them somewhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))
;; Stop backups
(setq make-backup-files nil)


(tool-bar-mode 0) ; Hide toolbar
(menu-bar-mode 0) ; Hide menu
(scroll-bar-mode 0) ; Hide scrollbar
(menu-bar-mode -1)
(global-linum-mode 1) ; display line numbers
(column-number-mode 1) ; display column/row of cursor in mode-line
(show-paren-mode 1)
(global-auto-revert-mode 1)
(setq pop-up-windows nil)          ;; No popup windows

;; Scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(global-set-key (kbd "M-<u>") 'scroll-down)
(global-set-key (kbd "M-<down>") 'scroll-up)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; helm
(require 'helm)
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(setq helm-projectile-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(helm-mode 1)

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; Run evilmode
(require 'evil)
(require 'evil-leader)
(setq evil-leader/leader ",")
(global-evil-leader-mode)
(evil-mode 1)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Projectile
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-completion-system 'helm)

;; FlyCheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ace window
(global-set-key (kbd "M-p") 'ace-window)

;; Loads the theme
(load-theme 'wombat)

;; avy
(require 'avy)
(setq avy-highlight-first t)
(setq avy-timeout-seconds 3)
(setq avy-background t)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; tags?
;; (require 'jtags)
;; (setq tags-table-list '("/Users/jordicoscolla/android-sdk/sources/android-23"
;;                         "/Users/jordicoscolla/tmp/comicstrips/spider/android/app"))
;; (setq tags-revert-without-query 't)

;; Android
;; (require 'android-mode)
;; (add-hook 'java-mode-hook 'android-mode)


;; JS2 + JSX
(defun modify-syntax-table-for-jsx ()
  "Better identation for jsx don't know if really works."
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<"))

(add-hook 'js2-mode-hook 'modify-syntax-table-for-jsx)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

(require 'powerline)
(powerline-default-theme)

;; All lisp code that must be loaded at init of emacs, a bit of organizatin
(let ((default-directory "~/.emacs.d/lisp/"))
 (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; theme font and lines
(require 'linum)
(require 'hlinum)
(setq linum-format " %3d ")
;; (set-frame-font "Source Code Pro Light 14")
;; (add-to-list 'default-frame-alist
;;              '(font . "Source Code Pro Light 14"))

(load "evil_custom.el")
(load "docker_custom.el")
(load "org_custom.el")
(load "blogit-for-ghost.el")
; (load "android.el")

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'nil)
  (setq mac-right-command-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

;; Added Automatically
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-builder (quote gradle))
 '(android-mode-sdk-dir /Users/jordicoscolla/android-sdk)
 '(custom-safe-themes
   (quote
    ("c4465c56ee0cac519dd6ab6249c7fd5bb2c7f7f78ba2875d28a50d3c20a59473" default)))
 '(flycheck-javascript-eslint-executable "/usr/local/bin/eslint")
 '(flycheck-pos-tip-timeout 1)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (jtags ac-etags malabar-mode org-bullets expand-region elogcat android-mode xkcd popup popup-complete popup-kill-ring popup-switcher smooth-scrolling evil-leader evil-magit evil-multiedit evil-surround org-download ace-window zenburn-theme hlinum ranger dired-ranger tern-auto-complete company-tern coffee-mode ggtags js3-mode flycheck-pos-tip flycheck-status-emoji jira jira-markup-mode flycheck yasnippet mocha ac-js2 js2-mode helm-projectile org-projectile projectile docker 0blayout py-autopep8 python-mode docker-tramp powerline-evil helm magit org evil company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
