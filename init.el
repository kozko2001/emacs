(require 'cl)
(require 'package)

;; If first time not have melpa initialize everything to have melpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; define the packages that we need to install
(defvar my-packages '(evil company org magit helm powerline-evil docker projectile))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop littering everywhere with save files, put them somewhere
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(tool-bar-mode 0) ; Hide toolbar
(menu-bar-mode 0) ; Hide menu
(scroll-bar-mode 0) ; Hide scrollbar
(menu-bar-mode -1)
(global-linum-mode 1) ; display line numbers
(column-number-mode 1) ; display column/row of cursor in mode-line
(show-paren-mode 1)

;; helm
(require 'helm)
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(helm-mode 1)

;; Run evilmode
(require 'evil)
(evil-mode 1)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Loads the theme
(load-theme 'wombat)

(require 'powerline)
(powerline-default-theme)

;; All lisp code that must be loaded at init of emacs, a bit of organizatin
(let ((default-directory "~/.emacs.d/lisp/"))
 (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/lisp/")


(load "evil_custom.el")
(load "docker_custom.el")
(load "org_custom.el")

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

;; Added Automatically
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-projectile org-projectile projectile docker 0blayout py-autopep8 python-mode docker-tramp powerline-evil helm magit org evil company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
