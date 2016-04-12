(require 'cl)
(require 'package)

;; make the cursor white so, everybody can see it!!!
(set-cursor-color "#ffffff") 

;; If first time not have melpa initialize everything to have melpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; define the packages that we need to install
(defvar my-packages '(evil company org magit helm powerline-evil docker projectile markdown-mode flychech))

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


;; helm
(require 'helm)
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(setq helm-M-x-fuzzy-match t)
(helm-mode 1)

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; Run evilmode
(require 'evil)
(evil-mode 1)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Projectile
(projectile-global-mode)
(helm-projectile-on)
(setq projectile-completion-system 'helm)

;; FlyCheck
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Loads the theme
(load-theme 'wombat)

;; JS2 + JSX
(defun modify-syntax-table-for-jsx ()
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<"))

(add-hook 'js2-mode-hook 'modify-syntax-table-for-jsx)

(require 'powerline)
(powerline-default-theme)

;; All lisp code that must be loaded at init of emacs, a bit of organizatin
(let ((default-directory "~/.emacs.d/lisp/"))
 (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/lisp/")


(load "evil_custom.el")
(load "docker_custom.el")
(load "org_custom.el")
(load "blogit-for-ghost.el")

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
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (flycheck yasnippet mocha ac-js2 js2-mode helm-projectile org-projectile projectile docker 0blayout py-autopep8 python-mode docker-tramp powerline-evil helm magit org evil company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
