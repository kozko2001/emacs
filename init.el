(require 'cl)
(require 'package)

;; If first time not have melpa initialize everything to have melpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;(when (not package-archive-contents)
;  (package-refresh-contents))

;; define the packages that we need to install
(defvar my-packages
  '(evil
    company
    org
    magit
    helm
    helm-descbinds
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
    smooth-scrolling
    expand-region
    bbdb				; big brother database, to get all the mails in my gnus
    which-key
    evil-leader
    evil-god-state
    init-open-recentf
    gradle-mode
    rainbow-delimiters
    reveal-in-osx-finder
    hydra
    helm-describe-modes
    jabber
    request
    diminish
    ))

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

;; Scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(global-set-key (kbd "M-<up>") 'scroll-down)
(global-set-key (kbd "M-<down>") 'scroll-up)

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; helm
(require 'helm)
(require 'helm-descbinds)
(require 'helm-describe-modes)
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-h b")                        'helm-descbinds)
(global-set-key (kbd "C-h m")                        'helm-describe-modes)

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
(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
   :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
   :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

(evil-leader/set-key
  "!" 'hydra-flycheck/body)

;; ace window
(global-set-key (kbd "M-p") 'ace-window)

;; which-key
(require 'which-key)
(which-key-mode)

;; evil god state - Enter in god mode with ç and exit with ç in evil normal mode
(evil-define-key 'normal global-map "ç" 'evil-execute-in-god-state)
(evil-define-key 'god global-map [escape] 'evil-god-state-bail)
(global-set-key (kbd "C-ç") 'evil-god-state-bail)

;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(require 'init-open-recentf)
(init-open-recentf)


;; Loads the theme
(load-theme 'wombat)

;; avy
(require 'avy)
(setq avy-highlight-first t)
(setq avy-timeout-seconds 3)
(setq avy-background t)

;; GNUS
(require 'gnus)
(setq nnml-directory "~/gmail")
(setq message-directory "~/gmail")
;; (require 'bbdb)
;; (require 'bbdb-autoloads)
;; (setq bbdb-file "~/.bbdb"
;;       bbdb-offer-save 'auto
;;       bbdb-notice-auto-save-file t
;;       bbdb-expand-mail-aliases t
;;       bbdb-canonicalize-redundant-nets-p t
;;       bbdb-always-add-addresses t
;;       bbdb-complete-name-allow-cycling t
;;       )
;; (add-hook 'message-mode-hook
;;           '(lambda ()
;;              (bbdb-initialize 'message)
;;              (bbdb-initialize 'gnus)
;;              (local-set-key "<TAB>" 'bbdb-complete-name)))


;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-e") 'er/expand-region)

;; tags?
;; (require 'jtags)
;; (setq tags-table-list '("/Users/jordicoscolla/android-sdk/sources/android-23"
;;                         "/Users/jordicoscolla/tmp/comicstrips/spider/android/app"))
;; (setq tags-revert-without-query 't)

;; Android
;; (require 'android-mode)
;; (add-hook 'java-mode-hook 'android-mode)
(require 'gradle-mode)
(add-hook 'java-mode-hook 'gradle-mode)

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
(set-frame-font "Source Code Pro Light 14")
(add-to-list 'default-frame-alist
             '(font . "Source Code Pro Light 14"))

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'diminish)
(diminish 'projectile-mode "")
(diminish 'flycheck-mode "")
(diminish 'undo-tree-mode "")
(diminish 'helm-mode "")
(diminish 'lisp-interaction-mode "")
(diminish 'which-key-mode "")
(diminish 'yas-minor-mode "")


(load "evil_custom.el")
(load "docker_custom.el")
(load "org_custom.el")
(load "blogit-for-ghost.el")
(load "logcat-kzk.el")
(load "hipchat.el")
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
 '(hipchat-nickname "Jordi Coscolla")
 '(jabber-account-list (quote (("274357_1747663@chat.hipchat.com"))))
 '(jabber-activity-count-in-title t)
 '(jabber-auto-reconnect t)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (diminish request jabber hydra reveal-in-osx-finder rainbow-delimiters init-open-recentf evil-god-state which-key gradle-mode bbdb jtags ac-etags malabar-mode org-bullets expand-region android-mode xkcd popup popup-complete popup-kill-ring popup-switcher smooth-scrolling evil-leader evil-magit evil-multiedit evil-surround org-download ace-window zenburn-theme hlinum ranger dired-ranger tern-auto-complete company-tern coffee-mode ggtags js3-mode flycheck-pos-tip flycheck-status-emoji jira jira-markup-mode flycheck yasnippet mocha ac-js2 js2-mode helm-projectile org-projectile projectile docker 0blayout py-autopep8 python-mode docker-tramp powerline-evil helm magit org evil company)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-activity-face ((t (:foreground "salmon1" :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "gray100"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "burlywood1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "sea green")))))
