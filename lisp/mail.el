;; ;; GNUS
;; (require 'gnus)
;; (setq nnml-directory "~/gmail")
;; (setq message-directory "~/gmail")

;; (setq gnus-use-cache t)
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

;; IMAP + dovecot + offlineimap
;; (setq gnus-select-method
;;       '(nnimap "Mail"
;; 	       (nnimap-address "localhost")
;; 	       (nnimap-stream network)
;; 	       (nnimap-authenticator login)))

;; (setq user-mail-address "kozko2001@gmail.com")
;; (setq gnus-ignored-from-addresses "kozko2001@gmail.com")

;; Mailbox + offlineimap
(message "[...setting up gnus]...")

(setq gnus-select-method '(nntp "news.gmane.org"))

(setq smtp-mail-server "kozko2001@gmail.com")
(setq user-mail-address "kozko2001@gmail.com")
(setq message-send-mail-real-function 'smtpmail-send-it)

(setq gnus-secondary-select-methods '((nnmaildir "kozko2001MailBox"  (directory "~/Maildir/Gmail")))
)
(setq gnus-message-archive-group "nnmaildir+mymailbox:outbox")



;; Prevents SHR (eww) to use the background color
;; Thanks https://ryuslash.org/tag/gnus.html
(require 'shr)
(defun oni:shr-colorize-remove-last-arg (args)
  "If ARGS has more than 3 items, remove the last one."
  (if (> (length args) 3)
      (butlast args)
    args))

(with-eval-after-load 'shr
  (advice-add #'shr-colorize-region :filter-args
              #'oni:shr-colorize-remove-last-arg))
