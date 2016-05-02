(add-hook 'org-mode-hook
	  (lambda()
	    (org-indent-mode t)))

(setq org-startup-truncated nil)

;; Automatically add, commit, and push when files change.
(defvar autocommit-dir-set '()
  "Set of directories for which there is a pending timer job")

(defun autocommit-schedule-commit (dn)
  "Schedule an autocommit (and push) if one is not already scheduled for the given dir."
  (if (null (member dn autocommit-dir-set))
      (progn
       (run-with-idle-timer 0 nil
        (lambda (dn)
          (setq autocommit-dir-set (remove dn autocommit-dir-set))
          (message (concat "Committing org files in " dn))
          (shell-command (concat "cd " dn " && git commit -m 'Updated org files.'"))
          (shell-command (concat "cd " dn " && git push & /usr/bin/true")))
        dn)
       (setq autocommit-dir-set (cons dn autocommit-dir-set)))))

(defun autocommit-after-save-hook ()
  "After-save-hook to 'git add' the modified file and schedule a commit and push in the idle loop."
  (when (eq major-mode 'org-mode)
    (let ((fn (buffer-file-name)))
      (message "git adding %s" fn)
      (shell-command (concat "git add " fn))
      (autocommit-schedule-commit (file-name-directory fn)))))

(add-hook 'after-save-hook 'autocommit-after-save-hook)

;;; Bullets 
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


(require 'hydra)
(defhydra hydra-org-clock (:color blue :hint nil)
  "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
	_i_n         _e_dit   _g_oto entry
	_c_ontinue   _q_uit   _d_isplay
	_o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))

;; https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates
(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   _p_ython        _i_ndex:
_a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
_s_rc     ^ ^         plant_u_ml      _H_TML:
_h_tml    ^ ^         ^ ^             _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("E" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("e" (progn
         (hot-expand "<s")
         (insert "emacs-lisp")
         (forward-line)))
  ("p" (progn
         (hot-expand "<s")
         (insert "python")
         (forward-line)))
  ("u" (progn
         (hot-expand "<s")
         (insert "plantuml :file CHANGE.png")
         (forward-line)))
  ("P" (progn
         (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

;; I bind it for myself like this:

(define-key org-mode-map "<"
  (lambda () (interactive)
     (if (looking-back "^")
         (hydra-org-template/body)
       (self-insert-command 1))))
(defun hot-expand (str)
  "Expand org template."
  (let (text)
    (when (region-active-p)
      (progn
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))))
    (insert str)
    (org-try-structure-completion)
    (when text (insert text))))


(define-key org-mode-map "<"
  (lambda () (interactive)
    (if (or (region-active-p) (looking-back "^"))
        (hydra-org-template/body)
      (self-insert-command 1))))

(evil-leader/set-key-for-mode 'org-mode "c" 'hydra-org-clock/body)
(evil-leader/set-key-for-mode 'org-mode "<" 'hydra-org-template/body)
