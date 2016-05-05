;;; RSS
(require 'elfeed)
(require 'elfeed-goodies)
(require 'evil)

(elfeed-goodies/setup)

(setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  ; no autotagging
	("http://www.commitstrip.com/en/feed/" webcomic)
	("https://www.reddit.com/r/androiddev/.rss" android programming)
	("https://www.reddit.com/r/programming/.rss" programming)
	("https://www.reddit.com/r/emacs/.rss" programming emacs)
        ("http://nedroid.com/feed/" webcomic)))

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))

;; elfeed doesn't work well with evil so disable, any way... you are not going to edit
(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)

;; ,p
(evil-leader/set-key "r" 'elfeed)
