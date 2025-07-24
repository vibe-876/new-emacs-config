(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-ensure nil)

(use-package magit)

(use-package ef-themes
  :defer nil
  :config
  (load-theme 'ef-symbiosis t)
  (set-frame-parameter nil 'alpha-background 98))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package elfeed
  :defer t
  :bind ("C-c e" . elfeed)
  :config
  (setq elfeed-feeds
	  '(("https://planet.emacslife.com/atom.xml" blog emacs)
	    ("https://summeremacs.github.io/posts/index.xml" blog emacs)
	    ("https://xkcd.com/rss.xml" comic)
	    ("https://www.smbc-comics.com/comic/rss" comic)
	    ("https://www.monkeyuser.com/index.xml" comic)
	    ("https://archlinux.org/feeds/news/" arch linux tech)
	    ("https://wolfgirl.dev/blog/rss.xml" blog tech prog)
	    ("https://izzys.casa/index.xml" blog tech prog)
	    ("https://faultlore.com/blah/rss.xml" blog tech prog)
	    ("https://welltypedwit.ch/rss.xml" tech blog)
	    ("https://feeds.libsyn.com/499093/rss" tech podcast)
	    ("http://hackaday.libsyn.com/rss" tech podcast)
	    ("https://rustacean-station.org/podcast.rss" tech prog podcast)
	    ("https://risky.biz/feeds/risky-business-news/" tech security podcast)
	    ;; Leadhead
	    ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3_kehZbfRz-KrjXIqeIiPw" blog video)
	    ;; Wendigoon
	    ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3cpN6gcJQqcCM6mxRUo_dA" video spooky)
	    ;; SOG
	    ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtMVHI3AJD4Qk4hcbZnI9ZQ" video blog)
	    ;; Philosophy Tube
	    ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2PA-AKmVpU6NKCGtZq_rKQ" video phil)
	    ("https://www.youtube.com/feeds/videos.xml?channel_id=UCctWUfejRybO1cCT6DmIRqQ" video speedrun))))

(defun cam/elfeed-remove-iplayer ()
  "I don't want iplayer or sounds from bbc,
just the articles. This marks them as read."
  (interactive)
  (let ((new-filter "@1-month-ago +unread +bbc !article"))

    (setq elfeed-search-filter new-filter)
    (elfeed-search-update :force)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)
    (elfeed-search-clear-filter)
    (message "BBC iplayer stuff removed.")))

(global-set-key (kbd "C-c a") 'org-agenda)

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (clojure-mode    . enable-paredit-mode))
  :after (cider))

(use-package rainbow-delimiters
  :defer nil
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode    . rainbow-delimiters-mode))
  :after (cider))

(use-package cider)

(use-package proof-general)

(use-package haskell-mode
  :bind
  (:map haskell-mode-map ("C-c C-c" . haskell-compile))
  (:map haskell-cabal-mode-map ("C-c C-c" . haskell-compile))
  :config
  (let ((cam-ghcup-path (expand-file-name "~/.ghcup/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" cam-ghcup-path))
    (add-to-list 'exec-path cam-ghcup-path)))

;(add-hook org-mode-hook flyspell-mode)

(use-package markdown-mode)
