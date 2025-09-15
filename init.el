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
  (load-theme 'ef-dream t)
  (set-frame-parameter nil 'alpha-background 98))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package elfeed
  :defer t
  :bind ("C-c e" . elfeed)
  :after (emms)
  :config
  (setq elfeed-feeds
	  '(("https://irreal.org/blog/?feed=rss2" blog emacs)
	    ("http://yummymelon.com/devnull/feeds/all.atom.xml" blog emacs)
	    ("https://protesilaos.com/codelog.xml" blog emacs)
	    ("https://protesilaos.com/poems.xml" blog poems)
	    ("https://protesilaos.com/commentary.xml" blog life)
	    ("https://shaiyaj.github.io/notes/feed.xml" blog)
	    ("https://xkcd.com/rss.xml" comic)
	    ("https://www.smbc-comics.com/comic/rss" comic)
	    ("https://archlinux.org/feeds/news/" arch linux tech)
	    ("https://www.debian.org/security/dsa" debian linux tech)
	    ("https://wolfgirl.dev/blog/rss.xml" blog tech prog)
	    ("https://izzys.casa/index.xml" blog tech prog)
	    ("https://faultlore.com/blah/rss.xml" blog tech prog)
	    ("https://nexy.blog/feed_rss_created.xml" blog tech)
	    ("https://welltypedwit.ch/rss.xml" tech blog)
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

(use-package emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv)))

(use-package elpher)

(ido-mode 1)

(use-package ibuffer
  :ensure nil
  :straight nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . (lambda ()
			  (ibuffer-switch-to-saved-filter-groups "Buffers")))
  :config
  (setq ibuffer-expert nil
	ibuffer-saved-filter-groups '(("Buffers"
				       ("Programming" (or (mode . clojure-mode)
							  (mode . cider-mode)
							  (mode . c-mode)
							  (mode . java-mode)
							  (mode . emacs-lisp-mode)
							  (mode . mhtml-mode)
							  (mode . makefile-gmake-mode)))
				       ("Documents" (or (mode . org-mode)
							(mode . markdown-mode)
							(mode . reader-mode)))
				       ("Magit" (or (mode . magit-status-mode)
						    (mode . magit-diff-mode)
						    (mode . magit-process-mode)))
				       ("Emacs" (or
						 (mode . emacs-lisp-mode)
						 (name . "^\\*Help\\*$")
						 (name . "^\\*Custom.*")
						 (name . "^\\*Org Agenda\\*$")
						 (name . "^\\*info\\*$")
						 (name . "^\\*scratch\\*$")
						 (name . "^\\*Backtrace\\*$")
						 (name . "^\\*Messages\\*$")
						 (name . "^\\*GNU Emacs\\*$")
						 (mode . emacs-news-view-mode)))
				       ("Unsaved" (modified))))))

(use-package reader
  :straight '(reader
	      :type git
	      :host codeberg
	      :repo "divyaranjan/emacs-reader"
	      :files ("*.el" "render-core.so")
	      :pre-build ("make" "all")))

(use-package paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (clojure-mode    . enable-paredit-mode)
	 (scheme-mode     . enable-paredit-mode))
  :after (cider))

(use-package rainbow-delimiters
  :defer nil
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
	 (clojure-mode    . rainbow-delimiters-mode)
	 (scheme-mode     . rainbow-delimiters-mode))
  :after (cider))

(use-package cider)

(use-package geiser-guile
  :hook ((scheme-mode . geiser)))

(use-package proof-general)

(use-package haskell-mode
  :bind
  (:map haskell-mode-map ("C-c C-c" . haskell-compile))
  (:map haskell-cabal-mode-map ("C-c C-c" . haskell-compile))
  :config
  (let ((cam-ghcup-path (expand-file-name "~/.ghcup/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" cam-ghcup-path))
    (add-to-list 'exec-path cam-ghcup-path)))

(use-package org
  :straight nil
  :ensure nil
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-directory (concat (getenv "HOME") "/Documents/Agenda/")
	org-agenda-files (list org-directory)
	org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))
	org-default-notes-file (concat (car org-agenda-files) "notes.org")
        org-capture-templates '(("t" "Todo")
				("tt" "Standard Thing" entry (file org-default-notes-file)
				 "* TODO %?\n %a\n DEADLINE: %^t")
				("tw" "Work Thing" entry (file org-default-notes-file)
				 "* TODO %?\n SCHEDULED: %^t")
				("a" "Assignment" entry (file "assignments.org")
				 "* TODO %?\n DEADLINE: %^t")
				("d" "Diary Entry" entry (file "diary.org")
				  "* %?\n %t")
				("i" "Ideas")
				("ip" "Programming Ideas" entry (file "ideas.org")
				 "* %?\n %i\n\n %t"))))

(use-package markdown-mode)
