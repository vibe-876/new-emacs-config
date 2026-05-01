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

(server-start)

(use-package magit)

(use-package ef-themes
  :defer nil
  :config
  (load-theme 'ef-dream t)
  (set-frame-parameter nil 'alpha-background 98))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq newsticker-url-list
      '(("Polywolf" "https://wolfgirl.dev/blog/rss.xml")
	("Irreal" "https://irreal.org/blog/?feed=rss2")
	("Prot -- Poems" "https://protesilaos.com/poems.xml")
	("Prot -- Programming" "https://protesilaos.com/codelog.xml")
	("welltypedwitch" "https://welltypedwit.ch/rss.xml")
	("Arch News" "https://archlinux.org/feeds/news/")
	("Charles Choi" "http://yummymelon.com/devnull/feeds/all.atom.xml")
	("stylewarning" "https://www.stylewarning.com/index.xml")
	("quote nil" "https://quotenil.com/blog.rss")))

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
							  (mode . scala-mode)
							  (mode . emacs-lisp-mode)
							  (mode . mhtml-mode)
							  (mode . makefile-gmake-mode)
							  (mode . sh-mode)))
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

(use-package geiser
  :hook ((scheme-mode . geiser-mode)))

(use-package geiser-guile
  :after geiser)

(use-package sly
  :config
  (setq inferior-lisp-program "/sbin/sbcl"))

(use-package dash)
(use-package lsp-mode)

(use-package lean4-mode
  :commands lean4-mode
  :straight (lean4-mode :type git :host github
			:repo "leanprover-community/lean4-mode"
			:files ("*.el" "data"))
  :config
  (add-to-list 'exec-path (concat (getenv "HOME")
				  "/.elan/bin")))

(use-package haskell-mode
  :bind
  (:map haskell-mode-map ("C-c C-c" . haskell-compile))
  (:map haskell-cabal-mode-map ("C-c C-c" . haskell-compile))
  :config
  (let ((cam-ghcup-path (expand-file-name "~/.ghcup/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" cam-ghcup-path))
    (add-to-list 'exec-path cam-ghcup-path)))

(defun cam/run-java (java-file)
  "Simple running of a java program, using comint mode.
See `cam/inferior-java-mode'."
  (interactive "bInit File: ")
  (unless (executable-find "java")
      (message "ERROR: Couldn't find Java executable!"))
  
  (let* ((class-file (file-name-base java-file))
	 (inferior-buffer-name "Java Process")
	 (default-directory (concat default-directory "../build")))
    (switch-to-buffer (concat "*Java Process*"))
    (erase-buffer)
    (make-comint-in-buffer inferior-buffer-name
			   nil
			   (executable-find "java")
			   nil
			   class-file)))

(defun cam/java-save-and-recompile nil
  (interactive)
  (save-buffer)
  (recompile))

;; (add-hook 'java-mode-hook (lambda () (local-set-key (kbd "C-c C-l") 'cam/run-java)))
;; (add-hook 'java-mode-hook (lambda () (local-set-key (kbd "C-x C-s") 'cam/java-save-and-recompile)))
;; (add-hook 'java-mode-hook (lambda ()
;; 			    (setq-local compile-command "ant -emacs -find build.xml")))


(global-set-key (kbd "M-c") 'compile)

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package scala-repl
  :after scala-mode
  :bind (("C-c C-l" . scala-repl-eval-buffer)))

(use-package sbt-mode
  :commands sbt-start sbt-commmand
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package org
  :straight nil
  :ensure nil
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-directory (concat (getenv "HOME") "/Documents/Agenda/")
	org-agenda-files (list org-directory)
	org-agenda-include-diary t
	org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))
	org-default-notes-file (concat (car org-agenda-files) "notes.org")
        org-capture-templates '(("t" "Todo")
				("tt" "Standard Thing" entry (file org-default-notes-file)
				 "* TODO %?\n DEADLINE: %^t")
				("tw" "Work Thing" entry (file "work.org")
				 "* TODO %?\n DEADLINE: %^t")
				("a" "Assignment" entry (file "assignments.org")
				 "* TODO %?\n DEADLINE: %^t")
				("d" "Diary Entry" entry (file "diary.org")
				  "* %?\n %t")
				("i" "Ideas")
				("ip" "Programming Ideas" entry (file "ideas.org")
				 "* %?\n %i\n\n %t"))
	org-html-head ""
	org-html-head-extra ""
	org-html-head-include-default-style nil
	org-html-head-include-scripts nil
	org-html-preamble nil
	org-html-postamble nil
	org-html-use-infojs nil
	org-export-with-email t
	org-export-with-toc nil))


(defun cam/start-slideshow ()
  (interactive)
  (text-scale-set 4)
  (org-tree-slide-mode 1))

(defun cam/end-slideshow ()
  (interactive)
  (text-scale-set 0)
  (org-tree-slide-mode 0))


(use-package org-tree-slide
  :after org
  :bind (("C-c s" . cam/start-slideshow)
	 ("C-c q" . cam/end-slideshow)))

(use-package xenops
  :after org
  :config
  (add-hook 'org-mode-hook #'xenops-mode)
  (add-hook latex-mode-hook #'xenops-mode)
  (add-hook 'LaTeX-mode-hook #'xenops-mode)
  (setq xenops-reveal-on-entry t))

(use-package markdown-mode)

(defun cam/vector-typst (vector-name)
  (interactive "sVector Name: ")
  (insert "underline(" vector-name ")"))

(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :bind (("C-c v" . cam/vector-typst)))
