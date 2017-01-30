;; my favorite folding mode for emacs
;; Use `C-c @ C-s' to show entry
;;

;;{{{ Instructions For Folding

;; C-c @ C-x hide entry
;; C-c @ C-u Get out/in of folding
;; C-c @ C-w Fold whole buffer
;; C-c @ C-o Unfold whole buffer

;;}}}

;;{{{ License (GPL3)

;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;}}}

;;{{{ Package manager

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; fetch packages available
(unless package-archive-contents
  (package-refresh-contents))

;; list of my packages
(setq my-package-list
      '(indent-guide
	use-package
	folding
	aggressive-indent
	ace-window
	racket-mode
	company-emoji
	emojify
	slime
	markdown-mode
	paredit
	powerline
	perl6-mode
	magit
	smex
	ido-vertical-mode
	ido-yes-or-no
	haskell-mode
	lua-mode
	org-journal
	ac-c-headers))

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (curr-package my-package-list)
  (unless (package-installed-p curr-package)
    (package-install curr-package)))

;;}}}

;;{{{ Basic setting
(server-start)
(setq inhibit-splash-screen t)
(load-theme 'deeper-blue t)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-fringe-mode 0)
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(global-prettify-symbols-mode 1)
(require 'indent-guide)
(indent-guide-global-mode)
(setq display-time-day-and-date t)
(display-time-mode 1)
;;}}}

;;{{{ Basic key-bindings
;; for easier auto complete
(global-set-key
 (kbd "C-; C-;")
 (lambda ()
   (interactive)
   (dabbrev-expand nil)))

(global-set-key (kbd "C-.") #'goto-line)

(require 'recentf)
(recentf-mode 1)

(defun sudo-edit ()
  (interactive)
  (find-file (concat "/sudo:root@localhost:"
		     buffer-file-name)))

(global-set-key (kbd "C-; r") 'sudo-edit)

;; Here I toggle copyright on top of file
(add-hook 'c-mode-common-hook #'elide-head)

(global-set-key (kbd "C-; l") 'elide-head)

(global-set-key (kbd "C-; L")
		(lambda ()
		  (interactive)
		  (elide-head t)))

;; vi like % for paren matching
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis,
   otherwise insert %. vi style of % jumping to
   matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(")
	 (forward-list 1) (backward-char 1))
	((looking-at "\\s\)")
	 (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-%") 'goto-match-paren)

(defun sort-lines (reverse beg end)
  "Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:

REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((inhibit-field-text-motion t))
	(sort-subr reverse 'forward-line 'end-of-line)))))

(global-set-key (kbd "C-; a") 'sort-lines)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file name new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))

(global-set-key (kbd "C-; n") 'rename-file-and-buffer)

(global-set-key (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;}}}

;;{{{ Misc
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(setq eshell-visual-commands '("wicd-curses"))

;; Org mode
(setq org-log-done t)

(defun add-to-end (list element)
  (append (list)
	  (list element)))

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(setq asm-comment-char ?#)
;;}}}

;;{{{ Use-package requirement
(require 'use-package)
;;}}}

;;{{{ Specific config

;;{{{ exwm
(require 'exwm)
(require 'exwm-config)

(exwm-config-ido)

(setq exwm-workspace-number 4)

(add-hook 'exwm-update-class-hook
	  (lambda ()
	    (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
			(string= "gimp" exwm-instance-name))
	      (exwm-workspace-rename-buffer exwm-class-name))))

(add-hook 'exwm-update-title-hook
	  (lambda ()
	    (when (or (not exwm-instance-name)
		      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
		      (string= "gimp" exwm-instance-name))
	      (exwm-workspace-rename-buffer exwm-title))))

(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-f") #'exwm-layout-set-fullscreen)

(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
		      `(lambda ()
			 (interactive)
			 (exwm-workspace-switch-create ,i))))

(exwm-input-set-key (kbd "s-&")
		    (lambda (command)
		      (interactive (list (read-shell-command "$ ")))
		      (start-process-shell-command command nil command)))

(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

(setq default-simulation-keys
      '(([?\C-b] . left)
	([?\C-f] . right)
	([?\C-p] . up)
	([?\C-n] . down)
	([?\C-a] . home)
	([?\C-e] . end)
	([?\M-v] . prior)
	([?\C-v] . next)
	([?\C-d] . delete)
	([?\C-k] . (S-end delete))))

(exwm-input-set-simulation-keys default-simulation-keys)

(add-hook 'exwm-manage-finish-hook
	  (lambda ()
	    (when (and exwm-class-name
		       (string= exwm-class-name "Firefox"))
	      (exwm-input-set-local-simulation-keys
	       (append default-simulation-keys
		       '(([?\C-s] . ?\C-f)
			 ([?\C-i] . ?\C-k)))))))

(exwm-input-set-key (kbd "s-i")
		    (lambda ()
		      (interactive)
		      (start-process-shell-command "firefox" nil "firefox")))

(exwm-enable)
;;}}}

;;{{{ C
(add-hook 'c-mode-hook #'aggressive-indent-mode)

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  '(("->" . ?→)
		    ("!=" . ?≠)
		    ("<=" . ?≤)
		    (">=" . ?≥)))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (add-to-list 'ac-sources 'ac-source-c-headers)
	    (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    (setq show-trailing-whitespace t)
	    (c-set-style "linux")))
;;}}}

;;{{{ elisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq prettify-symbols-alist
		  '(("lambda" . ?λ)
		    ("->" . ?→)
		    ("!=" . ?≠)
		    ("<=" . ?≤)
		    (">=" . ?≥)))))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(global-set-key (kbd "C-; c")
		(lambda ()
		  (interactive)
		  (byte-recompile-directory
		   (file-name-directory (buffer-file-name)))))

(global-set-key (kbd "C-; C")
		(lambda ()
		  (interactive)
		  (byte-recompile-file
		   (buffer-file-name))))
;;}}}

;;{{{ Latex previewing
;; http://bnbeckwith.com/blog/org-mode-tikz-previews-on-windows.html
(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
(setq org-latex-create-formula-image-program 'imagemagick)

;;}}}

;;{{{ Org
(add-hook 'org-mode-hook
	  (lambda()
	    (flyspell-mode 1)
	    (auto-fill-mode)))
;;}}}

;;{{{ Folding
(use-package folding
  :config
  (folding-add-to-marks-list 'ruby-mode "#{{{" "#}}}" nil t)
  (folding-add-to-marks-list 'c-mode "//{{{" "//}}}" nil t)
  (folding-mode-add-find-file-hook)
  (add-hook 'emacs-lisp-mode-hook #'folding-mode)
  (add-hook 'c-mode-common-hook #'folding-mode))
;;}}}

;;{{{ Unicode font loading

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux (Symbola or Emoji One Color)
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)
(require 'company-emoji)
(add-to-list 'company-backends 'company-emoji)
(add-hook 'after-init-hook #'global-emojify-mode)

;;}}}

;;{{{ Perl6-mode
(use-package perl6-mode
  :ensure t :defer t)
;;}}}

;;{{{ Haskell-mode
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))
;;}}}

;;{{{ Paredit
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook
	  'override-slime-repl-bindings-with-paredit)

(use-package paredit
  :init
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'racket-mode-hook           #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)
  (global-set-key (kbd "C-; k") 'delete-backward-char))
;;}}}

;;{{{ Ido
(use-package ido
  :init
  (ido-mode t))
;;}}}

;;{{{ Ido vetical mode
(use-package ido-vertical-mode
  :config
  (setq ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
		      :background nil
		      :foreground "orange")
  (set-face-attribute 'ido-vertical-only-match-face nil
		      :background nil
		      :foreground nil)
  (set-face-attribute 'ido-vertical-match-face nil
		      :foreground nil)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))
;;}}}

;;{{{ Ido yes or no
(use-package ido-yes-or-no
  :init
  (ido-yes-or-no-mode))
;;}}}

;;{{{ Slime
(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl"))
;;}}}

;;{{{ Markdown mode (requires pandoc, not an emacs package)
(use-package markdown-mode
  :init
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  :mode (("\\.text\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode))
  :config
  (custom-set-variables
   '(markdown-command "pandoc")))
;;}}}

;;{{{ Smex
(use-package smex
  :init
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))
;;}}}

;;{{{ Powerline
(use-package powerline
  :init
  (require 'powerline-themes)
  (require 'powerline-separators)
  (require 'cl-lib)

  ;;{{{ Ugly code for custom theme
  (defface my-powerline-active1 '((t (:background "SkyBlue3" :inherit mode-line)))
    "My Powerline face 1."
    :group 'powerline)
  (defface my-powerline-active2 '((t (:background "SteelBlue" :inherit mode-line)))
    "My Powerline face 2."
    :group 'powerline)
  (defface my-powerline-inactive1
    '((t (:background "SteelBlue4" :inherit mode-line-inactive)))
    "My Powerline face 1."
    :group 'powerline)
  (defface my-powerline-inactive2
    '((t (:background "DodgerBlue4" :inherit mode-line-inactive)))
    "My Powerline face 2."
    :group 'powerline)

  (defun my-powerline-default-theme ()
    "Setup the default mode-line."
    (interactive)
    (setq-default mode-line-format
		  '("%e"
		    (:eval
		     (let* ((active (powerline-selected-window-active))
			    (mode-line
			     (if active
				 'mode-line
			       'mode-line-inactive))
			    (face1
			     (if active
				 'my-powerline-active1
			       'my-powerline-inactive1))
			    (face2
			     (if active
				 'my-powerline-active2
			       'my-powerline-inactive2))
			    (separator-left
			     (intern
			      (format "powerline-%s-%s"
				      'butt 'left)))
			    (separator-right
			     (intern
			      (format "powerline-%s-%s"
				      'wave 'right)))
			    (lhs
			     (list
			      (powerline-raw "%*" nil 'l)
			      (powerline-buffer-size nil 'l)
			      (powerline-raw mode-line-mule-info nil 'l)
			      (powerline-buffer-id nil 'l)
			      (when (and (boundp 'which-func-mode)
					 which-func-mode)
				(powerline-raw which-func-format nil 'l))
			      (powerline-raw " ")
			      (funcall separator-left mode-line face1)
			      (when (boundp 'erc-modified-channels-object)
				(powerline-raw erc-modified-channels-object
					       face1 'l))
			      (powerline-major-mode face1 'l)
			      (powerline-process face1)
			      (powerline-minor-modes face1 'l)
			      (powerline-narrow face1 'l)
			      (powerline-raw " " face1)
			      (funcall separator-left face1 face2)
			      (powerline-vc face2 'r)))
			    (rhs (list
				  (powerline-raw global-mode-string face2 'r)
				  (funcall separator-right face2 face1)
				  (powerline-raw "%4l" face1 'l)
				  (powerline-raw ":" face1 'l)
				  (powerline-raw "%3c" face1 'r)
				  (funcall separator-right face1 mode-line)
				  (powerline-raw " ")
				  (powerline-raw "%6p" nil 'r)
				  (powerline-hud face2 face1))))
		       (concat (powerline-render lhs)
			       (powerline-fill face2 (powerline-width rhs))
			       (powerline-render rhs))))))))
;;}}}
(my-powerline-default-theme)
;;}}}

;;}}}

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "pandoc")
 '(package-selected-packages
   (quote
    (exwm ac-c-headers darkroom aggressive-indent ace-window org-journal lua-mode haskell-mode ido-yes-or-no ido-vertical-mode smex magit perl6-mode powerline paredit markdown-mode slime emojify company-emoji racket-mode folding use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
