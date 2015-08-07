;; my favorite folding mode for emacs
;; Use `C-c @ C-s' to show entry
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

;;{{{ Built-in emacs configurations

;; (scroll-bar-mode -1)
(set-language-environment "UTF-8")
;; Here I toggle copyright on top of file
(add-hook 'c-mode-common-hook #'elide-head)
(global-set-key (kbd "C-c r") 'elide-head)
(global-set-key (kbd "C-c R") 
		(lambda ()
		  (interactive)
		  (elide-head t)))

;; (defun c-lineup-arglist-tabs-only (ignored)
;;   "Line up argument lists by tabs, not spaces"
;;   (let* ((anchor (c-langelem-pos c-syntactic-element))
;;          (column (c-langelem-2nd-pos c-syntactic-element))
;;          (offset (- (1+ column) anchor))
;;          (steps (floor offset c-basic-offset)))
;;     (* (max steps 1)
;;        c-basic-offset)))


;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             ;; Add kernel style
;;             (c-add-style
;;              "linux-tabs-only"
;;              '("linux" (c-offsets-alist
;;                         (arglist-cont-nonempty
;;                          c-lineup-gcc-asm-reg
;;                          c-lineup-arglist-tabs-only))))))


(add-hook 'c-mode-hook
          (lambda ()
            ;; Enable kernel mode for the appropriate files
	    (setq indent-tabs-mode t)
	    (setq show-trailing-whitespace t)
	    (c-set-style "linux")))
 

;; Show parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Org mode
(setq org-log-done t)

;; for easier auto complete
(global-set-key
 (kbd "C-; C-;") 
 (lambda ()
   (interactive)
   (dabbrev-expand nil)))

;; vi like % for paren matching
(global-set-key (kbd "C-%") 'goto-match-paren)
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

(setq inhibit-splash-screen t)
(load-theme 'deeper-blue t)
(defun add-to-end (list element)
  (append (list) 
	  (list element)))

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

(global-set-key (kbd "C-x tt") 'rename-file-and-buffer)

;put backups in temporary file directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(global-set-key (kbd "C-; r") 'sudo-edit)
(require 'recentf)
(recentf-mode 1)
(defun sudo-edit ()
  (interactive)
  (if (string= (substring buffer-file-name 0 21)
	       "/sudo:root@localhost:")
      (message "You're already here!")
    (find-file (concat "/sudo:root@localhost:"
		       buffer-file-name))))

;;}}}

;;{{{ Setup package manager

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
      '(use-package
	 folding
	 god-mode
	 racket-mode
	 vala-mode
	 unicode-fonts
	 cider
	 markdown-mode
	 paredit
	 rinari
	 scss-mode
	 web-mode
	 multi-web-mode
	 powerline
	 neotree
	 hydra
	 multiple-cursors
	 perl6-mode
	 magit
	 smex
	 ido-vertical-mode
	 ido-yes-or-no
	 evil-god-state))

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (curr-package my-package-list)
  (unless (package-installed-p curr-package)
    (package-install curr-package)))

;;}}}

;;{{{ Use-package requirement
(require 'use-package)
;;}}}

;;{{{ Packages configuration

  ;;{{{ Folding

(use-package folding
  :config
  ;; (autoload 'folding-mode          "folding" "Folding mode" t)
  ;; (load "~/.emacs.d/folding.el" 'nomessage 'noerror)
  (folding-add-to-marks-list 'ruby-mode "#{{{" "#}}}" nil t)
  (folding-add-to-marks-list 'c-mode "//{{{" "//}}}" nil t)
  (folding-mode-add-find-file-hook)
  (add-hook 'emacs-lisp-mode-hook #'folding-mode)
  (add-hook 'c-mode-common-hook #'folding-mode))

  ;;}}}

  ;;{{{ Unicode font loading
(global-set-key (kbd "C-; C-u") 
		(lambda ()
		  (interactive)
		  (require 'unicode-fonts)
		  (unicode-fonts-setup)))
  ;;}}}

  ;;{{{ Vala-mode
(use-package vala-mode
  :init
  (autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
  (setq auto-mode-alist
	(append '(("\\.vala$" . vala-mode)) auto-mode-alist)))
  ;;}}}

  ;;{{{ Perl6-mode
(use-package perl6-mode
  :ensure t
  :defer t)
  ;;}}}

  ;;{{{ Paredit

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
 (global-set-key (kbd "C-; k") 'delete-backward-char))

  ;;}}}

  ;;{{{ Hydra-splitter (hydra-mode)

(require 'hydra-examples)
(defhydra hydra-splitter (global-map "C-x aq")
  "splitter"
  ("g" text-scale-increase "in")
  ("f" text-scale-decrease "out")

  ("SPC" recenter-top-bottom)
  ("a" move-beginning-of-line)
  ("e" move-end-of-line)
  ("n" next-line)
  ("p" previous-line)
  ("h" hydra-move-splitter-left)
  ("j" hydra-move-splitter-down)
  ("k" hydra-move-splitter-up)
  ("l" hydra-move-splitter-right)
  ("0" delete-window "delete this")
  ("1"  delete-other-windows "delete other")
  ("2" (lambda ()
	 (interactive)
	 (split-window-below)
	 (windmove-down)) "split down")
  ("3" (lambda ()
	 (interactive)
	 (split-window-right)
	 (windmove-right)) "split right")
  ("o" other-window "other")
  (";" (lambda ()
	 (interactive)
	 (setq hydra-buff (buffer-name))
	 (delete-window)
	 (split-window-right)
	 (windmove-right)
	 (switch-to-buffer (get-buffer hydra-buff))) "Rshift")
  ("'" (lambda ()
	 (interactive)
	 (setq hydra-buff (buffer-name))
	 (delete-window)
	 (split-window-below)
	 (windmove-down)
	 (switch-to-buffer (get-buffer hydra-buff))) "Dshift"))

(global-set-key
 (kbd "C-x ae")
 (defhydra toggle ()
   "toggle"
   ("a" abbrev-mode "abbrev" :color blue)
   ("d" toggle-debug-on-error "debug" :color blue)
   ("f" auto-fill-mode "fill" :color blue)
   ("t" toggle-truncate-lines "truncate" :color blue)
   ("w" whitespace-mode "whitespace" :color blue)
   ("q" nil "cancel")))

  ;;}}}

  ;;{{{ Multiple-cursors
(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-x af") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-x aa") 'mc/mark-all-like-this))
  ;;}}}

  ;;{{{ Evil mode
(use-package evil-mode
  :init
  (evil-mode)
  (setq evil-insert-state-cursor   '((bar . 10) "green"))
  (setq evil-normal-state-cursor   '((bar . 7) "yellow"))
  (setq evil-visual-state-cursor   '((bar . 7) "magenta"))
  (setq evil-motion-state-cursor   '((bar . 7) "dark red"))
  (setq evil-replace-state-cursor  '((bar . 7) "deep sky blue"))
  (setq evil-operator-state-cursor '((bar . 7) "peru"))

  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (setq evil-default-state 'insert)
  ;; (evil-define-key 'normal global-map  [space] 'mc/edit-lines)
  ;; (setq evil-default-state 'evil-emacs-state)
  ;; (define-key evil-insert-state-map (kbd "jk") 'evil-normal-state)
  ;; (define-key evil-insert-state-map (kbd "jj") 'insert-jay)  
  ;; (defun insert-jay ()
  ;;   (interactive)
  ;;   (insert "j"))
  )
  ;; (define-key help-mode-map (kbd "i") 'evil-emacs-state)
  ;; (define-key grep-mode-map (kbd "i") 'evil-emacs-state)
  
  ;;}}}

  ;;{{{ Evil God State
(use-package evil-god-state
  :init
  (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
  (add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
  (add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))
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
  (load (expand-file-name
	 "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (require 'slime-autoloads)
  (slime-setup '(slime-fancy)))

  ;;}}}

  ;;{{{ Markdown mode (requires pandoc, not an emacs package)
(use-package markdown-mode
  :init
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  ;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
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

  ;;{{{ Neotree

(use-package neotree
  :bind
  ("<f2>" . neotree-toggle))

  ;;}}}

  ;;{{{ Web developement

    ;;{{{ Rinari for rails
;; For some useful rails erb that rinari does not have,
;; yes I even checked the source files
(defun rinari-erb-eval ()
  (interactive)
  (insert "<%  %>")
  (backward-char 3))

(defun rinari-erb-comment ()
  (interactive)
  (insert "<%#  %>")
  (backward-char 3))

(fset 'sandbox-rails
      [?\C-u ?\C-c ?' ?c ?- ?- ?s ?a ?n ?d ?b ?o ?x return])

(defun rinari-kill-server ()
  "If rinari-web-server is running, kill it"
  (interactive)
  (let ((rinari-web-server-buffer "*server*"))
    (when (get-buffer rinari-web-server-buffer)
      (set-process-query-on-exit-flag (get-buffer-process rinari-web-server-buffer) nil)
      (kill-buffer rinari-web-server-buffer))))

(use-package rinari
  :init
  (global-rinari-mode)
  :bind
  ("C-' C-e e" . rinari-erb-eval)
  ("C-' C-e c" . rinari-erb-comment)
  ("C-' C-s" . sandbox-rails)
  ("C-' C-k" . rinari-kill-server))
    ;;}}}

    ;;{{{ Scss mode for rails

(use-package scss-mode
  ;; :config
  ;; (setq exec-path (cons (expand-file-name "~/.rvm/gems/ruby-2.1.5/bin/sass") exec-path))
  :init
  (add-hook 'css-mode-hook
	    (lambda ()
	      (setq css-indent-offset 2))))

    ;;}}}

    ;;{{{ Web-mode

(defun cwebber-web-mode-customizations ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2))

(use-package web-mode
  :mode
  (("\\.erb\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook 'cwebber-web-mode-customizations))

    ;;}}}

    ;;{{{ Multi-Web-Mode

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

    ;;}}}

 ;;}}}

  ;;{{{ Guitar.el
;; (add-to-list 'load-path "~/stuff/guitar.el")
;; (load "guitar.el")
;; (require 'guitar-mode)
;;}}}

  ;;{{{ Powerline, eye candy, mostly to impress non emacs users
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
			    (mode-line (if active 'mode-line 'mode-line-inactive))
			    (face1 (if active 'my-powerline-active1 'my-powerline-inactive1))
			    (face2 (if active 'my-powerline-active2 'my-powerline-inactive2))
			    (separator-left (intern (format "powerline-%s-%s"
							    powerline-default-separator
							    (car powerline-default-separator-dir))))
			    (separator-right (intern (format "powerline-%s-%s"
							     powerline-default-separator
							     (cdr powerline-default-separator-dir))))
			    (lhs (list (powerline-raw "%*" nil 'l)
				       (powerline-buffer-size nil 'l)
				       (powerline-raw mode-line-mule-info nil 'l)
				       (powerline-buffer-id nil 'l)
				       (when (and (boundp 'which-func-mode) which-func-mode)
					 (powerline-raw which-func-format nil 'l))
				       (powerline-raw " ")
				       (funcall separator-left mode-line face1)
				       (when (boundp 'erc-modified-channels-object)
					 (powerline-raw erc-modified-channels-object face1 'l))
				       (powerline-major-mode face1 'l)
				       (powerline-process face1)
				       (powerline-minor-modes face1 'l)
				       (powerline-narrow face1 'l)
				       (powerline-raw " " face1)
				       (funcall separator-left face1 face2)
				       (powerline-vc face2 'r)))
			    (rhs (list (powerline-raw global-mode-string face2 'r)
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
			       (powerline-render rhs)))))))
    ;;}}}
  (my-powerline-default-theme))
  ;;}}}

  ;;{{{ Zone
;; (require 'zone)
;; (put 'erase-buffer 'disabled nil)
  ;;}}}

  ;;{{{ Workgroups
;; (use-package workgroups2
;;   :init
;;   (setq wg-prefix-key (kbd "C-c z"))
;;   (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
;;   (workgroups-mode 1))
  ;;}}}

;;}}}
