;hide passwords
(add-hook 'comint-output-filler-functions
	  'comint-watch-for-password-prompt)
;note: C-h k is very useful
;require: vala mode, markdown mode, and pandoc, nyan mode
;also overtone https://github.com/overtone/overtone/wiki/Getting-Started
;I also use cider C-u M-x cider-jack-in, mostly to brag my text editor is
;a musical instrament
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
; theme switching
(setq theme-list
      '(deeper-blue
	heroku
	birds-of-paradise-plus
	cyberpunk))

(global-set-key (kbd "C-x t") 'my-theme-switcher)
(defun my-theme-switcher ()
  (interactive)
  (load-theme (car theme-list) t)
  (setq theme-list 
	(add-to-end (cdr theme-list) 
		(car theme-list))))

(defun add-to-end (list element)
  (append (list) 
	  (list element)))

;put backups in temporary file directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;setup package manager
(require 'package)
(add-to-list 
 'package-archives 
 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

; fetch packages available
(unless package-archive-contents
  (package-refresh-contents))

; list of my packages
(setq my-package-list 
      '(god-mode
	vala-mode
	cider
	markdown-mode
	evil
	rinari
	birds-of-paradise-plus-theme
	heroku-theme
	cyberpunk-theme
	scss-mode
	web-mode
	))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (curr-package my-package-list)
  (unless (package-installed-p curr-package)
    (package-install curr-package)))

;vala mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(setq auto-mode-alist
   (append '(("\\.vala$" . vala-mode)) auto-mode-alist))

;markdown mode (requires pandoc, not an emacs package)
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
 '(markdown-command "/usr/bin/pandoc")

;my attemp of making an automatic emacs guitar
(global-set-key (kbd "C-x C-*") 'overtone)

(fset 'overtone
      [?\C-u ?\M-x ?c ?i ?d ?e ?r ?- ?j ?a ?c ?k ?- ?i ?n return
	     ?s ?t ?u ?f ?f ?/ ?o ?v ?e ?r ?t ?o ?n ?e ?/ ?t ?u 
	     ?t ?o ?r ?i ?a ?l return return ?a])

(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;god mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode-all)
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
(global-set-key (kbd "C-x C-j") 'other-window)

;rinari for rails
(require 'rinari)
(global-rinari-mode)

;rails console in sandbox
(global-set-key (kbd "C-' C-s") 'sandbox-rails)
   
(fset 'sandbox-rails
      [?\C-u ?\C-c ?' ?c ?- ?- ?s ?a ?n ?d ?b ?o ?x return])

(global-set-key (kbd "C-' C-k") 'rinari-kill-server)
(defun rinari-kill-server ()
  "If rinari-web-server is running, kill it"
  (interactive)
  (let ((rinari-web-server-buffer "*server*"))
    (when (get-buffer rinari-web-server-buffer)
      (set-process-query-on-exit-flag (get-buffer-process rinari-web-server-buffer) nil)
      (kill-buffer rinari-web-server-buffer))))

;scss mode for rails
(setq exec-path (cons (expand-file-name "~/.rvm/gems/ruby-2.1.5/bin/sass") exec-path))
(add-hook 'css-mode-hook
	  (lambda ()
	    (setq css-indent-offset 2)))

;webmode for .erb files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

; vi like % for paren matching
(global-set-key (kbd "C-%") 'goto-match-paren)
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/")
(load "nyani.el")
(require 'nyani)
