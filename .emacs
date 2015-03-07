;hide passwords
(add-hook 'comint-output-filler-functions
	  'comint-watch-for-password-prompt)
;show parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;org mode
(setq org-log-done t)

;note: C-h k is very useful
;require: vala mode, markdown mode, and pandoc, nyan mode
;also overtone https://github.com/overtone/overtone/wiki/Getting-Started
;I also use cider C-u M-x cider-jack-in, mostly to brag my text editor is
;a musical instrament
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
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
	slime
	paredit
	rinari
	scss-mode
	web-mode
	powerline
	neotree
	projectile
	projectile-rails
	hydra
	multiple-cursors
	workgroups2
	magit
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

;paredit
   (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;hydra-splitter
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

(global-set-key (kbd "C-x af") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x aa") 'mc/mark-all-like-this)

;slime
(require 'slime)
(setq inferior-lisp-program "/usr/bin/sbcl")
(slime-setup '(slime-repl))

;markdown mode (requires pandoc, not an emacs package)
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))

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

; for easier auto complete
(global-set-key 
 (kbd "C-; C-;") 
 (lambda ()
   (interactive)
   (dabbrev-expand nil)))
; for some useful rails erb that rinari does not have
; yes I even checked the source file
(global-set-key
 (kbd "C-' C-e e")
 (lambda ()
   (interactive)
   (insert "<%  %>")
   (backward-char 3)))

(global-set-key
 (kbd "C-' C-e c")
 (lambda ()
   (interactive)
   (insert "<%#  %>")
   (backward-char 3)))

;neotree
(global-set-key [f2] 'neotree-toggle)

;projectile
(projectile-global-mode)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

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
(defun cwebber-web-mode-customizations ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-comment-style 2)
 ;(local-set-key (kbd "RET") 'newline-and-indent)
  )
(add-hook 'web-mode-hook 'cwebber-web-mode-customizations)

; vi like % for paren matching
(global-set-key (kbd "C-%") 'goto-match-paren)
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;(add-to-list 'load-path "~/.emacs.d/")
;(load "nyani.el")
;(require 'nyani)

;; (add-to-list 'load-path "~/stuff/guitar.el")
;; (load "guitar.el")
;; (require 'guitar-mode)

; powerline, eye candy, mostly to impress non emacs users
(require 'powerline)
(require 'powerline-themes)
(require 'powerline-separators)
(require 'cl-lib)

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
(my-powerline-default-theme)

;; (require 'zone)
;; (put 'erase-buffer 'disabled nil)

(require 'workgroups2)
(setq wg-prefix-key (kbd "C-c z"))
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")
(workgroups-mode 1)
