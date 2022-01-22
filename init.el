;;; init.el -*- lexical-binding: t; -*-


;; straight package manager settings
;; bootstrapping straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'general)
(setq straight-use-package-by-default t)

;; benchmark startup time
;; (use-package benchmark-init
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; not loading lisp-mode on scratch buffer tremendously reduces startup time
;; but it makes opening new prog-mode buffers insanely long...
;; (setq initial-major-mode 'fundamental-mode)


;; cleanup package config and auto-save files
(use-package no-littering)
(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; appearance settings
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(alpha 92))
(set-face-attribute 'default nil
		    :family "Menlo"
		    :height 120)
(set-face-attribute 'fixed-pitch nil
		    :family "Menlo"
		    :height 120)
(set-face-attribute 'variable-pitch nil
		    :family "Lucida Grande"
		    :height 130)
;; (set-face-attribute 'text-mode-default nil
;;		    :family "Lucida Grande"
;;		    :height 120)

;; dashboard
(use-package dashboard
;  :after projectile
  :config
  (use-package page-break-lines)
  (setq dashboard-banner-logo-title "")
  (setq dashboard-startup-banner (concat user-emacs-directory  "startup.png"))
  (setq dashboard-image-banner-max-height (round (* .7  (window-pixel-height))))
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts t)
  (setq dashboard-items '((recents  . 5)))
  (setq dashboard-set-footer nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook)
  (bind-keys :map dashboard-mode-map
	     ("i" . dashboard-previous-line)
	     ("k" . dashboard-next-line)))

;; one can never have more themes
;; (use-package zenburn-theme)
;; (use-package solarized-theme
;; (use-package ample-theme)
(use-package flatland-theme)
(load-theme 'flatland t)
;; icons
(use-package all-the-icons
  :config
  (use-package all-the-icons-dired)
  (use-package all-the-icons-completion)
  (all-the-icons-completion-mode)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setq all-the-icons-dired-monochrome nil)
  )

;; modeline
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)

  :config
  (set-face-attribute 'mode-line nil :family "Lucida Grande" :box nil :height 130)
  (set-face-attribute 'mode-line-inactive nil :family "Lucida Grande" :box nil :height 130)
  (setq doom-modeline-height 22)
  (setq doom-modeline-bar-width 10)
  (setq doom-modeline-hud t)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-number-limit 99)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-display-default-persp-name nil)
  (setq doom-modeline-persp-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-gnus-timer 2)
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))
  (setq doom-modeline-irc nil)
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-env-load-string "...")
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)
  ;; display time
  (setq display-time-default-load-average nil)
  (setq display-time-format "%a %H:%M")
  (display-time-mode 1)
  )
;; (use-package powerline
;;   :config
;;   (powerline-default-theme))

;; tabs
(use-package centaur-tabs :straight (:type git :host github :repo "Simon-Lin/centaur-tabs" :branch "master")
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "Lucida Grande" 130)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-height 24)
  (setq centaur-tabs-label-fixed-length 25)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-show-count t)
  (setq centaur-tabs-enable-ido-completion nil)
  (defun centaur-tabs-buffer-groups () ;; don't single out org-mode
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")
      ((or (derived-mode-p 'prog-mode)
	   (eq major-mode 'org-mode))
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
			  help-mode))
       "Help")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  (setq centaur-tabs-show-jump-identifier 'always)
  (general-define-key
   "M-b" 'centaur-tabs-ace-jump
   "A-1" 'centaur-tabs-select-visible-tab
   "A-2" 'centaur-tabs-select-visible-tab
   "A-3" 'centaur-tabs-select-visible-tab
   "A-4" 'centaur-tabs-select-visible-tab
   "A-5" 'centaur-tabs-select-visible-tab
   "A-6" 'centaur-tabs-select-visible-tab
   "A-7" 'centaur-tabs-select-visible-tab
   "A-8" 'centaur-tabs-select-visible-tab
   "A-9" 'centaur-tabs-select-visible-tab)

  (centaur-tabs-mode t))

;; misc appearance settings
(setq-default cursor-type 'bar)
(setq ring-bell-function 'ignore)
(setq visible-bell t)
(dolist (mode '(text-mode-hook
		prog-mode-hook
		conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
(global-hl-line-mode 1)
(global-prettify-symbols-mode)
(global-visual-line-mode)
;; (pixel-scroll-mode)   NOT working

;; completion framework
(use-package selectrum
  :config
  (defun my-selectrum-backward-kill-word-wrapper (&optional arg)
    "if selectrum--last-command is find-file, call backward-kill-sexp, otherwise call backward-kill-word."
    (interactive "p")
    (if minibuffer-completing-file-name ;; a file action is invoked in minibuffer
	(selectrum-backward-kill-sexp arg)
      (backward-kill-word (1+ arg))))
  (setq selectrum-extend-current-candidate-highlight t)
  (bind-key "A-e" 'my-selectrum-backward-kill-word-wrapper 'selectrum-minibuffer-map)
  ;; (unbind-key "M-i" 'selectrum-minibuffer-map) ;; M-i has two duplicate keymaps, unbind the first one (no longer needed since I use A-I for page up)
  (selectrum-mode 1))

(use-package consult ;; keybindings of consult are in the global keymaps section
  :config
  (consult-customize
   ;; turn off preview for opening recent files
   consult-recent-file :preview-key nil
   consult-recent-file-other-window :preview-key nil)
  
  ;; quick hack for using consult to open recent file in other window
  (defun consult-recent-file-other-window ()
    (interactive)
    (cl-letf (((symbol-function 'find-file) #'find-file-other-window))
      (consult-recent-file))))

(use-package prescient
  :config
  (use-package selectrum-prescient)
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))
(use-package marginalia
  :config
  (marginalia-mode))
(use-package embark
  :bind
  (("A-q <A-return>" . embark-act))
  (:map minibuffer-local-map ("<A-return>" . embark-act))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))

;;   (defun embark-which-key-indicator ()
;;     "An embark indicator that displays keymaps using which-key.
;; The which-key help message will show the type and value of the
;; current target followed by an ellipsis if there are further
;; targets."
;;     (lambda (&optional keymap targets prefix)
;;       (if (null keymap)
;;           (which-key--hide-popup-ignore-command)
;;	(which-key--show-keymap
;;	 (if (eq (plist-get (car targets) :type) 'embark-become)
;;              "Become"
;;            (format "Act on %s '%s'%s"
;;                    (plist-get (car targets) :type)
;;                    (embark--truncate-target (plist-get (car targets) :target))
;;                    (if (cdr targets) "…" "")))
;;	 (if prefix
;;              (pcase (lookup-key keymap prefix 'accept-default)
;;                ((and (pred keymapp) km) km)
;;                (_ (key-binding prefix 'accept-default)))
;;            keymap)
;;	 nil nil t (lambda (binding)
;;                      (not (string-suffix-p "-argument" (cdr binding))))))))

;;   (setq embark-indicators
;;	'(embark-which-key-indicator
;;	  embark-highlight-indicator
;;	  embark-isearch-highlight-indicator))

;;   (defun embark-hide-which-key-indicator (fn &rest args)
;;     "Hide the which-key indicator immediately when using the completing-read prompter."
;;     (which-key--hide-popup-ignore-command)
;;     (let ((embark-indicators
;;            (remq #'embark-which-key-indicator embark-indicators)))
;;       (apply fn args)))

;;   (advice-add #'embark-completing-read-prompter
;;               :around #'embark-hide-which-key-indicator)
  )

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; spellcheck and autocomplete
;; use applespell engine, need enchant installed on macOS
(setq ispell-program-name "enchant-2")
(use-package popup
  :defer t
  :bind (:map popup-menu-keymap
	      ("A-i" . popup-previous)
	      ("A-k" . popup-next)
	      ("A-j" . popup-open)
	      ("A-l" . popup-close)))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("A-'" . flyspell-correct-wrapper)))

(use-package flyspell-correct-popup
  :after flyspell-correct)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package company
  :config
  (use-package company-posframe)
  (setq company-posframe-show-metadata nil
	company-posframe-show-indicator nil)
  :bind (:map company-active-map
	      ("A-k" . company-select-next)
	      ("A-i" . company-select-previous)
	      ("TAB" . company-complete-common-or-cycle)
	      ("<backtab>" . (lambda () (interactive) (company-complete-common-or-cycle -1))))
  :hook ((prog-mode . company-mode)
	 (text-mode . company-mode)
	 (company-mode . company-posframe-mode)))

(use-package company-math
  :defer t
  :init
  (defun company-math-setup ()
  (setq-local company-backends
	      (append '((company-math-symbols-latex company-latex-commands))
		      company-backends)))
  (add-hook 'TeX-mode-hook 'company-math-setup))

(use-package company-auctex
  :defer t
  :init
  (add-hook 'TeX-mode-hook 'company-auctex-init))


;; flycheck
(use-package flycheck
  :init
  (setq sentence-end-double-space nil)
  (setq flycheck-keymap-prefix (kbd "A-q !"))
  :hook
  (prog-mode . flycheck-mode))

(use-package consult-flycheck
  :after flycheck
  :bind ("A-\"" . consult-flycheck))


;; alternative kill and copy behavior (credit: Xah)
(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').
URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
	(kill-new (buffer-string))
	(delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
	       (kill-region (region-beginning) (region-end) t)
	     (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').
URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2018-09-10"
  (interactive)
  (if current-prefix-arg
      (progn
	(copy-region-as-kill (point-min) (point-max)))
    (if (use-region-p)
	(progn
	  (copy-region-as-kill (region-beginning) (region-end)))
      (if (eq last-command this-command)
	  (if (eobp)
	      (progn )
	    (progn
	      (kill-append "\n" nil)
	      (kill-append
	       (buffer-substring-no-properties (line-beginning-position) (line-end-position))
	       nil)
	      (progn
		(end-of-line)
		(forward-char))))
	(if (eobp)
	    (if (eq (char-before) 10 )
		(progn )
	      (progn
		(copy-region-as-kill (line-beginning-position) (line-end-position))
		(end-of-line)))
	  (progn
	    (copy-region-as-kill (line-beginning-position) (line-end-position))
	    (end-of-line)
	    (forward-char)))))))

;; expand-region
(use-package expand-region
  :config
  :bind
  (("A-y". er/expand-region)
   ("A-Y" . er/contract-region)))

;; misc functions
(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package which-key
  :config
  (which-key-mode))

(setq scroll-preserve-screen-position t)
;; (setq prettify-symbols-unprettify-at-point t)
(setq debug-on-error t)
(setq set-mark-command-repeat-pop t)
(setq select-enable-clipboard nil)
(setq shift-select-mode nil)
(delete-selection-mode 1)
(tooltip-mode -1)
(setq tab-bar-show nil)


(defun new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (fundamental-mode)))

;; advice wrapper for yes-or-no-p -> y-or-n-p
(defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
  (cl-letf (((symbol-function 'yes-or-no-p) #'y-or-n-p))
    (apply orig-fun r)))

(advice-add 'kill-buffer :around #'yes-or-no-p->-y-or-n-p)
(advice-add 'revert-buffer :around #'yes-or-no-p->-y-or-n-p)

(defun my-scroll-other-window ()
  (interactive)
  (let* ((win (other-window-for-scrolling))
         (mode (with-selected-window win major-mode)))
    (if (eq mode 'pdf-view-mode)
	(with-current-buffer (window-buffer win)
	  (save-excursion
            (goto-char (window-point win))
            (with-selected-window win
              (pdf-view-scroll-up-or-next-page))
	    (set-window-point win (point))))
      (scroll-other-window))))

(defun my-scroll-other-window-down ()
  (interactive)
  (let* ((win (other-window-for-scrolling))
         (mode (with-selected-window win major-mode)))
    (if (eq mode 'pdf-view-mode)
	(with-current-buffer (window-buffer win)
	  (save-excursion
            (goto-char (window-point win))
            (with-selected-window win
              (pdf-view-scroll-down-or-previous-page))
	    (set-window-point win (point))))
      (scroll-other-window-down))))

;; drag stuff around
(use-package drag-stuff
  :config
  (require 'drag-stuff)
  (drag-stuff-global-mode 1)
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (general-define-key :keymaps 'drag-stuff-mode-map
		      "M-i" 'drag-stuff-up
		      "M-k" 'drag-stuff-down
		      "M-j" 'drag-stuff-left
		      "M-l" 'drag-stuff-right))
  

;; global keymaps
(setq mac-command-modifier 'alt)
(setq mac-option-modifier 'meta)
(setq mac-pass-command-to-system nil) ;; set macos system-short off

;; top level keymaps
(general-define-key
 "A-a" 'execute-extended-command
 "A-A" 'shell-command
 ;;A-b  buffer prefix
 "A-c" 'xah-copy-line-or-region
 "A-C-c" 'clipboard-kill-ring-save
 "A-d" 'delete-backward-char
 "A-e" 'backward-kill-word
 "A-E" 'backward-kill-sexp
 "A-f" 'delete-forward-char
 "A-g" 'consult-line
 "A-G" 'consult-mark
 "A-h" 'move-beginning-of-line
 "A-H" 'backward-paragraph
 "A-i" 'previous-line
 "A-I" 'scroll-down-command
 "A-C-i" 'my-scroll-other-window-down
 "A-j" 'backward-char
 "A-J" 'backward-sentence
 "A-k" 'next-line
 "A-K" 'scroll-up-command
 "A-C-k" 'my-scroll-other-window
 "A-l" 'forward-char
 "A-L" 'forward-sentence
 ;;A-m  C-u
 ;;A-n  C-g
 "A-o" 'forward-word
 "A-O" 'forward-sexp
 "A-p" 'recenter-top-bottom
 ;;A-q  file/system prefix
 "A-r" 'kill-word
 "A-R" 'kill-sexp
 "A-s" 'kill-line
 "A-S" '("backward-kill-line" . (lambda () (interactive) (kill-line 0)))
 "A-t" 'consult-imenu
 "A-u" 'backward-word
 "A-U" 'backward-sexp
 "A-v" 'yank
 "A-V" 'consult-yank-pop
 "A-C-v" 'clipboard-yank
 ;;A-w  major-mode-keymaps
 "A-x" 'xah-cut-line-or-region
 "A-C-x" 'clipboard-kill-region
 ;;A-y 'expand-region
 ;;A-Y 'contract-region
 "A-z" 'undo-tree-undo
 "A-Z" 'undo-tree-redo
 "A-SPC" 'set-mark-command
 "A-C-SPC" 'exchange-point-and-mark
 "A-[" 'centaur-tabs-backward
 "A-]" 'centaur-tabs-forward
 "A-{" 'centaur-tabs-backward-group
 "A-}" 'centaur-tabs-forward-group
 "M-[" 'centaur-tabs-move-current-tab-to-left
 "M-]" 'centaur-tabs-move-current-tab-to-right
 "A-;" 'move-end-of-line
 "A-:" 'forward-paragraph
 ;;A-'  flyspell-correct
 "A-\\" 'comment-line
 "A-," 'beginning-of-defun
 "A-<" 'beginning-of-buffer
 "A-." 'end-of-defun
 "A->" 'end-of-buffer
 "A-=" '("increase-buffer-font-size" . (lambda () (interactive) (text-scale-increase 1)))
 "A-+" '("increase-buffer-font-size" . (lambda () (interactive) (text-scale-increase 1)))
 "A--" '("decrease-buffer-font-size" . (lambda () (interactive) (text-scale-decrease 1)))
 )
(define-key key-translation-map (kbd "A-n") (kbd "C-g"))
(define-key key-translation-map (kbd "A-m") (kbd "C-u"))
(define-key key-translation-map (kbd "A-/") (kbd "C-h"))
(define-key minibuffer-local-map (kbd "A-a") 'mark-whole-buffer)


;; file and system actions (leading key A-q)
(defun open-user-init-file ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(general-define-key :prefix "A-q"
		    "TAB" 'indent-rigidly
		    "A-a" 'mark-whole-buffer
		    ;;A-b/b citar
		    "A-c" 'save-buffers-kill-emacs
		    ;;A-C  restart-emacs
		    "A-d" 'dired
		    "A-e" 'eval-last-sexp
		    "e"   'eval-defun
		    "A-f" 'find-file
		    "f"   'find-file-other-window
		    ;;A-g  magit
		    "A-i" 'open-user-init-file
		    "A-m" 'consult-bookmark
		    ;;A-p  projectile-map
		    "A-r" 'consult-recent-file
		    "r"   'consult-recent-file-other-window
		    "A-s" 'save-buffer
		    "A-u" 'undo-tree-visualize
		    ;;A-y  yas-insert-snippet
		    )

;; buffer, windows and frame actions (leading key A-b)
(general-define-key :prefix "A-b"
		    "A-b" 'consult-buffer
		    "b" 'consult-buffer-other-window
		    "A-k" '("kill-current-buffer" . (lambda () (interactive) (kill-buffer (current-buffer))))
		    "k"   'kill-buffer
		    "n"   'new-empty-buffer
		    "A-j" 'previous-buffer
		    "A-l" 'next-buffer
		    "A-o" 'other-window
		    "A-1" 'delete-other-windows
		    "A-2" 'split-window-below
		    "A-3" 'split-window-right
		    "A-6" 'enlarge-window
		    "A-0" 'delete-window
		    ">" 'enlarge-window-horizontally
		    "<" 'shrink-window-horizontally
		    "=" 'balance-windows
		    "<A-return>" 'toggle-frame-maximized
		    "<A-S-return>" 'toggle-frame-fullscreen
		    "A-a" 'make-frame-command
		    "A-f" 'other-frame
		    "A-s" 'switch-to-buffer-other-frame
		    "A-w" 'delete-frame
		    "A-q" 'delete-other-frames
		    "A-t" 'tab-bar-new-tab
		    "A-]" 'tab-bar-switch-to-next-tab
		    "A-[" 'tab-bar-switch-to-prev-tab
		    "A-c" 'tab-bar-close-tab
		    )
;; ace window
;; (use-package ace-window
;;   :bind (("A-b A-o" . ace-window)))

;; Dired hacks
(use-package dired-hacks-utils
  :defer t
  :hook
  (dired-mode . dired-utils-format-information-line-mode)
  (dired-mode . dired-filter-mode)
  ;; (dired-mode . dired-du-mode) ;; too slow -- just turn it on when I want to check folder size
  :config
  (use-package dired-subtree)
  (use-package dired-filter)
  (use-package dired-du)
  (setq dired-du-size-format t)
  (setq insert-directory-program "/usr/local/bin/gls") ;; use gls from coreutils for --group-directories-first option
  (setq dired-listing-switches "-agho --group-directories-first")
  (general-define-key :keymaps 'dired-mode-map
		    "i" 'dired-previous-line
		    "k" 'dired-next-line
		    "j" '("go-up-one-level" . (lambda () (interactive) (find-alternate-file "..")))
		    "l" 'dired-subtree-insert
		    "j" 'dired-subtree-remove
		    "g" 'dired-goto-file
		    "o" 'dired-find-file
		    "r" 'revert-buffer
		    "q" '("quit-window" . (lambda () (interactive) (quit-window t)))
		    "RET" 'dired-find-alternate-file
		    "<A-return>" 'dired-find-file-other-window
		    )
  (define-key dired-mode-map (kbd "A-w") dired-filter-map)
  (put 'dired-find-alternate-file 'disabled nil) ;; disable annoying popups when calling find-alternate-file
  ;; (setq dired-subtree-use-backgrounds nil)
  )

;; helpful: emacs help+
(use-package helpful
  :defer t
  :init
  ;; disable annoying C-h n shit
  (unbind-key "C-h n")
  (unbind-key "C-h C-n")
  ;; describe bindings has some weird bugs and is often mispressed
  (unbind-key "C-h b")
  :general ("C-h A-f" 'helpful-callable
	    "C-h A-F" 'helpful-function
	    "C-h A-v" 'helpful-variable
	    "C-h A-k" 'helpful-key
	    "C-h A-C" 'helpful-command
	    "C-h A-j" 'describe-mode
	    "C-h A-o" 'describe-symbol
	    "C-h A-k" 'describe-key
	    "C-h A-b" 'embark-bindings
	    ))

;; restart emacs within emacs
(use-package restart-emacs
  :config
  (global-set-key `[(alt q) (alt shift c)] 'restart-emacs))

;; smartparens setup
(use-package smartparens
  :config
  ;; pad space when inserting unnamed equations
  (sp-local-pair 'LaTeX-mode "\\[" nil :post-handlers '(:add " | "))
  (sp-local-pair 'org-mode "\\[" nil :post-handlers '(:add " | "))
  :hook
  (prog-mode . smartparens-mode)
  (smartparens-mode . show-smartparens-mode)
  :general
  (:keymaps 'smartparens-mode-map ;; sexp manipulation maps use "A-C" or "A-S" modifiers
	    "A-C-j" 'sp-beginning-of-sexp
	    "A-C-l" 'sp-end-of-sexp
	    "A-O" 'sp-forward-sexp
	    "A-U" 'sp-backward-sexp
	    ;; "A-C-i" 'sp-backward-up-sexp
	    ;; "A-C-k" 'sp-down-sexp
	    "A-T" 'sp-mark-sexp
	    "A-R" 'sp-kill-sexp
	    "A-E" 'sp-backward-kill-sexp
	    "A-D" 'sp-unwrap-sexp
	    "A-F" 'sp-unwrap-sexp
	    "A-C-[" 'sp-backward-slurp-sexp
	    "A-C-]" 'sp-forward-slurp-sexp
	    "A-C-{" 'sp-forward-barf-sexp
	    "A-C-}" 'sp-backward-barf-sexp))
(require 'smartparens-config)

;; lisp modes config
(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-interaction-mode . rainbow-delimiters-mode))
(define-key lisp-interaction-mode-map (kbd "<A-return>") 'eval-print-last-sexp)



;; pdf tools
(use-package pdf-tools
  :bind (:map pdf-view-mode-map
	      ("<wheel-left>" . image-backward-hscroll)
	      ("<wheel-right>" . image-forward-hscroll)
	      ("i" . pdf-view-scroll-down-or-previous-page)
	      ("k" . pdf-view-scroll-up-or-next-page)))
(pdf-loader-install)


;; LaTeX

;; cdlatex
(use-package cdlatex
  :defer t
  :init
  (setq cdlatex-takeover-parenthesis nil)
  (setq cdlatex-takeover-subsuperscript nil)
  
  :config
  (setq cdlatex-math-symbol-alist
	'((?\s "\\quad" "\\qquad")
	  (?1 "\\frac{?}{}" "\\dfrac{?}{}")
	  (?2 "\\sqrt{?}")
	  (?5 "\\hbar")
	  (?6 "\\partial")
	  (?7 "\\Braket{?}")
	  (?q "\\chi")
	  (?Q "\\Chi")
	  (?F "\\Phi")
	  (?j "\\theta" "\\vartheta")
	  (?J "\\Theta" "\\varTheta")
	  (?i "\\iota")
	  (?x "\\xi")
	  (?X "\\Xi")
	  (?w "\\omega")
	  (?W "\\Omega")
	  (?\( "\\left(?\\right)")
	  (?\) "\\right)")
	  (?\[ "\\left[?\\right]")
	  (?\] "\\right]")
	  (?\{ "\\left\\\{?\\right\\\}")
	  (?\} "\\right\}")
	  (?. "\\cdot" "\\cdots")
	  (?< "\\leftarrow" "\\Leftarrow" "\\longleftarrow")
	  (?> "\\rightarrow" "\\Rightarrow" "\\longrightarrow")))

  (defun my-cdlatex-read-char-with-help (alist start-level max-level prompt-format
					       header-format prefix bindings)
    "Read a char from keyboard and provide help if necessary.
    Also make space not advance to next level (I use space to insert \\quad)"
    (interactive)
    (let (char (help-is-on nil)
	       (level start-level))
      (catch 'exit
	(save-window-excursion
          (while t
            (if help-is-on
		(progn
                  (cdlatex-turn-on-help
                   (concat (format header-format prefix)
			   (if (assoc level bindings)
			       (concat "  Direct binding are `"
				       (cdr (assoc level bindings)) "' etc.")
			     ""))
                   level alist help-is-on nil)))
	    (message prompt-format level max-level)
	    (if (and (not help-is-on)
		     (sit-for cdlatex-auto-help-delay))
		(setq char ?\?)
	      (setq char (read-key)))
            (cond
	     ((= char ?\C-g)
	      (keyboard-quit))
             ((= char ?\?)
	      (if help-is-on
                  (progn
                    (setq help-is-on (+ help-is-on (- (window-height) 1)))
                    (if (> help-is-on (count-lines (point-min) (point-max)))
			(setq help-is-on 1)))
		(setq help-is-on 1)))
             ((equal char prefix)
	      (setq level (if (= level cdlatex-math-symbol-no-of-levels)
			      1
                            (1+ level))))
             (t (throw 'exit (cons char level)))))))))
  (advice-add 'cdlatex-read-char-with-help :override #'my-cdlatex-read-char-with-help)

  ;; make the quote modification work with A-n (read-key respects key-translation-map but read-char does not)
  (defun read-char->read-key (orig-fun &rest r)
    (cl-letf (((symbol-function 'read-char) #'read-key))
      (apply orig-fun r)))
  (advice-add 'cdlatex-math-modify :around #'read-char->read-key))


;; common helper functions for latex and org mode

(defun insert-left-right-pair (&optional pair)
  "with the pair given (one of (, [, {, |, . ),
insert the left right pair of the indicated pair.
if the active region is set, wrap the pair around the region.
without the pair given, prompt the user for inseted pair."
  (interactive "clnsert left-right pair: ")
  (let (p-beg p-end pos1 pos2)
    (cond
     ((eq pair ?\()
      (setq p-beg "(" p-end ")"))
     ((eq pair ?\[)
      (setq p-beg "[" p-end "]"))
     ((eq pair ?\{)
      (setq p-beg "\\{" p-end "\\}"))
     ((eq pair ?\|)
      (setq p-beg "|" p-end "|"))
     ((eq pair ?\.)
      (setq p-beg "." p-end "."))
     (t
      (error "Invalid pair -- must be one of (, [, {, |, .")))
    (if (use-region-p)
	(progn
	  (setq pos1 (region-beginning) pos2 (region-end))
	  (goto-char pos1)))
    (insert "\\left" p-beg " ")
    (if (use-region-p)
	(if (eq pair ?\{)
	    (goto-char (+ pos2 8))
	  (goto-char (+ pos2 7))))
    (insert " \\right" p-end)
    (if (use-region-p)
	(setq mark-active nil)
      (if (eq pair ?\{)
	  (backward-char 9)
	(backward-char 8)))))

(defun latex-backward-delete-word ()
  (interactive)
  (backward-kill-word 1)
  (when (eq (preceding-char) ?\\)
    (delete-backward-char 1)))

(defun latex-math-symobl-lookup ()
  (interactive)
  (find-file-other-window "~/.emacs.d/symbols.pdf"))

;; auctex
(use-package tex-site :straight auctex
  :defer t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (variable-pitch-mode 1)))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; somehow prettify symbols are broken in auctex. Need a manual fix
  (add-hook 'LaTeX-mode-hook (lambda () (setq prettify-symbols-alist tex--prettify-symbols-alist)))
  (add-hook 'LaTeX-mode-hook (lambda () (setq prettify-symbols-compose-predicate 'tex--prettify-symbols-compose-p)))
  
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)
  (setq-default LaTeX-default-environment "align")
  (setq reftex-plug-into-AUCTeX '(nil t t t t))
  (defun LaTeX-no-insert-label (orig-fun &rest args)
    "turn off automatic labeling"
    (apply orig-fun args '(t)))
  (advice-add 'LaTeX-label :around 'LaTeX-no-insert-label)

  ;; preview setup
  (use-package latex-preview-pane :straight (:type git :host github :repo "Simon-Lin/latex-preview-pane" :branch "master")
    :config
    (defun latex-preview-shortcut (&optional arg)
      "Turn on latex preview mode if it hasn't turned on yet.
Otherwise, call LatexMk.
With unversal prefix, turn off latex preview mode."
      (interactive "P")
      (if (and (boundp 'latex-preview-pane-mode) latex-preview-pane-mode)
	  (if arg
	      (latex-preview-pane-mode -1)
	    (save-buffer)
	    (TeX-command "LatexMk" 'TeX-master-file)
	    (latex-preview-pane-update))
	(latex-preview-pane-mode))))
  
  ;; latexmk setup
  (use-package auctex-latexmk)
  (auctex-latexmk-setup)
  (setq-default TeX-command-Show "LatexMk")
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)

  :general
  (:keymaps 'LaTeX-mode-map
	    "A-e" 'latex-backward-delete-word)
  (:keymaps 'LaTeX-mode-map :prefix "A-w"
	    "A-w" 'TeX-command-master
	    "A-a" 'TeX-command-run-all
	    "A-b" 'TeX-command-buffer
	    "A-e" 'LaTeX-environment
	    "A-i" 'LaTeX-environment
	    "A-f" 'TeX-font
	    "A-k" 'TeX-kill-job
	    "A-s" 'LaTeX-section
	    "A-j" 'LaTeX-insert-item
	    "A-p" 'latex-preview-shortcut
	    "A-9" 'reftex-label
	    "A-0" 'reftex-reference
	    "A-[" 'reftex-citation
	    "A-]" 'LaTeX-close-environment
	    "A-=" 'reftex-toc
	    "RET" 'TeX-insert-macro
	    "*" 'LaTeX-mark-section
	    "." 'LaTeX-mark-environment
	    ))


;; org settings
(use-package org
  :defer t
  :config
  (require 'tex-mode)
  (require 'latex)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'org-cdlatex-mode)
  ;; (add-hook 'org-mode-hook 'latex-math-mode)
  (add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1)))
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'org-mode-hook (lambda () (setq prettify-symbols-alist tex--prettify-symbols-alist)))
  (add-hook 'org-mode-hook (lambda () (setq prettify-symbols-compose-predicate 'tex--prettify-symbols-compose-p)))
  (add-hook 'org-mode-hook (lambda () (progn
				   (modify-syntax-entry ?$ ".")
				   (modify-syntax-entry ?\\ "\\")
				   (modify-syntax-entry ?/ ".")
				   (modify-syntax-entry ?| ".")
				   (modify-syntax-entry ?_ ".")
				   (modify-syntax-entry ?' "."))))

  (setq org-format-latex-options
	'(:foreground default :background default :scale 1.4 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
	     ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-latex-create-formula-image-program 'dvipng)
  (setq org-preview-latex-image-directory ".ltximg/")
  (setq org-highlight-latex-and-related '(native latex script entities))

  ;; function to insert latex environment in org-mode
  (setq my-org-environment-default "align*")
  (setq my-org-environment-list '("Bmatrix" "Vmatrix" "align" "align*" "aligned" "array" "bmatrix" "cases" "eqnarray" "eqnarray*" "equation" "equation*" "figure" "figure*" "matrix" "pmatrix" "smallmatrix" "vmatrix"))
  (defun my-org-insert-environment ()
    (interactive)
    (let ((env (completing-read "Environment type: "
				my-org-environment-list nil t nil nil my-org-environment-default)))
      (indent-according-to-mode)
      (insert "\\begin{" env "}\n\n")
      (indent-according-to-mode)
      (insert "\\end{" env "}")
      (forward-line -1)
      (indent-according-to-mode)))

  ;; header and bullet settings
  (use-package org-superstar
    :config
    (setq org-hide-leading-stars t)
    (set-face-attribute 'org-level-1 nil :weight 'semi-bold :height 1.25)
    (set-face-attribute 'org-level-2 nil :weight 'normal :height 1.15)
    (set-face-attribute 'org-level-3 nil :weight 'normal :height 1.05)
    :hook (org-mode . (lambda () (org-superstar-mode 1))))
  
  (setq org-startup-indented t)
  (set-face-attribute 'org-ellipsis nil :foreground "grey70" :underline nil :inherit 'shadow)  
  (setq org-ellipsis " ▾")  ; possible choices: ↴ ▼ ↝ » …
  
  (setq org-cycle-separator-lines 2)
  (setq org-catch-invisible-edits 'error)
  (setq org-todo-keywords
	'((sequence "TODO" "STARTED" "|" "DONE")))
  (setq org-todo-keyword-faces
	'(("STARTED" . (:foreground "ORANGE" :weight bold))))
  (setq org-log-done 'time)
  (setq org-startup-with-inline-images nil)
  (setq org-startup-folded t)
  (setq org-image-actual-width nil)
  
  (add-to-list 'org-latex-packages-alist '("" "braket" t))
  (add-to-list 'org-latex-packages-alist '("" "cancel" t))
  (add-to-list 'org-latex-packages-alist '("margin=1in" "geometry" nil))
  (setq org-latex-pdf-process (list "latexmk -f -pdf %f"))
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "\\[" "\\]") ;; inherit from latex mode, fix later

  :general
  (:keymaps 'org-mode-map
	    "M-i" 'org-metaup
	    "M-k" 'org-metadown
	    "M-j" 'org-metaleft
	    "M-l" 'org-metaright
	    "M-I" 'org-shiftmetaup
	    "M-K" 'org-shiftmetadown
	    "M-J" 'org-shiftmetaleft
	    "M-L" 'org-shiftmetaright
	    "<A-return>" 'org-meta-return
	    )
  (:keymaps 'org-mode-map :prefix "A-w"
	    "." 'org-time-stamp
	    "@" 'org-mark-subtree
	    "^" 'org-sort
	    "-" 'org-ctrl-c-minus
	    "*" 'org-ctrl-c-star
	    "=" 'org-table-eval-formula
	    "RET" 'org-ctrl-c-ret
	    "A-t" 'org-todo
	    "A-w" 'org-ctrl-c-ctrl-c
	    "A-x" 'org-refile
	    "A-c" 'org-copy
	    "A-d" 'org-deadline
	    "A-e" 'org-export-dispatch
	    "A-l" 'org-insert-link
	    "A-a" 'org-attach
	    "A-q" 'org-set-tags-command
	    "A-s" 'org-schedule
	    "A-p" 'org-latex-preview
	    "A-i" 'my-org-insert-environment
	    "A-r" 'org-redisplay-inline-images
	    )
  (:keymaps 'org-cdlatex-mode-map
	    "A-e" 'latex-backward-delete-word
	    "^" 'org-self-insert-command
	    "_" 'org-self-insert-command))


;; citar
(use-package citar
  :config
  (setq citar-bibliography '("~/Documents/Papers/master.bib"))
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)  ;; use consult-completing-read for enhanced interface
  (setq citar-symbols
	`((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
	  (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
	  (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")
  (setq citar-file-open-function (lambda (fpath) (call-process "open" nil 0 nil "-a" "/Applications/Preview.app" fpath)))

  :bind
  ("A-q A-b" . citar-open-library-files)
  ("A-q b" . citar-insert-citation)
  )


;; arxiv-mode
(use-package arxiv-mode :straight (:type git :host github :repo "Simon-Lin/arxiv-mode" :branch "master")
  :config
  (set-face-attribute 'arxiv-abstract-face nil :inherit 'default)
  (set-face-attribute 'arxiv-subfield-face nil :inherit 'font-lock-variable-name-face)
  (setq arxiv-use-variable-pitch t)
  (setq arxiv-default-download-folder "~/Documents/Papers/")
  (setq arxiv-default-bibliography "~/Documents/Papers/master.bib")
  (setq arxiv-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil "-a" "/Applications/Preview.app" fpath)))
  :bind (:map arxiv-mode-map
	      ("i" . arxiv-prev-entry)
	      ("k" . arxiv-next-entry))
  :hook
  ('arxiv-mode . 'centaur-tabs-local-mode)
  ('arxiv-abstract-mode . 'centaur-tabs-local-mode))

;; projectile
(use-package projectile
  :bind-keymap ("A-q A-p" . projectile-command-map))

;; magit
(use-package magit
  :bind
  (("A-q A-g" . magit-status)
   ("A-q g" . magit-dispatch)
   ("A-q G" . magit-file-dispatch))
  :config
  ;; custom keymap
  (general-define-key
   :keymaps 'magit-mode-map
   "i" 'magit-section-backward
   "k" 'magit-section-forward
   "I" 'magit-section-forward-sibling
   "K" 'magit-section-forward-sibling
   "n" 'magit-gitignore
   "N" 'magit-init
   "DEL" 'magit-delete-thing
   "<S-backspace>" 'magit-file-untrack
   )
  (general-define-key
   :keymaps 'git-commit-mode-map
   :prefix "A-w"
   "A-w" 'with-editor-finish
   "A-k" 'with-editor-cancel
   "A-i" 'git-commit-prev-message
   "A-k" 'git-commit-next-message)
  ;; Update help panel with custom keymap
  (transient-suffix-put 'magit-dispatch "i" :key "n") ;;magit-gitignore
  (transient-suffix-put 'magit-dispatch "I" :key "N") ;;magit-init
  (transient-suffix-put 'magit-dispatch "k" :key "DEL") ;;magit-discard
  )

;; snippets
;; (use-package yasnippet
;;   :config
;;   (bind-key "A-q A-y" 'yas-insert-snippet 'yas-minor-mode-map )
;;   (use-package yasnippet-snippets)
;;   (add-hook 'prog-mode-hook 'yas-minor-mode))

;; shells
(defun my-eshell-config ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  ;; Bind some useful keys for evil-mode
  (general-define-key :keymaps 'ehsell-mode-map
		      "A-j" 'eshell-bol
		      "M-i" 'eshell-previous-input
		      "M-k" 'eshell-next-input
		      "A-RET" 'eshell-queue-input
		      "M-j" 'eshell-previous-prompt
		      "M-l" 'eshell-next-prompt)
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))
(add-hook 'eshell-first-time-mode-hook 'my-eshell-config)
(setq eshell-destroy-buffer-when-process-dies t)

;; package devel tools
(use-package package-lint
  :defer t)


;;; init.el ends here
