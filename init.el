;;; init.el -*- lexical-binding: t; -*-



;;; ========== package manager settings ==========

;; bootstrapping straight
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

;; (straight-use-package 'use-package) ; use-package is built-in in emacs 29
(straight-use-package 'general)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-protocol 'ssh)

;; (setq debug-on-error t)

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
(no-littering-theme-backups)
;; (setq auto-save-file-name-transforms
;;       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
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

;;; ========== Appearance ==========

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
  ;; (setq dashboard-set-heading-icons t) ; buggy now. Waiting for the fix
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'all-the-icons)
  (dashboard-setup-startup-hook)
  (bind-keys :map dashboard-mode-map
	     ("i" . dashboard-previous-line)
	     ("k" . dashboard-next-line)))

;; one can never have more themes
;; (use-package zenburn-theme)
;; (use-package solarized-theme
;; (use-package flatland-theme)
;; (use-package color-theme-sanityinc-tomorrow)
(use-package material-theme)
(load-theme 'material t)

;; icons
(use-package all-the-icons
  :config
  (use-package all-the-icons-completion)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode))
(use-package nerd-icons)

;; modeline
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)

  :config
  (set-face-attribute 'mode-line nil :family "Lucida Grande" :box nil :height 130)
  (set-face-attribute 'mode-line-inactive nil :family "Lucida Grande" :box nil :height 130)

  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 10)
  (setq doom-modeline-hud t)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-buffer-name t)
  (setq doom-modeline-highlight-modified-buffer-name t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
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
  (setq doom-modeline-modal nil)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-gnus-timer 2)
  (setq doom-modeline-gnus-excluded-groups '("dummy.group"))
  (setq doom-modeline-irc nil)
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-battery t)
  (setq doom-modeline-time t)
  (setq doom-modeline-display-misc-in-all-mode-lines t)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")
  (setq doom-modeline-env-load-string "...")
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)
  (setq display-time-default-load-average nil)
  (setq display-time-format "%a %H:%M")
  (display-time-mode t)
  (display-battery-mode t))

(use-package procress :straight (:host github :repo "haji-ali/procress")
  :after (auctex)
  :commands procress-auctex-mode
  :init
  (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
  :config
  (procress-load-default-svg-images))

;; tabs
(use-package centaur-tabs :straight (:type git :host github :repo "Simon-Lin/centaur-tabs" :branch "master")
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-change-fonts "Lucida Grande" 130)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-height 30)
  (setq centaur-tabs-label-fixed-length 25)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker nil)
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
      ((eq major-mode 'org-mode)
       "Org")
      ((derived-mode-p 'prog-mode)
       "Programming")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode help-mode))
       "Help")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))

  (defun my-tabs-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar.
This function is similar to original function but displays the org-roam buffers by its title."
    (format " %s"
	    (let ((bufname (if centaur-tabs--buffer-show-groups
			       (centaur-tabs-tab-tabset tab)
			     (if (org-roam-buffer-p (car tab))
				 (with-current-buffer (car tab)
				   (org-roam-db--file-title))
			       (buffer-name (car tab))))))
	      (if (> centaur-tabs-label-fixed-length 0)
		  (centaur-tabs-truncate-string  centaur-tabs-label-fixed-length bufname)
		bufname))))
  
  (setq centaur-tabs-show-jump-identifier 'always)
  (set-face-attribute 'centaur-tabs-jump-identifier-selected nil
		      :foreground nil)
  (set-face-attribute 'centaur-tabs-jump-identifier-unselected nil
		      :foreground nil)
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

  (centaur-tabs-mode t)
  (setq centaur-tabs-tab-label-function 'my-tabs-buffer-tab-label))

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
;; (pixel-scroll-precision-mode)
;; (setq pixel-scroll-precision-interpolate-page t)
;; (setq pixel-scroll-precision-large-scroll-height 30.0)


;;; ========== Completion framework ==========

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :config
  (define-key vertico-map (kbd "A-e") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "A-d") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-multiform-mode))

(use-package consult ;; keybindings of consult are in the global keymaps section
  :config
  ;; quick hack for using consult to open recent file in other window
  (defun consult-recent-file-other-window ()
    (interactive)
    (cl-letf (((symbol-function 'find-file) #'find-file-other-window))
      (consult-recent-file)))
  (consult-customize
   ;; turn off preview for opening recent files
   consult-recent-file :preview-key nil
   consult-recent-file-other-window :preview-key nil)
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package prescient
  :after vertico corfu
  :config
  (use-package vertico-prescient)
  (use-package corfu-prescient)
  (corfu-prescient-mode 1)
  (vertico-prescient-mode 1)
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


;; (use-package jinx
;;
;; can't get it to work. Freeze issue related to thread handling in the OSX port. See:
;; https://github.com/minad/jinx/pull/91
;;
;;   :defer t
;;   :init
;;   (defun my-jinx--load-dicts ()
;;   "Initialize broker and dicts from jinx-mod dynamic module in a thread safe manner."
;;   (make-thread 'jinx--load-dicts-thread "jinx--load-dicts-thread"))

;;   (defvar jinx--mutex (make-mutex "jinx--mutex"))
  
;;   (defun jinx--load-dicts-thread ()
;;     "Thread runner for `jinx--load-dicts' to load the actual dictionaries."
;;     (with-mutex jinx--mutex
;;       (setq jinx--dicts (delq nil (mapcar #'jinx--mod-dict
;;                                           (split-string jinx-languages)))
;;             jinx--syntax-table (make-syntax-table jinx--base-syntax-table))
;;       (unless jinx--dicts
;; 	(message "Jinx: No dictionaries available for %S" jinx-languages))
;;       (dolist (dict jinx--dicts)
;; 	(cl-loop for c across (jinx--mod-wordchars dict) do
;; 		 (modify-syntax-entry c "w" jinx--syntax-table)))
;;       (modify-syntax-entry ?' "w" jinx--syntax-table)
;;       (modify-syntax-entry ?’ "w" jinx--syntax-table)
;;       (modify-syntax-entry ?. "." jinx--syntax-table)))

;;   (advice-add 'jinx--load-dicts :override #'my-jinx--load-dicts)
;;   (global-jinx-mode)
  
;;   :bind (("A-'" . jinx-correct)
;;          ("M-'" . jinx-languages)))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*.el"))
  :config
  (setq corfu-cycle t
	corfu-auto t                 ;; Enable auto completion
	corfu-quit-at-boundary nil   ;; Never quit at completion boundary
	corfu-quit-no-match t        ;; Never quit, even if there is no match
	corfu-preview-current t      ;; Disable current candidate preview
	corfu-preselect 'valid       ;; Preselect the prompt
	corfu-on-exact-match nil     ;; Configure handling of exact matches
	corfu-scroll-margin 5)       ;; Use scroll margin
  (setq corfu-auto-delay 0.4)
  (setq corfu-echo-delay '(0.7 . 0.2))
  ;; (setq corfu-popupinfo-delay 0)
  :init
  (corfu-echo-mode)
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  
  (use-package company-math :defer t)
  (use-package company-auctex :defer t)
  (use-package company-reftex :defer t)
  (defun latex-capf-setup ()
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-math-symbols-latex))
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-math-symbols-unicode))
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-auctex-macros))
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-auctex-symbols))
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-auctex-environments))
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-reftex-labels))
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-reftex-citations)))
  (add-hook 'LaTeX-mode-hook 'latex-capf-setup))

(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (use-package company
;;   :config
;;   (use-package company-posframe)
;;   (setq company-posframe-show-metadata nil
;; 	company-posframe-show-indicator nil)
;;   :bind (:map company-active-map
;; 	      ("A-k" . company-select-next)
;; 	      ("A-i" . company-select-previous)
;; 	      ("TAB" . company-complete-common-or-cycle)
;; 	      ("<backtab>" . (lambda () (interactive) (company-complete-common-or-cycle -1))))
;;   :hook ((prog-mode . company-mode)
;; 	 (text-mode . company-mode)
;; 	 (company-mode . company-posframe-mode)))

;; (use-package company-math :defer t)
;; (use-package company-auctex :defer t)
;; (use-package company-reftex :defer t)

;; (defun company-latex-setup ()
;;     (setq-local company-backends
;; 		(append '((company-math-symbols-latex company-latex-commands)
;; 			  (:separate company-reftex-labels company-reftex-citations)
;; 			  (:separate company-auctex-macros company-auctex-symbols company-auctex-environments))
;; 			company-backends)))
;; (add-hook 'TeX-mode-hook 'company-latex-setup)

;; flycheck
(use-package flycheck
  :init
  (setq sentence-end-double-space nil)
  (setq flycheck-keymap-prefix (kbd "A-q !"))
  :hook
  (prog-mode . flycheck-mode))

(use-package consult-flycheck
  :after flycheck
  :bind ("A-|" . consult-flycheck))



;;; ========== Misc functions ==========

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
(setq set-mark-command-repeat-pop t)
(setq select-enable-clipboard nil)
(setq shift-select-mode nil)
(save-place-mode)
(delete-selection-mode 1)
(tooltip-mode -1)
(setq tab-bar-show nil)
(setq use-short-answers t) ; new in Emacs 28


(defun new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (fundamental-mode)))

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

(defun open-user-init-file ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(defun forward-word-or-other-window ()
  "Cycle through the windows via repeated press of A-b A-o A-o..."
  (interactive)
  (if (eq last-command 'other-window)
      (progn
	(other-window 1)
	(setq this-command 'other-window))
    (forward-word)))

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



;;; ========== Global keybinding ==========

(setq mac-command-modifier 'alt)
(setq mac-option-modifier 'meta)
(setq mac-pass-command-to-system nil) ;; set macos system-short off

;; Re-instate my own A- bindings after loading of `iso-transl'
(with-eval-after-load 'iso-transl
  (map-keymap
     (lambda (key cmd)
       (let ((key-desc (single-key-description key)))
         (when (string-match-p "^A-" key-desc)
           ;; Nuke all the A- bindings
           (define-key key-translation-map (kbd key-desc) nil))))
     key-translation-map)
  ;; Catch the thief
  (debug-on-entry 'iso-transl-define-keys))

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
 "A-J" 'backward-list
 "A-k" 'next-line
 "A-K" 'scroll-up-command
 "A-C-k" 'my-scroll-other-window
 "A-l" 'forward-char
 "A-L" 'forward-list
 ;;A-m  C-u
 ;;A-n  C-g
 "A-o" 'forward-word-or-other-window
 "A-O" 'forward-sexp
 "A-p" 'recenter-top-bottom
 ;;A-q  file/system prefix
 "A-r" 'kill-word
 "A-R" 'kill-sexp
 "A-s" 'kill-line
 "A-S" '("backward-kill-line" . (lambda () (interactive) (kill-line 0)))
 "A-t" 'consult-imenu
 "A-T" 'consult-outline
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
 "A-`" 'indent-for-tab-command
 "A-[" 'centaur-tabs-backward
 "A-]" 'centaur-tabs-forward
 "A-{" 'centaur-tabs-backward-group
 "A-}" 'centaur-tabs-forward-group
 "M-[" 'centaur-tabs-move-current-tab-to-left
 "M-]" 'centaur-tabs-move-current-tab-to-right
 "A-;" 'move-end-of-line
 "A-:" 'forward-paragraph
 ;;A-'  flyspell-correct
 "A-\"" 'completion-at-point
 "A-\\" 'comment-line
 "A-," 'beginning-of-defun
 "A-<" 'beginning-of-buffer
 "A-." 'end-of-defun
 "A->" 'end-of-buffer
 ;;A-(1-9) 'jump-to-tabs
 "A-(" 'tab-bar-switch-to-prev-tab
 "A-)" 'tab-bar-switch-to-next-tab
 "A-=" '("increase-buffer-font-size" . (lambda () (interactive) (text-scale-increase 1)))
 "A-+" '("increase-buffer-font-size" . (lambda () (interactive) (text-scale-increase 1)))
 "A--" '("decrease-buffer-font-size" . (lambda () (interactive) (text-scale-decrease 1)))
 )
(define-key key-translation-map (kbd "A-n") (kbd "C-g"))
(define-key key-translation-map (kbd "A-m") (kbd "C-u"))
(define-key key-translation-map (kbd "A-/") (kbd "C-h"))
(define-key minibuffer-local-map (kbd "A-a") 'mark-whole-buffer)


;; file and system actions (leading key A-q)

(general-define-key :prefix "A-q"
		    "TAB" 'indent-rigidly
		    "A-a" 'mark-whole-buffer
		    ;;"A-b" bib-map
		    "A-c" 'save-buffers-kill-emacs
		    "A-C" 'restart-emacs
		    "A-d" 'dired
		    "d"   'dirvish
		    "A-e" 'eval-last-sexp
		    "A-E" 'eval-buffer
		    "e"   'eval-defun
		    "A-f" 'find-file
		    "f"   'find-file-other-window
		    ;;A-g  magit
		    "g"   'revert-buffer
		    "A-i" 'open-user-init-file
		    "A-m" 'consult-bookmark
		    ;;A-p  projectile-map
		    "A-r" 'consult-recent-file
		    "r"   'consult-recent-file-other-window
		    ;;A-o org-roam prefix
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
		    "A-o" 'other-window
		    "A-w" 'delete-window
		    "A-s" 'window-swap-states
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
		    "A-d" 'delete-frame
		    "A-q" 'delete-other-frames
		    "A-t" 'tab-bar-new-tab
		    "A-]" 'tab-bar-switch-to-next-tab
		    "A-[" 'tab-bar-switch-to-prev-tab
		    "A-c" 'tab-bar-close-tab
		    )

;; literature/bibliography tools
(general-define-key :prefix "A-q A-b"
		    "A-b" 'citar-open
		    "A-o" 'citar-open-note
		    "A-f" 'citar-open-files
		    "A-i" 'citar-insert-citation
		    "A-r" 'arxiv-read-new
		    "A-l" 'inspire-literature-search
		    "A-a" 'inspire-author-search
		    "A-e" 'ebib
		    )

;; ace window
;; (use-package ace-window
;;   :config (setq aw-scope 'frame)
;;   :bind (("A-b A-o" . ace-window)))


;;; ========== Utilities ==========

(use-package dirvish
  :config
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq insert-directory-program "/opt/homebrew/bin/gls") ;; use gls from coreutils for --group-directories-first option
  (setq dired-listing-switches "-agho --group-directories-first")
  
  (setq dirvish-attributes '(file-size all-the-icons))
  (setq dirvish-bookmarks-alist
	'(("h" "~/" "Home")
	  ("w" "~/Downloads/" "Downloads")
	  ("d" "~/Documents/" "Documents")
	  ("v" "/Volumes/" "Volumes")
	  ("t" "~/.Trash/" "Trash")))
  (dirvish-override-dired-mode)
  (use-package dired-subtree)
  (use-package dired-filter)
  
  (general-define-key :keymaps 'dired-mode-map
		      "i" 'dired-previous-line
		      "k" 'dired-next-line
		      "l" 'dired-subtree-insert
		      "j" 'dired-subtree-remove
		      "g" 'dired-goto-file
		      "f" 'dirvish-file-info-menu
		      "r" 'dirvish-show-history
		      "b" 'dirvish-goto-bookmark
		      "a" 'dirvish-mark-actions-menu
		      "?" 'dirvish-dispatch
		      "`" 'dirvish-toggle-fullscreen
		      ;; "q" '("quit-window" . (lambda () (interactive) (quit-window t)))
		      ;; "RET" 'dired-find-alternate-file
		      "<A-return>" 'dired-find-file-other-window
		      )
  (define-key dired-mode-map (kbd "A-w") dired-filter-map)
  
  :hook
    (dired-mode . dired-filter-mode)
    (dirvish-mode . centaur-tabs-local-mode))


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
	    "C-h A-K" 'describe-keymap
	    "C-h A-b" 'embark-bindings))

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

;; projectile
(use-package projectile
  :bind-keymap ("A-q A-p" . projectile-command-map))

;; pdf tools
(use-package pdf-tools
  :config
  (setq pdf-view-use-scaling t)

  :bind (:map pdf-view-mode-map
	      ("<wheel-left>" . image-backward-hscroll)
	      ("<wheel-right>" . image-forward-hscroll)
	      ("<mouse-4>" . (lambda () (interactive) (pdf-history-backward 1)))
	      ("<mouse-5>" . (lambda () (interactive) (pdf-history-forward 1)))
	      ("i" . pdf-view-scroll-down-or-previous-page)
	      ("k" . pdf-view-scroll-up-or-next-page)))
(pdf-loader-install)

;; snippets
;; (use-package yasnippet
;;   :config
;;   (bind-key "A-q A-y" 'yas-insert-snippet 'yas-minor-mode-map )
;;   (use-package yasnippet-snippets)
;;   (add-hook 'prog-mode-hook 'yas-minor-mode))



;;; ========== LaTeX ==========

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
  (setq cdlatex-math-modify-alist
	'((?t "\\text" nil t nil nil)
	  (?k "\\ket" nil t nil nil)
	  (?K "\\bra" nil t nil nil)
	  (?B "\\mathbb" nil t nil nil)
	  (?F "\\mathfrak" nil t nil nil)))

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
  (interactive "cInsert left-right pair: ")
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

(defun latex-preview-format-scale ()
    "Set the correct scale for latex fragments
Images somehow are rendered 1.5 times bigger on retina screens.
Counter that by dividing the factor out."
    (let ((true-scale 1.12))
      (/ true-scale
	 (if (equal (frame-monitor-attribute 'name)  "Built-in Retina Display")
	     1.5 1))))

;; auctex
(use-package tex-site :straight auctex
  :defer t
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  (add-hook 'LaTeX-mode-hook (lambda () (variable-pitch-mode 1)))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; somehow prettify symbols are broken in auctex. Need a manual fix
  (add-hook 'LaTeX-mode-hook (lambda () (setq prettify-symbols-alist tex--prettify-symbols-alist)))
  (add-hook 'LaTeX-mode-hook (lambda () (setq prettify-symbols-compose-predicate 'tex--prettify-symbols-compose-p)))
  
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t
	TeX-error-overview-open-after-TeX-run t
	TeX-parse-self t)
  (setq-default LaTeX-default-environment "align")
  (setq reftex-plug-into-AUCTeX '(nil t t t t)
	reftex-insert-label-flags '("s" "sfte"))
  (defun LaTeX-no-insert-label (orig-fun &rest args)
    "turn off automatic labeling"
    (apply orig-fun args '(t)))
  (advice-add 'LaTeX-label :around 'LaTeX-no-insert-label)

  ;; preview setup
  (use-package preview-dvisvgm)   ;does not seem to be working now
  (setq preview-image-type 'dvipng
	preview-scale-function #'latex-preview-format-scale)
  
  (defun my-latex-preview (&optional arg)
    "Make previewing in LaTeX act like org-latex-preview."
    (interactive "P")
    (cond
     ;; if called with C-u, clear preview in section
     ((equal arg '(4)) (preview-clearout-section))
     ;; if called with C-u C-u, preview entire document
     ((equal arg '(16)) (preview-document))
     ;; if called with C-u C-u C-u, clear preview for the document
     ((equal arg '(64)) (preview-clearout-document))
     (t
      ;; if an region is active, preview the region
      (if (TeX-active-mark)
	  (preview-region (region-beginning) (region-end))
	
	;; if an preview overlay exists, toggle preview status
	(catch 'done
	  (dolist (ovr (overlays-at (point)))
	    (if-let (preview-state (overlay-get ovr 'preview-state))
		(when preview-state
		  (unless (eq preview-state 'disabled)
		    (preview-clearout-at-point))
		    ;; (preview-toggle ovr 'toggle (selected-window)))
		  (throw 'done t))))
	  
	  ;; if no preview exists, preview current environment if cursor is in a environment
	  ;; otherwise preview current section
	  (if (string= (LaTeX-current-environment) "document")
	      (preview-section)
	    (preview-environment 1))))))
    (deactivate-mark))

  (defun my-latex-includegraphics (&optional char)
    "Insert a latex includegraphics snippet."
    (interactive "cInclude graphics with (s)cale/(w)idth/(h)eight: ")
    (let ((form "") (size "") (unit "")
	  (units '("\\textwidth" "\\linewidth" "\\columnwidth" "cm" "mm" "pt" "in")))
      (cond ((eq char ?s)
	     (setq form "scale="))
	    ((eq char ?w)
	     (setq form "width="))
	    ((eq char ?h)
	     (setq form "height="))
	    ((eq char ?\r)
	     (setq form "no input. insert blank template."))
	    (t (error "Invalid command -- must be one of s/w/h/RET")))
      (unless (eq char ?\r)
	(setq size (read-string (concat form ": ")))
	(unless (eq char ?s)
	  (setq unit (completing-read (concat form size) units nil t nil nil "\\linewidth"))))
      (insert "\\includegraphics[" form size unit "]{}"))
    (indent-according-to-mode)
    (backward-char 1))
  
  :general
  (:keymaps 'LaTeX-mode-map
	    "A-e" 'latex-backward-delete-word)
  (:keymaps 'LaTeX-mode-map :prefix "A-w"
	    "A-w" 'TeX-command-master
	    "A-a" (lambda () (interactive) (save-buffer) (TeX-command-run-all nil))
	    "A-b" 'TeX-command-buffer
	    "A-e" 'LaTeX-environment
	    "A-f" 'my-latex-includegraphics
	    "A-i" 'LaTeX-environment
	    "A-j" 'LaTeX-insert-item
	    "A-k" 'TeX-kill-job
	    "A-l" 'reftex-label
	    "A-p" 'my-latex-preview
	    "A-r" 'reftex-reference
	    "A-t" 'reftex-toc
	    "A-s" 'LaTeX-section
	    "A-v" 'TeX-view
	    "A-9" 'reftex-label
	    "A-0" 'reftex-reference
	    "A-[" 'reftex-citation
	    "A-]" 'LaTeX-close-environment
	    "A-=" 'reftex-toc
	    "RET" 'TeX-insert-macro
	    "*" 'LaTeX-mark-section
	    "." 'LaTeX-mark-environment)
  (:keymaps 'TeX-error-overview-mode-map
	    "i" 'TeX-error-overview-previous-error
	    "k" 'TeX-error-overview-next-error))

;;; ========== org ==========

(setq my-bib-file "~/Documents/Papers/master.bib"
      my-bib-dir "~/Documents/Papers/")

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
  
  (defun my-org-latex-preview (&optional arg)
    "Set the render scale in to match different monitor before calling `latex-preview'."
    (interactive "P")
    (plist-put org-format-latex-options :scale (latex-preview-format-scale))
    (plist-put org-format-latex-options :background 'default)
    (funcall-interactively 'org-latex-preview arg))
  (setq org-element-use-cache nil) ; setting it to t cause some error messages when previewing. turning it off for now. revisit after org 9.7 update (along with transparent background issues).
  
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-image-directory ".ltximg/")
  (setq org-highlight-latex-and-related '(native latex script entities))
  (defun my-resize-org-latex-overlays ()
    (cl-loop for o in (car (overlay-lists))
	     if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
	     do (plist-put (cdr (overlay-get o 'display))
			   :scale (expt text-scale-mode-step
					text-scale-mode-amount))))

  ;; function to insert latex environment in org-mode
  (setq my-org-environment-default "align*")
  (setq my-org-environment-list '("Bmatrix" "Vmatrix" "align" "align*" "aligned" "array" "bmatrix" "cases" "eqnarray" "eqnarray*" "equation" "equation*" "figure" "figure*" "matrix" "pmatrix" "smallmatrix" "vmatrix"))
  (defun my-org-insert-environment ()
    (interactive)
    (let ((env (completing-read "Environment type: "
				my-org-environment-list nil t nil nil my-org-environment-default)))
      (indent-according-to-mode)
      (insert "\\begin{" env "}")
      (newline-and-indent 2)
      (insert "\\end{" env "}")
      (previous-line)))

  ;; header and bullet settings
  (use-package org-superstar
    :config
    (setq org-hide-leading-stars t)
    (set-face-attribute 'org-level-1 nil :weight 'semi-bold :height 1.25 :box nil)
    (set-face-attribute 'org-level-2 nil :weight 'normal :height 1.15 :box nil)
    (set-face-attribute 'org-level-3 nil :weight 'normal :height 1.05 :box nil)
    :hook (org-mode . (lambda () (org-superstar-mode 1))))
  
  (setq org-startup-indented t)
  (setq org-return-follows-link t)
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

  (setq org-cite-global-bibliography (list (expand-file-name my-bib-file)))
  (setq org-cite-export-processors '((latex . (bibtex "JHEP")) (t basic)))
  (setq org-export-with-toc nil)

  (add-to-list 'org-latex-packages-alist '("" "braket" t))
  (add-to-list 'org-latex-packages-alist '("" "cancel" t))
  (add-to-list 'org-latex-packages-alist '("margin=1in" "geometry" nil))
  (setq org-format-latex-header (concat org-format-latex-header "\n\\usepackage{sansmathfonts}\n\\DeclareMathOperator{\\tr}{Tr}"))
  (with-eval-after-load 'ox
    (setf (nth 0 org-latex-classes)
	  '("article" "\\documentclass[11pt]{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\DeclareMathOperator{\\tr}{Tr}\n[EXTRA]"
	    ("\\section{%s}" . "\\section*{%s}")
	    ("\\subsection{%s}" . "\\subsection*{%s}")
	    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	    ("\\paragraph{%s}" . "\\paragraph*{%s}")
	    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "\\[" "\\]") ;; inherit from latex mode, fix later

  (use-package org-transclusion
    :config
    (setq org-transclusion-exclude-elements "drawer keyword")
    (set-face-attribute 'org-transclusion-fringe nil :foreground "green" :background "green"))
  
  :hook
  (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'my-resize-org-latex-overlays nil t)))
  
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
	    "A-t" 'consult-org-heading
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
	    "t" 'org-transclusion-mode
	    "A-w" 'org-ctrl-c-ctrl-c
	    "A-x" 'org-refile
	    "A-c" 'org-copy
	    "A-d" 'org-deadline
	    "A-e" 'org-export-dispatch
	    "A-l" 'org-insert-link
	    "A-a" 'org-attach
	    "A-q" 'org-set-tags-command
	    "A-s" 'org-schedule
	    "A-p" 'my-org-latex-preview
	    "A-i" 'my-org-insert-environment
	    "A-r" 'org-redisplay-inline-images
	    "A-," 'org-insert-structure-template
	    )
  (:keymaps 'org-cdlatex-mode-map
	    "A-e" 'latex-backward-delete-word
	    "^" 'org-self-insert-command
	    "_" 'org-self-insert-command))
;; roam
(use-package org-roam
  :config
  (setq org-roam-directory (expand-file-name "~/org/roam"))
  (org-roam-db-autosync-mode)
  (setq org-roam-completion-everywhere t)
  
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    (let ((level (org-roam-node-level node)))
      (concat
       (when (> level 0) (concat (org-roam-node-file-title node) " > "))
       (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
       (org-roam-node-title node))))
  
  (cl-defmethod org-roam-node-info ((node org-roam-node))
    (cdr (assoc-string "INFO" (org-roam-node-properties node))))

  (setq org-roam-node-display-template
	(concat "${hierarchy:30} " "${info:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (setq org-roam-capture-templates
	'(("d" "default" plain "%?"
	   :target (file+head "%<%s>.org" ":PROPERTIES:\n:INFO:   %^{info}\n:END:\n#+title: ${title}")
	   :unnarrowed t
	   :empty-lines-before 1)
	  ("t" "talk" plain "\n+ Speaker: %?\n+ Title: \n+ Date: %t\n+ Event: \n---------------------------------------------\n"
	   :target (file+head "%<%s>.org"
			      ":PROPERTIES:\n:INFO:   %^{talk-title}\n:END:\n#+title: ${title}\n#+filetags: :talk:")
	   :unnarrowed t
	   :empty-lines-before 1)
	  ("p" "paper" plain "\n+ Title: ${citar-title}\n+ Author(s): ${citar-author}\n+ Created: %t \n---------------------------------------------\n%?"
	   :target (file+head "%<%s>.org" 
			      ":PROPERTIES:\n:INFO:   ${citar-title}\n:END:\n#+title: ${citar-citekey}\n#+filetags: :paper:")
	   :unnarrowed t
	   :empty-lines-before 1)))

  (use-package consult-org-roam
    :init
    (setq consult-org-roam-buffer-narrow-key ?r)
    :config
    (consult-org-roam-mode 1))
  
  :general
  ("A-q A-o A-f" 'org-roam-node-find
   "A-q A-o A-o" 'org-roam-node-find
   "A-q A-o A-i" 'org-roam-node-insert
   "A-q A-o A-b" 'org-roam-buffer-toggle)
  
  (:keymaps 'org-capture-mode-map
	    "A-w A-w" 'org-capture-finalize
	    "A-w A-k" 'org-capture-kill
	    "A-w A-r" 'org-capture-refile))

;; citar
(use-package citar
  :config
  (setq citar-bibliography my-bib-file
	citar-library-paths (list my-bib-dir))
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)  ;; use consult-completing-read for enhanced interface
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "file-o"
              :face 'all-the-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (all-the-icons-octicon
              "link"
              :face 'all-the-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (all-the-icons-material
              "speaker_notes"
              :face 'all-the-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (all-the-icons-faicon
              "circle-o"
              :face 'all-the-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
  (list citar-indicator-files-icons
        citar-indicator-links-icons
        citar-indicator-notes-icons
        citar-indicator-cited-icons))
  
  (setq citar-symbol-separator "  ")
  (setq citar-file-open-functions
	`(("html" . citar-file-open-external)
	  ("pdf" . (lambda (fpath) (call-process "open" nil 0 nil "-a" "Preview.app" fpath)))
	  ("t" . find-file)))

  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)

  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))


(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package citar-org-roam
  :after citar
  :config
  (setq citar-org-roam-capture-template-key "p")
  (citar-org-roam-mode))


;;; ========== Major modes ==========

;; arxiv-mode
(use-package arxiv-mode :straight (:type git :host github :repo "Simon-Lin/arxiv-mode" :branch "master")
  :config
  (set-face-attribute 'arxiv-abstract-face nil :inherit 'default)
  (set-face-attribute 'arxiv-subfield-face nil :inherit 'font-lock-variable-name-face)
  (setq arxiv-use-variable-pitch t)
  (setq arxiv-startup-with-abstract-window t)
  (setq arxiv-default-download-folder my-bib-dir)
  (setq arxiv-default-bibliography my-bib-file)
  (setq arxiv-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil "-a" "Preview.app" fpath)))
  :bind (:map arxiv-mode-map
	      ("i" . arxiv-prev-entry)
	      ("k" . arxiv-next-entry)
	      ("l" . arxiv-lookup-inspire))
  :hook
  ('arxiv-mode . 'centaur-tabs-local-mode)
  ('arxiv-abstract-mode . 'centaur-tabs-local-mode))

(defun arxiv-lookup-inspire ()
  "Look up the inspire record of the current arxiv entry." 
  (interactive)
  (let ((arxiv-id (alist-get 'id (nth arxiv-current-entry arxiv-entry-list))))    
    (inspire-literature-search (read-string "Search on inspire-hep: " arxiv-id))))

;; inspire
(use-package inspire :straight (:type git :host github :repo "Simon-Lin/inspire.el" :branch "master")
  :config
  (setq inspire-default-download-folder my-bib-dir)
  (setq inspire-master-bibliography-file my-bib-file)
  (setq inspire-pdf-open-function (lambda (fpath) (call-process "open" nil 0 nil "-a" "Preview.app" fpath)))
  :bind (:map inspire-mode-map
	      ("i" . inspire-prev-entry)
	      ("k" . inspire-next-entry))
  :hook
  ('inspire-mode . 'centaur-tabs-local-mode)
  ('inspire-record-mode . 'centaur-tabs-local-mode)
  ('inspire-author-mode . 'centaur-tabs-local-mode))

;; ebib
(use-package ebib
  :config
  (setq ebib-preload-bib-files (list my-bib-file))
  (setq ebib-window-vertical-split nil
	ebib-index-window-size 30)
  (defvar ebib-frame nil)
  (defun ebib-newframe (orig-fun &rest args)
    "Display ebib window in a new frame."
    (unless (frame-live-p ebib-frame)
      (setq ebib-frame (make-frame '((name . "*ebib*") (width . 240) (height . 80)))))
    (select-frame  ebib-frame)
    (apply orig-fun args))
  (defun ebib-kill-frame ()
    (when (frame-live-p ebib-frame)
      (unless ebib--initialized
	(delete-frame ebib-frame)
	(setq ebib-frame nil))))
  (advice-add 'ebib :around 'ebib-newframe)
  (advice-add 'ebib-quit :after 'ebib-kill-frame)

  :bind
  (:map ebib-index-mode-map
	("i" . ebib-prev-entry)
	("k" . ebib-next-entry))
  (:map ebib-entry-mode-map
	("i" . ebib-prev-field)
	("k" . ebib-next-field)))



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

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode)
;;   (setq treesit-auto-install 'prompt))
;; this package has some problem causing emacs to quit whenever there is a completion option.
;; not sure what the cuse is. Disable it for now.



;;; init.el ends here

