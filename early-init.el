;;; early-init.el -*- lexical-binding: t; -*-

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 16 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216)))

;; disable the default package.el because we use straight.el for pacakge management
(setq package-enable-at-startup nil)

;; disable the loading of visual elements here hastens startup time
;; note that menu-bar functions differently in emacs-mac port.
;; see https://bitbucket.org/mituharu/emacs-mac/src/892fa7b2501a403b4f0aea8152df9d60d63f391a/doc/emacs/macport.texi?at=master#macport.texi-529
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; use Unicode
(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq frame-inhibit-implied-resize t)
;; need to set the frame property here so dashboard knows about maximzed frame width
(add-to-list 'initial-frame-alist '(fullscreen . fullboth))
(add-to-list 'default-frame-alist '(alpha-background . 30))
