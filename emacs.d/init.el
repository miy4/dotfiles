;; init.el --- Personal Emacs configuration

;;; Commentary:

;; This package represents my personal Emacs configuration.  That's all :)

;;; Code:

;; You can launch Emacs like this;
;; % emacs -q -l path/to/somewhere/init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; You have emacs-native-comp (aka gccemacs)?
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t))

;; Package Manager
(eval-when-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))


;; Set up general purpose vars and key bindings

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

(setq gc-cons-threshold 12800000)
(setq read-process-output-max (* 1024 1024))
(setq backup-inhibited t)
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))
(setq auto-save-list-file-prefix (concat user-emacs-directory "auto-save/.saves-"))
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default kill-whole-line t)

;; Prevent from killing buffers
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

(fset 'yes-or-no-p 'y-or-n-p)
(setq mouse-yank-at-point t)

(setq show-paren-delay 0)
(show-paren-mode t)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "M-o") 'my/open-line-below)
(global-set-key (kbd "M-O") 'my/open-line-above)

(defun my/open-line-below ()
  "Open a new line below the current line."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun my/open-line-above ()
  "Open a new line above the current line."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(define-prefix-command 'my-toggle-map)
(global-set-key (kbd "C-c t") 'my-toggle-map)
(define-key my-toggle-map (kbd "l") 'toggle-truncate-lines)
(define-key my-toggle-map (kbd "t") 'my/toggle-transparency)

(defvar my/active-transparency 90)
(defvar my/inactive-transparency 100)
(defun my/toggle-transparency ()
  (interactive)
  (let ((alpha-setting (cons my/active-transparency my/inactive-transparency))
        (alpha-current (frame-parameter nil 'alpha)))
    (if (equal alpha-current alpha-setting)
        (my/disable-transparency)
      (my/enable-transparency alpha-setting))))

(defun my/enable-transparency (&optional alpha)
  (interactive)
  (let ((setting (or alpha (90 . 90))))
    (set-frame-parameter nil 'alpha setting)))

(defun my/disable-transparency ()
  (interactive)
  (set-frame-parameter nil 'alpha '(100 . 100)))

(define-prefix-command 'my-window-map)
(global-set-key (kbd "C-c w") 'my-window-map)

;; visual things
(setq frame-title-format "%f")
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)
(size-indication-mode t)
(setq ring-bell-function 'ignore)
(if (>= (x-display-pixel-width) 1920)
    (add-to-list 'default-frame-alist '(font . "Cica-12"))
  (add-to-list 'default-frame-alist '(font . "Cica")))

;; macOS setting
(when (eq system-type 'darwin)
  ;; Use command-key as meta
  (setq ns-command-modifier 'meta)

  ;; File name coding system
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))


;; Setup packages

;; https://github.com/Malabarba/paradox
(use-package paradox
  :commands (paradox-list-packages paradox-upgrade-packages)
  :init
  (setq paradox-github-token t)
  (load (locate-user-emacs-file ".paradox-github-token") :noerror :nomessage))

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(use-package mozc
  :if (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  :custom
  (default-input-method "japanese-mozc")
  (mozc-leim-title "あ")
  :bind
  (([henkan] . (lambda ()
                 (interactive)
                 (mozc-mode 1)))
   :map mozc-mode-map
   ([muhenkan] . (lambda ()
                   (interactive)
                   (mozc-handle-event 'enter)
                   (mozc-mode -1)))))

;; https://github.com/challenger-deep-theme/emacs
(use-package challenger-deep-theme
  :hook
  (window-setup . (lambda () (unless (display-graphic-p (selected-frame))
                               (set-face-background 'default "unspecified-bg" (selected-frame)))))
  :config
  (load-theme 'challenger-deep t))

;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :hook
  (emacs-startup . doom-modeline-mode)
  :init
  (set-face-attribute 'mode-line nil :family "Cica" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "Cica" :height 100)
  :config
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info buffer-position parrot)
    '(misc-info persp-name lsp debug minor-modes buffer-encoding major-mode process vcs checker)))

;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'column))

(use-package linum
  :ensure nil
  :commands linum-mode
  :custom
  (linum-format "%4d "))

;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode 1))

;; https://github.com/Malabarba/beacon
(use-package beacon
  :custom
  (beacon-size 20)
  (beacon-color "#62d6e8")
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.1)
  :config
  (beacon-mode 1))

;; http://www.dr-qubit.org/emacs.php
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package winner
  :ensure nil
  :bind
  (:map my-window-map
        ("u" . winner-undo)
        ("U" . winner-redo))
  :config
  (winner-mode))

;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :bind
  (:map my-window-map
        ("W" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (ace-window-display-mode 1))

;; https://github.com/wasamasa/shackle
(use-package shackle
  :custom
  (shackle-default-alignment 'below)
  (shackle-default-size 0.3)
  (shackle-rules
   '((compilation-mode :align t)
     ("*Completions*"  :align t)
     ("*quickrun*"     :select t :inhibit-window-quit t :other t)
     ("*Help*"         :select t :inhibit-window-quit t :other t)
     ("*Messages*"     :select t :inhibit-window-quit t :other t)))
  :config
  (shackle-mode 1))

(define-prefix-command 'ivy-command-map)
(global-set-key (kbd "C-z") 'ivy-command-map)

;; https://github.com/abo-abo/swiper
(use-package ivy)
(use-package swiper
  :after ivy)
(use-package counsel
  :after swiper
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   :map ivy-command-map
   ("m" . ivy-switch-buffer)
   ("s" . swiper)
   ("y" . counsel-yank-pop)
   ("d" . counsel-descbinds)
   ("i" . counsel-imenu))
  :custom
  (ivy-format-function 'ivy-format-function-line)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "")
  (ivy-initial-inputs-alist nil)
  (counsel-yank-pop-separator "\n-------\n")
  :config
  (ivy-mode 1))

;; https://github.com/DarwinAwardWinner/amx
(use-package amx)

;; https://github.com/mkcms/ivy-yasnippet
(use-package ivy-yasnippet
  :after (ivy yasnippet)
  :bind
  (:map ivy-command-map
   ("a" . ivy-yasnippet)))

(use-package view
  :ensure nil
  :bind
  (("C-c v" . view-mode)
   :map view-mode-map
   ("h"   . backward-char)
   ("j"   . next-line)
   ("k"   . previous-line)
   ("l"   . forward-char)
   ("w"   . forward-word)
   ("b"   . backward-word)
   ("C-u" . scroll-down)
   ("SPC" . scroll-up)
   ("C-d" . scroll-up)
   ("^"   . mwim-beginning-of-code-or-line)
   ("0"   . beginning-of-line)
   ("$"   . end-of-line)
   ("g"   . beginning-of-buffer)
   ("G"   . end-of-buffer)
   ("f"   . avy-goto-char)))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-idle-delay 1.0)
  :config
  (which-key-mode))

(use-package dired
  :ensure nil
  :commands dired
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-dwim-target t)
  (dired-listing-switches "-lahGF --group-directories-first --time-style=long-iso")
  :config
  (when (eq system-type 'cygwin)
    (setq dired-guess-shell-alist-user
          '((".png"  "cygstart")
            (".jpeg" "cygstart")
            (".jpg"  "cygstart")
            (".gif"  "cygstart")
            (".pdf"  "cygstart")
            (".doc"  "cygstart")
            (".docx" "cygstart")
            (".xls"  "cygstart")
            (".xlsx" "cygstart")
            (".ppt"  "cygstart")
            (".pptx" "cygstart")))))

;; https://github.com/crocket/dired-single
(use-package dired-single
  :after dired
  :bind
  ("C-x C-d" . dired-single-magic-buffer-current-dir)
  ("C-x C-j" . dired-single-magic-buffer)
  :config
  (defun dired-single-magic-buffer-current-dir ()
    (interactive)
    (dired-single-magic-buffer default-directory))
  (defun dired-single-up-directory ()
    (interactive)
    (dired-single-buffer ".."))
  (bind-keys
   :map dired-mode-map
   ("<return>" . dired-single-buffer)
   ("^"        . dired-single-up-directory)))

;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :commands neotree
  :bind
  (:map neotree-mode-map
   ("C-c C-n"     . nil)
   ("C-c C-d"     . nil)
   ("C-c C-r"     . nil)
   ("C-c C-p"     . nil)
   ("."           . neotree-enter)
   ("<C-return>"  . neotree-change-root)
   ("C-c w"       . neotree-create-node)
   ("C-c +"       . neotree-create-node)
   ("C-c d"       . neotree-delete-node)
   ("C-c r"       . neotree-rename-node))
  :custom
  (neo-theme 'nerd)
  (neo-smart-open t))

;; https://github.com/bmag/imenu-list
(use-package imenu-list
  :commands imenu-list-smart-toggle
  :custom-face
  (imenu-list-entry-face-0 ((t (:foreground "#565575"))))
  (imenu-list-entry-face-1 ((t (:foreground "#cbe3e7"))))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize nil))

;; https://github.com/abo-abo/avy
(use-package avy
  :bind
  (("M-c" . avy-goto-char)
   ("M-l" . avy-goto-line))
  :custom
  (avy-background t))

;; https://github.com/rejeep/wrap-region.el
(use-package wrap-region
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "`" "`" nil 'markdown-mode))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind
  ("M-m" . er/expand-region)
  :custom
  (expand-region-contract-fast-key "M")
  (expand-region-reset-fast-key "*"))

;; https://github.com/fgallina/region-bindings-mode
(use-package region-bindings-mode
  :config
  (region-bindings-mode-enable))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind
  (:map region-bindings-mode-map
   ("a" . mc/mark-all-like-this)
   ("p" . mc/mark-previous-like-this)
   ("n" . mc/mark-next-like-this)
   ("u" . mc/unmark-next-like-this)
   ("U" . mc/unmark-previous-like-this)
   ("s" . mc/skip-to-next-like-this)
   ("S" . mc/skip-to-previous-like-this)
   ("e" . mc/edit-lines)
   ("^" . mc/edit-beginnings-of-lines)
   ("$" . mc/edit-ends-of-lines)))

;; https://github.com/alezost/mwim.el
(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))

;; https://github.com/thierryvolpiatto/zop-to-char
(use-package zop-to-char
  :bind
  ([remap zap-to-char] . zop-up-to-char))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package git-gutter
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:foreground "#ffb378"))))
  (git-gutter:added    ((t (:foreground "#95ffa4"))))
  (git-gutter:deleted  ((t (:foreground "#ff8080"))))
  :config
  (global-git-gutter-mode +1))

(use-package subword
  :ensure nil
  :hook
  (prog-mode . subword-mode))

;; http://company-mode.github.io/
(use-package company
  :defer t
  :bind
  (:map company-active-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t))

;; https://github.com/syohex/emacs-quickrun
(use-package quickrun
  :bind
  (("C-c C-q" . quickrun)
   ("C-c C-w" . quickrun-with-arg)))

;; https://github.com/ReanGD/emacs-multi-compile
(use-package multi-compile
  :bind
  ("C-c C-r" . multi-compile-run)
  :custom
  (multi-compile-completion-system 'ido))

;; https://github.com/nschum/highlight-symbol.el
(use-package highlight-symbol
  :defer t
  :custom-face
  (highlight-symbol-face ((nil (:background "#62d196" :foreground "#100e23"))))
  :custom
  (highlight-symbol-idle-delay 0.3))

;; https://github.com/capitaomorte/autopair
(use-package autopair
  :defer t)

;; https://github.com/flycheck/flycheck
(use-package flycheck
  :defer t
  :init
  (define-fringe-bitmap 'flycheck-fringe-dotdot
    (vector #b00110011
            #b00110011
            #b00000000
            #b11001100
            #b11001100
            #b00000000
            #b00110011
            #b00110011
            #b00000000
            #b11001100
            #b11001100
            #b00000000
            #b00110011
            #b00110011
            #b00000000
            #b11001100
            #b11001100))
  (define-fringe-bitmap 'flycheck-fringe-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00111000
            #b01111100
            #b11111110
            #b11111110
            #b11111110
            #b01111100
            #b00111000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-ball
    :fringe-face 'flycheck-fringe-info)
  :custom-face
  (flycheck-fringe-info    ((t (:foreground "#95ffa4"))))
  (flycheck-fringe-warning ((t (:foreground "#ffe9aa"))))
  (flycheck-fringe-error   ((t (:foreground "#ff8080")))))

;; https://github.com/meqif/flymake-diagnostic-at-point
(use-package flymake-diagnostic-at-point
  :disabled
  :after flymake
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.1)
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
  (flymake-diagnostic-at-point-error-prefix " ")
  :custom-face
  (popup-tip-face ((t (:background "#100e23" :foreground "#c991e1")))))

;; https://github.com/capitaomorte/yasnippet
(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
   ("<tab>".  nil)
   ("TAB"  . nil)
   ("<backtab>" . yas-expand))
  :custom
  ;(yas-snippet-dirs '("~/opt/emacs/snippets" "~/.config/emacs/snippets" "~/.emacs.d/snippets" "~/.emacs.d/site-snippets"))
  (yas-snippet-dirs '("~/opt/emacs/snippets"))
  :config
  (yas-reload-all))

;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-diagnostics-provider :auto)
  (lsp-eldoc-render-all t)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . (lambda () (local-set-key (kbd "M-/") 'company-capf))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil))

;; TODO lsp-treemacs + all-the-icons
(use-package treemacs)
(use-package treemacs-all-the-icons
  :after treemacs)
(use-package lsp-treemacs
  :after treemacs-all-the-icons
  :commands lsp-treemacs-errors-list)

(use-package lsp-java)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java
  :ensure nil)

;; https://github.com/tarsius/auto-compile
(use-package auto-compile
  :hook
  (emacs-lisp . auto-compile-mode))

;; depends: http://www.shellcheck.net/
(use-package sh-script
  :hook
  (sh-mode . yas-minor-mode)
  (sh-mode . flycheck-mode))

;; https://github.com/dominikh/go-mode.el
;; depends: https://github.com/saibing/bingo
;; depends: go get golang.org/x/tools/cmd/goimports
(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp)
  (go-mode . autopair-mode)
;  (go-mode . highlight-symbol-mode)
  (go-mode . yas-minor-mode)
  (before-save . gofmt-before-save)
  :custom
  (gofmt-command "goimports"))

;; https://github.com/rust-lang/rust-mode
;; depends: rustup component add rls
;; depends: rustup component add rust-analysis
;; depends: rustup component add rust-src
(use-package rust-mode
  :mode "\\.rs\\'"
  :hook
  (rust-mode . lsp)
;  (rust-mode . (lambda () (let ((lsp-rust-configuration (make-hash-table)))
;      (puthash :clippy_preference "on" lsp-rust-configuration)
;      (lsp--set-configuration `(:rust ,lsp-rust-configuration)))))
  (rust-mode . autopair-mode)
  (rust-mode . highlight-symbol-mode)
  (rust-mode . yas-minor-mode)
  :custom
  (rust-format-on-save t)
  (lsp-rust-server 'rust-analyzer))

(use-package java-mode
  :ensure nil
  :hook
  (java-mode . lsp)
  (java-mode . autopair-mode))

;; https://github.com/fxbois/web-mode
(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.css\\'"   . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  ;; https://github.com/fxbois/web-mode/issues/358
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t))

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :mode
  (("\\`README\\.md\\'" . gfm-mode)
   ("\\.md\\'"          . markdown-mode)
   ("\\.markdown\\'"    . markdown-mode))
  :config
  (bind-keys
   :map markdown-mode-map
   ("M-p" . nil)
   ("M-n" . nil)))

;; https://github.com/spotify/dockerfile-mode
;; requires: hadolint
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

;; https://github.com/dryman/toml-mode.el
(use-package toml-mode
  :mode "\\.toml\\'")

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package eww
  :commands eww
  :bind
  (:map eww-mode-map
   ("g" . beginning-of-buffer)
   ("G" . end-of-buffer)
   ("j" . scroll-up-line)
   ("k" . scroll-down-line)
   ("b" . scroll-down)
   ("F" . eww-forward-url)
   ("B" . eww-back-url)
   ("r" . eww-reload))
  :config
  (defvar eww-disable-colorize t)
  (defun my/shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    (unless eww-disable-colorize
      (funcall orig start end fg)))
  (advice-add 'shr-colorize-region :around 'my/shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'my/shr-colorize-region--disable)
  (defun my/eww-disable-color ()
    (interactive)
    (setq-local eww-disable-colorize t)
    (eww-reload))
  (defun my/eww-enable-color ()
    (interactive)
    (setq-local eww-disable-colorize nil)
    (eww-reload)))

;;; init.el ends here
