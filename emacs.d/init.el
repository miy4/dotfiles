;; init.el --- Personal Emacs configuration

;;; Commentary:

;; This package represents my personal Emacs configuration. That's all :)

;;; Code:

;; emacs -q -l path/to/somewhere/init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

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

(progn "Set up general purpose vars and key bindings"
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)

  (setq backup-inhibited t)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default kill-whole-line t)

  (fset 'yes-or-no-p 'y-or-n-p)
  (setq mouse-yank-at-point t)

  (setq show-paren-delay 0)
  (show-paren-mode t)

  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-m") 'newline-and-indent)
  (global-set-key (kbd "M-o") 'my/open-line-below)
  (global-set-key (kbd "M-O") 'my/open-line-above)

  (define-prefix-command 'my-toggle-map)
  (global-set-key (kbd "C-c t") 'my-toggle-map)
  (define-key my-toggle-map (kbd "l") 'toggle-truncate-lines)
  (define-key my-toggle-map (kbd "t") 'my/toggle-transparency))

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


(defun my/open-line-below ()
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun my/open-line-above ()
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(progn "Package Manager"
  ;; https://github.com/Malabarba/paradox
  (use-package paradox
    :defer t
    :init
    (setq paradox-github-token t)
    (load (locate-user-emacs-file ".paradox-github-token") :noerror :nomessage)))

(progn "Console Integration"
  ;; https://github.com/purcell/exec-path-from-shell
  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns x))
    :config
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(progn "Server Daemon"
  (use-package server
    :ensure nil
    :config
    (unless (server-running-p)
      (server-start))))

(progn "Input Method"
  (use-package mozc
    :ensure nil
    :if (and (eq system-type 'gnu/linux)
             (getenv "WSLENV"))
    :init
    (setq default-input-method "japanese-mozc")
    (custom-set-variables '(mozc-leim-title "あ"))))

(progn "Looks of Emacs"
  (setq frame-title-format "%f")
  (setq inhibit-startup-screen t)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (column-number-mode t)
  (size-indication-mode t)
  (setq ring-bell-function 'ignore)
  (add-to-list 'default-frame-alist '(font . "Cica"))

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
    (after-init . doom-modeline-mode)
    :config
    (doom-modeline-def-modeline 'main
      '(bar workspace-number window-number god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp debug minor-modes input-method buffer-encoding major-mode process vcs checker)))

  ;; https://github.com/DarthFennec/highlight-indent-guides
  (use-package highlight-indent-guides
    :hook
    (prog-mode . highlight-indent-guides-mode)
    :config
    (setq highlight-indent-guides-method 'column))

  (use-package linum
    :ensure nil
    :defer t
    :config
    (setq linum-format "%4d "))

  ;; https://github.com/k-talo/volatile-highlights.el
  (use-package volatile-highlights
    :diminish volatile-highlights-mode
    :config
    (volatile-highlights-mode 1))

  ;; https://github.com/Malabarba/beacon
  (use-package beacon
    :config
    (setq beacon-size 20)
    (setq beacon-color "#62d6e8")
    (setq beacon-blink-delay 0.1)
    (setq beacon-blink-duration 0.1)
    (beacon-mode 1))

  ;; http://www.dr-qubit.org/emacs.php
  (use-package undo-tree
    :diminish undo-tree-mode
    :config
    (global-undo-tree-mode 1)))

(progn "Window management"
  (use-package winner
    :ensure nil
    :config
    (winner-mode))

  ;; https://github.com/abo-abo/ace-window
  (use-package ace-window
    :after avy
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (ace-window-display-mode 1))

  (define-prefix-command 'my-window-map)
  (global-set-key (kbd "C-c w") 'my-window-map)
  (define-key my-window-map (kbd "u") 'winner-undo)
  (define-key my-window-map (kbd "U") 'winner-redo)
  (define-key my-window-map (kbd "W") 'ace-window))

(progn "Interface Enhancement"
  ;; https://github.com/wasamasa/shackle
  (use-package shackle
    :config
    (setq shackle-default-alignment 'below)
    (setq shackle-default-size 0.3)
    (setq shackle-rules
          '((compilation-mode :align t)
            ("*Completions*"  :align t)
            ("*quickrun*"     :select t :inhibit-window-quit t :other t)
            ("*Help*"         :select t :inhibit-window-quit t :other t)
            ("*Messages*"     :select t :inhibit-window-quit t :other t)))
    (shackle-mode 1))

  ;; https://github.com/emacs-helm/helm
  (use-package helm
    :defer t
    :diminish helm-mode
    :config
    (bind-keys
     :map helm-map
     ("C-M-n" . helm-next-source)
     ("C-M-p" . helm-previous-source)
     ("C-z"   . helm-select-action)
     ("C-h"   . delete-backward-char))
    (set-face-attribute 'helm-selection nil :background "magenta" :foreground "white"))

  (use-package helm-config :ensure helm
    :bind
    (("C-c h"   . helm-for-files)
     ("M-x"     . helm-M-x)
     ("M-y"     . helm-show-kill-ring)
     ("C-x C-f" . helm-find-files))
    :init
    (autoload 'helm-command-prefix "helm-config" nil t 'keymap)
    (bind-key "C-z" 'helm-command-prefix)
    :config
    (require 'helm)
    (bind-keys
     :map helm-command-prefix
     ("i" . helm-imenu)
     ("m" . helm-mini))
    (setq helm-delete-minibuffer-contents-from-point t)
    (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
      "Emulate `kill-line' in helm minibuffer"
      (kill-new (buffer-substring (point) (field-end)))))

  (use-package helm-files :ensure helm
    :defer t
    :config
    (bind-keys
     :map helm-read-file-map
     ("TAB" . helm-execute-persistent-action))
    (bind-keys
     :map helm-find-files-map
     ("TAB" . helm-execute-persistent-action)
     ("C-h" . delete-backward-char)))

  ;; https://github.com/emacs-helm/helm-descbinds
  (use-package helm-descbinds
    :commands (helm-descbinds helm-descbinds-mode)
    :init
    (with-eval-after-load 'helm-config
      (bind-key "d" 'helm-descbinds helm-command-map))
    :config
    (helm-descbinds-mode))

  ;; https://github.com/ShingoFukuyama/helm-swoop
  (use-package helm-swoop
    :defer t
    :init
    (with-eval-after-load 'helm-config
      (bind-key "s" 'helm-swoop helm-command-map)
      (bind-key "S" 'helm-swoop-back-to-last-point helm-command-map))
    :config
    (setq helm-swoop-move-to-line-cycle t))

  ;; https://github.com/emacs-jp/helm-c-yasnippet
  (use-package helm-c-yasnippet
    :defer t
    :init
    (with-eval-after-load 'helm-config
      (bind-key "y" 'helm-yas-complete helm-command-map))
    :config
    (setq helm-yas-space-match-any-greedy t))

  (use-package view
    :ensure nil
    :bind
    ("C-c v" . view-mode)
    :config
    (bind-keys
     :map view-mode-map
     ("h"     . backward-char)
     ("j"     . next-line)
     ("k"     . previous-line)
     ("l"     . forward-char)
     ("w"     . forward-word)
     ("b"     . backward-word)
     ("C-u"   . scroll-down)
     ("SPC"   . scroll-up)
     ("C-d"   . scroll-up)
     ("^"     . mwim-beginning-of-code-or-line)
     ("0"     . beginning-of-line)
     ("$"     . end-of-line)
     ("g"     . beginning-of-buffer)
     ("G"     . end-of-buffer)
     ("f"     . avy-goto-char))))

(progn "Keys Cheatsheet"
  ;; https://github.com/justbur/emacs-which-key
  (use-package which-key
    :config
    (setq which-key-popup-type 'side-window)
    (setq which-key-side-window-location 'bottom)
    (setq which-key-idle-delay 1.0)
    (which-key-mode)))

(progn "File Manager"
  (use-package dired
    :ensure nil
    :defer t
    :config
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)
    (setq dired-dwim-target t)
    (setq dired-listing-switches "-lahGF --group-directories-first --time-style=long-iso")
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
    :init
    (setq-default neo-smart-open t)
    (setq-default neo-dont-be-alone t)
    :config
    (setq neo-theme 'nerd)
    (bind-keys
     :map neotree-mode-map
     ("C-c C-n"     . nil)
     ("C-c C-d"     . nil)
     ("C-c C-r"     . nil)
     ("C-c C-p"     . nil)
     ("."           . neotree-enter)
     ("<C-return>"  . neotree-change-root)
     ("C-c w"       . neotree-create-node)
     ("C-c +"       . neotree-create-node)
     ("C-c d"       . neotree-delete-node)
     ("C-c r"       . neotree-rename-node))))

(progn "Navigation"
  ;; https://github.com/abo-abo/avy
  (use-package avy
    :bind
    (("M-c" . avy-goto-char)
     ("M-l" . avy-goto-line))
    :config
    (setq avy-background t)))

(progn "Editing"
  ;; https://github.com/rejeep/wrap-region.el
  (use-package wrap-region
    :config
    (wrap-region-global-mode t)
    (wrap-region-add-wrapper "`" "`" nil 'markdown-mode))

  ;; https://github.com/magnars/expand-region.el
  (use-package expand-region
    :bind
    (("M-m" . er/expand-region))
    :config
    (setq expand-region-contract-fast-key "M")
    (setq expand-region-reset-fast-key "*"))

  ;; https://github.com/fgallina/region-bindings-mode
  (use-package region-bindings-mode
    :config
    (region-bindings-mode-enable))

  ;; https://github.com/magnars/multiple-cursors.el
  (use-package multiple-cursors
    :config
    (bind-keys
     :map region-bindings-mode-map
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
    ([remap zap-to-char] . zop-up-to-char)))

(progn "Development"
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
    (global-git-gutter-mode +1)))

(progn "Programming"
  (use-package subword
    :ensure nil
    :defer t
    :hook
    (prog-mode . subword-mode))

  ;; http://company-mode.github.io/
  (use-package company
    :defer t
    :config
    (setq company-idle-delay 0.1)
    (setq company-minimum-prefix-length 2)
    (setq company-selection-wrap-around t)
    (bind-keys
     :map company-active-map
     ("M-n" . nil)
     ("M-p" . nil)
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous)))

  ;; https://github.com/syohex/emacs-quickrun
  (use-package quickrun
    :bind
    (("C-c C-q" . quickrun)
     ("C-c C-w" . quickrun-with-arg)))

  ;; https://github.com/ReanGD/emacs-multi-compile
  (use-package multi-compile
    :bind
    ("C-c C-r" . multi-compile-run)
    :config
    (setq multi-compile-completion-system 'ido))

  ;; https://github.com/nschum/highlight-symbol.el
  (use-package highlight-symbol
    :defer t
    :init
    (setq highlight-symbol-idle-delay 0.3)
    (setq highlight-symbol-colors '("magenta" "blue" "orange"))
    :config
    (set-face-attribute 'highlight-symbol-face nil :background "green" :foreground "black"))

  ;; https://github.com/capitaomorte/autopair
  (use-package autopair
    :defer t)

  ;; https://github.com/flycheck/flycheck
  (use-package flycheck
    :defer t)

  ;; https://github.com/capitaomorte/yasnippet
  (use-package yasnippet
    :defer t
    :config
    (bind-keys
     :map yas-minor-mode-map
     ("<tab>".  nil)
     ("TAB"  . nil)
     ("<backtab>" . yas-expand))
    (setq yas-snippet-dirs '("~/.config/emacs/snippets" "~/.emacs.d/snippets" "~/.emacs.d/site-snippets"))
    (yas-reload-all))

  ;; https://github.com/joaotavora/eglot
  (use-package eglot
    :config
    (when (executable-find "bingo")
      (add-to-list 'eglot-server-programs '(go-mode . ("bingo" "-mode" "stdio"))))))

(progn "Emacs Lisp"
  ;; https://github.com/tarsius/auto-compile
  (use-package auto-compile
    :defer t
    :hook
    (emacs-lisp . auto-compile-mode)))

(progn "Shell Script"
  ;; depends: http://www.shellcheck.net/
  (use-package sh-script
    :defer t
    :hook
    (sh-mode . yas-minor-mode)
    (sh-mode . flycheck-mode)))

(progn "Golang"
  ;; https://github.com/dominikh/go-mode.el
  ;; depends: https://github.com/saibing/bingo
  (use-package go-mode
    :mode "\\.go\\'"
    :hook
    (go-mode . eglot-ensure)
    (go-mode . company-mode)
    (go-mode . autopair-mode)
    (go-mode . highlight-symbol-mode)
    (before-save . gofmt-before-save) ;; instead of 'eglot-format
    :bind
    (:map go-mode-map
     ("M-/" . company-complete))))

(progn "Web Development"
  ;; https://github.com/fxbois/web-mode
  (use-package web-mode
    :mode
    (("\\.html?\\'" . web-mode)
     ("\\.css\\'"   . web-mode))
    :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    ;; https://github.com/fxbois/web-mode/issues/358
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-pairing t)))

(progn "Markdown"
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
     ("M-n" . nil))
    (defun my/markdown-preview-file ()
      "run Marked on the current file and revert the buffer"
      (interactive)
      (shell-command
       (format "open -a /Applications/Marked\\ 2.app %s"
               (shell-quote-argument (buffer-file-name)))))
    (define-key markdown-mode-map (kbd "C-c m") 'my/markdown-preview-file)))

(progn "Docker"
  ;; https://github.com/spotify/dockerfile-mode
  ;; requires: hadolint
  (use-package dockerfile-mode
    :mode "\\Dockerfile\\'"))

(progn "TOML"
  (use-package toml-mode
    :mode "\\.toml\\'"))

(progn "YAML"
   (use-package yaml-mode
     :mode "\\.yml\\'"))

(progn "Web Browser"
  (use-package eww
    :defer t
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
      (eww-reload))
    (bind-keys
     :map eww-mode-map
     ("g" . beginning-of-buffer)
     ("G" . end-of-buffer)
     ("j" . scroll-up-line)
     ("k" . scroll-down-line)
     ("b" . scroll-down)
     ("F" . eww-forward-url)
     ("B" . eww-back-url)
     ("r" . eww-reload))))

(when (eq system-type 'darwin)
  ;; Use command-key as meta
  (setq ns-command-modifier 'meta)

  ;; File name coding system
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)

  ;; Clipboard integration
  (setq interprogram-cut-function
        '(lambda (text &optional push)
           (let ((process-connection-type nil))
             (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
               (process-send-string proc text)
               (process-send-eof proc)))))
  (setq interprogram-paste-function
        '(lambda () (shell-command-to-string "pbpaste"))))


(when (eq system-type 'gnu/linux)
  ;; Clipboard integration
  (when (executable-find "xsel")
    (setq interprogram-cut-function
          '(lambda (text &optional push)
             (let ((process-connection-type nil))
               (let ((proc (start-process "xsel" "*Messages*" "xsel" "--display" ":0" "--input" "--clipboard")))
                 (process-send-string proc text)
                 (process-send-eof proc)))))
    (setq interprogram-paste-function
          '(lambda () (shell-command-to-string "xsel --display :0 --output --clipboard")))))
