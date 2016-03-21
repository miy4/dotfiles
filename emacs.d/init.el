(defun setup-general ()
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)

  (setq backup-inhibited t)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq-default kill-whole-line t)
  (global-set-key (kbd "C-c l") 'toggle-truncate-lines)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-m") 'newline-and-indent)
  (global-set-key (kbd "M-o") 'my/open-line-below)
  (global-set-key (kbd "M-O") 'my/open-line-above)

  (fset 'yes-or-no-p 'y-or-n-p)
  (setq mouse-yank-at-point t) 

  (setq show-paren-delay 0)
  (show-paren-mode t))

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

(defun setup-package ()
  ;; https://github.com/Malabarba/paradox
  (use-package paradox :ensure t
    :defer t
    :init
    (setq paradox-github-token t)
    (load (locate-user-emacs-file ".paradox-github-token") :noerror :nomessage)))

(defun setup-console-integration ()
  (use-package exec-path-from-shell :ensure t))

(defun setup-server-daemon ()
  (use-package server
  :config
  (unless (server-running-p)
    (server-start))))

(defun setup-visual ()
  (setq frame-title-format "%f")
  (setq inhibit-startup-screen t)
  (if window-system
      (menu-bar-mode 1) (menu-bar-mode -1))
  (if window-system
      (tool-bar-mode 0))
  (if window-system
      (scroll-bar-mode 0))
  (column-number-mode t)
  (size-indication-mode t)
  (setq ring-bell-function 'ignore)

  ;; https://github.com/bbatsov/solarized-emacs
  (use-package solarized-theme :ensure t
    :config
    (load-theme 'solarized-dark t)
    (enable-theme 'solarized-dark)
    (defun my/on-after-init ()
      (unless (display-graphic-p (selected-frame))
        (set-face-background 'default "unspecified-bg" (selected-frame))))
    (add-hook 'window-setup-hook 'my/on-after-init))

  ;; https://github.com/Malabarba/smart-mode-line
  (use-package smart-mode-line :ensure t
    :init
    (setq sml/theme 'dark)
    :config
    (setq sml/no-confirm-load-theme t)
    (sml/setup))

  (use-package linum
    :defer t
    :config
    (setq linum-format "%4d ")
    (set-face-attribute 'linum nil :background "unspecified-bg"))

  ;; https://github.com/k-talo/volatile-highlights.el
  (use-package volatile-highlights :ensure t
    :diminish volatile-highlights-mode
    :config
    (volatile-highlights-mode 1))

  ;; https://github.com/Malabarba/beacon
  (use-package beacon :ensure t
    :config
    (setq beacon-size 20)
    (setq beacon-color "yellow")
    (setq beacon-blink-delay 0.1)
    (setq beacon-blink-duration 0.1)
    (beacon-mode 1))

  ;; http://www.dr-qubit.org/emacs.php
  (use-package undo-tree :ensure t
    :config
    (global-undo-tree-mode 1)))

(defun setup-interface-enhancement ()
  ;; https://github.com/knu/elscreen
  (use-package elscreen :ensure t
    :config
    (setq elscreen-prefix-key (kbd "C-c z"))
    (setq elscreen-display-tab nil)
    (setq elscreen-tab-display-kill-screen nil)
    (setq elscreen-tab-display-control nil)
    (elscreen-start))

  (global-set-key (kbd "M-g M-g") 'winner-undo)
  (winner-mode)

  ;; https://github.com/wasamasa/shackle
  (use-package shackle :ensure t
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
  (use-package helm :ensure t
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
    (("C-c h"   . helm-mini)
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
     ("m" . helm-imenu)
     ("s" . helm-elscreen))
    (setq helm-delete-minibuffer-contents-from-point t)
    (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
      "Emulate `kill-line' in helm minibuffer"
      (kill-new (buffer-substring (point) (field-end)))))

  (use-package helm-files :ensure helm
    :defer t
    :config
    (bind-keys
     :map helm-read-file-map
     ("TAB" . helm-execute-persistent-action)
     :map helm-find-files-map
     ("TAB" . helm-execute-persistent-action)
     ("C-h" . delete-backward-char)))

  ;; https://github.com/emacs-helm/helm-descbinds
  (use-package helm-descbinds :ensure t
    :commands (helm-descbinds helm-descbinds-mode)
    :init
    (with-eval-after-load 'helm-config
      (bind-key "d" 'helm-descbinds helm-command-map))
    :config
    (helm-descbinds-mode))

  ;; https://github.com/ShingoFukuyama/helm-swoop
  (use-package helm-swoop :ensure t
    :defer t
    :init
    (with-eval-after-load 'helm-config
      (bind-key "i" 'helm-swoop helm-command-map)
      (bind-key "I" 'helm-swoop-back-to-last-point helm-command-map))
    :config
    (setq helm-swoop-move-to-line-cycle t))

  ;; https://github.com/emacs-jp/helm-c-yasnippet
  (use-package helm-c-yasnippet :ensure t
    :defer t
    :init
    (with-eval-after-load 'helm-config
      (bind-key "y" 'helm-yas-complete helm-command-map))
    :config
    (setq helm-yas-space-match-any-greedy t)))

(defun setup-keys-cheat-sheet ()
  ;; https://github.com/justbur/emacs-which-key
  (use-package which-key
    :ensure t
    :config
    (setq which-key-popup-type 'side-window)
    (setq which-key-side-window-location 'right)
    (setq which-key-idle-delay 1.0)
    (which-key-mode)))

(defun setup-file-manager ()
  ;; https://github.com/jaypei/emacs-neotree
  (use-package neotree :ensure t
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

(defun setup-navigation ()
  ;; https://github.com/abo-abo/avy
  (use-package avy :ensure t
    :bind
    (("M-c" . avy-goto-char)
     ("M-l" . avy-goto-line))
    :config
    (setq avy-background t))

  ;; https://github.com/abo-abo/ace-window
  (use-package ace-window :ensure t
    :bind
    (("M-p" . ace-window))
    :config
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (ace-window-display-mode 1)))

(defun setup-editing ()
  ;; https://github.com/rejeep/wrap-region.el
  (use-package wrap-region :ensure t
    :config
    (wrap-region-global-mode t)
    (wrap-region-add-wrapper "`" "`" nil 'markdown-mode))

  ;; https://github.com/magnars/expand-region.el
  (use-package expand-region :ensure t
    :bind
    (("M-m" . er/expand-region))
    :config
    (setq expand-region-contract-fast-key "M")
    (setq expand-region-reset-fast-key "*"))

  ;; https://github.com/fgallina/region-bindings-mode
  (use-package region-bindings-mode :ensure t
    :config
    (region-bindings-mode-enable))

  ;; https://github.com/magnars/multiple-cursors.el
  (use-package multiple-cursors :ensure t
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
  (use-package mwim :ensure t
    :bind
    (("C-a" . mwim-beginning-of-code-or-line)
     ("C-e" . mwim-end-of-code-or-line)))

  ;; https://github.com/thierryvolpiatto/zop-to-char
  (use-package zop-to-char :ensure t
    :bind
    ([remap zap-to-char] . zop-up-to-char)))

(defun setup-programming ()
  ;; http://company-mode.github.io/
  (use-package company :ensure t
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
  (use-package quickrun :ensure t
    :bind
    (("C-c C-q" . quickrun)
     ("C-c C-w" . quickrun-with-arg)))

  ;; https://github.com/ReanGD/emacs-multi-compile
  (use-package multi-compile :ensure t
    :bind
    ("C-c C-r" . multi-compile-run)
    :config
    (setq multi-compile-completion-system 'ido))

  ;; https://github.com/nschum/highlight-symbol.el
  (use-package highlight-symbol :ensure t
    :defer t
    :init
    (setq highlight-symbol-idle-delay 0.3)
    (setq highlight-symbol-colors '("magenta" "blue" "orange"))
    :config
    (set-face-attribute 'highlight-symbol-face nil :background "green" :foreground "black"))

  ;; https://github.com/capitaomorte/autopair
  (use-package autopair :ensure t
    :defer t)

  ;; https://github.com/flycheck/flycheck
  (use-package flycheck :ensure t
    :defer t)

  ;; https://github.com/capitaomorte/yasnippet
  (use-package yasnippet :ensure t
    :defer t
    :config
    (bind-keys
     :map yas-minor-mode-map
     ("<tab>".  nil)
     ("TAB"  . nil)
     ("<backtab>" . yas-expand))
    (setq yas-snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/site-snippets"))
    (yas-reload-all)))

(defun setup-emacs-lisp ()
  ;; https://github.com/tarsius/auto-compile
  (use-package auto-compile :ensure t
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook 'auto-compile-mode)))

(defun setup-shell-script ()
  ;; depends: http://www.shellcheck.net/
  (use-package sh-script
    :defer t
    :config
    (add-hook 'sh-mode-hook 'yas-minor-mode)
    (add-hook 'sh-mode-hook 'flycheck-mode)))

(defun setup-golang ()
  ;; https://github.com/dominikh/go-mode.el
  ;; depends: go get github.com/nsf/gocode
  ;; depends: go get github.com/rogpeppe/godef
  ;; depends: go get golang.org/x/tools/cmd/goimports
  (use-package go-mode :ensure t
    :mode "\\.go\\'"
    :init
    (with-eval-after-load 'multi-compile
      (add-to-list 'multi-compile-alist
                   '(go-mode . (("build" . "go build -v")
                                ("test"  . "go build -v && go test -v")
                                ("clean" . "go clean")))))
    :config
    (add-hook 'go-mode-hook 'highlight-symbol-mode)
    (add-hook 'go-mode-hook 'flycheck-mode)
    (add-hook 'go-mode-hook 'autopair-mode)
    (add-hook 'go-mode-hook 'linum-mode)
    (add-hook 'go-mode-hook 'yas-minor-mode)
    (add-hook 'go-mode-hook '(lambda ()
                               (require 'go-eldoc)
                               (setq gofmt-command "goimports")))
    (add-hook 'before-save-hook 'gofmt-before-save)
    (bind-keys
     :map go-mode-map
     ("M-." . godef-jump)
     ("M-," . pop-tag-mark)))

  ;; https://github.com/syohex/emacs-go-eldoc
  (use-package go-eldoc :ensure t
    :defer t
    :config
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "green"
                        :weight 'bold))

  ;; https://github.com/nsf/gocode
  (use-package company-go :ensure t
    :defer t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-go))
    (with-eval-after-load 'go-mode
      (add-hook 'go-mode-hook 'company-mode))))

(defun setup-web-development ()
  ;; https://github.com/fxbois/web-mode
  (use-package web-mode :ensure t
    :mode
    (("\\.html?\\'" . web-mode)
     ("\\.css\\'"   . web-mode))
    :config
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    ;; https://github.com/fxbois/web-mode/issues/358
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-pairing t)))

(defun setup-javascript ()
  ;; https://github.com/mooz/js2-mode
  ;; depends: npm install -g tern
  ;; depends: npm install -g eslint
  (use-package js2-mode :ensure t
    :mode "\\.js\\'"
    :config
    (defun my/js2-mode-hook ()
      (setq js2-basic-offset 2)
      (setq-default flycheck-disabled-checkers
                    (append flycheck-disabled-checkers
                          '(javascript-jshint)))) ;; JSHint disabled
    (add-hook 'js2-mode-hook 'flycheck-mode)
    (add-hook 'js2-mode-hook 'autopair-mode)
    (add-hook 'js2-mode-hook 'linum-mode)
    (add-hook 'js2-mode-hook 'my/js2-mode-hook))

  ;; https://github.com/ternjs/tern
  (use-package tern :ensure t
    :defer t
    :init
    (with-eval-after-load 'js2-mode
      (add-hook 'js2-mode-hook 'tern-mode)))

  ;; https://github.com/proofit404/company-tern
  (use-package company-tern :ensure t
    :defer t
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-tern))
    (with-eval-after-load 'js2-mode
      (add-hook 'js2-mode-hook 'company-mode))))

(defun setup-typescript ()
  ;; https://github.com/ananthakumaran/tide
  (use-package typescript-mode :ensure t
    :mode "\\.ts\\'"
    :config
    (add-hook 'typescript-mode-hook
              (lambda ()
                (flycheck-mode t)
                (setq flycheck-check-syntax-automatically '(save mode-enabled))))
    (add-hook 'typescript-mode-hook 'autopair-mode)
    (add-hook 'typescript-mode-hook 'linum-mode)
    (add-hook 'typescript-mode-hook 'company-mode)
    (add-hook 'typescript-mode-hook 'eldoc-mode)
    (setq company-idle-delay 0.1)
    (setq company-tooltip-align-annotations t))

  ;; https://github.com/ananthakumaran/tide
  (use-package tide :ensure t
    :defer t
    :init
    (with-eval-after-load 'typescript-mode
      (add-hook 'typescript-mode-hook 'tide-setup))))

(defun setup-markdown ()
  ;; https://github.com/jrblevin/markdown-mode
  (use-package markdown-mode :ensure t
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
       (format "open -a ~/Applications/Marked\\ 2.app %s"
               (shell-quote-argument (buffer-file-name)))))
    (define-key markdown-mode-map (kbd "C-c m") 'my/markdown-preview-file)))

(defun setup-docker ()
  ;; https://github.com/spotify/dockerfile-mode
  (use-package dockerfile-mode :ensure t
  :mode "Dockerfile\\'"))

(defun setup-web-browser ()
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

(defun setup-note-taking ()
  ;; https://github.com/alpha22jp/simplenote2.el
  (use-package simplenote2 :ensure t
    :defer t
    :config
    (simplenote2-setup)
    (setq simplenote2-markdown-notes-mode 'markdown-mode)
    (bind-keys
     :map simplenote2-note-mode-map
     ("C-c t" . simplenote2-add-tag)
     ("C-c s" . simplenote2-push-buffer)
     ("C-c l" . simplenote2-pull-buffer))))

(defun setup-darwin ()
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

(defun setup-linux ()
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


;; emacs -q -l path/to/somewhere/init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Package Manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; Package Configuration
(eval-when-compile
  (require 'use-package))
(package-install 'use-package)

(setup-general)

(setup-package)
(setup-console-integration)
(setup-server-daemon)
(setup-visual)
(setup-interface-enhancement)
(setup-keys-cheat-sheet)
(setup-file-manager)
(setup-navigation)
(setup-editing)

(setup-programming)
(setup-emacs-lisp)
(setup-shell-script)
(setup-golang)
(setup-web-development)
(setup-javascript)
(setup-typescript)
(setup-markdown)
(setup-docker)
(setup-note-taking)

(when (eq system-type 'darwin)
  (setup-darwin))

(when (eq system-type 'gnu/linux)
  (setup-linux))
