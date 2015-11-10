(defun my/el-get-install ()
  (add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defun my/el-get-dependencies ()
  (el-get-bundle exec-path-from-shell)  ; configuration
  (el-get-bundle use-package)
  (el-get-bundle popwin)                ; window
  (el-get-bundle win-switch)
  (el-get-bundle solarized-emacs)       ; theme
  (el-get-bundle highlight-symbol)      ; highlight
  (el-get-bundle elpa:mwim)             ; key util
  (el-get-bundle smartrep)
  (el-get-bundle anzu)                  ; edit
  (el-get-bundle expand-region)         ; region
  (el-get-bundle wrap-region)
  (el-get-bundle multiple-cursors)
  (el-get-bundle region-bindings-mode)  ; completion
  (el-get-bundle auto-complete)
  (el-get-bundle autopair)              ; parens
  (el-get-bundle flycheck)              ; validation
  (el-get-bundle quickrun)              ; execution
  (el-get-bundle helm)                  ; helm/anything
  (el-get-bundle helm-swoop)
  (el-get-bundle helm-ag)
  (el-get-bundle helm-dash)
  (el-get-bundle helm-descbinds)
  (el-get-bundle ace-jump-mode)         ; acejump
  (el-get-bundle elscreen :type git :url "git@github.com:knu/elscreen.git") ; session manager
  (el-get-bundle markdown-mode)         ; markdown
  (el-get-bundle go-mode)               ; golang
  (el-get-bundle go-autocomplete)
  (el-get-bundle go-eldoc)
  (el-get-bundle js2-mode)              ; javascript
  (el-get-bundle tern)
  (el-get-bundle typescript-mode
    :type github
    :pkgname "ananthakumaran/typescript.el")
  (el-get-bundle tide
    :type github
    :pkgname "ananthakumaran/tide"
    :depends (dash flycheck typescript-mode))
  (el-get-bundle web-mode)
  (el-get-bundle company-mode)
  (el-get-bundle simplenote2)           ; memo
  (el-get-bundle gist)                  ; github/gist
)

(defun my/emacsclient-settings ()
  (when (require 'server nil t)
    (unless (server-running-p)
      (server-start))))

(defun my/theme-settings () 
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t)
  (enable-theme 'solarized-dark)
  (defun on-after-init ()
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame))))
  (add-hook 'window-setup-hook 'on-after-init))

(defun my/language-settings ()
  ;; 文字コードを指定する
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix))

(defun my/open-line-below ()
  "次の行に挿入"
  (interactive)
  (unless (eolp)
    (end-of-line))
    (newline-and-indent))

(defun my/open-line-above ()
  "前の行に挿入"
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
    (indent-according-to-mode))

(defun my/edit-settings ()
  (setq backup-inhibited t)            ; バックアップとオートセーブファイルは不要
  (setq-default tab-width 4)           ; TABの表示幅。初期値は8
  (setq-default indent-tabs-mode nil)  ; インデントにタブ文字を使用しない
  (setq-default kill-whole-line t)     ; 行全体を削除
  (global-set-key (kbd "C-c l") 'toggle-truncate-lines)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "C-m") 'newline-and-indent)
  (global-set-key (kbd "M-o") 'my/open-line-below)
  (global-set-key (kbd "M-O") 'my/open-line-above))

(defun my/ui-settings ()
  (setq inhibit-startup-screen t)            ; スタートアップメッセージを非表示
  (fset 'yes-or-no-p 'y-or-n-p)              ; Emacsからの質問をy/nで回答する
  (setq mouse-yank-at-point t)               ; カーソルの位置にペーストされるようにする
  (if window-system
      (menu-bar-mode 1) (menu-bar-mode -1))  ; ターミナル上のEmacsはメニューバー非表示
  (if window-system
      (tool-bar-mode 0))                     ; ツールバーを非表示
  (if window-system
      (scroll-bar-mode 0))                   ; スクロールバーを非表示
  (setq frame-title-format "%f")             ; タイトルバーにファイルのフルパスを表示
  (column-number-mode t)                     ; カラム番号を表示
  (size-indication-mode t)                   ; ファイルサイズを表示
  (setq ring-bell-function 'ignore)          ; ビープ音OFF
  (with-eval-after-load 'linum
    (setq linum-format "%4d ")               ; -nw環境にはフリンジがない
    (set-face-attribute 'linum nil :background "unspecified-bg")))

(defun my/window-settings ()
  ;; popwin
  (require 'popwin)
  (popwin-mode 1)
  (global-set-key (kbd "M-z") popwin:keymap)
  (push '("*quickrun*") popwin:special-display-config))

(defun my/highlight-settings ()
  ;; 括弧の対応関係のハイライト
  (setq show-paren-delay 0)
  (show-paren-mode t)

  ;; highlight-symbol
  ;; https://github.com/nschum/highlight-symbol.el
  (setq highlight-symbol-idle-delay 0.3)
  (setq highlight-symbol-colors '("magenta" "blue" "orange"))
  (with-eval-after-load 'highlight-symbol-mode
    (set-face-attribute 'highlight-symbol-face nil :background "green" :foreground "black")))

(defun my/key-util-settings ()
  ;; mwim
  ;; https://github.com/alezost/mwim.el
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line))

(defun my/completion-settings ()
  ;; dabbrev-expandをhippie-expandに変更
  (global-set-key (kbd "M-/") 'hippie-expand)

  ;; auto-complete
  (ac-config-default))

(defun my/add-keys-to-ace-jump-mode (prefix c &optional mode)
  (define-key global-map
    (read-kbd-macro (concat prefix (string c)))
    `(lambda ()
       (interactive)
       (funcall (if (eq ',mode 'word)
                    #'ace-jump-word-mode
                  #'ace-jump-char-mode) ,c))))

(defun my/region-settings ()
  ;; wrap-region
  ;; https://github.com/rejeep/wrap-region.el
  (wrap-region-global-mode +1)

  ;; expand-region
  ;; https://github.com/magnars/expand-region.el
  (global-set-key (kbd "M-m") 'er/expand-region)

  ;; region-bindings-mode
  ;; https://github.com/fgallina/region-bindings-mode
  (require 'region-bindings-mode)
  (region-bindings-mode-enable)

  ;; multiple-cursors
  (define-key region-bindings-mode-map (kbd "e") 'mc/edit-lines)
  (define-key region-bindings-mode-map (kbd "a") 'mc/mark-all-like-this)
  (define-key region-bindings-mode-map (kbd "p") 'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map (kbd "n") 'mc/mark-next-like-this)
  (define-key region-bindings-mode-map (kbd "m") 'mc/mark-more-like-this-extended)
  (define-key region-bindings-mode-map (kbd "u") 'mc/unmark-next-like-this)
  (define-key region-bindings-mode-map (kbd "U") 'mc/unmark-previous-like-this)
  (define-key region-bindings-mode-map (kbd "s") 'mc/skip-to-next-like-this)
  (define-key region-bindings-mode-map (kbd "S") 'mc/skip-to-previous-like-this))

(defun my/helm-anything-settings ()
  (require 'helm-config)
  (helm-mode 1)

  (global-set-key (kbd "C-c h") 'helm-mini)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)

  (custom-set-variables
   '(helm-command-prefix-key "C-z"))
  (define-key helm-command-map (kbd "m") 'helm-imenu)
  (define-key helm-command-map (kbd "d") 'helm-descbinds)

  ;; TABで補完する
  ;; 代わりにCtrl-zでアクション選択する
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  ;; ミニバッファでCtrl-hでdelete-backward-charしてほしい
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

  ;; ミニバッファでCtrl-kした時、kill-line風に動いてほしい
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (set-face-attribute 'helm-selection nil :background "magenta" :foreground "white")

  ;; helm-ag
  (global-set-key (kbd "M-g .") 'helm-ag)
  (global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)

  ;; helm-swoop
  (define-key helm-command-map (kbd "i") 'helm-swoop)
  (define-key helm-command-map (kbd "I") 'helm-swoop-back-to-last-point)
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

  ;; helm-dash
  (define-key helm-command-map (kbd "a") 'helm-dash)
  ;; docsetの置き場所
  (setq helm-dash-docsets-path (expand-file-name "~/.docsets"))
  (setq helm-dash-common-docsets '("Go" "Bash" "Java" "Markdown" "Python 2" "JavaScript" "C"))
  ;; 検索開始に必要な最低入力文字数
  (setq helm-dash-min-lengh 2)
  ;; デフォルトのブラウザ
  (setq helm-dash-browser-func 'eww))

(defun my/navigation-settings ()
  ;; ace-jump
  (require 'ace-jump-mode)
  (set-face-attribute 'ace-jump-face-background nil :background "unspecified-bg")

  ;; ESC ESC-0〜9、a〜z、!〜?でHead Charを指定して、ジャンプ
  ;; http://d.hatena.ne.jp/rkworks/20120520/1337528737
  (loop for c from ?0 to ?9 do (my/add-keys-to-ace-jump-mode "ESC M-" c))
  (loop for c from ?a to ?z do (my/add-keys-to-ace-jump-mode "ESC M-" c))
  (loop for c from ?! to ?~ do (my/add-keys-to-ace-jump-mode "ESC M-" c))

  ;; anzu
  ;; https://github.com/syohex/emacs-anzu
  (global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
  (global-set-key (kbd "C-x %") 'anzu-query-replace-at-cursor-thing)

  ;; win-switch
  (setq win-switch-idle-time 0.75)
  (win-switch-set-keys '("o") 'next-window)
  (win-switch-set-keys '("p") 'previous-window)
  (win-switch-set-keys '("3") 'split-horizontally)
  (win-switch-set-keys '("2") 'split-vertically)
  (win-switch-set-keys '("0") 'delete-window)
  (global-set-key (kbd "C-x o") 'win-switch-dispatch))

(defun my/session-settings ()
  ;; el-screen
  ;; https://github.com/knu/elscreen
  (setq elscreen-prefix-key (kbd "C-c z"))
  (setq elscreen-display-tab nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (elscreen-start))

(defun my/programming-settings ()
  (global-set-key (kbd "C-c C-q") 'quickrun)
  (global-set-key (kbd "C-c C-w") 'quickrun-with-arg)
  (global-set-key (kbd "C-c C-r") 'compile)
  (global-set-key (kbd "C-c RET") 'recompile))

(defun my/markdown-settings ()
  (defun markdown-preview-file ()
    "run Marked on the current file and revert the buffer"
    (interactive)
    (shell-command
     (format "open -a ~/Applications/Marked\\ 2.app %s"
             (shell-quote-argument (buffer-file-name)))))
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c m") 'markdown-preview-file)))

(defun my/golang-settings ()
  (defun my/turn-on-flycheck-mode ()
    (linum-mode 1)
    (flycheck-mode 1))
  (defun my/go-mode-compile-command ()
    (setq compile-command "go build -v && go test -v"))
  (defun my/go-mode-gofmt-command ()
    (setq gofmt-command "goimports"))
  (defun my/turn-on-autopair-mode()
    (autopair-mode))
  (defun my/go-eldoc-setup ()
    (require 'go-eldoc)
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "green"
                        :weight 'bold))
  (with-eval-after-load 'go-mode
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    (add-hook 'go-mode-hook 'highlight-symbol-mode)
    (add-hook 'go-mode-hook 'my/go-eldoc-setup)
    (add-hook 'go-mode-hook 'my/turn-on-flycheck-mode)
    (add-hook 'go-mode-hook 'my/turn-on-autopair-mode)
    (add-hook 'go-mode-hook 'my/go-mode-compile-command)
    (add-hook 'go-mode-hook 'my/go-mode-gofmt-command)

    (define-key go-mode-map (kbd "M-.") 'godef-jump)
    (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)

    (require 'go-autocomplete)))

(defun my/web-settings ()
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (with-eval-after-load 'web-mode
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    ;; force autoclosing and autopairing in a terminal
    ;; https://github.com/fxbois/web-mode/issues/358
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-pairing t)))

(defun my/javascript-settings ()
  ;; 依存:
  ;; npm install -g tern
  ;; npm install -g eslint
  (defun my/js2-turn-on-flycheck-mode ()
    (linum-mode 1)
    (flycheck-mode 1))
  (defun my/js2-turn-on-autopair-mode()
    (autopair-mode))
  (defun my/js2-mode-format ()
    (setq js2-basic-offset 2))
  (defun my/js2-mode-linting ()
    ;; JSHintを抑止
    ;; linterは JSHint > ESLint > Closure Linter の順に優先される
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint))))
  (defun my/js2-enable-tern-mode ()
    (tern-mode t))

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook 'my/js2-turn-on-flycheck-mode)
    (add-hook 'js2-mode-hook 'my/js2-turn-on-autopair-mode)
    (add-hook 'js2-mode-hook 'my/js2-mode-format)
    (add-hook 'js2-mode-hook 'my/js2-mode-linting)
    (add-hook 'js2-mode-hook 'my/js2-enable-tern-mode))
  (with-eval-after-load 'tern
    (require 'tern-auto-complete)
      (tern-ac-setup)))

(defun my/typescript-settings ()
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-hook 'typescript-mode-hook
            (lambda ()
              (tide-setup)
              (flycheck-mode t)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode t)
              (company-mode t)
              (linum-mode t)
              (autopair-on)))
  (setq company-idle-delay 0.1)
  (setq company-tooltip-align-annotations t))

(defun my/memo-settings ()
  ;; simplenote2
  ;; https://github.com/alpha22jp/simplenote2.el
  (defun my/simplenote2-setting ()
    (simplenote2-set-markdown))
  (defun my/simplenote2-keybindings ()
    (local-set-key (kbd "C-c t") 'simplenote2-add-tag)
    (local-set-key (kbd "C-c s") 'simplenote2-push-buffer)
    (local-set-key (kbd "C-c l") 'simplenote2-pull-buffer))

  (with-eval-after-load 'simplenote2
    (simplenote2-setup)
    (setq simplenote2-markdown-notes-mode 'markdown-mode)

    (add-hook 'simplenote2-create-note-hook
              'my/simplenote2-setting)
    (add-hook 'simplenote2-note-mode-hook
              'my/simplenote2-keybindings)))

(defun my/eww-settings ()
  ;; 背景、文字色を無効にする
  ;; http://rubikitch.com/2014/11/19/eww-nocolor/
  (defvar eww-disable-colorize t)
  (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    (unless eww-disable-colorize
      (funcall orig start end fg)))
  (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
  (defun eww-disable-color ()
    (interactive)
    (setq-local eww-disable-colorize t)
    (eww-reload))
  (defun eww-enable-color ()
    (interactive)
    (setq-local eww-disable-colorize nil)
    (eww-reload))

  ;; キーバインドをKeySnail風にする
  (with-eval-after-load 'eww
    (define-key eww-mode-map "g" 'beginning-of-buffer)
    (define-key eww-mode-map "G" 'end-of-buffer)
    (define-key eww-mode-map "j" 'scroll-up-line)
    (define-key eww-mode-map "k" 'scroll-down-line)
    (define-key eww-mode-map "b" 'scroll-down)
    (define-key eww-mode-map "F" 'eww-forward-url)
    (define-key eww-mode-map "B" 'eww-back-url)
    (define-key eww-mode-map "r" 'eww-reload)))

(defun my/darwin-settings ()
  ;; commandキーをメタキーとして使う
  (setq ns-command-modifier 'meta)

  ;; ファイル名の扱い
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)

  ;; クリップボード連携
  (setq interprogram-cut-function
        '(lambda (text &optional push)
           (let ((process-connection-type nil))
             (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
               (process-send-string proc text)
               (process-send-eof proc)))))
  (setq interprogram-paste-function
        '(lambda () (shell-command-to-string "pbpaste"))))

(defun my/linux-settings ()
  ;; クリップボード連携
  (when (executable-find "xsel")
    (setq interprogram-cut-function
          '(lambda (text &optional push)
             (let ((process-connection-type nil))
               (let ((proc (start-process "xsel" "*Messages*" "xsel" "--display" ":0" "--input" "--clipboard")))
                 (process-send-string proc text)
                 (process-send-eof proc)))))
    (setq interprogram-paste-function
          '(lambda () (shell-command-to-string "xsel --display :0 --output --clipboard")))))


;; emacs directory
;; emacs -q -l path/to/somewhere/init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(my/el-get-install)
(my/el-get-dependencies)

(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

(my/emacsclient-settings)
(my/theme-settings)

(my/language-settings)
(my/edit-settings)
(my/ui-settings)
(my/window-settings)
(my/highlight-settings)
(my/key-util-settings)
(my/completion-settings)
(my/region-settings)
(my/helm-anything-settings)
(my/navigation-settings)
(my/session-settings)

(my/programming-settings)
(my/markdown-settings)
(my/golang-settings)
(my/web-settings)
(my/javascript-settings)
(my/typescript-settings)
(my/memo-settings)
(my/eww-settings)

(when (eq system-type 'darwin)
  (my/darwin-settings))

(when (eq system-type 'gnu/linux)
  (my/linux-settings))
