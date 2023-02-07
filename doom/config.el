;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controllind fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Cica" :size 18)
      doom-big-font (font-spec :family "Cica" :size 24)
      doom-variable-pitch-font (font-spec :family "Cica")
      doom-unicode-font (font-spec :family "Cica"))
;(setq doom-font (font-spec :family "Iosevka Fixed" :size 18 :width 'expanded)
;      doom-big-font (font-spec :family "Iosevka Fixed" :size 24 :width 'expanded)
;      doom-variable-pitch-font (font-spec :family "Iosevka")
;      doom-unicode-font (font-spec :family "源ノ角ゴシック JP"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-challenger-deep)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
;(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

                                        ;(map! "C-h" (cmd! (delete-backward-char)))
(map! "C-;" (cmd! (evil-escape)))
(map! "C-c l" #'evil-avy-goto-line)
(map! "C-c y" #'counsel-yank-pop)

(setq +format-on-save-enabled-modes
      '(not markdown-mode)) ; prettier is too nosy

(use-package! expand-region
  :config
  (map! "M-m" #'er/expand-region)
  :custom
  (expand-region-contract-fast-key "M")
  (expand-region-reset-fast-key "*"))

;(use-package! mozc
;  :if (and (eq system-type 'gnu/linux)
;           (getenv "WSL_INTEROP"))
;  :custom
;  (default-input-method "japanese-mozc")
;  :config
;  (map! [henkan] (lambda () (interactive) (mozc-mode 1)))
;  (map! :map mozc-mode-map
;        [muhenkan] (lambda () (interactive) (mozc-handle-event 'enter) (mozc-mode -1))))

(use-package! skk
  :if (and (eq system-type 'gnu/linux)
           (getenv "WSL_INTEROP"))
  :hook
  (evil-insert-state-exit . (lambda () (interactive) (skk-mode-exit)))
  :custom
  (default-input-method "japanese-skk")
  (skk-user-directory "~/.local/share/ddskk")
  (skk-large-jisyo (concat skk-user-directory "/dict/SKK-JISYO.L"))
  (skk-egg-like-newline t)
  :config
  (map! [henkan] (lambda () (interactive) (skk-j-mode-on)))
  (map! [muhenkan] (lambda () (interactive) (skk-mode-exit))))

(use-package! which-key
  :custom
  (which-key-idle-delay 0.5))

(use-package! lsp-ui
  :custom
  ;(lsp-ui-doc-position 'top)
  (lsp-ui-doc-enable nil))

(use-package! lsp-mode
  :hook
  (lsp-mode . (lambda () (local-set-key (kbd "M-/") 'company-capf))))

(use-package! avy
  :custom
  (avy-single-candidate-jump t))

(use-package! counsel
  :custom
  (counsel-yank-pop-separator "\n-------\n"))
