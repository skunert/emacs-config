;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(after! rustic
  (map! :map rustic-mode-map :localleader ("o" #'rustic-open-dependency-file))
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-on-save t)
)

;; (use-package! lsp-rust
;;   :config
;;   (setq lsp-rust-analyzer-server-display-inlay-hints nil)
;; )

(use-package! lsp-ui
  :config
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-max-height 14)
)



(after! lsp-rust
  ;; disable the eldoc stuff
  (setq lsp-eldoc-hook nil)
  (setq lsp-rust-analyzer-use-client-watching nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-rust-analyzer-server-display-inlay-hints nil)

  (setq lsp-enable-file-watchers nil)
  (setq lsp-rust-analyzer-proc-macro-enable nil)
  (setq lsp-rust-analyzer-lru-capacity 256)
  (setq lsp-rust-analyzer-diagnostics-disabled ["macro-error" "unresolved-proc-macro" "unresolved-import" "mismatched-arg-count"])
  (setq lsp-rust-analyzer-cargo-run-build-scripts t)
  (setq lsp-rust-analyzer-cargo-watch-command "check")
  (evil-define-key 'normal rustic-mode-map
    "J" #'lsp-rust-analyzer-join-lines)
  (lsp-rust-analyzer-inlay-hints-mode 0)

  ; Performance
  ; Use roughly one gigabyte
  (setq gc-cons-threshold 1000000000)
  ; 1MB
  (setq read-process-output-max (* 2048 1024))
  (setq lsp-idle-delay 0.500)
  (map! :leader
        :mode rustic-mode
        :desc "View file symbols"
        :n "s i" #'consult-lsp-file-symbols)

  (map! :leader
        :mode rustic-mode
        :desc "Add cargo dependency"
        :n "m a" #'rustic-cargo-add)
)

(use-package! evil
  :config
  (setq! evil-snipe-scope 'visible
         ;; evil-want-C-d-scroll nil
         ;; evil-want-C-u-scroll nil
         evil-kill-on-visual-paste nil)
)

(after! vertico
  (map! :map minibuffer-local-map
        "C-e" #'+vertico/embark-export-write)
)

(use-package! inertial-scroll)
(map! "C-j" #'evil-scroll-line-down)
(map! "C-k" #'evil-scroll-line-up)
;; (map! "C-d" #'inertias-up)
;; (map! "C-u" #'inertias-down)
(after! inertial-scroll
 (setq inertias-update-time 20)
 (setq inertias-initial-velocity 200)
 (setq inertias-friction 450)
 (setq inertias-brake-coef 0.2)
)

(map! :leader
      :desc "Toggle full screen"
      :n "t F" #'toggle-frame-maximized)

(map! :leader
      :desc "Replace with anzu"
      :n "s r" #'anzu-query-replace)

  (map! :leader
        :desc "Search with deadgrep"
        :n "s d" #'deadgrep)

(use-package! vertico-posframe
  :init
  (setq vertico-posframe-border-width 4)
  (setq vertico-posframe-width 300)
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-center)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  ;; NOTE: this is needed to make sure marginalia columns don't get misaligned
  (setq marginalia-margin-threshold 500)
  :config
  (vertico-posframe-mode 1)
)

(use-package! treemacs
  :config
  (treemacs-follow-mode 1)
)

(setq doom-theme 'doom-dark+)

(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq display-line-numbers-type 'relative)

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Make flycheck use direnv to get the correct env for finding an executable
;; We also need to enable `envrc-mode` manually for this buffer to make sure we set the
;; env variables for this buffer (the mode is probably enabled later).
(setq flycheck-executable-find
     (lambda (cmd) (envrc-mode 1)(envrc--update-env default-directory)(executable-find cmd)))

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(setq auth-sources '("~/.authinfo"))
;; Save all buffers when emacs looses the focus
(add-hook 'focus-out-hook 'save-all)
(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
;; Autosave to the file directly
(auto-save-visited-mode 1)

;;(after! prog-mode
;;  (set-company-backend! 'prog-mode 'company-abbrev-code))

;; Save all buffers before searching a project
(advice-add #'+default/search-project :before (lambda (&rest _) (evil-write-all nil)))

(after! company
  ;; Trigger completion immediately.
  (setq company-idle-delay 0))

(setenv "SSH_AUTH_SOCK" "/Users/skunert/.gnupg/S.gpg-agent.ssh")
