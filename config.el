;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(after! rustic
  (map! :map rustic-mode-map :localleader ("o" #'rustic-open-dependency-file))
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-trigger 'on-save)
)

(use-package! lsp-rust
  :config
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
)

(after! lsp-rust
  ;; disable the eldoc stuff
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-rust-analyzer-use-client-watching nil)
  (setq lsp-auto-guess-root t)

  (setq lsp-enable-file-watchers nil)
  ;; (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-macro" "unresolved-proc-macro" "unresolved-import" "mismatched-arg-count"])
  (setq lsp-rust-analyzer-cargo-run-build-scripts t)
  (setq lsp-rust-analyzer-cargo-watch-command "check")
  (evil-define-key 'normal rustic-mode-map
    "J" #'lsp-rust-analyzer-join-lines)
  (lsp-rust-analyzer-inlay-hints-mode 1)

  ; Performance
  ; Use roughly one gigabyte
  (setq gc-cons-threshold 1000000000)
  ; 1MB
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.500)
)

(use-package! evil
  :config
  (setq! evil-want-C-d-scroll nil
         evil-want-C-u-scroll nil)
)

(use-package! inertial-scroll)
(map! "C-j" #'evil-scroll-line-down)
(map! "C-k" #'evil-scroll-line-up)
(map! "C-d" #'inertias-up)
(map! "C-u" #'inertias-down)
(after! inertial-scroll
 (setq inertias-update-time 20)
 (setq inertias-initial-velocity 200)
 (setq inertias-friction 450)
 (setq inertias-brake-coef 0.2)
)

(defun +private/treemacs-back-and-forth ()
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (aw-flip-window)
    (treemacs-select-window)))

(map! :after treemacs
      :leader
      :desc "Switch to treemacs sidebar and back"
      :n "-" #'+private/treemacs-back-and-forth)

(map! :leader
      :desc "Toggle full screen"
      :n "t F" #'toggle-frame-maximized)

(after! vertico
  (vertico-posframe-mode 1)
  (setq marginalia-margin-threshold 400)
)
(use-package! treemacs
  :config
  (treemacs-follow-mode 1)
)


(setq doom-theme 'doom-oceanic-next)

(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq display-line-numbers-type 'relative)

(setq show-trailing-whitespace t)

;; Enable global whitespace mode
(global-whitespace-mode 1)

(add-hook 'diff-mode-hook 'whitespace-mode)

(setq whitespace-style '(face tabs tab-mark spaces space-mark trailing lines-tail))

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
