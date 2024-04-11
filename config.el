;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; This is required because otherwqise `org-link-set-parameter` can not be found
(require 'ol)
(after!
 rustic
 (map! :map rustic-mode-map :localleader ("o" #'rustic-open-dependency-file))
 (setq rustic-lsp-server 'rust-analyzer)
 (setq rustic-format-on-save t)
 (setq rustic-rustfmt-args ' "+nightly")
 (smartparens-mode nil))


(use-package!
 lsp-ui
 :config
 (setq lsp-ui-sideline-show-hover nil)
 (setq lsp-ui-doc-show-with-mouse nil)
 (setq lsp-ui-doc-enable t)
 (setq lsp-ui-doc-show-with-cursor t)
 (setq lsp-ui-doc-max-height 14))

(use-package!
 dogears
 :config
 (dogears-mode 1)
 (map! :leader :desc "Dogears go" :n "f d" #'dogears-go))

(after! marginalia (setq marginalia-align 'right))

(after!
 lsp-rust
 (setq lsp-rust-analyzer-use-client-watching nil)
 (setq lsp-auto-guess-root t)
 (setq lsp-auto-execute-action nil)

 (setq lsp-enable-file-watchers nil)
 (setq lsp-headerline-breadcrumb-enable t)
 (setq lsp-headerline-breadcrumb-mode 1)
 (setq lsp-headerline-breadcrumb-segments '(symbols))
 (setq lsp-response-timeout 7)
 (setq lsp-rust-analyzer-lru-capacity 256)
 (setq lsp-rust-analyzer-lru-capacity 256)
 (setq lsp-rust-analyzer-rustfmt-extra-args ["+nightly"])
 (setq lsp-rust-analyzer-diagnostics-disabled
       ["macro-error"
        "unresolved-proc-macro"
        "unresolved-import"
        "mismatched-arg-count"])
 (setq lsp-rust-analyzer-cargo-run-build-scripts t)
 (setq lsp-rust-analyzer-server-display-inlay-hints nil)
 (evil-define-key 'normal rustic-mode-map "J" #'lsp-rust-analyzer-join-lines)

 (setq gc-cons-threshold 1000000000)
 (setq message-log-max 3000)
 ; 1MB
 (setq lsp-idle-delay 1.0)
(add-hook '+lsp-optimization-mode-hook
          (lambda ()
            (if (symbol-value '+lsp-optimization-mode)
            (setq-default read-process-output-max (* 4096 1024)))))
 ;; Disable evil-snipe, because we want to use avy-goto-char-timer instead
 (remove-hook 'doom-first-input-hook #'evil-snipe-mode)

 ;; Remap 's' to use avy
 (setq! avy-all-windows t)
 (defun avy-action-embark (pt)
   (unwind-protect
       (save-excursion
         (goto-char pt)
         (embark-act))
     (select-window (cdr (ring-ref avy-ring 0))))
   t)

 (map!
  :leader
  :mode rustic-mode
  :desc "Open external documentation"
  :n
  "o d"
  #'lsp-rust-analyzer-open-external-docs)

 (map!
  :leader
  :mode rustic-mode
  :desc "View file symbols"
  :n
  "s i"
  #'consult-lsp-file-symbols)

 (map!
  :leader
  :mode rustic-mode
  :desc "Add cargo dependency"
  :n
  "m a"
  #'rustic-cargo-add)

 (map!
  :leader
  :mode rustic-mode
  :desc "Peek usage"
  :n
  "c p d"
  #'lsp-ui-peek-find-definitions)

 (map!
  :leader
  :mode rustic-mode
  :desc "Peek usage"
  :n
  "c p D"
  #'lsp-ui-peek-find-references)

 (map!
  :leader
  :mode rustic-mode
  :desc "Peek references"
  :n
  "c p i"
  #'lsp-ui-peek-find-implementation))

(after!
 evil
 (setq! evil-snipe-scope 'visible evil-kill-on-visual-paste nil)
 (define-key evil-motion-state-map "s" 'avy-goto-char-timer)
 (define-key evil-normal-state-map "s" 'avy-goto-char-timer)
 (map! :mode rustic-mode :m [tab] #'ts-fold-toggle))

;; Customize keybindings for export
(after!
 vertico
 (map! :map minibuffer-local-map "C-e" #'embark-export)
 (map! :map minibuffer-local-map "C-w" #'+vertico/embark-export-write)
 (setq! vertico-count 45)
 (map! :map minibuffer-local-map "C-h" #'embark-bindings))

(after!
 tree-sitter
 (custom-set-faces!
  '(tree-sitter-hl-face:function.macro
    :slant italic
    :inherit tree-sitter-hl-face:function.call)))

(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))


(setq text-quoting-style "grave")

(use-package! olivetti)
(after!
 olivetti
 (remove-hook 'olivetti-mode-on-hook 'visual-line-mode)
 (olivetti-set-width 130))
(add-hook 'olivetti-mode-hook (lambda () (olivetti-set-width 130)))


(use-package! balanced-windows :config (balanced-windows-mode))
(after!
 orderless
 (setq orderless-matching-styles '(orderless-literal orderless-initialism)))

(map! "C-j" #'evil-scroll-line-down)
(map! "C-k" #'evil-scroll-line-up)
(map! "C-s" #'save-buffer)

(require 'org-contrib)
(map! :leader :desc "Toggle full screen" :n "t F" #'toggle-frame-maximized)

(map! :leader :desc "Replace with anzu" :n "s r" #'anzu-query-replace-regexp)

(map! :leader :desc "Search with deadgrep" :n "s R" #'deadgrep)
(map! :leader :desc "Insert from kill ring" :n "v p" #'yank-from-kill-ring)

(use-package!
 tempel
 :config
 (setq! global-tempel-abbrev-mode 1)
 (setq! tempel-path "~/.doom.d/templates")
 (map! :leader :desc "Expand with tempel" :n "v t e" #'tempel-expand)
 (map! :leader :desc "Insert with tempel" :n "v t i" #'tempel-insert)

 (map! :leader :desc "Tempel done" :n "v t d" #'tempel-done)

 (map! :map tempel-map "<tab>" #'tempel-next)

 (map! :map tempel-map "S-<tab>" #'tempel-previous))

(set-popup-rule! "^ ?\\*Treemacs" :ignore t)

(after!
 vterm
 (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
 ;; vterm should not be allowed to mess with out cursor https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
 (advice-add
  #'vterm--redraw
  :around
  (lambda (fun &rest args)
    (let ((cursor-type cursor-type))
      (apply fun args)))))

(use-package! elisp-autofmt)

(use-package!
 bookmark+
 :init (require 'bookmark+)
 :config
 (setq bookmark-save-flag 1)
 (map! :leader :desc "Jump to bookmark with tag" :n "m t" #'bmkp-some-tags-jump)
 (map! :leader :desc "Edit bookmark tag" :n "m T" #'bmkp-edit-tags))

(after!
 vertico-posframe
 (setq vertico-posframe-width 200)
 (setq vertico-posframe-height 40))

(use-package!
 treemacs
 :config (treemacs-follow-mode 1) (treemacs-project-follow-mode 1))

(setq doom-theme 'doom-dracula)

(if (eq system-type 'darwin)
    (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))
  (setq doom-font
        (font-spec
         :weight 'semi-bold
         :family "JetBrainsMono Nerd Font"
         :size 17)))

(unless (eq system-type 'darwin)
  (setq default-frame-alist '((undecorated . t))))


(setq display-line-numbers-type 'relative)

;; Make flycheck use direnv to get the correct env for finding an executable
;; We also need to enable `envrc-mode` manually for this buffer to make sure we set the
;; env variables for this buffer (the mode is probably enabled later).
(setq flycheck-executable-find
      (lambda (cmd)
        (envrc-mode 1)
        (envrc--update-env default-directory)
        (executable-find cmd)))

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(setq auth-sources '("~/.authinfo"))
;; Save all buffers when emacs looses the focus
(add-hook 'focus-out-hook 'save-all)

(after!
 magit
 (setq ghub-graphql-items-per-request 10)
 (setq magit-diff-highlight-indentation nil)
 (setq magit-diff-highlight-trailing nil)
 (setq magit-diff-paint-whitespace nil)
 (setq magit-diff-highlight-hunk-body nil)
 (setq magit-diff-refine-hunk nil))

(use-package!
 magit-delta
 :config (add-to-list 'magit-delta-delta-args "--no-gitconfig"))

(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
(add-hook 'magit-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'code-review-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'rustic-mode-hook (lambda () (turn-off-smartparens-mode)))
(add-hook 'rustic-mode-hook (lambda () (olivetti-mode 1)))
;; Autosave to the file directly
(auto-save-visited-mode 1)


;; Save all buffers before searching a project
(advice-add
 #'+default/search-project
 :before (lambda (&rest _) (evil-write-all nil)))

(after!
 company
 ;; Trigger completion immediately.
 (setq company-idle-delay 0))

;; (setenv "SSH_AUTH_SOCK" "/Users/skunert/.gnupg/S.gpg-agent.ssh")

(setq dap-cpptools-extension-version "1.5.1")

(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\justfile\\'" . just-mode))


(with-eval-after-load 'lsp-rust
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb))

(with-eval-after-load 'dap-cpptools
  ;; Add a template specific for debugging Rust programs.
  ;; It is used for new projects, where I can M-x dap-edit-debug-template
  (dap-register-debug-template
   "Rust::GDB Run Configuration"
   (list
    :type "gdb"
    :request "launch"
    :name "GDB::Run"
    :gdbpath "rust-gdb"
    :target nil
    :cwd nil))
  (dap-register-debug-template
   "Rust::CppTools Run Configuration"
   (list
    :type "cppdbg"
    :request "launch"
    :name "Rust::Run"
    :MIMode "lldb"
    :miDebuggerPath "rust-lddb"
    :environment []
    :program "${workspaceFolder}/target/debug/hello / replace with binary"
    :cwd "${workspaceFolder}"
    :console "external"
    :dap-compilation "cargo build"
    :dap-compilation-dir "${workspaceFolder}")))

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1))

(smartparens-global-mode nil)

;; accept completion from copilot and fallback to company
(use-package!
 copilot
 :hook (prog-mode . copilot-mode)
 :config (setq! copilot-indent-offset-warning-disable t)
 :bind
 (:map
  copilot-completion-map
  ("<tab>" . 'copilot-accept-completion)
  ("TAB" . 'copilot-accept-completion)
  ("C-TAB" . 'copilot-accept-completion-by-word)
  ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package! gptel :config (setq! gptel-model "gpt-4-0125-preview"))
(after! copilot (add-to-list 'copilot-major-mode-alist '("rustic" . "rust")))

(require 'browse-at-remote)

(use-package! lacarte)
(use-package!
 difftastic
 :demand t
 :bind
 (:map
  magit-blame-read-only-mode-map
  ("D" . difftastic-magit-show)
  ("S" . difftastic-magit-show))
 :config
 (eval-after-load 'magit-diff
   '(transient-append-suffix
     'magit-diff '(-1 -1)
     [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
      ("S" "Difftastic show" difftastic-magit-show)])))

(magit-wip-mode)

(use-package!
 obsidian
 :config
 (obsidian-specify-path "~/Documents/default")
 (global-obsidian-mode 1)
 :custom
 ;; This directory will be used for `obsidian-capture' if set.
 (obsidian-inbox-directory "emacs-inbox")
 :bind
 (:map
  obsidian-mode-map
  ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
  ("C-c C-o" . obsidian-follow-link-at-point)
  ;; Jump to backlinks
  ("C-c C-b" . obsidian-backlink-jump)
  ;; If you prefer you can use `obsidian-insert-link'
  ("C-c C-l" . obsidian-insert-wikilink)))

(use-package! consult-gh)

(after!
 company
 (setq company-global-modes
       '(not erc-mode
             circe-mode
             message-mode
             help-mode
             gud-mode
             vterm-mode
             markdown-mode)))

(after! ts-fold (global-ts-fold-mode 1))

(load! "github.el")
