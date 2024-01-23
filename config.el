;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(after!
 rustic
 (map! :map rustic-mode-map :localleader ("o" #'rustic-open-dependency-file))
 (setq rustic-lsp-server 'rust-analyzer)
 (setq rustic-format-on-save t)
 (setq rustic-rustfmt-args '"+nightly")
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
 (setq lsp-response-timeout 20)
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
 (setq read-process-output-max (* 2048 1024))
 (setq lsp-idle-delay 1.0)

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

(defun hacky-new-item ()
  (interactive)
  (+org/insert-item-below 1)
  (+evil/insert-newline-above 1))

(after!
 evil
 (setq! evil-snipe-scope 'visible evil-kill-on-visual-paste nil)
 (map! :map evil-org-mode-map :n "<C-return>" #'hacky-new-item)
 (map! :map evil-org-mode-map :i "<C-return>" #'hacky-new-item)
 (map! :mode rustic-mode :m [tab] #'ts-fold-toggle))

;; Customize keybindings for export
(after!
 vertico
 (map! :map minibuffer-local-map "C-e" #'+vertico/embark-export-write)
 (setq! vertico-count 45)
 (map! :map minibuffer-local-map "C-h" #'embark-bindings))

(after! org-roam (setq org-agenda-files '("~/org/roam/daily" "~/org/roam")))

(after!
 tree-sitter
 (custom-set-faces!
  '(tree-sitter-hl-face:function.macro
    :slant italic
    :inherit tree-sitter-hl-face:function.call)))

(setq org-hide-emphasis-markers t)
(setq org-appear-autolinks t)
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))


(setq text-quoting-style "grave")
(use-package!
 org-excalidraw
 :config (setq org-excalidraw-directory "~/org/excalidraw"))

(use-package! olivetti)
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

(use-package!
 bufler
 :config (setq! bufler-workspace-mode t)
 (map! :leader :desc "Bufler switch buffer" :n "b B" #'bufler-switch-buffer))

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

(use-package!
 vterm-toggle
 :config
 ;;(setq! vterm-toggle-fullscreen-p nil)
 ;;(setq! vterm-toggle-hide-method 'quit-window)


 (map! :leader :desc "Toggle vterm window" :n "v v" #'vterm-toggle)

 (map! :leader :desc "Next vterm window" :n "v n" #'vterm-toggle-forward)

 (map! :leader :desc "Previous vterm window" :n "v b" #'vterm-toggle-backward))

(after!
 vterm (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
 ;; vterm should not be allowed to mess with out cursor https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-1183650463
 (advice-add
  #'vterm--redraw
  :around
  (lambda (fun &rest args)
    (let ((cursor-type cursor-type))
      (apply fun args))))

 (set-popup-rule! "^\\*vterm" :ignore t)

 ;; Display vterm in current buffer
 (add-to-list
  'display-buffer-alist
  '((lambda (buffer-or-name _)
      (let ((buffer (get-buffer buffer-or-name)))
        (with-current-buffer buffer
          (or (equal major-mode 'vterm-mode)
              (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
    (display-buffer-reuse-window display-buffer-same-window)))

 (add-to-list
  '+popup--display-buffer-alist
  '((lambda (buffer-or-name _)
      (let ((buffer (get-buffer buffer-or-name)))
        (with-current-buffer buffer
          (or (equal major-mode 'vterm-mode)
              (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
    (display-buffer-reuse-window display-buffer-same-window))))

(use-package! org-super-agenda)
(use-package! elisp-autofmt)
(after!
 org-super-agenda
 (org-super-agenda-mode 1)
 (setq!
  org-super-agenda-groups
  '( ;; Each group has an implicit boolean OR operator between its selectors.
    (:name "Today" :scheduled today :scheduled past :order 0)
    (:name "Doing" :tag ("inprogress") :order 1)
    (:name "Next" :tag ("next") :todo ("NEXT") :order 5)
    (:name "Reading List" :todo "READ" :order 6)
    (:name "Waiting" :todo "WAIT" :order 9)
    (:name "Research" :todo "RESEARCH" :order 10)
    (:name "Leftover" :todo "TODO" :order 11)))
 ;; Without this, a custom keymap will be activated when hovering
 ;; above headings set by this package
 (setq! org-super-agenda-header-map (make-sparse-keymap))
 (setq! org-agenda-span 10))

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

(use-package! restclient)

(setq doom-theme 'doom-dracula)

(setq doom-font
      (font-spec :weight 'semi-bold :family "JetBrainsMono Nerd Font" :size 17))

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
  (setq magit-diff-refine-hunk nil)
)

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

(use-package! websocket :after org-roam)

(use-package!
 org-roam-ui
 :after org-roam ;; or :after org
 ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
 ;;         a hookable mode anymore, you're advised to pick something yourself
 ;;         if you don't care about startup time, use
 ;;  :hook (after-init . org-roam-ui-mode)
 :config
 (setq
  org-roam-ui-sync-theme t
  org-roam-ui-follow t
  org-roam-ui-update-on-save t
  org-roam-ui-open-on-start t))

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

;; (toggle-frame-maximized)

(smartparens-global-mode nil)

(setq +org-capture-todo-file "roam/inbox.org")

;; (use-package!
;;  consult-gh
;;  :config
;;  (add-to-list 'consult-gh-default-orgs-list "skunert")
;;  (add-to-list 'consult-gh-default-orgs-list "paritytech")
;;  (setq consult-gh-default-clone-directory "~/work/repos"))

;; accept completion from copilot and fallback to company
(use-package!
 copilot
 :hook (prog-mode . copilot-mode)
 :bind
 (:map
  copilot-completion-map
  ("<tab>" . 'copilot-accept-completion)
  ("TAB" . 'copilot-accept-completion)
  ("C-TAB" . 'copilot-accept-completion-by-word)
  ("C-<tab>" . 'copilot-accept-completion-by-word)))

(after! copilot (add-to-list 'copilot-major-mode-alist '("rustic" . "rust")))
(use-package! shell-maker)
(use-package!
 chatgpt-shell
 :config (setq chatgpt-shell-model-version "gpt-4")
 (setq chatgpt-shell-openai-key
       (auth-source-pick-first-password :host "api.openai.com")))

(require 'browse-at-remote)

;; This is required since "browse-at-remote--file-url" converts from points to
;; lines, but we need to do this conversion before with the original buffer.
(defun org-capture-browse-at-remote--file-url
    (filename &optional start-line end-line)
  "Return the URL to browse FILENAME from lines START to END. "
  (let* ((remote-ref (browse-at-remote--remote-ref filename))
         (remote (car remote-ref))
         (ref (cdr remote-ref))
         (relname (f-relative filename (f-expand (vc-git-root filename))))
         (target-repo (browse-at-remote--get-url-from-remote remote))
         (remote-type
          (browse-at-remote--get-remote-type
           (plist-get target-repo :unresolved-host)))
         (repo-url (plist-get target-repo :url))
         (url-formatter
          (browse-at-remote--get-formatter 'region-url remote-type)))
    (unless url-formatter
      (error (format "Origin repo parsing failed: %s" repo-url)))

    (funcall url-formatter
             repo-url ref relname
             (if start-line
                 start-line)
             (if (and end-line (not (equal start-line end-line)))
                 end-line))))

(defun org-capture-get-remote-url (filepath original-buffer)
  "Get the remote url on github in a capture template."
  (let* ((range-beginning
          (with-current-buffer original-buffer
            (region-beginning)))
         (range-end
          (with-current-buffer original-buffer
            (region-end)))
         (point-begin (min range-beginning range-end))
         (point-end (max range-beginning range-end))
         (start-line
          (when point-begin
            (with-current-buffer original-buffer
              (line-number-at-pos point-begin))))
         (end-line
          (when point-end
            (with-current-buffer original-buffer
              (line-number-at-pos point-end)))))
    (org-capture-browse-at-remote--file-url filepath start-line end-line)))

(setq default-frame-alist '((undecorated . t)))
(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))
(define-key org-mode-map (kbd "C-c e") 'org-toggle-emphasis)
(map! :leader :desc "Insert with temple" :n "n e" #'org-toggle-emphasis)
(setq +org-capture-notes-file "roam/inbox.org")
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "PROJ(p)"
         "LOOP(r)"
         "STRT(s)"
         "HOLD(h)"
         "IDEA(i)"
         "|"
         "DONE(d)"
         "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
        (sequence "WAIT" "NEXT" "TODO" "|" "DONE")
        (sequence "RESEARCH" "|" "RESEARCHED")
        (sequence "READ" "|" "FINISHED")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(setq org-capture-templates-contexts '(("r" ((in-mode . "rustic-mode")))))
(setq
 org-capture-templates
 '(("l"
    "Research item"
    entry
    (file+headline +org-capture-todo-file "Inbox")
    "* RESEARCH %?\12%i\12%a"
    :prepend t)
   ("r"
    "Rust project note"
    entry
    (file+headline +org-capture-todo-file "Inbox")
    "* %? \n:CODEREF:\n#+begin_src rust\n%i#+end_src\n[[%(org-capture-get-remote-url \"%F\" (org-capture-get :original-buffer))][View on GitHub]]\n%F\n:END:\n"
    :prepend t)))

(setq org-roam-dailies-capture-templates
      '(("d"
         "default"
         entry
         "* %? bla"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
        ("t"
         "Capture Todo"
         entry
         "* TODO %?"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
        ("a"
         "Capture Actionable Todo"
         entry
         "* TODO %? :next:"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(load! "difftastic.el")

(setq! org-agenda-start-day "-1d")
(setq! org-agenda-span 3)
(setq! org-agenda-start-on-weekday nil)
(setq! org-agenda-overriding-header "")
(use-package!
 org-modern
 :hook (org-mode . global-org-modern-mode)
 :config (setq org-modern-label-border 0.3))

(setq org-agenda-sticky t)
(setq org-priority-default 67)
(setq
 org-agenda-prefix-format
 '(
   ;; (agenda  . " %i %-12:c%?-12t% s") ;; file name + org-agenda-entry-type
   (agenda . "  • ")
   (timeline . "  % s") (todo . " %i") (tags . " %i") (search . " %i")))
(add-hook 'org-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)
            (org-indent-mode 0)
            (org-appear-mode 1))
          100)

(defun startup-layout ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally) ;; -> |
  (split-window-horizontally) ;; -> |
  (balance-windows))

(startup-layout)
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

(after!
 org
 (setq! org-pretty-entities t)
 (setq! org-auto-align-tags nil)
 (setq! org-agenda-tags-column 0))

(after!
 org-modern
 (custom-set-faces!
  '(org-block-begin-line :foreground: "red" :background "#383B4C" :extend nil)
  '(org-block :background "#2E303E" :extend t)
  '(org-block-end-line :foreground: "red" :background "#383B4C" :extend nil)))


(after! ts-fold (global-ts-fold-mode 1))

(load! "github.el")
