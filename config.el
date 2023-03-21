;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(after!
 rustic
 (map! :map rustic-mode-map :localleader ("o" #'rustic-open-dependency-file))
 (setq rustic-lsp-server 'rust-analyzer)
 (setq rustic-format-on-save t)
 (setq rustic-analyzer-command '("rust-analyzer-wrapper"))
 (smartparens-mode nil))

(use-package!
 lsp-ui
 :config
 (setq lsp-ui-sideline-show-hover nil)
 (setq lsp-ui-doc-show-with-mouse nil)
 (setq lsp-ui-doc-enable t)
 (setq lsp-ui-doc-show-with-cursor t)
 (setq lsp-ui-doc-max-height 14))

(after!
 lsp-rust
 (setq lsp-eldoc-hook nil)
 (setq lsp-rust-analyzer-use-client-watching nil)
 (setq lsp-auto-guess-root t)
 (setq lsp-auto-execute-action nil)

 (setq lsp-enable-file-watchers nil)
 (setq lsp-headerline-breadcrumb-enable t)
 (setq lsp-headerline-breadcrumb-mode 1)
 (setq lsp-response-timeout 20)
 (setq lsp-rust-analyzer-lru-capacity 256)
 (setq lsp-rust-analyzer-diagnostics-disabled
       ["macro-error"
        "unresolved-proc-macro"
        "unresolved-import"
        "mismatched-arg-count"])
 (setq lsp-rust-analyzer-cargo-run-build-scripts t)
 (setq lsp-rust-analyzer-cargo-watch-command "")
 (setq lsp-rust-analyzer-server-display-inlay-hints t)
 (setq
  lsp-rust-analyzer-cargo-override-command
  ["sh"
   "-c"
   "SKIP_WASM_BUILD=1 cargo check --message-format json --tests --workspace"])
 (evil-define-key 'normal rustic-mode-map "J" #'lsp-rust-analyzer-join-lines)
 (lsp-rust-analyzer-inlay-hints-mode 1)

 (setq gc-cons-threshold 1000000000)
 (setq message-log-max 3000)
 ; 1MB
 (setq read-process-output-max (* 2048 1024))
 (setq lsp-idle-delay 1.0)
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
 (map! :m [tab] #'ts-fold-toggle))

;; Customize keybindings for export
(after!
 vertico
 (map! :map minibuffer-local-map "C-e" #'+vertico/embark-export-write)
 (map! :map minibuffer-local-map "C-h" #'embark-bindings))

(after! org-roam (setq org-agenda-files '("~/org/roam/daily" "~/org/roam")))

(after!
 tree-sitter
 (custom-set-faces!
  '(tree-sitter-hl-face:function.macro
    :slant italic
    :inherit tree-sitter-hl-face:function.call)))

(add-hook
 'org-mode-hook
 (lambda ()
   (company-mode -1)
   (display-line-numbers-mode 0)
   (org-indent-mode 0)
   (org-appear-mode 1)))
(setq org-hide-emphasis-markers t)
(setq org-appear-autolinks t)
(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter"))


(setq text-quoting-style "grave")
(use-package!
 org-excalidraw
 :config (setq org-excalidraw-directory "~/org/excalidraw"))

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
    (:name "Research" :todo "RESEARCH" :order 9)
    (:name "Waiting" :todo "WAIT" :order 10)
    (:name "Leftover" :todo "TODO" :order 11)))
 ;; Without this, a custom keymap will be activated when hovering
 ;; above headings set by this package
 (setq! org-super-agenda-header-map (make-sparse-keymap))
 (setq! org-agenda-span 10))

(use-package!
 bookmark+
 :init (require 'bookmark+)
 :config
 (map! :leader :desc "Jump to bookmark with tag" :n "m t" #'bmkp-some-tags-jump)
 (map! :leader :desc "Edit bookmark tag" :n "m T" #'bmkp-edit-tags))

(after! vertico-posframe (setq vertico-posframe-width 300))

(use-package!
 treemacs
 :config (treemacs-follow-mode 1) (treemacs-project-follow-mode 1))

(use-package! restclient)

(setq doom-theme 'doom-dracula)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))

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


(use-package!
 magit-delta
 :config (add-to-list 'magit-delta-delta-args "--no-gitconfig"))

(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
(add-hook 'magit-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'code-review-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'rustic-mode-hook (lambda () (turn-off-smartparens-mode)))
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

(require 'browse-at-remote)

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
    (browse-at-remote--file-url filepath start-line end-line)))

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
        (sequence "RESEARCH" "ANOTHER" "|" "RESEARCHED")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(setq org-capture-templates-contexts '(("r" ((in-mode . "rustic-mode")))))
(setq
 org-capture-templates
 '(("t"
    "Personal todo"
    entry
    (file+headline +org-capture-todo-file "Inbox")
    "* TODO %?\12%i\12%a"
    :prepend t)
   ("l"
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
    :prepend t)
   ("n"
    "Personal notes"
    entry
    (file+headline +org-capture-notes-file "Inbox")
    "* %u %?\12%i"
    :prepend t)
   ("j"
    "Journal"
    entry
    (file+olp+datetree +org-capture-journal-file)
    "* %U %?\12%i\12%a"
    :prepend t)
   ("p" "Templates for projects")
   ("pt"
    "Project-local todo"
    entry
    (file+headline +org-capture-project-todo-file "Inbox")
    " \n\n* TODO \n bla %?\12%i\12%a"
    :prepend t
    :empty-lines-before 3)
   ("pn"
    "Project-local notes"
    entry
    (file+headline +org-capture-project-notes-file "Inbox")
    "* %U %?\12%i\12%a"
    :prepend t)
   ("pc"
    "Project-local changelog"
    entry
    (file+headline +org-capture-project-changelog-file "Unreleased")
    "* %U %?\12%i\12%a"
    :prepend t)
   ("o" "Centralized templates for projects")
   ("ot"
    "Project todo"
    entry
    #'+org-capture-central-project-todo-file
    "* TODO %?\12 %i\12 %a"
    :heading "Tasks"
    :prepend nil)
   ("on"
    "Project notes"
    entry
    #'+org-capture-central-project-notes-file
    "* %U %?\12 %i\12 %a"
    :prepend t
    :heading "Notes")
   ("oc"
    "Project changelog"
    entry
    #'+org-capture-central-project-changelog-file
    "* %U %?\12 %i\12 %a"
    :prepend t
    :heading "Changelog")))

(setq org-roam-dailies-capture-templates
      '(("d"
         "default"
         entry
         "* %? bla"
         :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(load! "./secrets.el")
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

(defun startup-layout ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally) ;; -> |
  (split-window-horizontally) ;; -> |
  (split-window-vertically) ;;  -> --
  (org-roam-dailies-goto-today)
  (next-multiframe-window)
  ;; (split-window-horizontally) ;; -> |
  ;; (org-agenda-list)
  ;; (next-multiframe-window)
  (org-todo-list)
  (next-multiframe-window)
  (balance-windows))

(startup-layout)
(magit-wip-mode)


(after!
 org-modern
 (custom-set-faces!
  '(org-block-begin-line :foreground: "red" :background "#383B4C" :extend nil)
  '(org-block :background "#2E303E" :extend t)
  '(org-block-end-line :foreground: "red" :background "#383B4C" :extend nil)))


(after! ts-fold (global-ts-fold-mode 1))
