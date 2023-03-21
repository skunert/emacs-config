(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/.local/etc/bookmarks")
 '(counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
 '(projectile-globally-ignored-directories
   '("~/.emacs.d/.local/" ".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "target"))
 '(projectile-globally-ignored-files '(".DS_Store" "Icon\12" "TAGS" "target")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#2E303E" :extend t))))
 '(org-block-begin-line ((t (:foreground: "red" :background "#383B4C" :extend nil))))
 '(org-block-end-line ((t (:foreground: "red" :background "#383B4C" :extend nil))))
 '(tree-sitter-hl-face:function.macro ((t (:slant italic :inherit tree-sitter-hl-face:function.call))))
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
