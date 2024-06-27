(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy 1)
 '(backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path '(nil "src"))
 '(custom-safe-themes
   '("c71fd8fbda070ff5462e052d8be87423e50d0f437fbc359a5c732f4a4c535c43" "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" default))
 '(line-move-visual t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tab-bar-mode t))

;;
;; viper conf and tips
;;
;; TODO Typing "5:" déclanche la commande Ex sur les lignes :current,5
;; example sur la ligne 10: "10:" -> ":5,10"
;;
;; TODO "v" selection ne fonctionne pas
;;
;; TODO ":wq" ne quite pas
;;
;; TODO "définir comment fonctionne les macros vim" ("macros and registers"
;; fonctionnerait, mais la macro laisse l'éditeur en état Insert, qui pose
;; problème pour @@)
;;

;; (setq viper-mode t)
;; (setq viper-inhibit-startup-message 't)
;; (setq viper-expert-level '5)
;; (setq viper-want-ctl-h-help t)
;; (setq visible-bell t)
;; (setq show-trailing-whitespace nil)
;; (setq require-final-newline t)
;; (setq inhibit-startup-screen t)
;; (setq global-display-line-numbers-mode t)
;; (setq indent-tabs-mode t)
;; (setq compilation-window-height 7)
;; (setq electric-indent-mode t)
;; (define-key viper-emacs-global-user-map 'ESC 'scroll-down)
;; (require 'viper)
;;
;; (global-display-line-numbers-mode 1)
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'monokai)

;; ANSI color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Some key bindings

(global-set-key [f3] 'next-match)
(defun prev-match () (interactive nil) (next-match -1))
(global-set-key [(shift f3)] 'prev-match)
(global-set-key [backtab] 'auto-complete)

;; OCaml configuration
;;  - better error and backtrace matching

(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
    2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))

(add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
(add-hook 'caml-mode-hook 'set-ocaml-error-regexp)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)
; Or enable it globally:
; (add-hook 'after-init-hook 'global-company-mode)

;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
