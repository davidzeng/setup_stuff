;; Custom .emacs file
;; David Underhill
;; 2008-Jun-26

;; general stuff and appearance
(split-window-horizontally)             ;; start with vertical split (e.g. C-x 3)
(line-number-mode 1)                    ;; show lines # on
(column-number-mode 1)                  ;; show col # on
(mouse-wheel-mode t)                    ;; mouse wheel scrolls
(fset 'yes-or-no-p 'y-or-n-p)           ;; allow y/n instead of requiring yes/no
(setq-default cursor-type '(bar . 2))   ;; cursor => a slim (2 pixel) vertical bar
(load-file "~/.xemacs/color-dark.el")   ;; the color scheme to and frame size to use
(load-file "~/.xemacs/mac.el")          ;; set option as meta key (vice cmd key)

;; editor tweaks
(setq-default auto-fill-mode 1)         ;; use fill mode
(setq-default fill-column 80)           ;; fill to 80 chars
(setq-default indent-tabs-mode nil)     ;; don't use tabs
(setq standard-indent 4)                ;; indent 4 spaces
(setq-default tab-width 4)              ;; tabs take up 4 cols

(setq-default require-final-newline t)  ;; append newline if file doesn't end with one
(require 'paren) (show-paren-mode t)    ;; highlight matching parens
(setq scroll-step 1)                    ;; only scroll one line at a time, vice half a page
(setq kill-whole-line t)                ;; kill whole line on C-k if at beginning of line

;; limit line width
(setq fundamental-mode-hook '(lambda () (auto-fill-mode 1)))
(setq tex-mode-hook '(lambda () (auto-fill-mode 1)))
(setq text-mode-hook '(lambda () (auto-fill-mode 1)))

;; custom key bindings
(global-set-key "\M-g" 'goto-line)      ;; M-g to goto a line
;(global-set-key (kbd "C-<right>")       ;; auto-complete (do twice to list choices)
;                'dabbrev-completion)
;(global-set-key (kbd "M-<right>")       ;; auto-expand (repeat shortcut to cycle
;                'dabbrev-expand)        ;;   through choices)

;; mode hooks
(load-file "~/.xemacs/lua-mode.el")
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.decaf$" . java-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Makefile" . makefile-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(load-file "~/.xemacs/php-mode.el")
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))

;; custom functions
(load-file "~/.xemacs/htmlize.el") ;; M-x htmlize-buffer to convert syntax highlighted buffer to HTML
(load-file "~/.xemacs/del-spc-at-end-of-line.el") ;; delete spaces at the end of lines on save
;;(load-file "~/.xemacs/auto-repl-code.el")         ;; replace: '__'=>'->', '___'=>'__', and '..'=>'[]'
(load-file "~/.xemacs/repl_in_buffers.el")        ;; M-x query-replace-in-open-buffers => replace in
                                                  ;;   all open buffers

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(delete-selection-mode nil)
 '(scroll-bar-mode (quote right))
 '(tab-width 2)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "LimeGreen"))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))) t)
 '(font-lock-function-name-face ((t (:foreground "indian red"))))
 '(font-lock-keyword-face ((((class color)) (:bold t :foreground "cyan"))))
 '(font-lock-preprocessor-face ((((class color)) (:bold t :foreground "CornFlowerBlue"))))
 '(font-lock-string-face ((((class color)) (:italic t :foreground "yellow"))))
 '(font-lock-type-face ((t (:foreground "#9290ff"))))
 '(font-lock-variable-name-face ((t (:foreground "Khaki"))))
 '(highlight ((t (:foreground "black" :background "darkseagreen2"))))
 '(isearch ((t (:foreground "black" :background "paleturquoise"))))
 '(paren-match ((t (:background "darkseagreen4"))) t)
 '(show-paren-match ((((class color)) (:foreground "black" :background "yellow"))))
 '(show-paren-match-face ((((class color)) (:foreground "black" :background "yellow"))) t)
 '(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))
 '(show-paren-mismatch-face ((((class color)) (:foreground "white" :background "red"))) t))

