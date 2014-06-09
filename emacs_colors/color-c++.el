;;;;---------------------------------------------------------------------------
;; .emacs configuration file
;; author: Brad Settlemyer
;; tested on: XEmacs 20.7
;;            XEmacs 21.1.14
;;            XEmacs 21.4.4
;;            XEmacs 21.4.6
;;            GNU Emacs  20.4
;;            GNU Emacs  21.2
;;
;; packages supported:
;;   time, cl, cc-mode, font-lock, ede, eieio, elib, jde, func-menu,
;;   html-mode, semantic, speedbar, workshop, xml-mode
;;
;; last mod: 2002-09-01
;;;;---------------------------------------------------------------------------

;; General setup
(setq-default indent-tabs-mode nil)
(setq delete-key-deletes-forward t)
(setq mouse-yank-at-point t)
(line-number-mode t)
(column-number-mode t)


;; Set color scheme (set lconfig-dark-bg-scheme to t for reverse color scheme)
(defconst color-scheme 'dark)
(defconst foreground-color "gray85")
(defconst background-color "gray25")
(defconst cursor-color "red3")
(defconst pointer-color "white")

(if (featurep 'xemacs)
    (let ((frame (selected-frame)))
      (set-face-foreground 'default foreground-color)
      (set-face-background 'default background-color)
      (setq frame-background-mode color-scheme)
      color-scheme
      (set-frame-property frame
                          'custom-properties
                          (mapcar (lambda (symbol)
                                    (if (eql symbol 'light)
                                        'dark
                                      symbol))
                                  (frame-property frame
                                                  'custom-properties))))
  (progn
     (add-to-list 'default-frame-alist '(foreground-color . "gray85"))
     (add-to-list 'default-frame-alist '(background-color . "gray25"))
     (add-to-list 'default-frame-alist '(cursor-color . "red3"))
     (add-to-list 'default-frame-alist '(background-mode . dark))
     (set-cursor-color cursor-color)
     (set-mouse-color pointer-color))
)


;; Setup save options (auto and backup) -- still buggy need new Replace func
(setq auto-save-timeout 2000)
(setq make-backup-files t)


;; Printing setup
(setq ps-n-up-printing 2)
(setq ps-print-header nil)


;; Global Key Bindings
(define-key global-map "\C-xw" 'what-line)
(define-key global-map "\C-z" 'undo)
(define-key global-map [delete] 'delete-char)
(define-key global-map [backspace] 'delete-backward-char)
(define-key global-map [f1] 'help-command)
(define-key global-map [f2] 'undo)
(define-key global-map [f3] 'isearch-forward)
(define-key global-map [f4] 'other-window)
(define-key global-map [f12] 'revert-buffer)
(define-key global-map [button4] 'previous-line)
(define-key global-map [button5] 'next-line)


;; Setup time mode
(autoload 'display-time "time" "Display Time" t)
(condition-case err
    (display-time)
  (error (message "Unable to load Time package.")))
(setq display-time-24hr-format nil)
(setq display-time-day-and-date t)


;; Setup text mode
(add-hook 'text-mode-hook '(lambda() (auto-fill-mode 1)))
(add-hook 'text-mode-hook '(lambda() (setq fill-column 78)))


;; Setup psgml-mode
(setq sgml-indent-step 2)
(setq sgml-indent-data t)
(setq sgml-warn-about-undefined-entities nil)
(setq sgml-warn-about-undefined-elements nil)
(defun user-mail-address() "bws@deepcopy.org")
(add-to-list 'auto-mode-alist '("\\.xsd$"    . xml-mode))


;; Setup Common Lisp mode
(condition-case err
    (require 'cl)
  (error (message "Unable to load Common Lisp package.")))


;; Setup C mode
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'c-mode-common-hook "cc-mode" "C Mode Hooks" t)
(autoload 'c-add-style "cc-mode" "Add coding style" t)


;; Associate extensions with modes
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; Create my own coding style
;; No space before { and function sig indents 4 if argument overflow
(setq bws-c-style
      '((c-auto-newline                 . nil)
        (c-basic-offset                 . 4)
        (c-comment-only-line-offset     . 0)
        (c-echo-syntactic-information-p . nil)
        (c-hungry-delete-key            . t)
        (c-tab-always-indent            . t)
        (c-toggle-hungry-state          . t)
        (c-hanging-braces-alist         . ((substatement-open after)
                                          (brace-list-open)))
        (c-offsets-alist                . ((arglist-close . c-lineup-arglist)
                                           (case-label . 4)
                                           (substatement-open . 0)
                                           (block-open . 0) ; no space before {
                                           (knr-argdecl-intro . -)))
        (c-hanging-colons-alist         . ((member-init-intro before)
                                           (inher-intro)
                                           (case-label after)
                                           (label after)
                                           (access-label after)))
        (c-cleanup-list                 . (scope-operator
                                           empty-defun-braces
                                           defun-close-semi))))



;; Construct a hook to be called when entering C mode
(defun lconfig-c-mode ()
  (progn (define-key c-mode-base-map "\C-m" 'newline-and-indent)
         (define-key c-mode-base-map "\C-z" 'undo)
         (define-key c-mode-base-map [f4] 'speedbar-get-focus)
         (define-key c-mode-base-map [f5] 'next-error)
         (define-key c-mode-base-map [f6] 'run-program)
         (define-key c-mode-base-map [f7] 'compile)
         (define-key c-mode-base-map [f8] 'set-mark-command)
         (define-key c-mode-base-map [f9] 'insert-breakpoint)
         (define-key c-mode-base-map [f10] 'step-over)
         (define-key c-mode-base-map [f11] 'step-into)
         (c-add-style "Brad's Coding Style" bws-c-style t)))
(add-hook 'c-mode-common-hook 'lconfig-c-mode)


;; Setup font-lock syntax coloring package
(defconst lconfig-font-lock-faces
  (list '(font-lock-builtin-face
          ((((class color) (background dark)) (:foreground "cyan" :bold t))
           (((class color)) (:foreground "DarkBlue" :bold t))))
        '(font-lock-comment-face
          ((((class color) (background dark)) (:foreground "LightPink"))
           (((class color)) (:foreground "FireBrick"))))
        '(font-lock-constant-face
          ((((class color) (background dark)) (:foreground "SpringGreen"))
           (((class color)) (:foreground "ForestGreen"))))
        '(font-lock-doc-string-face
          ((((class color) (background dark)) (:foreground "SpringGreen"))
           (((class color)) (:foreground "ForestGreen"))))
        '(font-lock-function-name-face
          ((((class color) (background dark)) (:foreground "wheat3"))
           (((class color)) (:foreground "DarkBlue"))))
        '(font-lock-keyword-face
          ((((class color) (background dark)) (:foreground "SkyBlue" :bold t))
           (((class color)) (:foreground "DarkBlue" :bold t))))
        '(font-lock-preprocessor-face
          ((((class color) (background dark)) (:foreground "SkyBlue"))
           (((class color)) (:foreground "gray40"))))
        '(font-lock-reference-face
          ((((class color) (background dark)) (:foreground "yellow"))
           (((class color)) (:foreground "maroon4"))))
        '(font-lock-string-face
          ((((class color) (background dark)) (:foreground "SpringGreen"))
           (((class color)) (:foreground "ForestGreen"))))
        '(font-lock-type-face
          ((((class color) (background dark)) (:foreground "orange1"))
           (((class color)) (:foreground "maroon4"))))
        '(font-lock-variable-name-face
          ((((class color) (background dark)) (:foreground "yellow"))
           (((class color)) (:foreground "SaddleBrown"))))
        '(font-lock-warning-name-face
          ((((class color) (background dark)) (:foreground "DarkOrange"))
           (((class color)) (:foreground "DarkOrange"))))))

;; If possible set up a custom color scheme, otherwise turn colors off
(autoload 'custom-set-faces "font-lock" "Set the color scheme" t)
(autoload 'font-lock-fontify-buffer "font-lock" "Fontify Buffer" t)
(condition-case err
    (progn (apply 'custom-set-faces lconfig-font-lock-faces)
           (add-hook 'c-mode-common-hook 'font-lock-fontify-buffer)
           (add-hook 'emacs-lisp-mode-hook 'font-lock-fontify-buffer)
           )
  (error (progn
           (message "Could not customize colors, disabling colored fonts.")
           (setq-default font-lock-auto-fontify t))))


;; Setup Workshop, the Sun C++ ide integration package
(condition-case err
    (progn
      (setq load-path (append load-path '("/opt/SUNWspro/lib")))
      (require 'workshop))
  (error (message "Unable to load Sun Workshop package.")))


;; Setup JDE, the Java Development Environment for Emacs
;; Add load paths to development versions of jde
(add-to-list 'load-path (expand-file-name "~/elisp/jde/lisp"))
(add-to-list 'load-path (expand-file-name "~/elisp/ede"))
(add-to-list 'load-path (expand-file-name "~/elisp/eieio"))
(add-to-list 'load-path (expand-file-name "~/elisp/elib"))
(add-to-list 'load-path (expand-file-name "~/elisp/semantic"))
(add-to-list 'load-path (expand-file-name "~/elisp/speedbar"))
(defun lconfig-jde-mode-hook ()
  (progn (define-key jde-mode-map "\M-." 'jde-complete-at-point-menu)
         (define-key jde-mode-map "\M-," 'jde-complete-at-point)
         (define-key jde-mode-map [f4] 'speedbar-frame-mode)
         (define-key jde-mode-map [f5] 'next-error)
         (define-key jde-mode-map [f6] 'jde-run)
         (define-key jde-mode-map [f7] 'jde-compile)
         (define-key jde-mode-map [f8] 'set-mark-command)
         (define-key jde-mode-map [f9] 'insert-breakpoint)
         (define-key jde-mode-map [f10] 'step-over)
         (define-key jde-mode-map [f11] 'step-into)
         (setq c-basic-offset 4)))

(autoload 'jde-mode "jde" "JDE mode" t)
(condition-case err
    (progn (add-to-list 'auto-mode-alist '("\\.java$" . jde-mode))
           (setq jde-complete-use-menu nil)
           (add-hook 'jde-mode-hook 'lconfig-jde-mode-hook))
  (error (message "Unable to load JDEE package.")))


;; Setup CPerl mode
(setq cperl-brace-offset -4)
(setq cperl-indent-level 4)


;; Setup func-menu, the function menu quicklink package (XEmacs only)
(autoload 'function-menu "func-menu" "Load the parsing package" t)
(autoload 'fume-add-menubar-entry "func-menu" "Add function menu" t)
(autoload 'fume-list-functions "func-menu" "List functions in window" t)
(autoload 'fume-prompt-function-goto "func-menu" "Goto function" t)
(setq fume-max-items 35)
(setq fume-fn-window-position 3)
(setq fume-auto-position-popup t)
(setq fume-display-in-modeline-p t)
(setq fume-menubar-menu-location "Info")
(setq fume-buffer-name "Function List*")
(setq fume-no-prompt-on-valid-default nil)
;(global-set-key [f8] 'function-menu)
;(define-key global-map "\C-cl" 'fume-list-functions)
;(define-key global-map "\C-cg" 'fume-prompt-function-goto)
(condition-case err
    (progn (function-menu)
           (add-hook 'c-mode-common-hook 'fume-add-menubar-entry))
  (error (message "Unable to load Function Menu package")))


;; Setup speedbar, an additional frame for viewing source files
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(autoload 'speedbar-toggle-etags "speedbar" "Add argument to etags command" t)
(setq speedbar-frame-plist '(minibuffer nil
                             border-width 0
                             internal-border-width 0
                             menu-bar-lines 0
                             modeline t
                             name "SpeedBar"
                             width 24
                             unsplittable t))
(condition-case err
    (progn (speedbar-toggle-etags "-C"))
  (error (message "Unable to load Speedbar package.")))


;; XEmacs specific setup
(if (featurep 'xemacs)
    (progn (set-specifier default-toolbar-visible-p nil)
           (setq font-lock-use-default-colors nil)
           (setq font-lock-use-fonts t)
           (setq font-lock-use-colors t)
           (setq font-lock-maximum-decoration t)))


;; GNU specific general setup
(if (not (featurep 'xemacs))
  (condition-case err
      (progn (set-scroll-bar-mode 'right)
             (global-font-lock-mode t))
    (error (message "Not running GNU emacs 20.4 or above."))))


;; Setup my own packages
(add-to-list 'load-path (expand-file-name "~/elisp/"))
(require 'cpp-font-lock)

;; Add final message so using C-h l I can see if .emacs failed
(message ".emacs loaded successfully.")
