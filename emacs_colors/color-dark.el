
;; dark color scheme

;; set the default frame properties
(setq default-frame-alist
     '((width . 230) (height . 85)     ;; frame size
       (background-color . "gray15")   ;; bg: very light grey
       (foreground-color . "gray85")   ;; fg: very dark grey
       (cursor-color . "LightGoldenrod") ;; cursor color
       (mouse-color . "yellow")        ;; mouse color
       (font . "-*-Courier-normal-r-*-*-12-*-*-*-c-*-iso8859-1")
      ))

;; start in the top-left corner
(setq initial-frame-alist '((top . 2) (left . 2)))

;; set the face colors
(custom-set-faces
     '(font-lock-comment-face       ((t (:foreground "LimeGreen"))))
     '(font-lock-preprocessor-face  ((((class color)) (:bold t :foreground "CornFlowerBlue"))))
     '(font-lock-keyword-face       ((((class color)) (:bold t :foreground "cyan"))))
     '(font-lock-function-name-face ((t (:foreground "indian red"))))
     '(font-lock-variable-name-face ((t (:foreground "Khaki"))))
     '(font-lock-type-face          ((t (:foreground "#9290ff"))))

     '(paren-match                  ((t (:background "darkseagreen4"))) t)
     '(show-paren-match-face        ((((class color)) (:foreground "black" :background "yellow"))))
     '(show-paren-mismatch-face     ((((class color)) (:foreground "white" :background "red"))))

     '(isearch                      ((t (:foreground "black" :background "paleturquoise"))) t)

     '(highlight                    ((t (:foreground "black" :background "darkseagreen2"))))

     '(font-lock-doc-string-face    ((t (:foreground "green2"))) t)
     '(font-lock-string-face        ((((class color)) (:italic t :foreground "yellow"))))
)

;; set the color of the line highlight
;;(set-face-background 'hl-line "#AA5")  ;; (Emacs 22 only)
(set-face-background 'highlight "#AA5")  ;; (Emacs 21 only)
;;(global-hl-line-mode 1)                 ;; highlight line we're on
