
;; light color scheme

;; set the default frame properties
(setq default-frame-alist
      '((width . 90) (height . 60)     ;; frame size
	(background-color . "gray95")   ;; bg: very light grey
	(foreground-color . "gray05")   ;; fg: very dark grey
	(cursor-color . "black")        ;; cursor color
	(mouse-color . "black")         ;; mouse color
       (font . "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-1")
      ))

;; start in the top-left corner
(setq initial-frame-alist '((top . 2) (left . 2)))

;; set the face colors
(custom-set-faces
     '(font-lock-comment-face       ((t (:foreground "forest green"))))
     '(font-lock-preprocessor-face  ((((class color)) (:italic t :foreground "maroon"))))
     '(font-lock-keyword-face       ((((class color)) (:bold t :foreground "blue"))))
     '(font-lock-function-name-face ((t (:foreground "indian red"))))
     '(font-lock-variable-name-face ((t (:foreground "olive drab"))))
     '(font-lock-type-face          ((t (:foreground "navy"))))
)

;; light yellow line highlight
;;(set-face-background 'hl-line "#FFA") ;; (Emacs 22 only)
(set-face-background 'highlight "#FFA") ;; (Emacs 21 only)
(global-hl-line-mode 1)                 ;; highlight line we're on
