;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File name: ` ~/.xemacs/init.el '
;;; ---------------------
;;;
;;; Copyright (c) 2002 SuSE Gmbh Nuernberg, Germany.
;;;
;;; Author: Werner Fink, <feedback@suse.de> 2002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Load custom file
;; ----------------
(setq custom-file "~/.xemacs/custom.el")
(load "~/.xemacs/custom.el" t t)

;;
;; More coding systems (UNICODE)
;; -----------------------------
(if (locate-library "un-define") (require 'un-define))


;;
;; Remember font and more settings
;; -------------------------------
(setq options-save-faces t)

;;
;; Load AucTeX by default
;; ----------------------
;(require 'tex-site)
;(setq-default TeX-master nil)
;; Users private libaries
;(setq TeX-macro-private '("~/lib/tex-lib/"))
;;   AUC-TeX-Macros
;(setq TeX-style-private   "~/lib/xemacs/site-lisp/auctex/style/")
;;   Autom. Auc-TeX-Macros
;(setq TeX-auto-private    "~/lib/xemacs/site-lisp/auctex/auto/")
;;;


;;
;; DGU Options
;;

(load "~/.xemacs/llvm-mode.el" t t)
)
