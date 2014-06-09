;; Purpose: perform a query-replace across all open buffers

;; Original Code: http://my.opera.com/AxxL/blog/2007/09/23/emacs-query-replace-in-open-buffers
;; Written by: Axel (http://my.opera.com/AxxL/about/)

(defun query-replace-in-open-buffers (arg1 arg2)
  "query-replace in open files"
  (interactive "sQuery Replace in open Buffers: \nsquery with: ")
  (mapcar
   (lambda (x)
     (find-file x)
     (save-excursion
       (beginning-of-buffer)
       (query-replace arg1 arg2)))
   (delq
    nil
    (mapcar
     (lambda (x)
       (buffer-file-name x))
     (buffer-list)))))
