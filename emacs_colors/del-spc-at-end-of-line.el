;; Purpose: removes spaces at ends of lines on save

;; Original Source: http://www.koders.com/lisp/fid2D200A939B183C0A88072EACDAD9B1C422301410.aspx
;; Written by: ??

(defun agulbra-clean-out-spaces ()
  "Remove spaces at ends of lines"
  (interactive)
  (and ;;(eq major-mode 'c++-mode)
       (not buffer-read-only)
       (save-excursion
         (goto-char (point-min))
         (let ((count 0)
               (bmp (buffer-modified-p)))
           (while (re-search-forward "  *$" nil t)
             (setq count (1+ count))
             (replace-match "" t t))
           (and (> count 0)
                (progn
                  (set-buffer-modified-p bmp)
                  (message "Cleaned %d lines" count)))))) nil)
(add-hook 'write-file-hooks 'agulbra-clean-out-spaces)
