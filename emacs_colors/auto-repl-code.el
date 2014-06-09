;; Purpose: translate '--' to '->' and '---' to '--' and '..' to '[]'

;; Original code: http://infolab.stanford.edu/~manku/dotemacs.html
;; Written by Gurmeet Singh Manku (manku@cs.stanford.edu)

;; Modified by David Underhill (dgu@cs.stanford.edu)
;;   Change: --- collapses back to --  (was more useful when thees were _'s)
;;           Collapse -- instead of __ (the former is easier and less used)

(defun my-editing-function (first last len)
  (interactive)
  (if (and (boundp 'major-mode)
           (member major-mode (list 'c-mode 'c++-mode 'gud-mode 'fundamental-mode 'ruby-mode))
           (= len 0)
           (> (point) 4)
           (= first (- (point) 1)))
      (cond
       ((and (string-equal (buffer-substring (point) (- (point) 2)) "--")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "---")))
        (progn (delete-backward-char 2) (insert-char ?- 1) (insert-char ?> 1)))

       ((string-equal (buffer-substring (point) (- (point) 3)) "->_")
        (progn (delete-backward-char 3) (insert-char ?_ 2)))

       ((and (string-equal (buffer-substring (point) (- (point) 2)) "..")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "...")))
        (progn (delete-backward-char 2) (insert-char ?[ 1) (insert-char ?] 1) (backward-char 1)))

       ((and (> (point-max) (point))
             (string-equal (buffer-substring (+ (point) 1) (- (point) 2)) "[.]"))
        (progn (forward-char 1) (delete-backward-char 3) (insert-char ?. 1) (insert-char ?. 1) ))
       )
    nil))
(add-hook 'after-change-functions 'my-editing-function)
