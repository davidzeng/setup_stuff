;; Set the option key to be meta instead of the command key.
;; This matches the emacs in the console.
(setq mac-command-modifier 'alt
     mac-option-modifier 'meta)

;; Ensure .h files load obj-c if necessary
(defun bh-choose-header-mode ()
 (interactive)
 (if (string-equal (substring (buffer-file-name) -2) ".h")
     (progn
       ;; OK, we got a .h file, if a .m file exists we'll assume it's
       ; an objective c file. Otherwise, we'll look for a .cpp file.
       (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
             (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
         (if (file-exists-p dot-m-file)
             (progn
               (objc-mode)
               )
           (if (file-exists-p dot-cpp-file)
               (c++-mode)
             )
           )
         )
       )
   )
 )
(add-hook 'find-file-hook 'bh-choose-header-mode)


;; Compile an xcode project
(defun bh-compile ()
 (interactive)
 (let ((df (directory-files "."))
       (has-proj-file nil)
       )
   (while (and df (not has-proj-file))
     (let ((fn (car df)))
       (if (> (length fn) 10)
           (if (string-equal (substring fn -10) ".xcodeproj")
               (setq has-proj-file t)
             )
         )
       )
     (setq df (cdr df))
     )
   (if has-proj-file
       (compile "xcodebuild -configuration Debug")
     (compile "make")
     )
   )
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-width 2)
 '(c-basic-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
