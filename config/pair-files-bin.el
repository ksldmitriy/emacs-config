(defun choose-pair-files (corn-file left-file-out right-file-out)
  (let* ((out-buffer (generate-new-buffer "out-buf"))
         (exit-code
          (call-process
           "~/.emacs.d/github/pair-files-bin/bin/pair-files"
           nil out-buffer t corn-file)))

    (cl-block
     func-body (set-buffer out-buffer)

     (if (not (eq 0 exit-code))
         (progn
           (message "pair-files failed with: %s" (buffer-string))
           (cl-return-from "func-body")))

     (if (not (eq 2 (count-lines (point-min) (point-max))))
         (progn
           (message "invalid pair-files output lines count")
           (cl-return-from func-body)))

     (goto-char (point-min))
     (set
      left-file-out (string-trim-right (thing-at-point 'line t) "\n"))
     (forward-line 1)
     (set
      right-file-out
      (string-trim-right (thing-at-point 'line t) "\n")))

    (if (get-buffer out-buffer)
        (kill-buffer out-buffer))))

(defun open-pair-files (newtab &optional prev-tab)
  (interactive)
  (let (corn-file
        left-file
        right-file)

    (setq corn-file
          (read-file-name "input filename: "
                          nil nil nil nil ;
                          (lambda (file)
                            (or (file-directory-p file)
                                (string-suffix-p ".cpp" file)
                                (string-suffix-p ".c" file)
                                (string-suffix-p ".hpp" file)
                                (string-suffix-p ".h" file)))))

    (choose-pair-files corn-file 'left-file 'right-file)

    (if newtab
        (tab-bar-new-tab (if prev-tab 0 1)))
    (delete-other-windows)

    (find-file left-file)
    (funcall-interactively 'find-file left-file)
    (split-window-right)
    (other-window 1)
    (funcall-interactively 'find-file right-file)
    (other-window 1)))
