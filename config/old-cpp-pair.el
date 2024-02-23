
(defun open-pair-files (NEWTAB)
  (cl-flet
   ((open-pair-buffers (left right new-tab)))

   (let ((cpp-file nil)
         (extension nil)
         (prev-buf (current-buffer))
         (basename)
         (cpp-header-extension ".hpp"))
     (progn
       (setq cpp-file (read-file-name "input filename: "))
       (setq extension (url-file-extension cpp-file))
       (setq basename (file-name-sans-extension cpp-file))

       (if (let* ((normalized-file-path (file-truename cpp-file))
                  (normalized-directory
                   (file-name-as-directory
                    (file-truename "/home/dima/repos/unreal"))))
             (string-prefix-p
              normalized-directory normalized-file-path))
           (setq cpp-header-extension ".h"))

       (cl-block
        my-func
        (if (not
             (or (string= extension cpp-header-extension)
                 (string= extension ".cpp")))
            (progn
              (print
               (format "%s%s" "invalid file extension: " extension))
              (cl-return-from my-func))
          (progn
            (if NEWTAB
                (progn
                  (switch-to-buffer prev-buf)
                  (tab-bar-new-tab))
              nil)
            (delete-other-windows)
            (find-file (format "%s%s" basename ".cpp"))
            (split-window-right)
            (other-window 1)
            (find-file (format "%s%s" basename cpp-header-extension))
            (other-window 1))))))))
