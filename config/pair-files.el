(defun open-cpp-pair-files (corn-file NEWTAB)
  (cl-flet
   (
    ; funcs beg
    ;; read-file beg
    (read-corn-filename
     ()
     (read-file-name "input filename: "
                     nil nil nil nil ;
                     (lambda (file)
                       (or (file-directory-p file)
                           (string-suffix-p ".cpp" file)
                           (string-suffix-p ".c" file)
                           (string-suffix-p ".hpp" file)
                           (string-suffix-p ".h" file)
                           (string-suffix-p ".frag" file)
                           (string-suffix-p ".vert" file)))))
    ;; read-file end

    ;; open-pair-buffers beg 
    (open-pair-buffers
     (left right)
     (let ((prev-buf (current-buffer)))
       (if NEWTAB
           (progn
             (switch-to-buffer prev-buf)
             (tab-bar-new-tab))
         nil)
       (delete-other-windows)
       (find-file left)
       (split-window-right)
       (other-window 1)
       (find-file right)
       (other-window 1)))
    ;; open-pair-buffers end

    ;; contains-sub-dir beg
    (contains-sub-dirs
     (dir sub-dirs)
     (let ((result nil))

       (cl-dolist
        (sub-dir sub-dirs)
        (if (file-directory-p (concat dir sub-dir))
            (progn
              (setq result (concat dir sub-dir "/"))
              (cl-return))))

       result))
    ;; contains-sub-dirs beg

    ;; is-src beg
    (is-src
     (filename)
     (let ((extension (file-name-extension filename)))
       (and (member extension '("cpp" "c")) t)))
    ;; is-src end

    ; funcs end
    )


   (cl-flet
    (
     ;; find-root-dirs beg
     (find-root-dirs
      (dir)
      (let ((cur-dir dir)
            (src-dir nil)
            (header-dir nil)
            (is-found nil))

        (while (and (not is-found) cur-dir)
          (setq is-found
                (and (setq src-dir
                           (contains-sub-dirs
                            cur-dir '("src" "source")))
                     (setq header-dir
                           (contains-sub-dirs cur-dir '("include")))))
          (if (not is-found)
              (setq cur-dir (file-name-parent-directory cur-dir))))

        (if is-found
            (vector src-dir header-dir)
          (vector dir dir))))
     ;; find-root-dirs end

     ;; child-dir-delta beg
     (child-dir-delta
      (root-dirs dir)
      (let ((prefix-dir (aref root-dirs 1))
            (delta nil))

        (if (string-prefix-p (aref root-dirs 0) dir)
            (setq prefix-dir (aref root-dirs 0)))

        (setq delta (substring dir (length prefix-dir)))

        delta))
     ;; child-dir-delta end
     )

    ;(clear-messages-buffer)

    (let ((corn-basename nil)
          (corn-dir nil)
          (root-dirs nil)
          (delta-dir nil)
          (src-dir nil)
          (header-dir nil)
          (headers-extension ".hpp")
          (source-extension ".cpp")
          (source-file nil)
          (header-file nil))

      (setq corn-basename
            (file-name-sans-extension
             (file-name-nondirectory corn-filename)))

      (setq is-src (is-src corn-filename))

      (setq corn-dir (file-name-directory corn-filename))
      (setq root-dirs (find-root-dirs corn-dir))

      (setq delta-dir (child-dir-delta root-dirs corn-dir))

      (setq src-dir (concat (aref root-dirs 0) delta-dir))
      (setq header-dir (concat (aref root-dirs 1) delta-dir))

      (setq source-file
            (concat src-dir corn-basename source-extension))
      (setq header-file
            (concat header-dir corn-basename headers-extension))

      (message "corn: %s" corn-filename)
      (message "corn base: %s" corn-basename)
      (message "corn dir: %s" corn-dir)
      (message "is src: %S" is-src)
      (message "root dir: %S" root-dirs)
      (message "delta dir: %S" delta-dir)
      (message "src dir: %S" src-dir)
      (message "header dir: %S" header-dir)
      (message "source file: %S" source-file)
      (message "header file: %S" header-file)

      (open-pair-buffers source-file header-file)
      ;
      ))))

(global-set-key
 (kbd "C-c o")
 (lambda ()
   (interactive)
   (open-pair-files nil)))
(global-set-key
 (kbd "C-c O")
 (lambda ()
   (interactive)
   (open-pair-files t)))

(defun open-pair-glsl-files (corn-file NEWTAB)
  (let ((corn-basename (file-name-sans-extension corn-filename)))

    (if NEWTAB
        (tab-bar-new-tab))
    (delete-other-windows)
    (find-file (concat corn-basename ".vert"))
    (split-window-right)
    (other-window 1)
    (find-file (concat corn-basename ".frag"))
    (other-window 1)))

(defun open-pair-files (NEWTAB)
  (cl-flet
   (
    ; funcs beg
    ;; read-file beg
    (read-corn-filename
     ()
     (read-file-name "input filename: "
                     nil nil nil nil ;
                     (lambda (file)
                       (or (file-directory-p file)
                           (string-suffix-p ".cpp" file)
                           (string-suffix-p ".c" file)
                           (string-suffix-p ".hpp" file)
                           (string-suffix-p ".h" file)
                           (string-suffix-p ".frag" file)
                           (string-suffix-p ".vert" file))))))
   ;; read-file end


   (let ((corn-filename nil))
     (setq corn-filename (read-corn-filename))
     (if (or (string-suffix-p ".frag" corn-filename)
             (string-suffix-p ".vert" corn-filename))
         (open-pair-glsl-files corn-filename NEWTAB)
       (open-cpp-pair-files corn-filename NEWTAB)))))
