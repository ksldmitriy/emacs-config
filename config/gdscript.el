
(use-package
 gdscript-mode
 :config
 (setq gdscript-use-tab-indents nil)
 (setq gdscript-indent-offset 4)
 (add-hook 'gdscript-mode-hook (lambda () (lsp-mode)))

 ;; redefine format func
 (defun gdscript-comint-gdformat--run (arguments)
   "Run gdformat in comint mode.

ARGUMENTS are command line arguments for gdformat executable.
When run it will kill existing process if one exists."
   (let ((buffer-name (gdscript-util--get-gdformat-buffer-name))
         (inhibit-read-only t))

     (when (not (executable-find gdscript-gdformat-executable))
       (error
        "Error: Could not find %s on PATH.  Please customize the gdscript-gdformat-executable variable"
        gdscript-gdformat-executable))

     (with-current-buffer (get-buffer-create buffer-name)
       (unless (derived-mode-p 'gdformat-mode)
         (gdformat-mode)
         (buffer-disable-undo))
       (erase-buffer)
       (let* ((line-length
               (list
                (format "--line-length=%s"
                        gdscript-gdformat-line-length)))
              (indent-mode-arg
               (and (not gdscript-use-tab-indents)
                    (list (format "-s %s" gdscript-indent-offset))))
              (buffer
               (comint-exec
                (current-buffer)
                buffer-name
                gdscript-gdformat-executable
                nil
                (append line-length indent-mode-arg arguments))))
         (set-process-sentinel
          (get-buffer-process buffer)
          'gdscript-comint-gdformat--sentinel)
         buffer)))))
