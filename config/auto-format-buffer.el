(require 'use-package)

(defun auto-format-buffer ()
  (interactive)
  (cond
   ((derived-mode-p 'emacs-lisp-mode)
    (elisp-autofmt-buffer))

   ((derived-mode-p 'c++-mode)
    (clang-format-buffer))

   ((derived-mode-p 'c-mode)
    (clang-format-buffer))

   ((derived-mode-p 'glsl-mode)
    (format-all-buffer 1))

   ((derived-mode-p 'gdscript-mode)
    (gdscript-format-buffer))

   ((derived-mode-p 'cmake-mode)
    (message "cmake mode"))

   ((derived-mode-p 'html-mode)
    (format-all-buffer))


   ((derived-mode-p 'sql-mode)
    (format-all-buffer))))
