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

   ((derived-mode-p 'cmake-mode)
    (message "cmake mode"))))
