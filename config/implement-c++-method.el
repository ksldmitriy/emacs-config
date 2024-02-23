(defun implement-c++-method ()
  (interactive)

  (let ((start (region-beginning))
        (end (region-end))
        (class-name nil)
        (my-regexp-pattern "([ *&]*)([\\S]*?\\([\\s\\S]*?\\));"))

    (if (not (region-active-p))
        (progn
          (setq start (point-at-bol))
          (setq end (point-at-eol))))

    (setq class-name (read-string "Class name: "))

    (vr/replace
     my-regexp-pattern (concat "\\1" class-name "::\\2 {\n\n}\n") start end)

    (deactivate-mark)))
