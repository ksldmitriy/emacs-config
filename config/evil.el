(require 'use-package)

(defun evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (keyboard-quit))

(use-package
 evil
 :demand t
 :bind
 (("<escape>" . keyboard-quit)

  :map
  evil-motion-state-map
  ("<remap> <evil-next-line>" . 'evil-next-visual-line)
  ("<remap> <evil-previous-line>" . 'evil-previous-visual-line)
  ("g t" . nil)
  ("g T" . nil)
  ("J" . 'tab-previous)
  ("K" . 'tab-next)
  ("/" . 'swiper)
  ("," . nil)
  (", f" . 'auto-format-buffer)

  :map
  evil-normal-state-map
  ("<remap> <evil-next-line>" . 'evil-next-visual-line)
  ("<remap> <evil-previous-line>" . 'evil-previous-visual-line)

  ("C-c t" .
   (lambda ()
     (interactive)
     (message "%s" (codeium-completion-at-point))
     (print (codeium-completion-at-point))))
  ("i" .
   (lambda ()
     (interactive)
     (evil-insert 1)))
  ("I" .
   (lambda ()
     (interactive)
     (evil-insert-line 1)))
  ("a" .
   (lambda ()
     (interactive)
     (evil-append 1)))
  ("A" .
   (lambda ()
     (interactive)
     (evil-append-line 1)))
  ("J" . 'tab-previous)
  ("K" . 'tab-next)
  ("s" . 'save-all-buffers)
  ("C-c o" .
   (lambda ()
     (interactive)
     (open-pair-files nil)))
  ("C-c O" .
   (lambda ()
     (interactive)
     (open-pair-files t)))
  ("C-c M-o" .
   (lambda ()
     (interactive)
     (open-pair-files t t)))
  ("U" . 'undo-tree-visualize)
  ("M" . 'evil-goto-mark)

  :map
  evil-insert-state-map
  ("C-c t" .
   (lambda ()
     (interactive)
     (message "%s" (codeium-completion-at-point))
     (print (codeium-completion-at-point))))
  ("C-SPC" . "_")
  ("C-g" . 'evil-keyboard-quit)
  ("C-f" . 'evil-keyboard-quit)
  ("C-j" . 'evil-keyboard-quit))

 :init
 (setq evil-want-keybinding nil)
 (setq evil-undo-system 'undo-tree)

 :config
 (evil-collection-init)
 (setq-default evil-cross-lines t)
 (evil-mode 1))

(use-package
 evil-collection
 :after evil
 :config
 (setq evil-want-integration t)
 (define-key evil-motion-state-map "g t" nil)
 (define-key evil-motion-state-map "g T" nil))
