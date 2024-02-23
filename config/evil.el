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
  ("g t" . nil)
  ("g T" . nil)
  ("J" . 'tab-previous)
  ("K" . 'tab-next)
  ("/" . 'swiper)
  ("," . nil)
  (", f" . 'auto-format-buffer)

  :map
  evil-normal-state-map
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

  :map
  evil-insert-state-map
  ("C-SPC" . "_")
  ("C-g" . 'evil-keyboard-quit)
  ("C-f" . 'evil-keyboard-quit)
  ("C-j" . 'evil-keyboard-quit))

 :init
 (setq evil-want-keybinding nil)
 (setq evil-undo-system 'undo-fu)

 :config
 (evil-collection-init)
 (evil-mode 1))

(use-package
 evil-collection
 :after evil
 :config
 (setq evil-want-integration t)
 (define-key evil-motion-state-map "g t" nil)
 (define-key evil-motion-state-map "g T" nil))
