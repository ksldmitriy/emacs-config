(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
		("org" . "https://orgmode.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/chat-gpt/")

(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(global-set-key (kbd "C-x e") 'eval-buffer)

(require 'no-littering)

(use-package undo-fu)

(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t nil))

(setq-default cursor-type 'bar)
(setq indent-tabs-mode nil)
(setq-default tab-width 4)

(autoload 'View-scroll-half-page-forward "view") (autoload 'View-scroll-half-page-backward "view")

(require 'clang-format)
(global-set-key (kbd "C-c f") 'clang-format-buffer)

(setq clang-format-style-option "llvm")

(use-package modern-cpp-font-lock
  :ensure t)


(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(setq font-lock-maximum-decoration t)

;; (use-package rtags
;;   :ensure t
;;   :hook (c++-mode . rtags-start-process-unless-running)
;;   :config (setq rtags-completions-enabled t
;; 		rtags-path "/home/dima/.emacs.d/rtags/src/rtags.el"
;; 		rtags-rc-binary-name "/home/dima/.emacs.d/rtags/bin/rc"
;; 		rtags-use-helm t
;; 		rtags-rdm-binary-name "/home/dima/.emacs.d/rtags/bin/rdm")
;;   :bind (("C-c E" . rtags-find-symbol)
;;   	 ("C-c e" . rtags-find-symbol-at-point)
;;   	 ("C-c O" . rtags-find-references)
;;   	 ("C-c o" . rtags-find-references-at-point)
;;   	 ("C-c s" . rtags-find-file)
;;   	 ("C-c v" . rtags-find-virtuals-at-point)
;;   	 ("C-c F" . rtags-fixit)
;; ;  	 ("C-c f" . rtags-location-stack-forward)
;;   	 ("C-c b" . rtags-location-stack-back)
;;   	 ("C-c n" . rtags-next-match)
;;   	 ("C-c p" . rtags-previous-match)
;;   	 ("C-c P" . rtags-preprocess-file)
;;   	 ("C-c R" . rtags-rename-symbol)
;;   	 ("C-c x" . rtags-show-rtags-buffer)
;;   	 ("C-c T" . rtags-print-symbol-info)
;;   	 ("C-c t" . rtags-symbol-type)
;;   	 ("C-c I" . rtags-include-file)
;;   	 ("C-c i" . rtags-get-include-file-for-symbol)))

;; (setq rtags-display-result-backend 'helm)


;; (require 'rtags)
;; (cmake-ide-setup)
;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)


;; (require 'rtags)
;; (cmake-ide-setup)
;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

(display-line-numbers-mode t)
;; ( Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-gruvbox))
 '(custom-safe-themes
   '("a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" default))
 '(highlight-indent-guides-method 'bitmap)
 '(ivy-initial-inputs-alist nil)
 '(package-selected-packages
   '(lsp-origami lsp-ivy lsp-ui lsp-mode visual-fill-column org-bullets doom-themes highlight-indentation highlight-indent-guides ivy-rich which-key whick-key rainbow-delimiters ranbow-delimiters all-the-icons doom-modeline ivy--actions-list ivy beacon no-littering rainbow-mode cl-format yafolding vdiff markdown-mode golden-ratio-scroll-screen origami latex-preview-pane clang-format yasnippet-snippets use-package undo-fu rtags move-text modern-cpp-font-lock gruvbox-theme ggtags flycheck-color-mode-line evil-collection company cmake-ide)))

(setq custom--inhibit-theme-enable nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 123 :width normal))))
 '(flycheck-info ((t (:background "background" :underline nil))))
 '(flycheck-note ((t nil)))
 '(flycheck-warning ((t (:background "background" :underline nil))))
 '(link ((t (:foreground "#d3869b" :underline t :weight bold))))
 '(lsp-face-highlight-textual ((t (:background "background" :foreground "foreground" :weight bold))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight semi-light :height 2.0))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.4))))
 '(org-link ((t (:inherit link :foreground "#458588" :underline nil)))))

;; Custom )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
(set-face-attribute 'flycheck-warning nil :underline nil)
(setq flycheck-indication-mode nil)


(require 'yasnippet)
(yas-global-mode 1)

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(setq company-clang-insert-arguments nil)
(setq company-tooltip-align-annotations t)

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()
  (interactive)                    
  (scroll-down (window-half-height)))

(global-set-key (kbd "C-M-j") 'scroll-up-half)
(global-set-key (kbd "C-M-k") 'scroll-down-half)

(global-set-key (kbd "C-S-s") 'replace-string)
(global-set-key(kbd "C-s") 'isearch-forward)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-set-key (kbd "M-j") 'move-text-down)
(global-set-key (kbd "M-k") 'move-text-up)

(beacon-mode 1)

(setq gdb-many-windows 1)

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
	"h" 'dired-up-directory
	"l" 'dired-find-file))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(setq chatgpt-shell-openai-key "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")

(require 'chatgpt-shell)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (dolist (map (list ivy-minibuffer-map
                     ivy-switch-buffer-map))
    (define-key map (kbd "C-j") 'ivy-next-line)
    (define-key map (kbd "C-k") 'ivy-previous-line-or-history))
  
  (define-key ivy-switch-buffer-map (kbd "C-M-K") 'ivy-switch-buffer-kill)

  (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
  ;; Move C-h to C-S-h
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-history-element)
  (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
      ;;'((t . regexp-quote)))

(global-set-key "\C-s" 'swiper)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon nil))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package highlight-indent-guides
  :ensure t)

(setq create-lockfiles nil)

;; org mode

(defun my_org-mode-setup ()
  (visual-line-mode 1)
  (org-indent-mode 1)
  (my_org-font-setup)
  (display-line-numbers-mode 0)
  ) 

(defun my_org-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :ensure t
  :hook (org-mode . my_org-mode-setup)
  :config
  (setq org-ellipsis " ")
  (setq org-hide-emphasis-markers t)
  (my_org-font-setup)
  
  (add-to-list 'org-file-apps '(directory . emacs))
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  
  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" .  "") prettify-symbols-alist)
                             (push '("[X]" . "" ) prettify-symbols-alist)
                             (push '("[-]" . "" ) prettify-symbols-alist)
                             (prettify-symbols-mode))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("" "" "" "" "" "" "")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(setq initial-buffer-choice "~/.emacs.d/start-page.org")

;; ( lsp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq company-lsp-enable-snippet nil)
  :hook (
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  )

(use-package lsp-ui 
  :commands lsp-ui-mode
  :hook(c++-mode . lsp-ui-doc-frame-mode)
  :config 
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-signature-doc-lines 1)
  (setq lsp-ui-doc-max-width 100)
)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package which-key
    :config
    (which-key-mode))


;; lsp )

(tab-bar-mode 1)                           ;; enable tab bar
(setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil)       ;; hide tab close / X button
(setq tab-bar-tab-hints t)                 ;; show tab numbers
(setq tab-bar-format '(tab-bar-format-tabs separator))
