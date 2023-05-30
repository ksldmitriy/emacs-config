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
(add-hook 'c++-mode-hook 'display-line-numbers-mode)

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

;; ( Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-gruvbox))
 '(custom-safe-themes
   '("570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" default))
 '(highlight-indent-guides-method 'bitmap)
 '(ivy-initial-inputs-alist nil)
 '(org-startup-folded 'show3levels)
 '(package-selected-packages
   '(jit-spell spell-fu programmer-dvorak dap-mode browse-kill-ring lsp-origami lsp-ivy lsp-ui lsp-mode visual-fill-column org-bullets doom-themes highlight-indentation highlight-indent-guides ivy-rich which-key whick-key rainbow-delimiters ranbow-delimiters all-the-icons doom-modeline ivy--actions-list ivy beacon no-littering rainbow-mode cl-format yafolding vdiff markdown-mode golden-ratio-scroll-screen origami latex-preview-pane clang-format yasnippet-snippets use-package undo-fu rtags move-text modern-cpp-font-lock gruvbox-theme ggtags flycheck-color-mode-line evil-collection company cmake-ide)))

(setq custom--inhibit-theme-enable nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 123 :width normal))))
 '(button ((t (:foreground "#b8bb26" :underline nil :weight semi-bold))))
 '(flycheck-error ((t (:underline (:color "#fb4934" :style wave)))))
 '(flycheck-info ((t (:background "background" :underline nil))))
 '(flycheck-note ((t nil)))
 '(flycheck-warning ((t (:background "background" :underline nil))))
 '(font-lock-preprocessor-face ((t (:foreground "#ebdbb2" :slant normal :weight normal))))
 '(line-number ((t (:inherit default :foreground "#7c6f64" :slant normal :weight normal))))
 '(line-number-current-line ((t (:inherit (hl-line default) :background "#3c3836" :foreground "#fabd2f" :slant normal :weight normal))))
 '(link ((t (:foreground "#d3869b" :underline t :weight bold))))
 '(lsp-face-highlight-textual ((t (:background "background" :foreground "foreground" :weight semibold))))
 '(org-checkbox ((t (:inherit all-faces))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :foreground "#458588" :weight semi-light :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :foreground "#83a598" :weight semi-light :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :foreground "#83a598"))))
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
  (max 1 (/ (1- (window-height (selected-window))) 3)))

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
      '((t . regexp-quote)))

(global-set-key "\C-s" 'swiper)

(use-package all-the-icons
  :ensure t
  :config
  (all-the-icons-install-fonts t)
  )

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
(setq make-backup-files nil)

;; org mode

(defun my_org-mode-setup ()
  (visual-line-mode 1)
  (org-indent-mode 1)
  (my_org-font-setup)
  (display-line-numbers-mode -1)
  (text-scale-increase 2.2)
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
  (setq visual-fill-column-width 75
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
  (setq lsp-enable-snippet nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  :hook 
  (c++-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  )

(use-package lsp-ui 
  :commands lsp-ui-mode
  :config 
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 1)
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

(use-package browse-kill-ring
  :ensure t
  :bind (
         ("C-c p" . browse-kill-ring)
         )
  )


(use-package dap-mode
  :defer
  :custom
  (dap-auto-configure-mode t                           "Automatically configure dap.")
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
  :config
  ;;; dap for c++
  (require 'dap-lldb)
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb)

  ;;; set the debugger executable (c++)
  (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
  
  ;;; ask user for executable to debug if not specified explicitly (c++)
  (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))
  
  ;;; default debug template for (c++)
  (dap-register-debug-template
   "C++ LLDB dap"
   (list :type "lldb-vscode"
         :cwd  "${workspaceFolder}"
         :args nil
         :request "launch"
         :program nil))
  
  (defun dap-debug-create-or-edit-json-template ()
    "Edit the C++ debugging configuration or create + edit if none exists yet."
    (interactive)
    (let ((filename (concat (lsp-workspace-root) "/launch.json"))
	      (default "~/.emacs.d/default-dap-launch.json"))
      (unless (file-exists-p filename)
	    (copy-file default filename))
      (find-file-existing filename))))
