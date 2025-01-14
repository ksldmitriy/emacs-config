(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/chat-gpt/")

(setq read-process-output-max (* 4 (* 1024 1024))) ;; 1mb
(setq gc-cons-threshold 140000000)

(global-unset-key (kbd "C-x C-c"))

(package-initialize)
(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package no-littering)

(defun clear-messages-buffer ()
  (interactive)
  (with-current-buffer "*Messages*"
    (read-only-mode -1)
    (erase-buffer)
    (read-only-mode 1)))


(defun clear-all-hooks ()
  (interactive)
  (mapatoms
   (lambda (sym)
     (when (and (boundp sym)
                (string-match "-hook$" (symbol-name sym)))
       (set sym nil))))
  (message "%s" hook-list))


(global-set-key
 (kbd "C-x e")
 (lambda ()
   (interactive)
   (save-buffer)
   (if (region-active-p)
       (let ((start (region-beginning))
             (end (region-end)))
         (eval-region start end))
     (eval-buffer nil nil nil nil t))))

(global-set-key (kbd "M-d") nil)

(defun load-config (filename)
  (load
   (expand-file-name (concat (concat "config/" filename) ".el")
                     user-emacs-directory)))

(use-package toml)

(load-config "local")

(load-config "evil")

(setq-default cursor-type 'bar)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

(require 'clang-format)
;;(global-set-key (kbd ", f") 'clang-format-buffer)

(setq clang-format-style-option "llvm")

(use-package modern-cpp-font-lock :ensure t)


(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-display-line-numbers-mode)
(setq display-line-numbers-width 0)


(setq font-lock-maximum-decoration t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-suggest-hint-threshold 0)
 '(css-indent-offset 2)
 '(custom-enabled-themes '(doom-tomorrow-night))
 '(custom-safe-themes
   '("8aeb4dbed3dd5c639adcc574eb2e5698b08545dd3d1794ed7e9b4f2b8eb289e4"
     "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d"
     "eab123a5ed21463c780e17fc44f9ffc3e501655b966729a2d5a2072832abd3ac"
     "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434"
     "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40"
     "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2"
     "32f22d075269daabc5e661299ca9a08716aa8cda7e85310b9625c434041916af"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a"
     "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0"
     "1e404b50eb5479b4840fc7134d70118ce832365e39c57045c864182e4e2dbf03"
     "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d"
     "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
     "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "6e33d3dd48bc8ed38fd501e84067d3c74dfabbfc6d345a92e24f39473096da3f"
     "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
     "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
     "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079"
     "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874"
     "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
     "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738"
     "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
     "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392"
     "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1"
     "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006"
     "6cfb6e47fd691972df591bffb7fbe275b31780eb8d6cf69f21c604ca707885ef"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a"
     "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f"
     "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443"
     "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577"
     "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78"
     "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22"
     "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68"
     "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8"
     "98ef36d4487bf5e816f89b1b1240d45755ec382c7029302f36ca6626faf44bbd"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668"
     "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"
     "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554"
     "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa"
     "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039"
     "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4"
     "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68"
     "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426"
     "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee"
     "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b"
     "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414"
     "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4"
     "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525"
     "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2"
     "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195"
     "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d"
     "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970"
     "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d"
     "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7"
     "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe"
     "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20"
     "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7"
     default))
 '(display-line-numbers-width 0)
 '(evil-undo-system 'undo-tree)
 '(extended-command-suggest-shorter nil)
 '(format-all-default-formatters
   '(("Assembly" asmfmt)
     ("ATS" atsfmt)
     ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex)
     ("C" clang-format)
     ("C#" csharpier)
     ("C++" clang-format)
     ("Cabal Config" cabal-fmt)
     ("Clojure" zprint)
     ("CMake" cmake-format)
     ("Crystal" crystal)
     ("CSS" prettier)
     ("Cuda" clang-format)
     ("D" dfmt)
     ("Dart" dart-format)
     ("Dhall" dhall)
     ("Dockerfile" dockfmt)
     ("Elixir" mix-format)
     ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp)
     ("Erlang" efmt)
     ("F#" fantomas)
     ("Fish" fish-indent)
     ("Fortran Free Form" fprettify)
     ("GLSL" clang-format)
     ("Go" gofmt)
     ("GraphQL" prettier)
     ("Haskell" brittany)
     ("HCL" hclfmt)
     ("HLSL" clang-format)
     ("HTML" html-tidy-configured)
     ("HTML+EEX" mix-format)
     ("HTML+ERB" erb-format)
     ("Hy" emacs-hy)
     ("Java" google-java-format)
     ("JavaScript" prettier)
     ("JSON" prettier)
     ("JSON5" prettier)
     ("Jsonnet" jsonnetfmt)
     ("JSX" prettier)
     ("Kotlin" ktlint)
     ("LaTeX" latexindent)
     ("Less" prettier)
     ("Literate Haskell" brittany)
     ("Lua" lua-fmt)
     ("Markdown" prettier)
     ("Meson" muon-fmt)
     ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format)
     ("OCaml" ocp-indent)
     ("Perl" perltidy)
     ("PHP" prettier)
     ("Protocol Buffer" clang-format)
     ("PureScript" purty)
     ("Python" black)
     ("R" styler)
     ("Reason" bsrefmt)
     ("ReScript" rescript)
     ("Ruby" rufo)
     ("Rust" rustfmt)
     ("Scala" scalafmt)
     ("SCSS" prettier)
     ("Shell" shfmt)
     ("Solidity" prettier)
     ("SQL" sqlformat)
     ("Svelte" prettier)
     ("Swift" swiftformat)
     ("Terraform" terraform-fmt)
     ("TOML" prettier)
     ("TSX" prettier)
     ("TypeScript" prettier)
     ("V" v-fmt)
     ("Verilog" istyle-verilog)
     ("Vue" prettier)
     ("XML" html-tidy)
     ("YAML" prettier)
     ("Zig" zig)
     ("_Angular" prettier)
     ("_AZSL" clang-format)
     ("_Beancount" bean-format)
     ("_Caddyfile" caddy-fmt)
     ("_Flow" prettier)
     ("_Gleam" gleam)
     ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt)
     ("_Snakemake" snakefmt)))
 '(ivy-initial-inputs-alist nil)
 '(ivy-posframe-border-width 2)
 '(lsp-enable-suggest-server-download nil)
 '(menu-bar-mode nil)
 '(org-startup-folded nil)
 '(package-selected-packages
   '(company-qml
     qml-mode
     lsp-latex
     acutex
     toml
     undo-tree
     db-pg
     ivy-posframe
     rustic
     markdownfmt
     markdown-preview-mode
     company-lua
     flymake-lua
     indent-guide
     indent-control
     indent-tools
     add-node-modules-path
     gdscript-mode
     ytdl
     lua-mode
     gh-md
     cmake-font-lock
     cmake-mode
     format-all
     vterm
     js-format
     xml-format
     flycheck-rust
     ivy-posframe-mode
     command-log-mode
     twilight-anti-bright-theme
     twilight-theme
     spacemacs-theme
     Cmake-mode
     yaml-mode
     company-glsl
     glsl-mode
     :init
     visual-regexp-steroids
     visual-regexp
     typescript-mode
     nerd-icons-ibuffer
     jit-spell
     spell-fu
     programmer-dvorak
     dap-mode
     browse-kill-ring
     lsp-origami
     lsp-ivy
     lsp-ui
     lsp-mode
     visual-fill-column
     org-bullets
     doom-themes
     highlight-indent-guides
     ivy-rich
     which-key
     whick-key
     rainbow-delimiters
     ranbow-delimiters
     all-the-icons
     doom-modeline
     ivy--actions-list
     ivy
     beacon
     no-littering
     rainbow-mode
     cl-format
     yafolding
     vdiff
     markdown-mode
     golden-ratio-scroll-screen
     origami
     clang-format
     use-package
     move-text
     modern-cpp-font-lock
     ggtags
     flycheck-color-mode-line
     evil-collection
     company
     cmake-ide))
 '(suggest-key-bindings nil)
 '(tab-bar-mode t)
 '(tool-bar-mode nil))

(setq custom--inhibit-theme-enable nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default
   ((((class color) (min-colors 257))
     (:background "#1d1f21" :foreground "#c5c8c6"))
    (((class color) (min-colors 256))
     (:background nil :foreground "#c5c5c5"))
    (((class color) (min-colors 16))
     (:background nil :foreground "white"))))
 '(button ((t (:inherit link :underline nil))))
 '(flycheck-note ((t nil)))
 '(ivy-posframe ((t (:background "#111214"))))
 '(ivy-posframe-border
   ((t (:inherit internal-border :background "#41728e"))))
 '(org-link
   ((((class color) (min-colors 257))
     (:inherit link :foreground "#81a2be"))
    (((class color) (min-colors 256))
     (:inherit link :foreground "#88aabb"))
    (((class color) (min-colors 16))
     (:inherit link :foreground "brightblue")))))

;; Custom )

(use-package
 flycheck
 :ensure t
 :init
 (global-flycheck-mode)
 (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


(set-face-attribute 'flycheck-fringe-warning nil
                    :foreground (face-attribute 'fringe :background))
(set-face-attribute 'flycheck-warning nil :underline nil)
(setq flycheck-indication-mode nil)


(use-package
 yasnippet
 :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
 :config
 (yas-global-mode 1)
 (global-set-key (kbd "C-;") 'yas-expand))

(use-package
 company
 :ensure t
 :init
 (setq company-idle-delay 0)
 (setq company-minimum-prefix-length 0)
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
;(global-set-key (kbd "C-s") 'isearch-forward)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-set-key (kbd "M-j") 'move-text-down)
(global-set-key (kbd "M-k") 'move-text-up)

(beacon-mode 1)

(setq gdb-many-windows 1)

(use-package
 dired
 :ensure nil
 :custom ((dired-listing-switches "-agho --group-directories-first"))
 :config
 (evil-collection-define-key
  'normal
  'dired-mode-map
  "h"
  'dired-up-directory
  "l"
  'dired-find-file
  "K"
  nil
  "J"
  nil))

(use-package
 markdown-mode
 :ensure t
 :mode ("README\\.md\\'" . gfm-mode)
 :init (setq markdown-command "multimarkdown"))

(setq chatgpt-shell-openai-key
      "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")

(use-package
 ivy
 :ensure t
 :config
 (ivy-mode 1)
 (dolist (map (list ivy-minibuffer-map ivy-switch-buffer-map))
   (define-key map (kbd "C-j") 'ivy-next-line)
   (define-key map (kbd "C-k") 'ivy-previous-line-or-history))

 (define-key
  ivy-switch-buffer-map (kbd "C-M-K") 'ivy-switch-buffer-kill)

 (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
 ;; Move C-h to C-S-h
 (define-key
  ivy-minibuffer-map (kbd "M-k") 'ivy-previous-history-element)
 (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-history-element)
 (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
 (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
 (define-key
  ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit))

(setq ivy-re-builders-alist '((t . regexp-quote)))

(global-set-key (kbd "C-s") 'swiper)

(use-package
 all-the-icons
 :ensure t
 :config ;(all-the-icons-install-fonts t)
 )

(use-package
 doom-modeline
 :ensure t
 :init (doom-modeline-mode 1)
 :config
 (setq doom-modeline-icon t)
 (setq doom-modeline-major-mode-icon nil))

(use-package ivy-rich :ensure t :init (ivy-rich-mode 1))

(load-config "face-attributes")

(use-package
 highlight-indent-guides
 :config
 (setq highlight-indent-guides-auto-enabled nil)
 (setq highlight-indent-guides-method 'bitmap)

 (set-face-background
  'highlight-indent-guides-odd-face
  (get-face-attribute 'font-lock-comment-face :foreground))
 (set-face-background
  'highlight-indent-guides-even-face
  (get-face-attribute 'font-lock-comment-face :foreground))
 (set-face-foreground
  'highlight-indent-guides-character-face
  (get-face-attribute 'font-lock-comment-face :foreground))
 :hook
 ((python-mode . highlight-indent-guides-mode)
  ;(gdscript-mode . highlight-indent-guides-mode)
  ))

(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/aux/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/aux/")))

;; org mode

(defun my_org-mode-setup ()
  (visual-line-mode 1)
  (org-indent-mode 1)
  (my_org-font-setup)
  (display-line-numbers-mode -1)
  (text-scale-increase 2.2))

(defun my_org-font-setup ()
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) " (0 (prog1 ()
           (compose-region (match-beginning 1) (match-end 1)
                           "•")))))))

(use-package
 org
 :ensure t
 :hook (org-mode . my_org-mode-setup)
 :config
 (setq org-ellipsis " ")
 (setq org-hide-emphasis-markers t)
 (my_org-font-setup)

 (add-to-list 'org-file-apps '(directory . emacs))
 (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

 (add-hook
  'org-mode-hook
  (lambda ()
    "Beautify Org Checkbox Symbol"
    (push '("[ ]" . "") prettify-symbols-alist)
    (push '("[X]" . "") prettify-symbols-alist)
    (push '("[-]" . "") prettify-symbols-alist)
    (prettify-symbols-mode))))


(use-package
 org-bullets
 :after org
 :hook (org-mode . org-bullets-mode)
 :custom (org-bullets-bullet-list '("" "" "" "" "" "" "")))

(defun efs/org-mode-visual-fill ()
  (setq
   visual-fill-column-width 75
   visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package
 visual-fill-column
 :hook (org-mode . efs/org-mode-visual-fill))

;; cli arguments and initial buffer
(defvar cli-files nil)
(defun cli-open-file-function ()
  (setq cli-files (append cli-files (list argi))))

(setq command-line-functions
      (append command-line-functions (list #'cli-open-file-function)))

(defun choose-initial-buffer ()
  (let ((start-file (or (car cli-files) "~/.emacs.d/start-page.org")))
    (funcall-interactively 'find-file start-file)))

(setq initial-buffer-choice #'choose-initial-buffer)

;; lsp
(use-package
 lsp-mode
 :commands (lsp lsp-deferred)
 :init (setq lsp-keymap-prefix "C-c l")
 :config
 (setq company-lsp-enable-snippet nil)
 (setq lsp-enable-snippet nil)
 (setq lsp-completion-enable-additional-text-edit nil)
 :hook
 (c++-mode . lsp)
 (c-mode . lsp)
 (csharp-mode . lsp)
 (lsp-mode . lsp-enable-which-key-integration))

(use-package
 lsp-ui
 :commands lsp-ui-mode
 :config
 (setq lsp-ui-doc-show-with-cursor t)
 (setq lsp-ui-doc-delay 1)
 (setq lsp-headerline-breadcrumb-enable nil)
 (setq lsp-signature-doc-lines 1)
 (setq lsp-ui-doc-max-width 100))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package which-key :config (which-key-mode))

;; lsp )

(tab-bar-mode 1) ;; enable tab bar
(setq tab-bar-show 1) ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil) ;; hide tab close / X button
(setq tab-bar-tab-hints t) ;; show tab numbers
(setq tab-bar-format '(tab-bar-format-tabs separator))

(use-package
 browse-kill-ring
 :ensure t
 :bind (("C-c p" . browse-kill-ring)))

(use-package
 dap-mode
 :defer

 :custom
 (dap-auto-configure-mode t "Automatically configure dap.")
 (dap-auto-configure-features
  '(sessions
    locals breakpoints expressions tooltip)
  "Remove the button panel in the top.")

 :config
 (require 'dap-lldb)
 (require 'dap-cpptools)
 (require 'dap-gdb-lldb)
 (setq dap-lldb-debug-program `("lldb-vscode"))

 ;;; ask user for executable to debug if not specified explicitly (c++)
 (setq dap-lldb-debugged-program-function
       (lambda () (read-file-name "Select file to debug.")))
 :init
 (bind-keys
  :map dap-mode-map
  :prefix "M-d"
  :prefix-map
  my-dap-prefix-map
  ("D" . dap-disconnect)
  ("d" . dap-debug)
  ("r" . dap-debug-restart)
  ("R" . dap-debug-recent)
  ("b a" . dap-breakpoint-add)
  ("b d" . dap-breakpoint-delete)
  ("b D" . dap-breakpoint-delete-all)
  ("b t" . dap-breakpoint-toogle)
  ("i" . dap-step-in)
  ("o" . dap-step-out)
  ("n" . dap-next)
  ("c" . dap-continue)
  ("s u" . dap-up-stack-frame)
  ("s d" . dap-down-stack-frame)
  ("s s" . dap-switch-stack-frame)
  ("h" . dap-hydra)
  ("l" . dap-ui-locals))
 :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))

;; tabs

(global-set-key
 (kbd "C-x t >")
 (lambda ()
   (interactive)
   (tab-move 1)))
(global-set-key
 (kbd "C-x t <")
 (lambda ()
   (interactive)
   (tab-move -1)))


;; clipboard

(global-set-key (kbd "C-c C-c p") 'clipboard-yank)
(global-set-key (kbd "C-c C-c y") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-c C-c d") 'clipboard-kill-region)

(global-unset-key (kbd "ESC ESC ESC"))
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(defun open-config ()
  (interactive)
  (tab-bar-new-tab)
  (find-file
   (find-lisp-object-file-name
    'open-config (symbol-function 'open-config))))

(defun open-configs ()
  (interactive)
  (tab-bar-new-tab)
  (dired
   (concat
    (file-name-directory
     (find-lisp-object-file-name
      'open-config (symbol-function 'open-config)))
    "config")))

(put 'erase-buffer 'disabled nil)

(defun ttp ()
  (interactive)
  (let ((ttp-path))
    (setq ttp-path
          (s-replace
           "\n" "" (f-read-text "~/scripts/data/ttp-dir" 'utf-8)))
    (dired (concat ttp-path "/src"))))

(use-package
 visual-regexp-steroids
 :config
 (global-set-key (kbd "C-c r") 'vr/replace)
 (global-set-key (kbd "C-c s") 'vr/isearch-forward)
 (global-set-key (kbd "C-c S") 'vr/isearch-backward))


(use-package
 glsl-mode
 :config
 (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
 (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
 (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

(use-package company-glsl)

(use-package
 cc-mode
 :config
 (define-key c++-mode-map (kbd "C-c C-c") nil)
 (define-key c-mode-map (kbd "C-c C-c") nil)
 (define-key c++-mode-map (kbd "C-c i") 'implement-c++-method)
 (setq c-basic-offset 4)
 ;; (add-hook
 ;;  'c-mode-common-hook
 ;;  (lambda ()
 ;;    ;; (setq indent-tabs-mode nil)
 ;;    ;; (indent-tabs-mode nil)
 ;;    (hide-ifdef-mode)
 ;;    (setq hide-ifdef-shadow t)
 ;;    (hide-ifdefs)))
 )

(setq suggest-key-bindings nil)
(setq extended-command-suggest-shorter nil)

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t))
(global-set-key (kbd "M-s") 'save-all-buffers)

(use-package
 ivy-posframe
 :config (ivy-posframe-mode 1)
 (setq ivy-posframe-display-functions-alist
       '((swiper . ivy-display-function-fallback)
         (counsel-M-x . ivy-posframe-display-at-window-bottom-left)
         (t . ivy-posframe-display))))

(use-package
 undo-tree
 :config
 (setq undo-tree-history-directory-alist
       '(("." . "~/.emacs.d/undo")))
 (global-undo-tree-mode))

(load-config "auto-format-buffer")

(load-config "implement-c++-method")

(use-package
 format-all
 :config
 (define-format-all-formatter
  html-tidy-configured
  (:executable "tidy")
  (:languages "HTML")
  (:features)
  (:format
   (format-all--buffer-hard
    '(0 1)
    nil
    nil
    executable
    "-config"
    "~/.config/tidy/config"
    "-q"
    "--tidy-mark"
    "no")))

 (setq-default format-all-formatters
               '(("HTML" (html-tidy-configured))
                 ("CSS" prettier)
                 ("SQL" pgformatter))))

(load-config "gdscript")

(use-package rustic)

(load-config "pair-files-bin")

(load-config "latex")
