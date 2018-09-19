(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 800 1024))))

(add-to-list 'load-path (concat user-emacs-directory "load/"))

(eval-and-compile
  (require 'package)
  (setq package-archives
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa-unstable" . "https://melpa.org/packages/")))
  (package-initialize)

  ; We already initialized above.
  (setq package-enable-at-startup nil)

  (defvar my-ensured-packages (make-hash-table))

  ; Most things are installed by use-package, so that's all we need to get
  ; manually.
  (let ((p 'use-package))
    (unless (package-installed-p p)
      (unless (assq p package-archive-contents) (package-refresh-contents))
      (package-install p))
    (puthash p nil my-ensured-packages))

  (defvar use-package-verbose t)
  (require 'cl-lib)
  (require 'use-package)

  (defmacro my-use-package (name &rest args)
    "Like use-package, but saves the ensured package (if any) in
my-ensured-packages."
    (let* ((ensure (cadr (memq :ensure args)))
           (pkg (if (eq ensure t) name ensure)))
      (if ensure (puthash pkg nil my-ensured-packages)))
    (append `(use-package ,name) args)))

(use-package savehist
  :config
  (savehist-mode 1))

(my-use-package diminish
  :ensure t)

(my-use-package general
  :ensure t
  :config
  (general-evil-setup)
  (defalias 'gsetq #'general-setq))

(my-use-package evil
  :ensure t
  :diminish undo-tree-mode
  :init
  ; evil-collection replaces this.
  (gsetq evil-want-keybinding nil)

  (defvar evil-want-Y-yank-to-eol t)

  :config
  (evil-mode 1)

  (my-use-package evil-args
    :ensure t
    :config
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

  (my-use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))

  (my-use-package evil-commentary
    :ensure t
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode))

  (my-use-package evil-goggles
    :ensure t
    :diminish evil-goggles-mode
    :config
    (gsetq evil-goggles-blocking-duration 0.100
           evil-goggles-async-duration 0.400)
    (evil-goggles-mode))

  (my-use-package evil-snipe
    :ensure t
    :diminish evil-snipe-local-mode
    :config
    (evil-snipe-mode 1)
    (evil-snipe-override-mode 1)
    ; Don't consider evil-snipe's ; and , for repeating with . (dot).
    (evil-declare-ignore-repeat 'evil-snipe-repeat)
    (evil-declare-ignore-repeat 'evil-snipe-repeat-reverse)
    (gsetq evil-snipe-scope 'visible
           evil-snipe-repeat-scope 'whole-buffer)
    ; Don't repeat search on another press of s/S.
    (gsetq evil-snipe-repeat-keys nil))

  (my-use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  ; This isn't an evil-mode setting, but it's here to match Vim: you need to
  ; explicitly specify "+ if you want something there.
  (gsetq select-enable-clipboard nil)

  ; When closing split windows, return to the window we split from instead of
  ; some seemingly unpredictable choice.
  (defvar my-window-parents nil)
  (defun my-save-window-next-parent-function (split &rest args)
    (my-save-window-parent-function #'next-window split args))
  (defun my-save-window-prev-parent-function (split &rest args)
    (my-save-window-parent-function #'previous-window split args))
  (defun my-save-window-parent-function (get-child split args)
    ; Evil doesn't do "create split window and switch to it". It creates a split
    ; window in the opposite direction and stays at the current window. Thus
    ; what we consider the child is actually the original pre-split window.
    (let ((child (selected-window))
          (res (apply split args))
          (parent (apply get-child nil)))
      (setq my-window-parents (cons (cons parent child) my-window-parents))
      res))
  (defun my-restore-window-parent-function (delete &rest args)
    (let ((dead-child (selected-window)))
      (apply delete args)
      (dolist (x my-window-parents)
        (pcase x
          (`(,parent . ,child)
           (if (window-live-p parent)
               (when (eq child dead-child)
                 (progn
                   (select-window parent)
                   (setq my-window-parents (delq x my-window-parents))))
             (setq my-window-parents (delq x my-window-parents))))))))
  (advice-add 'evil-window-split
              :around #'my-save-window-next-parent-function)
  (advice-add 'evil-window-vsplit
              :around #'my-save-window-prev-parent-function)
  (advice-add 'evil-window-delete
              :around #'my-restore-window-parent-function)

  (gsetq evil-auto-balance-windows nil
         evil-vsplit-window-right 1)
  (setq-default evil-symbol-word-search t)

  ; I don't use | normally and would rather have \ as a leader than dedicate it
  ; to this.
  (define-key evil-normal-state-map "|" 'evil-execute-in-emacs-state)

  ; Enable register pasting into the minibuffer.
  (define-key minibuffer-local-map "\C-r" #'evil-paste-from-register)

  (define-key evil-normal-state-map "~" 'evil-invert-case)
  (define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line)

  ; Better completion for :e and :b.
  (define-key evil-ex-map "e " #'counsel-find-file)
  (define-key evil-ex-map "b " #'ivy-switch-buffer)

    (defun my-counsel-curdir-rg
        (&optional initial-input extra-rg-args rg-prompt)
      (interactive)
      (counsel-rg initial-input default-directory extra-rg-args rg-prompt))

  (general-nmap
    :prefix "\\"
    "s" #'my-counsel-curdir-rg
    "S" #'counsel-rg))

(my-use-package crux
  :ensure t
  :bind (("C-c e" . crux-eval-and-replace)))

(my-use-package column-enforce-mode
  :ensure t
  :commands column-enforce-mode
  :init
  (defun toggle-column-enforcement ()
    (interactive)
    ; Kind of the wrong place for this setting but it works for me.
    (setq-local column-enforce-column fill-column)
    (if (and (boundp 'column-enforce-mode) column-enforce-mode)
        (column-enforce-mode -1)
      (column-enforce-mode t)))
  (define-key evil-normal-state-map (kbd "RET") 'toggle-column-enforcement)
  (evil-declare-ignore-repeat 'toggle-column-enforcement)
  ; Use fill-column instead.
  (setq-default column-enforce-column nil))

(defun my-insert-file-created ()
  (interactive)
  (let ((start (point)))
    (insert "File created: "
            (shell-command-to-string "date --rfc-3339=seconds"))
    (comment-region start (point))))

(my-use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :config
  (eyebrowse-mode t)
  (gsetq eyebrowse-wrap-around t
         eyebrowse-new-workspace t)
  ; eyebrowse-setup-opinionated-keys minus the C-<, C->, C-' mappings. Also,
  ; don't override evil-commentary's mappings in visual state, we only use the
  ; tab mappings in normal state anyway.
  (let ((evil-motion-state-map evil-normal-state-map))
    (eyebrowse-setup-evil-keys))
  (define-key eyebrowse-mode-map (kbd "C-\"") 'eyebrowse-close-window-config)
  (define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
  (define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
  (define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
  (define-key eyebrowse-mode-map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
  (define-key eyebrowse-mode-map (kbd "M-9") 'eyebrowse-switch-to-window-config-9))

(column-number-mode)
(gsetq column-number-indicator-zero-based nil)
(setq-default display-line-numbers 'relative)
(show-paren-mode 1)

(my-use-package highlight-escape-sequences
  :ensure t
  :config
  (defconst my-hes-php-re
    (rx (submatch
         (and ?\\ (submatch
                   (or (repeat 1 3 (in "0-7"))
                       (and ?x (repeat 2 hex-digit))
                       (any "\"\'\\efnrtv$"))))))
    "Regexp to match PHP string escape sequences.")
  (gsetq hes-mode-alist (cons `(php-mode . ,my-hes-php-re) hes-mode-alist))
  (hes-mode))

; More code folding support, Evil knows to use it if available.
(my-use-package hideshow
  :diminish hs-minor-mode
  :config
  (general-add-hook
   '(python-mode-hook c-mode-hook c++-mode-hook java-mode-hook)
   #'hs-minor-mode))

(my-use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode 1))

(my-use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(my-use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

(my-use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom))

(my-use-package whitespace
  :init
  (add-hook 'whitespace-load-hook (lambda ()
    (gsetq whitespace-style (delq 'lines whitespace-style)))))

(my-use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(my-use-package cycbuf
  :ensure t
  :config
  (cycbuf-init))

(my-use-package ivy
  :ensure t
  :bind ("C-x b" . ivy-switch-buffer)
  :bind ("C-c r" . ivy-resume)
  :init
  (my-use-package counsel
    :ensure t
    :bind ("M-x" . my-M-x)
    :bind ("C-c f" . counsel-recentf)
    :bind ("C-h f" . counsel-describe-function)
    :bind ("C-h v" . counsel-describe-variable)
    :bind (:map minibuffer-local-map
           ("C-s" . counsel-minibuffer-history))
    :bind ("C-x C-f" . counsel-find-file)
    :init
    (defun my-M-x ()
      (interactive)
      (counsel-M-x ""))) ; Don't start with "^"
  (my-use-package ivy-hydra
    :ensure t)
  (my-use-package swiper
    :ensure t
    :bind ("C-s" . swiper)
    :bind ("M-s o" . swiper-all))
  (my-use-package counsel-dash
    :ensure t
    :commands counsel-dash
    :init
    ; Perhaps upstream at some point:
    ; https://github.com/nathankot/counsel-dash/issues/3
    (defun counsel-dash-at-point ()
      (interactive)
      (counsel-dash (thing-at-point 'symbol))))
  :config
  ; Enable register pasting into Ivy's minibuffer.
  (define-key ivy-minibuffer-map "\C-r" #'evil-paste-from-register))

; Helm is still needed for flycheck; also used for apropos as a bonus, though
; that one's not so important.
(my-use-package helm
  :ensure t
  ; It knows how to load itself
  :defer t
  :bind ("C-h a" . helm-apropos)
  :config
  (use-package grep)
  (gsetq helm-split-window-inside-p t)
  (push '("^\\*[Hh]elm.+\\*$" :regexp t :height 15)
        popwin:special-display-config))

(my-use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode)

(my-use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (my-use-package counsel-projectile
    :ensure t
    :config
    (gsetq projectile-completion-system 'ivy)
    (define-key projectile-command-map (kbd "s s") #'counsel-projectile-rg)

    ; This doesn't belong here as such, but using a similar key binding to
    ; counsel-projectile-rg is very mnemonic.
    (define-key projectile-command-map (kbd "s .") #'my-counsel-curdir-rg))

  (projectile-mode))

(my-use-package flycheck
  :ensure t
  :init
  (defun my-safe-gcc-p (x)
    (member x (list "avr-gcc")))
  (put 'flycheck-c/c++-gcc-executable 'safe-local-variable #'my-safe-gcc-p)
  (defun my-safe-gcc-arg-p (x)
    (or (string-prefix-p "-O" x)
        (string-prefix-p "-m" x)
        (string-prefix-p "-std" x)))
  (put 'flycheck-gcc-args 'safe-local-variable
       (lambda (xs) (cl-every #'my-safe-gcc-arg-p xs)))
  :config
  ; See https://github.com/flycheck/flycheck/issues/1446#issuecomment-381096987
  ; and follow-ups.
  (setcar (memq 'source-inplace (flycheck-checker-get 'typescript-tslint 'command))
          'source-original)

  (my-use-package flycheck-pos-tip
    :ensure t
    :config
    (gsetq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (my-use-package helm-flycheck
    :ensure t
    :commands helm-flycheck
    :init
    (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (push "*Flycheck errors*" popwin:special-display-config)
  (define-key evil-normal-state-map "[f" 'flycheck-previous-error)
  (define-key evil-normal-state-map "]f" 'flycheck-next-error)

  ; Run pylint after flake8, if both are available. (flake8 is lighter, so run
  ; it first.)
  (flycheck-add-next-checker 'python-flake8 'python-pylint))

(gsetq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(gsetq standard-indent 3)

(gsetq backward-delete-char-untabify-method nil)

(setq-default fill-column 80)
(add-hook 'python-mode-hook (lambda () (setq-local fill-column 79)))

(c-add-style "deewiant" '("k&r" (c-basic-offset . 3)))
(setq-default c-default-style "deewiant")

(use-package browse-url
  :config
  (gsetq browse-url-firefox-program "firefox-beta"
         browse-url-browser-function #'browse-url-firefox))

(my-use-package company
  :ensure t
  :diminish company-mode
  :config
  (defun my-complete-simply ()
    (interactive)
    (company-begin-backend 'company-dabbrev))
  (global-company-mode)
  (use-package company-dabbrev
    :config
    ; This basically makes this equivalent to company-dabbrev-code, but without
    ; the "ignore things in comments and strings" feature.
    (gsetq company-dabbrev-char-regexp "\\(?:\\sw\\|\\s_\\)"
           company-dabbrev-downcase nil
           company-dabbrev-ignore-case t)

    ; Replace company-dabbrev-code (in the grouped backend containing it) with
    ; company-dabbrev, move everything after that backend to before it because
    ; company-dabbrev will always succeed (at the time of writing that means only
    ; company-oddmuse, which seems like it should be earlier anyway since it's a
    ; mode-specific one), and delete the original lone company-dabbrev entry.
    ;
    ; Basically this, coupled with the company-dabbrev-char-regexp setting above,
    ; means that we get word-and-symbol completion as a last resort in all
    ; buffers, even from strings and comments.
    (message "Original backends: %s" company-backends)
    (let* ((bes (delq 'company-dabbrev company-backends))
           (dabbrev-be
            (cl-find-if (lambda (be)
                          (and (listp be) (memq 'company-dabbrev-code be)))
                        company-backends))
           (bes (delq dabbrev-be bes))
           (dabbrev-be2
            (cl-substitute 'company-dabbrev 'company-dabbrev-code dabbrev-be))
           (bes (append bes (list dabbrev-be2))))
      (gsetq company-backends bes)))

  (gsetq company-idle-delay 0
         company-minimum-prefix-length 2
         company-show-numbers t)
  (define-key evil-insert-state-map (kbd "C-x C-n") 'my-complete-simply)
  (define-key company-active-map (kbd "S-RET") 'comment-indent-new-line)
  (define-key company-active-map (kbd "S-<return>") 'comment-indent-new-line)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (my-use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode 1)))

(my-use-package dumb-jump
  :ensure t
  :diminish dumb-jump-mode
  :config
  (gsetq dumb-jump-force-searcher 'rg
         dumb-jump-selector 'ivy)
  (define-key evil-normal-state-map (kbd "C-]") 'dumb-jump-go)
  (define-key evil-normal-state-map (kbd "C-w C-]") 'dumb-jump-go-other-window)
  ; C-} = C-S-]
  (define-key evil-normal-state-map (kbd "C-}") 'dumb-jump-go-prefer-external)
  (define-key evil-normal-state-map (kbd "C-w C-}") 'dumb-jump-go-prefer-external-other-window))

(my-use-package ediff
  :defer t
  :config
  (gsetq ediff-window-setup-function #'ediff-setup-windows-plain))

(my-use-package generic-x
  :config
  (gsetq
    generic-extras-enable-list
    (append generic-default-modes generic-mswindows-modes generic-unix-modes
            generic-other-modes)))

(my-use-package magit
  :ensure t
  :diminish auto-revert-mode
  :bind ("C-x g" . magit-status)
  :bind ("C-x M-g" . magit-dispatch-popup)
  :bind ("C-x f" . magit-file-popup)
  :init
  (gsetq vc-handled-backends (delq 'Git vc-handled-backends))
  :config
  (gsetq git-commit-summary-max-length 50)

  (magit-change-popup-key 'magit-blame-popup :action ?b ?l)

  (defun my-override-commit-fill-column ()
    (setq-local fill-column 72))
  (add-hook 'git-commit-mode-hook #'my-override-commit-fill-column)

  (my-use-package evil-magit
    :ensure t))

(my-use-package antlr-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode)))

(my-use-package asm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.fasm?\\'" . asm-mode)))

(my-use-package clang-format
  :ensure t
  :commands clang-format clang-format-buffer clang-format-region
  :init

  ; Thanks to https://eklitzke.org/smarter-emacs-clang-format for this idea.
  (with-no-warnings
    (defvar-local my-dot-clang-format-dir 'unset))
  (defun my-clang-format-buffer-smart ()
    (interactive)
    (when (eq my-dot-clang-format-dir 'unset)
      (setq-local my-dot-clang-format-dir
                      (locate-dominating-file
                       (or buffer-file-name default-directory)
                       ".clang-format")))
    (if my-dot-clang-format-dir
        (clang-format-buffer)
      (when (called-interactively-p 'interactive)
        (message ".clang-format not found, so nothing done"))))

  (defun my-clang-format-buffer-smart-on-save ()
    (when (memq major-mode '(c-mode c++-mode))
      (add-hook 'before-save-hook #'my-clang-format-buffer-smart nil t)))
  (general-add-hook '(c-mode-hook c++-mode-hook)
                    #'my-clang-format-buffer-smart-on-save))

; Note: will fail if .clang-tidy file does not exist (can be empty to
; effectively disable the checker)
(my-use-package flycheck-clang-tidy
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))

(my-use-package irony
  :ensure t
  :diminish irony-mode
  :commands irony-mode
  :init
  (defun my-irony-mode-enable ()
    ; This is basically irony-supported-major-modes but hard-coded because we
    ; don't load irony until after the call to irony-mode. It shouldn't matter
    ; because the list presumably changes very rarely and we add the hooks
    ; manually anyway.
    (when (memq major-mode '(c-mode c++-mode objc-mode))
      (irony-mode 1)))
  (add-hook 'c-mode-common-hook (lambda () (abbrev-mode -1)))
  (general-add-hook '(c-mode-hook c++-mode-hook objc-mode-hook)
                    #'my-irony-mode-enable)
  :config
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (my-use-package company-irony
    :ensure t
    :commands company-irony company-irony-setup-begin-commands
    :config
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    (add-to-list 'company-backends 'company-irony)))

(my-use-package smart-dash
  :ensure t
  :commands smart-dash-mode
  :init
  (defun my-smart-dash-enable ()
    (when (memq major-mode '(c-mode c++-mode))
      (smart-dash-mode)))
  (general-add-hook '(c-mode-hook c++-mode-hook) #'my-smart-dash-enable))

(my-use-package clojure-mode
  :ensure t
  :commands clojure-mode)

(my-use-package cython-mode
  :ensure t
  :commands cython-mode
  :config
  (my-use-package flycheck-cython
    :ensure t))

(my-use-package dns-mode
  :config
  (gsetq dns-mode-soa-auto-increment-serial nil))

(my-use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode)

(my-use-package feature-mode
  :ensure t
  :commands feature-mode)

(my-use-package glsl-mode
  :ensure t
  :commands glsl-mode)

(my-use-package go-mode
  :ensure t
  :commands go-mode
  :config
  (gsetq gofmt-command "goimports")
  (defun my-add-gofmt-before-save-hook ()
    (add-hook 'before-save-hook #'gofmt-before-save nil t))
  (add-hook 'go-mode-hook #'my-add-gofmt-before-save-hook)
  (my-use-package company-go
    :ensure t
    :defer t
    :init
    (add-to-list 'company-backends 'company-go))
  (my-use-package go-eldoc
    :ensure t
    :commands go-eldoc-setup
    :init
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

(my-use-package groovy-mode
  :ensure t
  :commands groovy-mode)

(my-use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :config
  (my-use-package haskell-interactive-mode
    :config
    (add-hook 'haskell-mode-hook
      (lambda ()
        (interactive-haskell-mode)
        (diminish 'interactive-haskell-mode))))
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (my-use-package haskell-process)
  (my-use-package haskell-indentation
    :config
    (gsetq haskell-indentation-layout-offset 3
           haskell-indentation-left-offset 3
           haskell-indentation-starter-offset 3
           haskell-indentation-where-pre-offset 1
           haskell-indentation-where-post-offset 2))
  (my-use-package flycheck-haskell
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

(my-use-package jinja2-mode
  :ensure t
  :commands jinja2-mode)

(my-use-package js2-mode
  :ensure t
  :commands js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (gsetq js2-mode-show-strict-warnings nil))

; Removed from MELPA on 2018-01-28, see e.g.
; https://lists.llvm.org/pipermail/llvm-dev/2018-May/123171.html
;(my-use-package llvm-mode
;  :ensure t
;  :commands llvm-mode)

(my-use-package lua-mode
  :ensure t
  :commands lua-mode)

(my-use-package markdown-mode
  :ensure t
  :commands markdown-mode)

(my-use-package php-mode
  :ensure t
  :commands php-mode
  :config
  ; Don't override tab width to 4, please.
  (gsetq php-mode-coding-style 'default)

  (add-hook 'php-mode-hook
            (lambda () (modify-syntax-entry ?$ "." php-mode-syntax-table)))
  (add-hook 'php-mode-hook 'my-gtags-mode))

(my-use-package elpy
  :ensure t
  :diminish elpy-mode
  :commands elpy-enable
  :init
  ; Elpy needs to be enabled before activating python-mode so python-mode-hook
  ; would be too late.
  (with-eval-after-load 'python (elpy-enable))
  :config
  (dolist (mod '(elpy-module-flymake
                 elpy-module-highlight-indentation
                 elpy-module-yasnippet))
    (gsetq elpy-modules (delq mod elpy-modules)))
  (define-key evil-normal-state-local-map (kbd "C-]") 'elpy-goto-definition))

(my-use-package pyvenv
  :ensure t
  :commands pyvenv-activate pyvenv-workon)

(my-use-package puppet-mode
  :ensure t
  :commands puppet-mode)

(my-use-package rust-mode
  :ensure t
  :commands rust-mode
  :init
  (my-use-package racer
    :ensure t
    :diminish racer-mode
    :commands racer-mode
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'company-mode))
  (my-use-package cargo
    :ensure t
    :diminish cargo-minor-mode
    :commands cargo-minor-mode
    :init
    (add-hook 'rust-mode-hook #'cargo-minor-mode))
  (my-use-package flycheck-rust
    :ensure t
    :commands flycheck-rust-setup
    :init
    (add-hook 'rust-mode-hook #'flycheck-rust-setup))
  (gsetq rust-format-on-save t))

(my-use-package ensime
  :ensure t
  :pin melpa-stable
  :after scala-mode
  :bind (:map scala-mode-map
              ("C-c m e" . ensime)
         :map ensime-mode-map
              ("C-c m E" . ensime-reload))
  :init
  (add-hook 'ensime-mode-hook #'(lambda () (yas-minor-mode -1)))
  :config
  (gsetq ensime-startup-notification nil)
  ; Disabled because it doesn't actually work properly.
  ; (gsetq ensime-search-interface 'ivy)
  (evil-define-key* 'normal ensime-mode-map (kbd "C-]") #'ensime-edit-definition)
  (set-face-underline 'ensime-implicit-highlight nil))

(my-use-package toml-mode
  :ensure t
  :commands toml-mode)

(my-use-package tup-mode
  :ensure t
  :commands tup-mode)

(my-use-package tide
  :ensure t
  :commands tide-setup
  :diminish tide-mode
  :init
  (my-use-package prettier-js
    :ensure t
    :diminish prettier-js-mode
    :commands prettier-js-mode)

  (defun my-tide-jump-to-definition (&optional arg)
    "Like tide-jump-to-definition, but set jump point for Evil."
    (interactive "P")
    (evil-set-jump)
    (tide-jump-to-definition arg))

  (defun my-start-tide-mode ()
    (interactive)
    (tide-setup)
    (tide-hl-identifier-mode)
    (prettier-js-mode)
    (setq-local typescript-indent-level (or (plist-get (tide-tsfmt-options) ':indentSize)
                                            typescript-indent-level))
    (evil-local-set-key 'normal (kbd "C-]") #'my-tide-jump-to-definition))
  (add-hook 'typescript-mode-hook #'my-start-tide-mode))

(my-use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(my-use-package web-mode
  :ensure t
  :commands web-mode
  :init
  (my-use-package emmet-mode
    :ensure t
    :commands emmet-mode
    :diminish emmet-mode
    :init
    (general-add-hook '(sgml-mode-hook css-mode-hook web-mode-hook) #'emmet-mode))

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))

  (add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . web-mode))
  (defun my-start-tide-mode-for-jsx-tsx ()
    (interactive)
    (let ((ext (file-name-extension buffer-file-name)))
      (if (or (string-equal "tsx" ext) (string-equal "jsx" ext))
        (progn
          (my-start-tide-mode)
          (setq-local web-mode-code-indent-offset typescript-indent-level)
          (setq-local web-mode-markup-indent-offset typescript-indent-level)
          (setq-local emmet-expand-jsx-className? t))
        (add-to-list 'flycheck-disabled-checkers 'typescript-tslint))))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook #'my-start-tide-mode-for-jsx-tsx)

  (push 'web-mode-code-indent-offset safe-local-variable-values)
  (push 'web-mode-script-padding safe-local-variable-values)
  :config
  (gsetq web-mode-enable-auto-pairing nil
         web-mode-enable-auto-quoting nil))

; Backup and auto-save into a global directory instead of next to the edited
; file, don't clobber hard links when backing up, delete old backups silently,
; and back up version controlled files as well.
(let ((auto-save-dir (concat user-emacs-directory "auto-saves/")))
  (make-directory auto-save-dir t)
  (gsetq backup-by-copying t
         backup-directory-alist `(("." .
                                   ,(concat user-emacs-directory "backups/")))
         auto-save-file-name-transforms `((".*" ,auto-save-dir t))
         delete-old-versions t
         vc-make-backup-files t))

; The lock file directory is not settable so just disable locking.
(gsetq create-lockfiles nil)

; Write customizations into a separate file that we can manually look at if we
; want to.
(gsetq custom-file (concat user-emacs-directory "customs.el"))

(my-use-package all-the-icons
  :ensure t
  :config
  (defun my-mode-line-vc-git ()
    (let ((branch (mapconcat #'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1 :family ,(all-the-icons-octicon-family))
                   'display '(raise 0))
       (propertize (format " %s" branch)))))

  (defvar my-mode-line-vc
    '(:propertize
      (:eval (when vc-mode
               (cond
                ((string-match "Git[:-]" vc-mode) (my-mode-line-vc-git))
                (t (format "%s" vc-mode)))))))

  (gsetq mode-line-percent-position '("%7q"))
  (setq-default mode-line-format
    (list " "
          mode-line-mule-info
          mode-line-modified
          mode-line-frame-identification
          mode-line-buffer-identification
          " "
          mode-line-position
          evil-mode-line-tag
          my-mode-line-vc
          " "
          mode-line-modes
          mode-line-misc-info)))

(my-use-package color-theme-approximate
  :ensure t
  :config
  (color-theme-approximate-on))
(my-use-package spacemacs-dark-theme
  :ensure spacemacs-theme)

(gsetq blink-cursor-blinks 1
       echo-keystrokes 0.1
       window-min-height 2
       x-stretch-cursor t)

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (gsetq frame-title-format '(buffer-file-name "%f" ("%b"))))

; No GUI bars.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(gsetq inhibit-startup-screen t)

(let ((custom-dir (concat user-emacs-directory "custom")))
  (make-directory custom-dir t)
  (mapc 'load (directory-files custom-dir t ".*\.el$" t)))

(defun my-get-recursive-deps-of-package (pkg)
  (let* ((pkg-arch (assq pkg package-alist))
         (reqs (when pkg-arch (package-desc-reqs (cadr pkg-arch)))))
    (dolist (req reqs)
      (let ((reqs2 (my-get-recursive-deps-of-package (car req))))
        (when reqs2 (setq reqs (append reqs reqs2)))))
    reqs))

(defun my-get-all-package-revdeps ()
  (let ((revdeps (make-hash-table :size 512)))
    (dolist (pkg-info package-alist)
      (let* ((pkg (car pkg-info))
             (deps (my-get-recursive-deps-of-package pkg)))
        (dolist (dep-info deps)
          (let* ((dep (car dep-info))
                 (old-revdeps (gethash dep revdeps))
                 (new-revdeps (cons pkg old-revdeps)))
            (puthash dep (delete-dups new-revdeps) revdeps)))))
    revdeps))

(defun my-package-is-orphan (pkg revdeps)
  (when (eq (gethash pkg my-ensured-packages 'my-missing) 'my-missing)
    (let ((users (gethash pkg revdeps 'my-missing)))
      (or (eq users 'my-missing)
          (cl-reduce (lambda (x y) (and x y))
                     (mapcar (lambda (p) (my-package-is-orphan p revdeps))
                             users)
                     :initial-value t)))))

(defun my-get-unused-packages ()
  (let ((revdeps (my-get-all-package-revdeps))
        (implicits (cl-remove-if-not
                    (lambda (p)
                      (eq (gethash (car p) my-ensured-packages 'my-missing)
                          'my-missing))
                    package-alist))
        (result '()))
    (dolist (imp implicits)
      (when (my-package-is-orphan (car imp) revdeps)
        (push imp result)))
    (delete-dups result)))

; Populate my-ensured-packages at runtime as well.
(cl-macrolet
    ((my-populate-used-packages ()
       (cons 'progn
             (mapcar (lambda (pkg) `(puthash ',pkg nil my-ensured-packages))
                     (hash-table-keys my-ensured-packages)))))
  (my-populate-used-packages))

(let* ((orphans (my-get-unused-packages))
       (orphans-len (length orphans)))
  (when (and orphans (yes-or-no-p (format "Delete %d orphans? %s"
                                          orphans-len (mapcar 'car orphans))))
    (message "Deleting %d orphan packages: %s" orphans-len orphans)
    (let ((deleted-count 0))
      (dolist (orphan orphans)
        (setq deleted-count (1+ deleted-count))
        (message "  [%d/%d] %s" deleted-count orphans-len (car orphan))
        (package-delete (cadr orphan))))))
