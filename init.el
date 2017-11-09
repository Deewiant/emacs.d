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

(my-use-package evil
  :ensure t
  :diminish undo-tree-mode
  :config
  ; Wants to be enabled before evil-mode itself.
  (my-use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  (evil-mode 1)

  (my-use-package evil-commentary
    :ensure t
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode))

  (my-use-package evil-snipe
    :ensure t
    :diminish evil-snipe-local-mode
    :config
    (evil-snipe-mode 1)
    ; Don't consider evil-snipe's ; and , for repeating with . (dot).
    (evil-declare-ignore-repeat 'evil-snipe-repeat)
    (evil-declare-ignore-repeat 'evil-snipe-repeat-reverse)
    (setq evil-snipe-scope 'visible)
    (setq evil-snipe-repeat-scope 'whole-buffer)
    ; Don't repeat search on another press of s/S.
    (setq evil-snipe-repeat-keys nil))

  (my-use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  ; When closing split windows, return to the window we split from instead of
  ; some seemingly unpredictable choice.
  (defvar my-window-parents)
  (setq my-window-parents nil)
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

  (setq evil-auto-balance-windows nil)
  (setq-default evil-symbol-word-search t)
  (setq evil-vsplit-window-right 1)

  (defun yank-to-eol ()
    (interactive)
    (evil-yank-characters (point) (point-at-eol) evil-this-register))

  ; I don't use | normally and would rather have \ as a leader than dedicate it
  ; to this.
  (define-key evil-normal-state-map "|" 'evil-execute-in-emacs-state)

  ; Enable register pasting into the minibuffer.
  (define-key minibuffer-local-map "\C-r" #'evil-paste-from-register)

  (define-key evil-normal-state-map "~" 'evil-invert-case)
  (define-key evil-normal-state-map "Y" 'yank-to-eol)
  (define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line))

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
  (setq eyebrowse-wrap-around t)
  (setq eyebrowse-new-workspace t)
  (define-key evil-motion-state-map (kbd "gt") 'eyebrowse-next-window-config)
  (define-key evil-motion-state-map (kbd "gT") 'eyebrowse-prev-window-config))

(column-number-mode)

(my-use-package linum-relative
  :ensure t
  :diminish linum-relative-mode
  :config
  ; Update line numbers only after a delay. Rewrite linum-schedule to use a
  ; proper timer instead of a 0-delay one and linum-after-scroll to not do an
  ; immediate update.
  (setq linum-delay t)
  (defvar-local my-linum-current-timer nil)
  (defun linum-schedule ()
    (when (timerp my-linum-current-timer)
      (cancel-timer my-linum-current-timer))
    (setq my-linum-current-timer
          (run-with-idle-timer 0.2 nil #'linum-update-current)))
  (defun linum-after-scroll (_win _start) ())

  (setq linum-relative-current-symbol "")
  (global-linum-mode 1)
  (linum-relative-mode 1)

  ; Some themes see fit to override linum-format, breaking linum-relative, so
  ; make sure they fail.
  (customize-set-variable 'linum-format linum-format))

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
  (setq hes-mode-alist (cons `(php-mode . ,my-hes-php-re) hes-mode-alist))
  (hes-mode))

; More code folding support, Evil knows to use it if available.
(my-use-package hideshow
  :diminish hs-minor-mode
  :config
  (add-hook 'python-mode-hook #'hs-minor-mode)
  (add-hook 'c-mode-hook #'hs-minor-mode)
  (add-hook 'c++-mode-hook #'hs-minor-mode)
  (add-hook 'java-mode-hook #'hs-minor-mode))

(my-use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-mode 1))

(my-use-package editorconfig
  :diminish editorconfig-mode
  :ensure t
  :config
  (editorconfig-mode 1))

(my-use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (defun ws-butler-clean-region (beg end)
    "Delete trailing blanks in region BEG END. Don't touch indentation."
    (interactive "*r")
    (ws-butler-with-save
     (narrow-to-region beg end)
     ;;  _much slower would be:       (replace-regexp "[ \t]+$" "")
     (goto-char (point-min))
     (while (not (eobp))
       (end-of-line)
       (delete-horizontal-space)
       (forward-line 1)))
    ;; clean return code for hooks
    nil)
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
    (setq whitespace-style (delq 'lines whitespace-style)))))

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
    :bind ("M-x" . counsel-M-x)
    :bind ("C-c f" . counsel-recentf)
    :bind ("C-h f" . counsel-describe-function)
    :bind ("C-h v" . counsel-describe-variable)
    :bind (:map minibuffer-local-map
           ("C-s" . counsel-expression-history)))
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
  (setq helm-split-window-in-side-p t)
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
    (setq projectile-completion-system 'ivy)
    (define-key projectile-command-map (kbd "s s") #'counsel-projectile-rg))
  (projectile-global-mode))

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
  (my-use-package flycheck-pos-tip
    :ensure t
    :config
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
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

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq standard-indent 3)

(setq backward-delete-char-untabify-method nil)

(setq-default fill-column 80)
(add-hook 'python-mode-hook (lambda () (setq-local fill-column 79)))

(c-add-style "deewiant" '("k&r" (c-basic-offset . 3)))
(setq-default c-default-style "deewiant")

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
    (setq company-dabbrev-char-regexp "\\(?:\\sw\\|\\s_\\)")
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-case t)

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
      (customize-set-variable 'company-backends bes)))

  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
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
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-selector 'ivy)
  (define-key evil-normal-state-map (kbd "C-]") 'dumb-jump-go)
  (define-key evil-normal-state-map (kbd "C-w C-]") 'dumb-jump-go-other-window)
  ; C-} = C-S-]
  (define-key evil-normal-state-map (kbd "C-}") 'dumb-jump-go-prefer-external)
  (define-key evil-normal-state-map (kbd "C-w C-}") 'dumb-jump-go-prefer-external-other-window))

(my-use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(my-use-package generic-x
  :config
  (customize-set-variable
   'generic-extras-enable-list
   (append generic-default-modes generic-mswindows-modes generic-unix-modes
           generic-other-modes)))

(my-use-package magit
  :ensure t
  :diminish auto-revert-mode
  :bind ("C-x g" . magit-status)
  :bind ("C-x M-g" . magit-dispatch-popup)
  :bind ("C-x l" . magit-blame-popup)
  :config
  (setq git-commit-summary-max-length 50)
  (setq vc-handled-backends (delq 'Git vc-handled-backends))

  (magit-change-popup-key 'magit-blame-popup :action ?b ?l)

  (defun my-override-commit-fill-column ()
    (setq-local fill-column 72))
  (add-hook 'git-commit-mode-hook #'my-override-commit-fill-column)

  (my-use-package evil-magit
    :ensure t))

(my-use-package asm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.fasm?\\'" . asm-mode)))

(my-use-package clang-format
  :ensure t
  :commands clang-format clang-format-buffer clang-format-region
  :init
  (defun my-clang-format-hook ()
    (clang-format-buffer))
  (defun my-add-clang-format-hook-hook ()
    (when (memq major-mode '(c-mode c++-mode))
      (add-hook 'before-save-hook #'my-clang-format-hook nil t)))
  (add-hook 'c-mode-hook #'my-add-clang-format-hook-hook)
  (add-hook 'c++-mode-hook #'my-add-clang-format-hook-hook))

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
  (add-hook 'c-mode-hook 'my-irony-mode-enable)
  (add-hook 'c++-mode-hook 'my-irony-mode-enable)
  (add-hook 'objc-mode-hook 'my-irony-mode-enable)
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

(my-use-package clojure-mode
  :ensure t
  :commands clojure-mode)

(my-use-package cython-mode
  :ensure t
  :commands cython-mode
  :config
  (my-use-package flycheck-cython
    :ensure t))

(my-use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode)

(my-use-package feature-mode
  :ensure t
  :commands feature-mode)

(my-use-package glsl-mode
  :ensure t
  :commands glsl-mode)

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
    (setq haskell-indentation-ifte-offset 3)
    (setq haskell-indentation-layout-offset 3)
    (setq haskell-indentation-left-offset 3)
    (setq haskell-indentation-starter-offset 3)
    (setq haskell-indentation-where-pre-offset 1)
    (setq haskell-indentation-where-post-offset 2))
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
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(my-use-package llvm-mode
  :ensure t
  :commands llvm-mode)

(my-use-package markdown-mode
  :ensure t
  :commands markdown-mode)

(my-use-package php-mode
  :ensure t
  :commands php-mode
  :config
  ; Don't override tab width to 4, please.
  (setq php-mode-coding-style 'default)

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
    (setq elpy-modules (delq mod elpy-modules)))
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
  (setq rust-format-on-save t))

(my-use-package ensime
  :ensure t
  :pin melpa-stable
  :after scala-mode
  :bind (:map scala-mode-map
              ("C-c m e" . ensime)
         :map ensime-mode-map
              ("C-c m E" . ensime-reload))
  :init
  (add-hook 'ensime-mode-hook #'(lambda () (setq current-prefix-arg '(-1)) (call-interactively #'yas-minor-mode)))
  :config
  (setq ensime-startup-notification nil)
  (setq ensime-search-interface 'ivy)
  (evil-define-key* 'normal ensime-mode-map (kbd "C-]") #'ensime-edit-definition))

(my-use-package toml-mode
  :ensure t
  :commands toml-mode)

(my-use-package tup-mode
  :ensure t
  :commands tup-mode)

(my-use-package typescript-mode
  :ensure t
  :commands typescript-mode)

(my-use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(my-use-package web-mode
  :ensure t
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
  (push 'web-mode-code-indent-offset safe-local-variable-values)
  (push 'web-mode-script-padding safe-local-variable-values)
  :config
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-quoting nil))

; Backup and auto-save into a global directory instead of next to the edited
; file, don't clobber hard links when backing up, delete old backups silently,
; and back up version controlled files as well.
(let ((auto-save-dir (concat user-emacs-directory "auto-saves/")))
  (make-directory auto-save-dir t)
  (setq backup-by-copying t
        backup-directory-alist `(("." .
                                  ,(concat user-emacs-directory "backups/")))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        delete-old-versions t
        vc-make-backup-files t))

; The lock file directory is not settable so just disable locking.
(setq create-lockfiles nil)

; Write customizations into a separate file that we can manually look at if we
; want to.
(setq custom-file (concat user-emacs-directory "customs.el"))

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

(setq blink-cursor-blinks 1)
(setq echo-keystrokes 0.1)
(setq window-min-height 2)
(setq x-stretch-cursor t)

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

; No GUI bars.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-startup-screen t)

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
