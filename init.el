(add-to-list 'load-path "~/.emacs.d/load/")

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa-unstable" . "http://unstable.melpa.org/packages/")))
(package-initialize t)
(unless package-archive-contents (package-refresh-contents))

(let ((package-list
       '(color-theme-approximate
         column-enforce-mode
         company irony company-irony
         diminish
         dtrt-indent
         evil evil-commentary evil-jumper evil-matchit evil-snipe evil-surround
         flycheck flycheck-haskell flycheck-tip
         glsl-mode
         guide-key
         haskell-mode
         helm helm-ag helm-dash helm-projectile
         linum-relative
         projectile
         smooth-scrolling
         sublime-themes)))
  (dolist (p package-list)
    (if (package-installed-p p) (package-activate p) (package-install p))))

(require 'evil)
(require 'evil-snipe)
(require 'evil-surround)
(evil-mode 1)
(setq-default evil-symbol-word-search t)
(setq evil-vsplit-window-right 1)
(evil-commentary-mode)
(evil-jumper-mode)
(global-evil-matchit-mode 1)
(setq evil-snipe-override-evil t)
(global-evil-snipe-mode 1)
(global-evil-surround-mode 1)
(setq evil-snipe-scope 'visible)
(setq evil-snipe-repeat-scope 'whole-buffer)

; Don't consider evil-snipe's ; and , for repeating with . (dot).
(evil-declare-ignore-repeat 'evil-snipe-repeat)
(evil-declare-ignore-repeat 'evil-snipe-repeat-reverse)

(defun toggle-column-enforcement ()
  (interactive)
  (if (and (boundp 'column-enforce-mode) column-enforce-mode)
      (column-enforce-mode -1)
    (column-enforce-mode t)))
(defun yank-to-eol () (interactive) (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map "Y" 'yank-to-eol)
(define-key evil-normal-state-map (kbd "RET") 'toggle-column-enforcement)
(define-key evil-insert-state-map (kbd "RET") 'comment-indent-new-line)

(column-number-mode)

(require 'linum-relative)
(setq linum-relative-current-symbol "")
(global-linum-mode 1)

; Some themes see fit to override linum-format, breaking linum-relative, so make
; sure they fail.
(customize-set-variable 'linum-format linum-format)

(show-paren-mode 1)

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

(require 'dtrt-indent)
(dtrt-indent-mode 1)

(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(require 'diminish)
(diminish 'evil-commentary-mode)
(diminish 'evil-snipe-mode)
(diminish 'guide-key-mode)
(diminish 'undo-tree-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(progn
     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (require 'flycheck-tip)
     (flycheck-tip-use-timer 'verbose)))

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq standard-indent 3)

(setq-default fill-column 80)

(c-add-style "deewiant" '("k&r" (c-basic-offset . 3)))
(setq-default c-default-style "deewiant")

(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(eval-after-load 'company
  '(progn
     (diminish 'company-mode)
     (define-key company-active-map (kbd "RET") nil)
     (define-key company-active-map (kbd "<return>") nil)
     (define-key company-active-map (kbd "S-RET")
       'company-complete-selection)
     (define-key company-active-map (kbd "S-<return>")
       'company-complete-selection)))

(defun my-irony-mode-enable ()
  (when (memq major-mode irony-supported-major-modes)
    (irony-mode 1)))
(add-hook 'c-mode-hook 'my-irony-mode-enable)
(add-hook 'c++-mode-hook 'my-irony-mode-enable)
(add-hook 'objc-mode-hook 'my-irony-mode-enable)
(defun my-irony-mode-hook ()
   (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
   (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-indentation-ifte-offset 3)
(setq haskell-indentation-layout-offset 3)
(setq haskell-indentation-left-offset 3)
(setq haskell-indentation-starter-offset 3)
(setq haskell-indentation-where-pre-offset 1)
(setq haskell-indentation-where-post-offset 2)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

; Show the Haskell indentation guides only in the insert and emacs states.
(defun my-haskell-indentation-show-guides ()
  (when (eq major-mode 'haskell-mode)
    (haskell-indentation-enable-show-indentations)))
(defun my-haskell-indentation-hide-guides ()
  (when (eq major-mode 'haskell-mode)
    (haskell-indentation-disable-show-indentations)))
(add-hook 'evil-normal-state-entry-hook
          'my-haskell-indentation-hide-guides)
(dolist (state '(insert emacs))
  (eval `(progn
           (add-hook ',(intern (format "evil-%S-state-entry-hook" state))
                     'my-haskell-indentation-show-guides)
           (add-hook ',(intern (format "evil-%S-state-exit-hook" state))
                     'my-haskell-indentation-hide-guides))))

; Backup and auto-save into a global directory instead of next to the edited
; file, don't clobber hard links when backing up, delete old backups silently,
; and back up version controlled files as well.
(let ((auto-save-dir (concat user-emacs-directory "auto-saves/")))
  (make-directory auto-save-dir t)
  (setq backup-by-copying t
        backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
        auto-save-file-name-transforms '((".*" auto-save-dir t))
        delete-old-versions t
        vc-make-backup-files t))

(color-theme-approximate-on)
(load-theme 'spolsky t)
(setq evil-default-cursor '("grey" t))

(setq echo-keystrokes 0.1)
(setq x-stretch-cursor t)

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

; No scrollbars, no toolbar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-startup-screen t)

(let ((custom-dir (concat user-emacs-directory "custom")))
  (make-directory custom-dir t)
  (mapc 'load (directory-files custom-dir t ".*\.el$" t)))
