(add-to-list 'load-path "~/.emacs.d/load/")

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa-unstable" . "http://unstable.melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(setq package-list
      '(color-theme-approximate
        column-enforce-mode
        company irony company-irony
        diminish
        dtrt-indent
        evil evil-commentary evil-jumper evil-matchit evil-snipe evil-surround
        glsl-mode
        guide-key
        haskell-mode
        helm helm-projectile
        linum-relative
        projectile
        smooth-scrolling
        sublime-themes))
(dolist (p package-list) (unless (package-installed-p p) (package-install p)))

(require 'evil)
(require 'evil-snipe)
(require 'evil-surround)
(evil-mode 1)
(setq evil-vsplit-window-right 1)
(evil-commentary-mode)
(evil-jumper-mode)
(global-evil-matchit-mode 1)
(setq evil-snipe-override-evil t)
(global-evil-snipe-mode 1)
(global-evil-surround-mode 1)

(defun toggle-column-enforcement ()
  (interactive)
  (if column-enforce-mode
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
(diminish 'evil-snipe-mode)
(diminish 'undo-tree-mode)

(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq standard-indent 3)

(c-add-style "deewiant" '("k&r" (c-basic-offset . 3)))
(setq-default c-default-style "deewiant")

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(defun my-irony-mode-hook ()
   (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
   (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-indentation-layout-offset 3)
(setq haskell-indentation-left-offset 3)
(setq haskell-indentation-starter-offset 3)
(setq haskell-indentation-where-pre-offset 1)

; Backup into a global directory instead of next to the edited file,
; and don't clobber symlinks.
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

(color-theme-approximate-on)
(load-theme 'spolsky t)
(setq evil-default-cursor '("grey" t))

; No scrollbars, no toolbar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-startup-screen t)

(if (file-exists-p "~/.emacs.d/custom")
    (mapc 'load (directory-files "~/.emacs.d/custom" t ".*\.el$" t)))
