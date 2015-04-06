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
        company irony company-irony
        diminish
        evil evil-commentary evil-jumper evil-matchit evil-snipe evil-surround
        evil-tabs
        glsl-mode
        haskell-mode
        sublime-themes))
(dolist (p package-list) (unless (package-installed-p p) (package-install p)))

; This has to be done early, or newly opened buffers end up in some kind of
; incorrect state where evil-mode is enabled but doesn't work, so it needs to
; be toggled twice before things work.
(global-evil-tabs-mode t)

(require 'evil)
(require 'evil-snipe)
(require 'evil-surround)
(evil-mode 1)
(evil-commentary-mode)
(evil-jumper-mode)
(global-evil-matchit-mode 1)
(setq evil-snipe-override-evil t)
(global-evil-snipe-mode 1)
(global-evil-surround-mode 1)

(column-number-mode)

(require 'diminish)
(diminish 'evil-snipe-mode)
(diminish 'undo-tree-mode)

(setq-default indent-tabs-mode nil)

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
