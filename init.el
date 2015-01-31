(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa-unstable" . "http://unstable.melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(setq package-list
      '(color-theme-sanityinc-tomorrow
        company
        diminish
        evil evil-commentary evil-jumper evil-matchit evil-snipe evil-surround
	evil-tabs))
(dolist (p package-list) (unless (package-installed-p p) (package-install p)))

; This has to be done early, or newly opened buffers end up in some kind of
; incorrect state where evil-mode is enabled but doesn't work, so it needs to
; be toggled twice before things work.
(global-evil-tabs-mode t)

(require 'evil)
(require 'evil-surround)
(evil-mode 1)
(evil-commentary-mode)
(evil-jumper-mode)
(global-evil-matchit-mode 1)
(evil-snipe-replace-evil)
(global-evil-surround-mode 1)

(column-number-mode)

(require 'diminish)
(diminish 'undo-tree-mode)

(setq-default indent-tabs-mode nil)

(c-add-style "deewiant" '("k&r" (c-basic-offset . 3)))
(setq-default c-default-style "deewiant")

(add-hook 'after-init-hook 'global-company-mode)

; Backup into a global directory instead of next to the edited file,
; and don't clobber symlinks.
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

(load-theme 'sanityinc-tomorrow-night t)
(setq evil-default-cursor '("grey" t))

; No scrollbars, no toolbar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-startup-screen t)
