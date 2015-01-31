(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa-unstable" . "http://unstable.melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(setq package-list
      '(color-theme-sanityinc-tomorrow
        evil evil-commentary evil-jumper evil-matchit evil-snipe evil-surround
	evil-tabs))
(dolist (p package-list) (unless (package-installed-p p) (package-install p)))

(require 'evil)
(require 'evil-surround)
(evil-mode 1)
(evil-commentary-mode)
(evil-jumper-mode)
(global-evil-matchit-mode 1)
(evil-snipe-replace-evil)
(global-evil-surround-mode 1)
(global-evil-tabs-mode t)

(column-number-mode)

; Backup into a global directory instead of next to the edited file,
; and don't clobber symlinks.
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

(load-theme 'sanityinc-tomorrow-night t)

; No scrollbars, no toolbar.
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
