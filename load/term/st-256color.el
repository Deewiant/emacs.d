(defun terminal-init-st ()
   (load "term/xterm")
   (xterm-register-default-colors)
   (tty-set-up-initial-frame-faces))
