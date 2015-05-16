(modify-all-frames-parameters '((fullscreen . maximized)))
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(eval-when-compile (package-initialize))
(eval-when-compile (require 'cl))

(setq user-emacs-directory (file-name-directory (or load-file-name (buffer-file-name))))
(setq org-confirm-babel-evaluate nil)
(org-babel-load-file (expand-file-name "emacs-init.org" user-emacs-directory))
