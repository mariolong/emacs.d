(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(modify-all-frames-parameters '((fullscreen . maximized)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(setq user-emacs-directory (file-name-directory (or load-file-name (buffer-file-name))))
(setq org-confirm-babel-evaluate nil)
(org-babel-load-file (expand-file-name "emacs-init.org" user-emacs-directory))
