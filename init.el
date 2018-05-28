;; -*- lexical-binding: t -*-

(setq load-prefer-newer t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(modify-all-frames-parameters '((fullscreen . maximized)))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq ad-redefinition-action 'accept)

(setq initial-scratch-message ";;今天又是美好的一天\n")

(defun set-font-for-chinese ()
  (set-face-attribute 'default nil
                      :family "Source Code Pro" :height 180)
  (set-face-attribute 'variable-pitch nil
                      :family "Sans" :height 200 :weight 'regular)

  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Source Han Sans" :size 28)))

  (set-fontset-font (frame-parameter nil 'font) 'symbol
                    (font-spec :family "Symbola" :size 32)))

(add-to-list 'after-make-frame-functions
             (lambda (new-frame)
               (select-frame new-frame)
               (if window-system
                   (set-font-for-chinese))))

(when window-system
  (set-font-for-chinese))

(defconst user-emacs-directory (file-name-directory (or load-file-name (buffer-file-name))))
(defconst user-emacs-init-org (expand-file-name "emacs-init.org" user-emacs-directory))
(defconst emacs-init-el (expand-file-name "init.el" user-emacs-directory))
(defconst user-secret-file (expand-file-name "personal.el" user-emacs-directory))
;; (defconst org-gcal-file-schedule (expand-file-name "gschedule.org" user-emacs-directory))

(defconst user-cache-directory
  (file-name-as-directory (expand-file-name ".cache" user-emacs-directory))
  "My emacs storage area for persistent files.")

(set-default 'temporary-file-directory (expand-file-name "tmp" user-emacs-directory))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory))

(let* ((lisp-dir '("lisp/" "themes/")))
  (dolist (lisp-path lisp-dir)
    (when (not (file-exists-p lisp-path))
      (make-directory (concat user-emacs-directory lisp-path) t))
    (let* ((load-dir (concat user-emacs-directory lisp-path))
           (default-directory load-dir))
      (setq load-path
            (append
             (let ((load-path (copy-sequence load-path)))
               (append
                (copy-sequence (normal-top-level-add-to-load-path '(".")))
                (normal-top-level-add-subdirs-to-load-path)))
             load-path)))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(use-package quelpa-use-package
  :init
  (require 'quelpa-use-package))

(use-package paradox
  :config
    (setq paradox-execute-asynchronously t))

(use-package color-theme-sanityinc-tomorrow
    :init
      (load-theme 'sanityinc-tomorrow-night :no-confirm))

(use-package spaceline-config
  :ensure spaceline
  :commands spaceline-spacemacs-theme
  :init
  (setq-default powerline-default-separator 'wave
                spaceline-minor-modes-separator ""
                spaceline-workspace-numbers-unicode t
                spaceline-window-numbers-unicode t)
  (spaceline-spacemacs-theme)
  )

(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "white"
           (if overwrite-mode "#5599aa"
             "#f99157"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

(fringe-mode 4)

(use-package beacon
  :diminish ""
  :init
  (beacon-mode 1))

(use-package linum
  :init
  (add-hook 'prog-mode-hook
            '(lambda () (linum-mode 1))))

(setq column-number-mode t)

(use-package linum-relative
  :commands linum-relative-mode
  :init
    (linum-relative-mode t)
    (setq linum-relative-current-symbol "")
    (setq linum-relative-format "%4s")
    )

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."

  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\(ME\\)?\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)
     ("\\<\\(NOTE\\):" 1 'org-level-2 t)
     ("\\<\\(TODO\\):" 1 'org-todo t)
     ("\\<\\(DONE\\):" 1 'org-done t))
   ))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(mouse-avoidance-mode 'banish)

(global-font-lock-mode t)

(unbind-key "C-\\")

(use-package pangu-spacing
  :diminish ""
  :init
  (global-pangu-spacing-mode 1)

  :config
  ;; disable pangu-space in some modes
  (dolist (mm '(eww-mode elfeed-search-mode elfeed-show-mode))
    (add-to-list 'pangu-spacing-inhibit-mode-alist mm))

  ;; Always insert `real' space
  (dolist (mm '(markdown-mode-hook rst-mode-hook org-mode-hook))
    (add-hook mm '(lambda()
                    (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))))

(use-package fcitx
  :init (fcitx-aggressive-setup))

(prefer-coding-system 'utf-8)
(setq system-time-locale "en_US" )

(use-package noflet
  :defer 0)

(require 'noflet)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (noflet ((process-list ())) ad-do-it))

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq sh-basic-offset 4)

(use-package ethan-wspace
  :diminish "🄣"
  :init
  (global-ethan-wspace-mode 1)
  :config
  (progn
    ;; Turn off `mode-require-final-newline' since ethan-wspace
    ;; supersedes `require-final-newline'.
    (setq mode-require-final-newline nil)

    ;; Prevent etha-wspace touch my TAB on makefile mode
    (add-hook 'makefile-mode-hook
              '(lambda()
                 (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))))

    ;; Ignore no trailing newline error
    (setq-default ethan-wspace-errors (remove 'no-nl-eof ethan-wspace-errors))))

(setq-default truncate-lines t)

(setq visible-bell t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.1)

(delete-selection-mode 1)

(global-subword-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(setq select-enable-clipboard t
      select-enable-primary t)

(setq save-interprogram-paste-before-kill t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default text-scale-mode-step 1.1)
(bind-key "C-+" 'text-scale-increase)

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun current-date-time-nikola ()
  (format-time-string "%Y-%m-%d %H:%M:%S UTC+08:00"))

(defun insert-current-date-time ()
  "insert current date time at point"
  (interactive)
  (insert (current-date-time-nikola)))

(defun opencc-buffer ()
  "Convert chinese from simplified to variants and phrases of Taiwan"
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix)

  (let ((opencc-conv-temp-file (expand-file-name "opencc.tmp" temporary-file-directory)))
    (let ((str-for-opencc (buffer-substring-no-properties (point-max) 1)))
      (with-temp-file opencc-conv-temp-file
        (insert str-for-opencc "\n")))

    (let ((current-point (point))
          (result (shell-command-to-string
                   (concat "opencc -c s2twp.json -i " opencc-conv-temp-file))))

      (erase-buffer)
      (insert result)
      (goto-char current-point))))

(bind-key "C-c fo" 'opencc-buffer)

(use-package moedict
  :quelpa (moedict :fetcher github :repo "kuanyui/moedict.el")
  :ensure helm
  :ensure esqlite
  :bind ("C-c m" . moedict)
  :config
  (setq browse-url-chromium-program "google-chrome-stable"))

(use-package nov
  :bind (:map nov-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("d" . scroll-up-command)
              ("e" . scroll-down-command)
              ("u" . scroll-down-command)
              )
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )

(use-package pdf-tools
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("e" . pdf-view-scroll-down-or-previous-page)
              ("u" . pdf-view-scroll-down-or-previous-page)
              ("d" . pdf-view-scroll-up-or-next-page)
              ("H" . pdf-view-next-page)
              ("L" . pdf-view-previous-page)
              ("=" . pdf-view-fit-width-to-window)
              ("-" . pdf-view-shrink)
              ("+" . pdf-view-enlarge))
  :commands (pdf-view-mode)
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
    (setq pdf-view-continuous t)
    ))

(defun async-wpub (url &optional file-type opencc)
  (let* ((cmd-opencc (if opencc (if (y-or-n-p "Convert to Traditional Chinese?") "-o" "") ""))
         (cmd-type (if file-type (format "-f %s" file-type) ""))
         (cmd-wpub (if url (format "wpub %s %s '%s'" cmd-opencc cmd-type url) nil)))

    (if cmd-wpub
        (progn
          (message "%s" cmd-wpub)
          (start-file-process-shell-command "async-wpub"
                                            "*async-wpub*"
                                            cmd-wpub)))))

(defun org-to-kindle ()
  "Send current buffer to kindle via wpub.py"

  (interactive)

  (if (eq (current-buffer-mode) 'org-mode)
      (progn
        (let ((wpub-temp-file (expand-file-name (buffer-name) temporary-file-directory))
              (str (buffer-substring-no-properties (point-max) 1)))

          (with-temp-file wpub-temp-file
            (insert str "\n"))

          (async-wpub wpub-temp-file "org")))

    (message "Current buffer must be 'org-mode'!")))

(defun html-file-to-kindle (fname)
  (async-wpub fname "html" t))

(defun url-to-kindle (url)
  (if url
      (async-wpub url nil t)
    (message "url: %s" url)))

(defun eww-to-kindle ()
  "send the content of current url to kindle via wpub"

  (interactive)
  (url-to-kindle (eww-current-url))
  )

(defun eww-to-kindle-directly ()
  (interactive)

  (let ((fname (eww-temp-file-name-html (eww-current-url))))
    (eww-make-temp-file-source fname)
    (html-file-to-kindle fname)))

(defun eww-temp-file-name-html (fname)
  (expand-file-name (concat (md5 fname) ".html")
                    temporary-file-directory))

(defun eww-make-temp-file-source (temp-file-name)
  (let ((source (eww-current-source)))
    (with-temp-file temp-file-name
      (insert (string-as-multibyte source)))))

(defun elfeed-search-to-kindle ()
  "send current content to kindle"

  (interactive)
  (url-to-kindle (get-elfeed-search-url)))

(defun elfeed-show-to-kindle ()
  "send current content to kindle"

  (interactive)
  (url-to-kindle (get-elfeed-show-url)))

(bind-key "C-c ok" 'send-url-or-org-to-kindle)
(require 'eww)
(defun send-url-or-org-to-kindle ()
  "Send content of current buffer to kindle"
  (interactive)
  (cond
   ((not (eq (eww-current-url) nil)) (eww-to-kindle-directly))
   ((eq (current-buffer-mode) 'org-mode) (org-to-kindle))
   ((eq (current-buffer-mode) 'elfeed-search-mode) (elfeed-search-to-kindle))
   ((eq (current-buffer-mode) 'elfeed-show-mode) (elfeed-show-to-kindle))
   (t (message "%s" "Can't send this page to kindle."))))

(defun send-url-to-kindle-at-point ()
  "send contents in url-at-point to kindle"

  (interactive)

  (let ((url (get-text-property (point) 'shr-url)))
    (if url
        (url-to-kindle (get-text-property (point) 'shr-url))
      (message "%s" url)
      )))

(defun current-buffer-mode ()
  "Return the major-mode associated with current buffer."

  (with-current-buffer (buffer-name)
    major-mode))

(bind-key "C-c op" 'org-to-pdf)

(defun org-to-pdf ()
  "Conver org to pdf and open the pdf file."

  (interactive)
  (if (eq (current-buffer-mode) 'org-mode)
      (progn
        (let* ((pdf-name (concat (file-name-sans-extension buffer-file-name) ".pdf")))

          (message "convert to %s" (file-name-nondirectory pdf-name))

          (let* ((latex-engin "xelatex")
                 (template "/home/mario/.pandoc/default.latex")
                 (result (shell-command
                          (format "pandoc %s --latex-engine %s --template %s -s -o %s"
                                  buffer-file-name
                                  latex-engin
                                  template
                                  pdf-name
                                  ))))
            (if (eq result 0)
                (find-file pdf-name)
              (message result)))
          ))
    (message "Current buffer must be 'org-mode'!")))

(defun md-to-org ()
  "Convert markdown to org for edit. Especially for evernote/geeknote."

  (interactive)
  (if (eq (current-buffer-mode) 'markdown-mode)
      (let* ((default-directory "/mnt/lvm-data/Documents/")
             (md-buffer-name (buffer-name))
             (md-file-name (buffer-file-name))
             (md-org-buffer (find-file (read-file-name "" default-directory)))
             (md-org-file-name (buffer-file-name)))

        (if (not (file-exists-p md-org-file-name))
            (progn
              (shell-command (format "pandoc -f markdown -t org -o %s %s"
                                     md-org-file-name
                                     md-file-name))
              (revert-buffer nil t)
              (goto-char (point-min))
              (insert "#+TITLE:\n")
              (save-buffer)
              (kill-buffer md-buffer-name))
          (message "%s is already exist." md-org-file-name)))
    (message "Current buffer must be 'markdown-mode'!")))

(bind-key "C-c oo" 'write-buffer-as-orgmode)

(defun write-buffer-as-orgmode ()
(interactive)
  (cond
   ((eq (current-buffer-mode) 'markdown-mode) (md-to-org))
   ((eq (current-buffer-mode) 'eww-mode) (eww-to-org))
   (t (message "Current mode must be markdown or eww"))))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("s" . my/counsel-dired-sort)
              ("f" . dired-find-name-in-current-directory)
              ("\)" . dired-omit-and-remember))
  :config
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer t)
  (setq dired-ls-F-marks-symlinks t)
  ;; dired-listing-switches "-alh"
  (setq dired-listing-switches "--group-directories-first -alh")
  ;; dired-listing-switches "-lFaGh1v --group-directories-first"
  (setq dired-recursive-copies  'always
        dired-recursive-deletes 'top)
  (setq dired-omit-files "^\\...+$")
  
  (defvar v-dired-omit t
    "If dired-omit-mode enabled by default. Don't setq me.")
  
  (defun dired-omit-and-remember ()
    "This function is a small enhancement for `dired-omit-mode', which will
          \"remember\" omit state across Dired buffers."
  
    (interactive)
    (setq v-dired-omit (not v-dired-omit))
    (dired-omit-auto-apply)
    (revert-buffer))
  
  (defun dired-omit-auto-apply ()
    (setq dired-omit-mode v-dired-omit))
  
  (add-hook 'dired-mode-hook 'dired-omit-auto-apply)
  (defun dired-find-name-in-current-directory ()
    (interactive)
    (find-name-dired default-directory
                     (format "*%s*" (read-from-minibuffer "Pattern: ")))
    (set-buffer-multibyte t))
  (setq find-name-arg "-iname")
  (setq find-ls-option '("-print0 | xargs -0 ls -ald" . "")))

(defconst sorting-action '(("name" "")
                           ("size" "S")
                           ("extension" "X")
                           ("access time" "ut")
                           ("modified time" "t")
                           ("status change time" "ct")))

(defun sort-dir (sort-item)
  (dired-sort-other
   (concat dired-listing-switches
           (car (cdr (assoc sort-item sorting-action))))))

(defun my/counsel-dired-sort ()
  (interactive)

  (let ((sort-cmd (mapcar 'car sorting-action))
        (sort-dir (lambda (sort-item)
                    (dired-sort-other
                     (concat dired-listing-switches
                             (car (cdr (assoc sort-item sorting-action))))))))

    (ivy-read "sort by" sort-cmd
              :preselect "name"
              :initial-input "^"
              :action #'sort-dir)))

(use-package dired+
  :quelpa (:fetcher github :repo "emacsmirror/dired-plus")
  :init
  (diredp-toggle-find-file-reuse-dir 1)
  (setq dired-recursive-deletes 'always)

  (dolist (file `(("unrar x" "rar")
                  ("aegisub-3.2" "ass" "srt")
                  ("mpv"  "ogm" "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv" "mp4" "m4v" "webm")
                  ("mpv -playlist" "list" "pls")
                  ("feh -F --auto-rotate *" "gif" "jpeg" "jpg" "tif" "png")
                  ("google-chrome-stable" "xml" "xhtml" "html" "htm" "mht")
                  ))
    (add-to-list 'dired-guess-shell-alist-default
                 (list (concat "\\." (regexp-opt (cdr file) t) "$")
                       (car file)))))

(use-package openwith
  :init
  (openwith-mode t)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp '("flac" "mp3" "wav" "aiff" "m4a" "aac"))
               "mpv" '(file))
         (list (openwith-make-extension-regexp '("avi" "flv" "mov" "mp4" "rmvb" "m2ts" "webm"
                                                 "mpeg" "mpg" "ogg" "wmv" "mkv"))
               "mpv" '(file)))
        ))

(use-package async
  :init
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  (setq async-bytecomp-allowed-packages '(all)))

(use-package peep-dired
  :bind (:map dired-mode-map
              ("P" . peep-dired))
  :config
  (setq peep-dired-enable-on-directories t)
  (add-to-list 'peep-dired-ignored-extensions "m2ts"))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(bind-key "C-c fR" 'edit-current-file-as-root)
(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))

(bind-key "C-c fD" 'delete-current-buffer-file)
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(bind-key "C-c fr"  'rename-current-buffer-file)
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(bind-key "C-c fc"  'clone-file-and-open)
(defun clone-file-and-open (filename)
  "Clone the current buffer writing it into FILENAME and open it"
  (interactive "FClone to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))

(bind-key "C-c fx" 'set-file-executable)
(defun set-file-executable()
  "Add executable permissions on current file."

  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

(defun counsel-goto-recent-directory ()
  "Open recent directory with dired"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection :action 'dired)))

(bind-key "C-x C-\\" 'counsel-goto-recent-directory)

(defun counsel-goto-recent-directory-other-window ()
  "goto recnet directory other window."
  (interactive)
  (let ((old-buffer-name (buffer-name))
        (new-directory-name (counsel-goto-recent-directory)))
    (message "open recent dir old:%s new:%s" old-buffer-name new-directory-name)
    (if (eq (count-windows) 1)
        (split-window-right))

    (switch-to-buffer old-buffer-name)
    (other-window 1)
    (find-file new-directory-name)))

(bind-key "C-c ff" 'find-file-usually-files)
(defun find-file-usually-files ()
  "open usually using files."
  (interactive)
  (require 'elfeed-org)
  (let ((usually-files (list user-emacs-init-org
                             org-default-inbox-file
                             org-default-notes-file
                             "*Messages*"
                             "*scratch*")))

    (ivy-read "find file:" usually-files
              :action #'find-file-or-buffer)))

(defun find-file-or-buffer (file-or-buffer-name)
  (cond
   ((string= file-or-buffer-name "*scratch*")
    (switch-to-scratch-buffer))
   ((get-buffer file-or-buffer-name)
    (switch-to-buffer file-or-buffer-name))
   ((file-exists-p file-or-buffer-name)
    (find-file file-or-buffer-name))
   (t
    (message "Not found %s" file-or-buffer-name))))

(use-package saveplace
  :init
  (progn
    (setq save-place-file (expand-file-name "places" user-cache-directory))
    (save-place-mode 1)))

(use-package recentf
  :init
  (progn
    (setq recentf-save-file (expand-file-name "recentf" user-cache-directory))
    (recentf-mode 1)
  :config
    (setq recentf-max-saved-items 500
          recentf-max-menu-items 10)))

(bind-key "C-x k" 'kill-this-buffer)

(unbind-key "C-x C-c")
(bind-key "C-x C-c C-c" 'kill-emacs)

(bind-key "C-x C-c c" 'kill-other-buffers)

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
  (delete-other-windows))

(bind-key "C-x C-c s" 'kill-all-buffer-except-scratch-messages)

(defun kill-all-buffer-but-ibuffer ()
  "kill all buffer except message and scratch in ibuffer."
  (interactive)
  (call-interactively 'kill-all-buffer-except-scratch-messages)
  )

(defun kill-all-buffer-except-scratch-messages ()
  "clean opened buffer"
  (interactive)
  (mapc 'kill-buffer
        (kb/get-buffer-list-except-some (list "*scratch*" "*Messages*" "*Ibuffer*")))
  (delete-other-windows))

(defun kb/get-buffer-list-except-some (buf-names)
  (remove-if (lambda (x) (member (buffer-name x) buf-names))
             (buffer-list)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("c" . kill-all-buffer-but-ibuffer))
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1))))

(use-package ibuffer-vc
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode
                          'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)))
  )

(defun switch-to-scratch-buffer ()
  "switch buffer to scratch if not exist then create new one"
  (interactive)
  (unless (get-buffer "*scratch*")
    (with-current-buffer (get-buffer-create "*scratch*")
      (insert initial-scratch-message)
      (lisp-interaction-mode)))
  (switch-to-buffer "*scratch*"))

(use-package persistent-scratch
  :init
  (persistent-scratch-setup-default))

(bind-key "C-c fn" 'my/file-info)

(defun my/file-info ()
  "Show current buffer information."
  (interactive)
  (if (buffer-file-name (current-buffer))
      (progn
        (let* ((file-name (buffer-file-name (current-buffer)))
               (f-attr (file-attributes file-name))
               (f-size (nth 7 f-attr))  ; ファイルサイズ
               (f-mode (nth 8 f-attr))  ; ファイル屬性
               (mes1 (format "file path: %s\n" file-name))
               (mes2 (format "file size: %s byte\n" f-size))
               (mes3 (format "file type: %s" f-mode))
               (mess (concat mes1 mes2 mes3)))
          (message "%s" mess)))
    nil))

(bind-key "C-c fu" 'buffer-to-utf-8-unix)

(defun buffer-to-utf-8-unix()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

(bind-key "C-c fa" 'my/cycle-revert-buffer-decoding)

(defun my/cycle-revert-buffer-decoding ()
  (interactive)

  (let* ((decoders [utf-8 gb18030 big5 gbk])
         (index-before
          (if (get 'my/cycle-revert-buffer-decoding 'state)
              (get 'my/cycle-revert-buffer-decoding 'state)
            0))
         (index-after (% (+ index-before 1) (length decoders)))
         (next-decoder (aref decoders index-after)))
    (message "%s %s" decoders next-decoder)
    (put 'my/cycle-revert-buffer-decoding 'state index-after)
    (revert-buffer-with-coding-system next-decoder)))

(bind-key "C-c fv" 'revert-buffer)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(setq revert-without-query '(".*"))

(bind-key "M-o" 'mode-line-other-buffer)

(use-package uniquify
  :ensure nil
  :init
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package eyebrowse
:bind ("C-x C-w" . eyebrowse-mode))

(use-package winner)

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init
  (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 2.0))))))

(bind-key "C-x x" 'z/swap-windows)
(defun z/swap-windows ()
  "swap windows"
  (interactive)
  (ace-swap-window)
  (aw-flip-window))

(use-package minibuffer
:ensure nil
:config
(add-hook 'minibuffer-setup-hook '(lambda () (setq cursor-type 'bar)))

(defun my/minibuffer-insert (p)
  (kill-line 0) (insert p))

(defun my/minibuffer-switch-to-ramdisk ()
  "Insert ramdisk path according to system type"
  (interactive)
  (my/minibuffer-insert user-ramdisk-directory))

(defun my/minibuffer-switch-to-home ()
  "Insert $HOME path."
  (interactive)
  (my/minibuffer-insert (file-name-as-directory (getenv "HOME"))))

(defun my/minibuffer-switch-to-rootdir ()
  "Insert / path."
  (interactive)
  (my/minibuffer-insert "/"))

(defun my/minibuffer-switch-to-tramp ()
  "Insert /ssh:."
  (interactive)
  (my/minibuffer-insert "/ssh:"))

(defun my/minibuffer-switch-to-vm ()
  "Insert /ssh:vm:."
  (interactive)
  (my/minibuffer-insert "/ssh:vm:"))

(defun my/minibuffer-switch-to-cluster ()
  "Insert /ssh:cluster:."
  (interactive)
  (my/minibuffer-insert "/ssh:cluster:"))
(use-package savehist
  :init
  (setq savehist-file (expand-file-name "savehist.dat" user-cache-directory))
  (savehist-mode 1))
(bind-keys :map minibuffer-local-map
           ("C-w" . backward-kill-word)
           ("M-p" . previous-history-element)
           ("M-n" . next-history-element)
           ("C-g" . minibuffer-keyboard-quit)
           ("M-t" . my/minibuffer-switch-to-ramdisk)
           ("M-h" . my/minibuffer-switch-to-home)
           ("M-/" . my/minibuffer-switch-to-rootdir)
           ("M-s" . my/minibuffer-switch-to-tramp)
           ("M-v" . my/minibuffer-switch-to-vm)
           ("M-c" . my/minibuffer-switch-to-cluster))
(savehist-mode 1)
(setq history-length 1000))

(use-package popwin
  ;; :init
  ;; (setq helm-popwin
  ;; '(("*Flycheck errors*" :height 10)
  ;; ("*Helm Find Files*" :height 0.3)
  ;; ("^\*helm.+\*$" :regexp t :height 15)))
  :config
  (popwin-mode 1)
  (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
  )

(bind-key "C-x t" '(lambda()
                     (interactive)
                     (call-process "xterm" nil 0 nil
                                   "-e"
                                   "export SHELL=/usr/bin/fish; export XTERM_SHELL=$SHELL; fish")))

(defun tangle-init ()
    "If the current buffer is 'init.org' the code-blocks are tangled."
    (interactive)
    (require 'async)
    (let ((buffer-name "async-make-init.el"))
      (when (equal (buffer-file-name) user-emacs-init-org)
    (message "Begin re-generate init.el")
        (async-start-process buffer-name "sh"
                             '(lambda (result)
                                (message "Re-Generate init.el finish." result))
                             (expand-file-name "makeinit.sh" user-emacs-directory)))))

(add-hook 'after-save-hook 'tangle-init)

(defun reload-emacs ()
  "reload my emacs settings"
  (interactive)
  (load-file emacs-init-el)
  (delete-other-windows))

(defun eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

(bind-key "C-x w" 'delete-frame)

(use-package eww
  :bind (("C-c b" . browse-at-point))
  :config
  (bind-keys :map eww-mode-map
             ("f" . link-hint-open-link)
             ("j" . next-line)
             ("k" . previous-line)
             ("l" . forward-char)
             ("h" . backward-char)
             ("d" . scroll-up-command)
             ("e" . scroll-down-command)
             ("B" . eww-browse-with-external-browser)
             ("m" . endless/toggle-image-display)
             ("q" . eww-quit-reset-image-flag)
             ("H" . eww-back-url)
             ("L" . eww-forward-url)
             ("c" . eww-copy-title-url-org-capture)
             ("ok" . eww-to-kindle-directly)
             ("oK" . send-url-to-kindle-at-point)
             ("oo" . eww-to-org)
             ("yy" . eww-copy-page-url)
             ("yt" . eww-copy-current-title)
             ("yo" . eww-copy-title-url-org)
             ("yu" . eww-copy-title-url))
  (setq browse-url-browser-function 'eww-browse-url)
  
  (setq browse-url-generic-program (executable-find "google-chrome-stable")
        shr-external-browser 'browse-url-generic)
  (setq eww-search-prefix "https://www.google.com/search?q=")
  (setq shr-color-visible-luminance-min 70)
  (add-hook 'eww-mode-hook '(lambda ()
                              (toggle-truncate-lines nil)))
  
  (defun eww-quit-reset-image-flag ()
    "reset image-flag"
  
    (interactive)
    (setq endless/display-images t)
    ;; (quit-window)
    (kill-buffer "*eww*"))
  
  (defvar-local endless/display-images t)
  
  (defun endless/toggle-image-display ()
    "Toggle images display on current buffer."
    (interactive)
    (setq endless/display-images (null endless/display-images))
    (endless/backup-display-property endless/display-images))
  
  (defun endless/backup-display-property (invert &optional object)
    "Move the 'display property at POS to 'display-backup.
  Only applies if display property is an image.
  If INVERT is non-nil, move from 'display-backup to 'display
  instead.
  Optional OBJECT specifies the string or buffer. Nil means current
  buffer."
    (let* ((inhibit-read-only t)
           (from (if invert 'display-backup 'display))
           (to (if invert 'display 'display-backup))
           (pos (point-min))
           left prop)
      (while (and pos (/= pos (point-max)))
        (if (get-text-property pos from object)
            (setq left pos)
          (setq left (next-single-property-change pos from object)))
          (if (or (null left) (= left (point-max)))
            (setq pos nil)
          (setq prop (get-text-property left from object))
          (setq pos (or (next-single-property-change left from object)
                        (point-max)))
          (when (eq (car prop) 'image)
            (add-text-properties left pos (list from nil to prop) object))))))
  
  (defun my/thing-at-point-to-eww ()
    (if mark-active
        (s-trim (buffer-substring-no-properties (region-beginning) (region-end)))
      (unless (setq-local ret (thing-at-point 'url))
        (unless (setq-local ret (thing-at-point 'symbol))
          (setq-local ret "")))
      ret))
  
  (defun browse-at-point (arg)
    "browse at point: selected region or url or single word.
    if cursor at whitespace then call eww wait for input some thing."
    (interactive "P")
    (if (consp arg)
        (call-interactively 'eww)
      (let ((thing (my/thing-at-point-to-eww)))
        (if (string= "" thing)
            (call-interactively 'eww)
          (eww thing)))))
  (defun eww-current-title nil
    "Return title of the Web page the current EWW buffer is visiting."
    (plist-get eww-data :title))
  
  (defun eww-current-source ()
    "Return source of the Web page the current EWW buffer is visiting."
    (plist-get eww-data :source))
  (defun eww-copy-current-title ()
    (interactive)
    (yank-url-or-title (eww-current-title)))
  
  (defun eww-copy-title-url ()
    (interactive)
    (yank-url-or-title (concat (eww-current-title) "\n" (eww-current-url))))
  
  (defun eww-copy-title-url-org ()
    (interactive)
    (yank-url-or-title (org-make-link-string (eww-current-url) (eww-current-title))))
  
  (defun eww-copy-current-source ()
    (interactive)
    (yank-url-or-title (eww-current-source)))
  (defun eww-to-org ()
    "Save eww current page to an org file"
    (interactive)
    (require 'org-eww)
  
    (let* ((eww-org-default-directory "/mnt/lvm-data/Documents/eww/")
           (org-file-name (expand-file-name (concat
                                             (replace-regexp-in-string "\[ :　/\]" "-" (eww-current-title))
                                             ".org")
                                            eww-org-default-directory)))
      (org-eww-copy-for-org-mode)
      (switch-to-buffer (get-buffer-create org-file-name))
      (yank)
      (goto-char (point-min))
      (insert "#+TITLE:\n#+AUTHOR:\n#+DATE:\n\n")
      (write-file org-file-name)
      ))
  
  )

(defun eww-copy-title-url-org-capture ()
(interactive)
  (eww-copy-title-url-org)
  (org-capture :goto "t")
  (yank))

(use-package elfeed
  :ensure eww
  :bind ("C-c e" . elfeed)
  :config
  (unbind-key "y" elfeed-search-mode-map)
  (bind-keys :map elfeed-search-mode-map
             ("m" . elfeed-toggle-star)
             ("j" . next-line)
             ("k" . previous-line)
             ("l" . forward-char)
             ("h" . backward-char)
             ("f" . avy-goto-line)
             ("d" . scroll-up-command)
             ("e" . scroll-down-command)
             ("g" . elfeed-update)
             ("a" . my/cycle-elfeed-search-filter)
             ("R" . elfeed-search-mark-all-read-next-filter)
             ("B" . elfeed-search-external-browser)
             ("C-<return>" . elfeed-search-browse-url)
             ("c" . copy-elfeed-search-org-capture)
             ("w" . copy-elfeed-search-url)
             ("yy" . copy-elfeed-search-url)
             ("yt" . copy-elfeed-search-title)
             ("yu" . copy-elfeed-search-title-url)
             ("yo" . copy-elfeed-search-org)
             ("ok" . elfeed-search-to-kindle)
             ("oK" . send-url-to-kindle-at-point))
  
  (unbind-key "y" elfeed-show-mode-map)
  (bind-keys :map elfeed-show-mode-map
             ("j" . next-line)
             ("k" . previous-line)
             ("l" . forward-char)
             ("h" . backward-char)
             ("f" . link-hint-open-link)
             ("d" . scroll-up-command)
             ("e" . scroll-down-command)
             ("B" . elfeed-show-external-browser)
             ("C-<return>" . elfeed-show-visit)
             ("c" . copy-elfeed-show-org-capture)
             ("w" . copy-elfeed-show-url)
             ("yy" . copy-elfeed-show-url)
             ("yt" . copy-elfeed-show-title)
             ("yu" . copy-elfeed-show-title-url)
             ("yo" . copy-elfeed-show-org)
             ("ok" . elfeed-show-to-kindle)
             ("oK" . send-url-to-kindle-at-point))
  
  (custom-set-faces
   '(elfeed-search-unread-title-face ((t :inherit default)))
   '(elfeed-search-title-face ((t :inherit font-lock-comment-face)))
   '(elfeed-search-date-face ((t (:inherit font-lock-string-face))))
   '(elfeed-search-feed-face ((t (:inherit font-lock-doc-face))))
   '(elfeed-search-tag-face ((t (:inherit font-lock-constant-face))))
   )
  
  (defface elfeed-search-starred-title-face
    '((t :foreground "#cc6666"))
    "Marks a starred Elfeed entry.")
  
  (defface elfeed-search-mustread-title-face
    '((t :foreground "#cc6666"))
    "Marks a mustread Elfeed entry.")
  
  (defface elfeed-search-it-title-face
    '((t :foreground "#b5bd68"))
    "Marks a news Elfeed entry.")
  
  (defface elfeed-search-finace-title-face
    '((t :foreground "#8abeb7"))
    "Marks a finace news Elfeed entry.")
  
  (defface elfeed-search-read-title-face
    '((t :inherit font-lock-comment-face))
    "Marks a finace news Elfeed entry.")
  
  (defface elfeed-search-junk-title-face
    '((t :inherit font-lock-comment-face))
    "Marks a junk Elfeed entry.")
  
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  (push '(star elfeed-search-starred-title-face) elfeed-search-face-alist)
  (push '(mustread elfeed-search-mustread-title-face) elfeed-search-face-alist)
  (setq elfeed-use-curl t)
  (add-hook 'elfeed-new-entry-hook
            '(lambda() (progn
                         (elfeed-make-tagger :before "1 week ago"
                                             :remove 'unread))))
  (defconst elfeed-search-filter-default "@1-weeks-ago +unread")
  (setq elfeed-search-filter elfeed-search-filter-default)
  (defun elfeed-reset-to-default-filter ()
    "elfeed reset to default filter"
    (interactive)
    (elfeed-search-set-filter elfeed-search-filter-default))
  (add-hook 'elfeed-search-mode-hook
            '(lambda() (progn
                         (elfeed-update))))
  (defadvice elfeed-show-entry
      (after switch-buffer activate)
    "after elfeed show entry then refresh buffer"
    (elfeed-show-refresh))
  (defun elfeed-search-mark-all-read ()
    "mark all feed as read in elfeed search mode"
    (interactive)
    (call-interactively 'mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  
  (defun elfeed-search-mark-all-read-next-filter ()
    "mark all feed as read and switch to next filter"
    (interactive)
    (call-interactively 'elfeed-search-mark-all-read)
    (my/cycle-elfeed-search-filter)
    )
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  (defun my/cycle-elfeed-search-filter-1 ()
    "switch to next tag circularly."
    ;;(interactive)
    (let* ((filters (vconcat (cons "" (mapcar (lambda (x) (format "+%s" x))
                                              (seq-remove (lambda (x) (eq x 'unread))
                                                          (elfeed-db-get-all-tags))))))
           (index-before
            (if (get 'my/cycle-elfeed-search-filter 'filter-index)
                (get 'my/cycle-elfeed-search-filter 'filter-index)
              0))
           (index-after
            (% (+ index-before 1) (length filters)))
           (next-filter (format "@1-week-ago %s +unread"
                                (aref filters index-after))))
  
      (put 'my/cycle-elfeed-search-filter 'filter-index index-after)
      (elfeed-search-set-filter next-filter)
      (beginning-of-buffer)
      index-after))
  (defun elfeed-search-has-unread ()
    (if (and elfeed-search-filter-active elfeed-search-filter-overflowing)
        (return nil)
      (cl-loop with feeds = (make-hash-table :test 'equal)
               for entry in elfeed-search-entries
               count (elfeed-tagged-p 'unread entry) into unread-count
               until (> unread-count 0)
               finally
               (cl-return (> unread-count 0)))))
  (defun elfeed-entry-url (entry)
    (and entry (elfeed-entry-link entry)))
  
  (defun elfeed-link-title (entry)
    (and entry (elfeed-entry-title entry)))
  
  (defun elfeed-entry-title-url (entry)
    (concat (elfeed-link-title entry) "\n" (elfeed-entry-url entry)))
  
  (defun elfeed-entry-link-org (entry)
    "get link string as org-mode link format"
    (concat "[[" (elfeed-entry-url entry) "][" (elfeed-link-title entry) "]]"))
  
  (defun get-elfeed-search-url ()
    (elfeed-entry-url (elfeed-search-selected :ignore-region)))
  
  (defun get-elfeed-search-title ()
    (elfeed-link-title (elfeed-search-selected :ignore-region)))
  
  (defun get-elfeed-search-title-url ()
    (elfeed-entry-title-url (elfeed-search-selected :ignore-region)))
  
  (defun get-elfeed-search-org ()
    (elfeed-entry-link-org (elfeed-search-selected :ignore-region)))
  
  (defun get-elfeed-show-url ()
    (elfeed-entry-url elfeed-show-entry))
  
  (defun get-elfeed-show-title ()
    (elfeed-link-title elfeed-show-entry))
  
  (defun get-elfeed-show-title-url ()
    (elfeed-entry-title-url elfeed-show-entry))
  
  (defun get-elfeed-show-org ()
    (elfeed-entry-link-org elfeed-show-entry))
  
  
  (defun yank-url-or-title (url-or-title)
    (message "%s" url-or-title)
    (kill-new url-or-title))
  
  (defun copy-elfeed-search-url ()
    "copy url to clipboard"
    (interactive)
    (yank-url-or-title (get-elfeed-search-url)))
  
  (defun copy-elfeed-search-title ()
    "copy title to clipboard"
    (interactive)
    (yank-url-or-title (get-elfeed-search-title)))
  
  (defun copy-elfeed-search-title-url ()
    "copy title and url to clipboard"
    (interactive)
    (yank-url-or-title (get-elfeed-search-title-url)))
  
  (defun copy-elfeed-search-org ()
    "copy url to clipboard as org-mode formate"
    (interactive)
    (yank-url-or-title (get-elfeed-search-org)))
  
  (defun copy-elfeed-show-url ()
    "copy url to clipboard."
    (interactive)
    (yank-url-or-title (get-elfeed-show-url)))
  
  (defun copy-elfeed-show-title ()
    "copy title to clipboard."
    (interactive)
    (yank-url-or-title (get-elfeed-show-title)))
  
  (defun copy-elfeed-show-title-url ()
    "copy title and url to clipboard as title url."
    (interactive)
    (yank-url-or-title (get-elfeed-show-title-url)))
  
  (defun copy-elfeed-show-org ()
    "copy title and url to clipboard as org-mode link format"
    (interactive)
    (yank-url-or-title (get-elfeed-show-org)))
  
  (defun elfeed-search-external-browser ()
    "open current url via extenal browser"
    (interactive)
    (elfeed-search-browse-url t))
  
  (defun elfeed-show-external-browser ()
    "open current url via extenal browser"
    (interactive)
    (elfeed-show-visit t))
  (cl-defun elfeed-dead-feeds (&optional (years 1.0))
     "Return a list of feeds that haven't posted en entry in YEARS years."
     (let* ((living-feeds (make-hash-table :test 'equal))
     (seconds (* years 365.0 24 60 60))
     (threshold (- (float-time) seconds)))
     (with-elfeed-db-visit (entry feed)
     (let ((date (elfeed-entry-date entry)))
     (when (> date threshold)
     (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
     (cl-loop for url in (elfeed-feed-list)
     unless (gethash url living-feeds)
     collect url)))
  (defun copy-elfeed-search-org-capture ()
    (interactive)
    (copy-elfeed-search-org)
    (org-capture :goto "t")
    (yank))
  
  (defun copy-elfeed-show-org-capture ()
    (interactive)
    (copy-elfeed-show-org)
    (org-capture :goto "t")
    (yank))
  )

(defun my/cycle-elfeed-search-filter ()
  "Switch next filter has unread feeds."
  (interactive)
  (cl-loop for index-feed = (my/cycle-elfeed-search-filter-1)
           ;; do (message "%s %s" index-feed (elfeed-search-has-unread))
           until (or (= 0 index-feed)
                     (elfeed-search-has-unread))))

(use-package elfeed-goodies
  :ensure elfeed
  :commands elfeed-goodies/setup
  :init
  (setq elfeed-goodies/entry-pane-size 0.618)
  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure elfeed
  :commands elfeed-org
  :init
  (progn
    ;; (expand-file-name "emacs-init.org" user-emacs-directory)
    (setq rmh-elfeed-org-files (list "~/.elfeed.org"))
    (elfeed-org)))

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
         ("C-h M-m" . discover-my-mode)))

(use-package which-key
  :diminish ""
  :init
  (progn
    (setq which-key-idle-delay 0.5)
    (which-key-mode)
    (which-key-setup-side-window-bottom)
    (setq which-key-side-window-max-width 0.25)))

(unbind-key "C-z")
(unbind-key "C-x C-z")
(unbind-key "C-x m")

(use-package crux
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ))

(bind-key "C-<delete>" 'backward-kill-word)
(bind-key "M-<delete>" 'kill-word)

(electric-pair-mode 1)

(defmacro epm/add-mode-pairs (hook pairs)
  `(add-hook ,hook
             (lambda ()
               (setq-local electric-pair-pairs (append electric-pair-pairs ,pairs))
               (setq-local electric-pair-text-pairs electric-pair-pairs))))

(epm/add-mode-pairs 'python-mode-hook '((?\' . ?\')))
(epm/add-mode-pairs 'web-mode-hook '((?\' . ?\')))
(epm/add-mode-pairs 'haskell-mode-hook '((?\' . ?\')))
(epm/add-mode-pairs 'org-mode-hook '((?/ . ?/) (?= . ?=) (?~ . ?~) (?_ . ?_)))

(electric-quote-mode -1)

(use-package hungry-delete
  :diminish ""
  :init
  (global-hungry-delete-mode 1))

;move line up down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (let ((column (current-column))
          (pos (< (point) (mark)))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (and pos (exchange-point-and-mark))
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          ;; Account for changes to transpose-lines in Emacs 24.3
          (when (and (eval-when-compile
                       (not (version-list-<
                             (version-to-list emacs-version)
                             '(24 3 50 0))))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(bind-key "M-<up>" 'move-text-up)
(bind-key "M-<down>" 'move-text-down)

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  "Shift text or region right."

  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  "shift text or region left."

  (interactive "p")
  (shift-text (- count)))

(bind-key "C-x r <right>" 'shift-right)
(bind-key "C-x r <left>" 'shift-left)

(use-package undo-tree
  :diminish undo-tree-mode
  :init
    (global-undo-tree-mode)
  :config
    (setq undo-tree-history-directory-alist
          `(("." . ,(file-name-as-directory (expand-file-name "undo-tree" user-cache-directory)))))
    (setq undo-tree-auto-save-history t))

(bind-key "M-SPC" 'set-mark-command)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
       ((ignore-errors (org-narrow-to-block) t))
       (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; (define-key endless/toggle-map "n"
;; #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (define-key LaTeX-mode-map "\C-xn"
      nil)))

(use-package iedit
  :demand
  :config
  (custom-set-faces
   '(iedit-occurrence ((t (:inherit isearch-lazy-highlight-face) )))))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ))

(use-package ivy
  :diminish ""
  :bind
  (("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ("M-." . ivy-next-history-element))
  :init
  (progn
    (ivy-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (setq completion-in-region-function 'ivy-completion-in-region)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure
  :ensure wgrep
  :ensure rg
  :bind (("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         ("C-x f" . counsel-recentf)
         ("C-x l" . counsel-locate)
         ("C-x g" . counsel-rg)
         ("C-:" . counsel-company)
         ("C-x C-\\" . counsel-goto-recent-directory)
         ("C-x \\" . counsel-goto-recent-directory-other-window)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package company
  :diminish " ⓐ"
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (global-company-mode t)
  :config
  (progn
    (setq company-idle-delay 0.1)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 2)
    (setq company-echo-delay 0)

    (add-to-list 'company-backends 'company-ispell t)))

(use-package company-quickhelp
  :ensure
  :commands company-quickhelp-mode
  :init (company-quickhelp-mode 1))

(use-package ispell
  :config
  (progn
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args   '("--sug-mode=slow"
                                  "--lang=en_US"
                                  "--ignore=2"
                                  "--ignore-case"
                                  "--run-together"
                                  "--run-together-limit=5"
                                  "--run-together-min=2")))
     ;; how to fire 'hunspell'?
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-extra-args   '("-d en_US"))
      (setq ispell-local-dictionary-alist
            '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
              ("zh_TW" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))))

    (setq ispell-local-dictionary "en_US")
    (setq ispell-silently-savep t)))

(use-package flyspell-correct-ivy
  :ensure flyspell
  :ensure flyspell-correct
  :ensure ivy
  :bind (:map flyspell-mode-map
              ("M-$" . flyspell-correct-word-generic))
  :init
  (progn
    (setq flyspell-correct-interface 'flyspell-correct-ivy)
    ;; (bind-keys :map flyspell-mode-map
    ;; ("M-$" . flyspell-correct-word-generic))
    ;; bind flyspell-correct-word-generic
    ;; (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-word-generic)
    ;; (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-previous-word-generic)
    ))

(eval-after-load 'ispell
  '(progn
     (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
     (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
     ))

(use-package avy
  :bind (("M-g M-c" . avy-goto-char)
         ("M-g M-l" . avy-goto-line)))

(use-package ace-pinyin
  :ensure
  :ensure avy
  :diminish ""
  :commands ace-pinyin-global-mode
  :init
  (ace-pinyin-global-mode +1)
  :config
  (setq ace-pinyin-simplified-chinese-only-p nil))

(use-package link-hint
  :bind ("C-c ol" . link-hint-open-link)
  :init
  (dolist (map (list help-mode-map Info-mode-map))
      (bind-key "f" 'link-hint-open-link map)))

(use-package beginend
  :diminish beginend-global-mode
  :diminish beginend-prog-mode
  :diminish beginend-elfeed-search-mode
  :diminish beginend-dired-mode
  :diminish beginend-ibuffer-mode
  :init
  (setq beginend-global-mode t)
  (beginend-global-mode))

(use-package vlf
  :init
  (require 'vlf-setup)
  )

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :diminish org-src-mode
  :config
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil
        org-list-indent-offset 0
        org-blank-before-new-entry '((heading . t)
                                       (plain-list-item . t))
        org-startup-indented t)
  (add-to-list 'org-structure-template-alist
               '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
  
  (add-to-list 'org-structure-template-alist
               '("S" "#+BEGIN_SRC sh\n?\n#+END_SRC"))
  
  (add-to-list 'org-structure-template-alist
               '("T" "#+TITLE:?\n#+AUTHOR:\n#+DATE:"))
  
  (add-to-list 'org-structure-template-alist
               (list "N" (concat "#+BEGIN_COMMENT\n"
                                 ".. title:\n"
                                 ".. slug:\n"
                                 ".. date: " (current-date-time-nikola) "\n"
                                 ".. status: draft\n"      ;;published | draft
                                 ".. tags:\n"
                                 ".. category:\n"
                                 ".. link:\n"
                                 ".. description:\n"
                                 ".. type: text\n"         ;;text | micro
                                 "#+END_COMMENT\n"
                                 "#+OPTIONS: toc:nil ^:{}\n"
                                 "#+LANGUAGE: zh-TW\n"
                                 )))
  (require 'ob-sh)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ditaa . t)
     (dot . t)
     (plantuml . t)
     (js . t)
     (latex . t)
     (org . t)
     (python . t)
     (haskell . t)
     (clojure . t)
     (sh . t)
     (sqlite . t)
     (sql . t)
     ))
  (require 'ox-html)
  (defun eh-org-clean-space (text backend info)
    "在 export 為 HTML 時，刪除中文之間不必要的空格"
    (when (org-export-derived-backend-p backend 'html)
      (let ((regexp "[[:multibyte:]]")
            (string text))
        ;; org 預設將一個換行符轉換為空格，但中文不需要這個空格，刪除。
        (setq string
              (replace-regexp-in-string
               (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
               "\\1\\2" string))
        ;; 刪除粗體之前的空格
        (setq string
              (replace-regexp-in-string
               (format "\\(%s\\) +\\(<\\)" regexp)
               "\\1\\2" string))
        ;; 刪除粗體之後的空格
        (setq string
              (replace-regexp-in-string
               (format "\\(>\\) +\\(%s\\)" regexp)
               "\\1\\2" string))
        string)))
  
  (add-to-list 'org-export-filter-paragraph-functions
               'eh-org-clean-space)
  
  (bind-key "C-c l" 'org-store-link)
  (bind-key "C-c c" 'org-capture)
  (bind-key "C-c a" 'org-agenda)
  (setq org-agenda-window-setup 'current-window)
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)
  (setq org-directory "~/Dropbox/GTD")
  (setq org-default-notes-file (expand-file-name "myGTD.org" org-directory)
        org-default-inbox-file (expand-file-name "inbox.org" org-directory)
        org-my-notes-file (expand-file-name "notes.org" org-directory)
        org-my-travel-file (expand-file-name "travel.org" org-directory)
        org-my-food-file (expand-file-name "food.org" org-directory)
        )
  
  (setq org-capture-templates
        '(("t" "Todo" entry (file org-default-inbox-file)
           "* TODO %?\n%T")
          ("e" "Emacs" entry (file+headline org-default-notes-file "emacs")
           "* TODO %?\n%T")
          ("n" "Note" entry (file org-my-notes-file)
           "* %?\n%T")
          ("r" "tRavel" entry (file org-my-travel-file)
           "* %?\n%T")
          ("f" "Food" entry (file org-my-food-file)
           "* %?\n%T")
          ))
  
  (setq org-agenda-files (list org-default-notes-file
                               org-default-inbox-file
                               org-my-notes-file
                               org-my-travel-file
                               org-my-food-file
                               user-emacs-init-org
                               ))
  
  
  ;; (setq org-agenda-current-span 'day)
  ;; (setq org-agenda--span 2)
  
  (setq org-refile-targets '((nil :maxlevel . 2)  ; refile to headings in the current buffer
                             (org-agenda-files :level . 1)
                             ))
  
  (add-hook 'org-agenda-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-g") 'org-agenda-exit)))
  (bind-key "C-c fg" 'my/GTD-dropbox-status)
  (defun my/GTD-dropbox-status ()
    (interactive)
    (message "%s" (shell-command-to-string "cd ~/Dropbox/GTD&&dropbox-cli filestatus"))))

(eval-after-load 'ox-html
  '(setq org-html-text-markup-alist
         '((bold . "<b>%s</b>")
           (code . "<code>%s</code>")
           (italic . "<i>%s</i>")
           (strike-through . "<del>%s</del>")
           (underline . "<span class=\"underline\">%s</span>")
           (verbatim . "<kbd>%s</kbd>"))))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(defun org-archive-done-tasks ()
  "archive all the DONE/CNACELED tasks "
  (interactive)
  (dolist (status '("/DONE" "/CANCELED"))
    (org-map-entries (lambda ()
           (org-archive-subtree)
           (setq org-map-continue-from (outline-previous-heading)))
             status 'file)))

(use-package org-gcal
  :ensure
  :ensure request
  :ensure alert
  :config

  (when (file-exists-p user-secret-file)
    (load-file user-secret-file))

  (add-to-list 'org-agenda-files (cdr(car org-gcal-file-alist)))
  ;; don’t use org-gcal-sync because 亂碼
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-refresh-token) (org-gcal-fetch)))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-refresh-token) (org-gcal-fetch)))
  )

(use-package org-super-agenda
  :config
  (org-super-agenda-mode)
  (let ((org-super-agenda-groups
         '(;; Each group has an implicit boolean OR operator between its selectors.
           (:name "Today"  ; Optionally specify section name
                  :time-grid t  ; Items that appear on the time grid
                  :todo "TODAY")  ; Items that have this TODO keyword
           (:name "Important"
                  ;; Single arguments given alone
                  :tag "bills"
                  :priority "A")
           ;; Set order of multiple groups at once
           (:order-multi (2 (:name "Shopping in town"
                                   ;; Boolean AND group matches items that match all subgroups
                                   :and (:tag "shopping" :tag "@town"))
                            (:name "Food-related"
                                   ;; Multiple args given in list with implicit OR
                                   :tag ("food" "dinner"))
                            (:name "Personal"
                                   :habit t
                                   :tag "personal")
                            (:name "Space-related (non-moon-or-planet-related)"
                                   ;; Regexps match case-insensitively on the entire entry
                                   :and (:regexp ("space" "NASA")
                                                 ;; Boolean NOT also has implicit OR between selectors
                                                 :not (:regexp "moon" :tag "planet")))))
           ;; Groups supply their own section names when none are given
           (:todo "WAITING" :order 8)  ; Set order of this section
           (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                  ;; Show this group at the end of the agenda (since it has the
                  ;; highest number). If you specified this group last, items
                  ;; with these todo keywords that e.g. have priority A would be
                  ;; displayed in that group instead, because items are grouped
                  ;; out in the order the groups are listed.
                  :order 9)
           (:priority<= "B"
                        ;; Show this section after "Today" and "Important", because
                        ;; their order is unspecified, defaulting to 0. Sections
                        ;; are displayed lowest-number-first.
                        :order 1)
           ;; After the last group, the agenda will display items that didn't
           ;; match any of these groups, with the default order position of 99
           )))
    (org-agenda nil "a")))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(bind-key "M-;" #'comment-line)

(use-package aggressive-indent
  :diminish " Ⓘ"
  :init
  (global-aggressive-indent-mode 1)
  :config
  (dolist (mm '(haskell-mode haskell-cabal-mode haskell-interactive-mode
                             python-mode
                             pug-mode stylus-mode))
    (add-to-list 'aggressive-indent-excluded-modes mm)))

(use-package rainbow-mode
      :diminish rainbow-mode
      :init
      (progn
        (add-hook 'prog-mode-hook 'rainbow-mode)
        (add-hook 'css-mode-hook 'rainbow-mode)))

(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'org-mode-hook 'rainbow-delimiters-mode)))

(use-package  highlight-parentheses
  :diminish ""
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4")))

(use-package highlight-numbers
  :init
  (add-hook 'prog-mode-hook '(lambda()
                               (if (not (derived-mode-p 'json-mode))
                                   (highlight-numbers-mode)))))

(use-package flycheck
  :diminish (flycheck-mode . " ⓢ")
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-keymap-prefix (kbd "C-c v"))

  :config
  (progn
    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers '(javascript-jshint)))

    ;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))

    (flycheck-add-mode 'javascript-eslint 'web-mode)
    ;; customize flycheck temp file prefix
    (setq-default flycheck-temp-prefix ".flycheck")

    ;; use local eslint from node_modules before global
    ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
    (defun my/use-eslint-from-node-modules ()
      (let* ((root (locate-dominating-file
                    (or (buffer-file-name) default-directory)
                    "node_modules"))
             (eslint (and root
                          (expand-file-name "node_modules/eslint/bin/eslint.js"
                                            root))))
        (when (and eslint (file-executable-p eslint))
          (setq-local flycheck-javascript-eslint-executable eslint))))
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

    (setq flycheck-indication-mode 'right-fringe)
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)

    (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)))

(use-package magit
  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-auto-revert-mode nil)
    (setq magit-save-some-buffers nil)
    (setq magit-set-upstream-on-push t)
    (setq magit-diff-refine-hunk t)
    (setq magit-completing-read-function 'ivy-completing-read)
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only))
  :bind ("C-c g" . magit-status))

(use-package magit-filenotify
  :init (add-hook 'magit-status-mode-hook 'magit-filenotify-mode))

(bind-key "C-c fd" 'call-meld)

(defun call-meld ()
  "Find the differences of current buffer from last commit."
  (interactive)
  (save-excursion
    (shell-command (format "meld %s" (buffer-file-name))))
  )

(use-package diff-hl
  :bind (:map diff-hl-mode-map
              ("C-x v p" . diff-hl-previous-hunk)
              ("C-x v n" . diff-hl-next-hunk))
  :init
  (progn (global-diff-hl-mode +1)
         (diff-hl-dired-mode 1)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-vertically)
(setq ediff-merge-split-window-function 'split-window-vertically)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package projectile
  :diminish ""
  :init
  (progn (projectile-global-mode)
         (setq projectile-completion-system 'ivy)))

(use-package elpy
  :diminish "☀"
  :ensure
  :ensure flycheck
  :bind ("M-*" . pop-tag-mark)
  :commands elpy-enable
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi")
  
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  (setq elpy-modules (delq 'elpy-module-django elpy-modules))
  
  (add-to-list 'elpy-modules 'elpy-module-company)
  (add-to-list 'elpy-modules 'elpy-module-yasnippet)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (define-key python-mode-map (kbd "RET")
    'newline-and-indent)
  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'comment-inline-offset) 2))))

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
  (setq web-mode-engines-alist  '(("mako" . "\\.tmpl\\'")))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)

  (defun my-web-mode-hook ()
    "Hooks for Web mode."

    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-style-padding 4)
    (setq web-mode-script-padding 4))

  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package js2-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (add-hook 'js2-mode-hook
              (lambda () (setq js2-basic-offset 4)))
    (add-hook 'js2-mode-hook 'flycheck-mode)))

(use-package css-mode
  :init (setq css-indent-offset 2))

(use-package company-tern
:demand company
:demand tern
:commands company-backends
:init
(add-to-list 'company-backends 'company-tern))

(use-package json-mode
  :init (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(use-package hexo)

(use-package pug-mode)

(use-package stylus-mode
  :quelpa (:fetcher github :repo "vladh/stylus-mode")
      :ensure pug-mode)

(use-package intero
  :demand haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  )

(use-package haskell-mode
  :mode "\\.hs$" "\\.l?hs$"
  :config
  (progn (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
         (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
         (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

         (defun my-haskell-hook ()
           (setq mode-name " λ ")

           (haskell-indentation-mode t)

           (turn-on-haskell-doc)
           (diminish 'haskell-doc-mode "")

           (subword-mode)
           (diminish 'subword-mode "")

           (turn-on-eldoc-mode)
           (diminish 'eldoc-mode "")

           (turn-on-haskell-decl-scan)
           (setq evil-auto-indent nil))

         ;;(setq haskell-font-lock-symbols 'unicode)
         ;;(setq haskell-literate-default 'tex)
         ;;(setq haskell-stylish-on-save t)
         ;;(setq haskell-tags-on-save t)
         (add-hook 'haskell-mode-hook 'my-haskell-hook)))

(use-package hindent
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package elm-mode
:ensure company
:config
(add-to-list 'company-backends 'company-elm))

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (lispy-mode 1))))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines nil)))

(use-package edbi
  :ensure deferred
  :ensure epc
  :ensure ctable
  :ensure concurrent
  :ensure e2wm
  :init
  (autoload 'e2wm:dp-edbi "e2wm-edbi.el" nil t))

(use-package company-edbi
  :commands company-backends
  :init
  (setq edbi:completion-tool nil)
  (add-to-list 'company-backends 'company-edbi))

(use-package edbi-sqlite
    :bind (("C-x BB" . stock-db-stock-open)
           ("C-x BQ" . stock-db-stock-close))
    :init
(defvar edbi-eyebrowse-mode-prev nil)
(defvar edbi-eyebrowse-slot nil)

(defun switch-to-edbi-workspace ()
  (setq edbi-eyebrowse-mode-prev eyebrowse-mode)

  (unless eyebrowse-mode
    (eyebrowse-mode))

  (if edbi-eyebrowse-slot
      (eyebrowse-switch-to-window-config edbi-eyebrowse-slot)
    (progn
      (eyebrowse-create-window-config)
      (setq edbi-eyebrowse-slot (eyebrowse--get 'current-slot))
      (eyebrowse-rename-window-config edbi-eyebrowse-slot "stock")
      )))
(defun stock-db-stock-open()
  "open and manage my stock db."
  (interactive)
  (if edbi-eyebrowse-slot
      (eyebrowse-switch-to-window-config edbi-eyebrowse-slot)
    (progn
      (switch-to-edbi-workspace)
      (edbi-sqlite "/mnt/lvm-data/Programing/stock4/db/stock2.db")
      (e2wm:dp-edbi))))

(defun stock-db-stock-close ()
  "close my stock db"
  (interactive)
  (e2wm:stop-management)
  (when edbi-eyebrowse-slot
    (progn
      (eyebrowse-switch-to-window-config edbi-eyebrowse-slot)
      (setq edbi-eyebrowse-slot nil)
      (kill-edbi-buffers)
      (eyebrowse-close-window-config)
      )))
(defun kill-buffer-edbi (buffer-or-name)
  (when (or (string/starts-with buffer-or-name "*edbi")
            (string/starts-with buffer-or-name "*epc"))
    (kill-buffer buffer-or-name)))


;; (cond
;; ((string/starts-with buffer-or-name "*edbi")
;; (kill-buffer buffer-or-name))
;; ((string/starts-with buffer-or-name "*epc")
;; (kill-buffer buffer-or-name))
;; (t nil)))


(defun kill-edbi-buffers ()
  (interactive)
  (mapc 'kill-buffer-edbi (mapcar (function buffer-name) (buffer-list))))
)

(eval-after-load 'flycheck
  '(progn
     (flycheck-add-mode 'sql-sqlint 'edbi:sql-mode)))

(use-package systemd)

(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))

(use-package markdown-mode
  :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
      (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))))

(use-package nginx-mode)

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode)
  :config
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'fish_indent-before-save))))

(use-package clojure-mode
  :ensure t
  :config
  (use-package cider
    :ensure t
    :after clojure-mode
    :config
  
    ;; Enable eldoc in Clojure buffers
    (add-hook 'cider-mode-hook 'eldoc-mode)
  
    ;; Hide `*nrepl-connection*' and `*nrepl-server*' buffers from appearing
    ;; in some buffer switching commands like switch-to-buffer
    (setq nrepl-hide-special-buffers t)
  
    ;; Enabling CamelCase support for editing commands(like forward-word,
    ;; backward-word, etc) in the REPL is quite useful since we often have
    ;; to deal with Java class and method names. The built-in Emacs minor
    ;; mode subword-mode provides such functionality
    (add-hook 'cider-repl-mode-hook #'subword-mode)
  
    ;; The use of paredit when editing Clojure (or any other Lisp) code is
    ;; highly recommended. You're probably using it already in your
    ;; clojure-mode buffers (if you're not you probably should). You might
    ;; also want to enable paredit in the REPL buffer as well.
    ;; (add-hook 'cider-repl-mode-hook #'paredit-mode)
  
    ;; Auto-select the error buffer when it's displayed:
    (setq cider-auto-select-error-buffer t)
  
    ;; Controls whether to pop to the REPL buffer on connect.
    (setq cider-repl-pop-to-buffer-on-connect nil)
  
    ;; Make cider not prompt to save the corresponding file when loading a buffer.
    (setq cider-prompt-save-file-on-load nil)
  
    ;; T to wrap history around when the end is reached.
    (setq cider-repl-wrap-history t)
  
    ;; Log protocol messages to the `nrepl-message-buffer-name' buffer.
    (setq nrepl-log-messages t)
  
    ;; Make `cider-jack-in-clojurescript' directly connect with figwheel
    ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl#leiningen
    (setq cider-cljs-lein-repl
          "(do (require 'figwheel-sidecar.repl-api)
               (figwheel-sidecar.repl-api/start-figwheel!)
               (figwheel-sidecar.repl-api/cljs-repl))"))
  (use-package clojure-cheatsheet
    :ensure t)
  (defun my/cider-send-and-evaluate-sexp ()
    "Sends the s-expression located before the point or the active
    region to the REPL and evaluates it. Then the Clojure buffer is
    activated as if nothing happened."
    (interactive)
    (if (not (region-active-p))
        (cider-insert-last-sexp-in-repl)
      (cider-insert-in-repl
       (buffer-substring (region-beginning) (region-end)) nil))
    (cider-switch-to-repl-buffer)
    (cider-repl-closing-return)
    (cider-switch-to-last-clojure-buffer)
    (message ""))
  (bind-keys :map clojure-mode-map
             ;; M-m     . refactor command
             ("C-c C-f" . projectile-find-file)
             ("C-c M-c" . cider-connect)
             ("C-c M-j" . cider-jack-in)
             ("C-c '"   . my/narrow-or-widen-dwim)
             ("C-c h"   . clojure-cheatsheet)
             ("C-c C-k" . cider-load-buffer)
             ("C-x C-e" . cider-eval-last-sexp)
             ("C-c C-v" . my/cider-send-and-evaluate-sexp)
             ("C-c C-t" . projectile-toggle-between-implementation-and-test)))

(use-package clj-refactor
  :ensure t
  :after clojure-mode
  :config
  ;; Add clj-refactor to clojure-mode
  (add-hook 'clojure-mode-hook '(lambda () (clj-refactor-mode 1)))
  ;; Use `C-c C-x' as prefix
  (cljr-add-keybindings-with-prefix "M-m"))

(use-package clojars
  :ensure t)

(diminish 'overwrite-mode)
(diminish 'subword-mode)
(eval-after-load 'org-indent
  '(diminish 'org-indent-mode))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))
