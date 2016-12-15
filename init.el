;; -*- lexical-binding: t -*-

(setq load-prefer-newer t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(modify-all-frames-parameters '((fullscreen . maximized)))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq ad-redefinition-action 'accept)

(setq initial-scratch-message ";; This is scratch buffer.\n\n")

(defun set-font-for-chinese ()
  (set-face-attribute 'default nil
                      :family "Source Code Pro" :height 180)
  (set-face-attribute 'variable-pitch nil
                      :family "Sans" :height 200 :weight 'regular)

  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Source Han Sans" :size 28)))

  (set-fontset-font (frame-parameter nil 'font) 'symbol (font-spec :family "Symbola" :size 32)))

(add-to-list 'after-make-frame-functions (lambda (new-frame)
                                           (select-frame new-frame)
                                           (if window-system
                                               (set-font-for-chinese))))

(when window-system
  (set-font-for-chinese))

(setq user-emacs-directory (file-name-directory (or load-file-name (buffer-file-name))))
(defconst user-emacs-init-org (expand-file-name "emacs-init.org" user-emacs-directory))
;; (defconst user-emacs-init-el (expand-file-name "emacs-init.el" user-emacs-directory))
(defconst emacs-init-el (expand-file-name "init.el" user-emacs-directory))

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

(use-package paradox
  :init
  (setq paradox-execute-asynchronously t))

(defun reload-emacs ()
  "reload my emacs settings"

  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (delete-other-windows))

(defun eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."

  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

(use-package color-theme-sanityinc-tomorrow
  :init
    (load-theme 'sanityinc-tomorrow-night :no-confirm))

(use-package spaceline-config
  :ensure spaceline
  :commands spaceline-spacemacs-theme
  :init
  (progn
    (setq-default powerline-default-separator 'wave)
    (setq spaceline-minor-modes-separator "")
    (spaceline-spacemacs-theme)
    ))

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
  (progn
    (add-hook 'prog-mode-hook #'highlight-parentheses-mode)

    (setq hl-paren-delay 0.2)
    (setq hl-paren-colors '("Springgreen3"
                            "IndianRed1"
                            "IndianRed3"
                            "IndianRed4"))
    (global-highlight-parentheses-mode t)
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
    ))

(use-package beacon
:init
(beacon-mode 1))

(use-package pangu-spacing
  :diminish ""
  :config
  (progn
    (global-pangu-spacing-mode 1)

    (dolist (mode '(eww-mode elfeed-search-mode elfeed-show-mode))
      (add-to-list 'pangu-spacing-inhibit-mode-alist mode))

    ;; Always insert `real' space in org-mode.
    (dolist (mode '(markdown-mode-hook rst-mode-hook))
      (add-hook mode '(lambda()
                     (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))
    ))

(use-package fcitx
  :init (fcitx-aggressive-setup))

(defun opencc-buffer ()
  "Convert chinese from simplified to variants and phrases of Taiwan"

  (interactive)

  (set-buffer-file-coding-system 'utf-8-unix)

  (let ((opencc-conv-temp-file (expand-file-name "opencc.tmp" temporary-file-directory)))
    (let ((str (buffer-substring-no-properties (point-max) 1)))
      (with-temp-file opencc-conv-temp-file
        (insert str "\n")))

    (let ((current-point (point))
          (result
           (shell-command-to-string (concat "opencc -i " opencc-conv-temp-file))))

      (erase-buffer)
      (insert result)
      (goto-char current-point))))

(bind-key "C-c fo" 'opencc-buffer)

(use-package noflet)

(require 'noflet)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (noflet ((process-list ())) ad-do-it))

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(prefer-coding-system 'utf-8)
(setq system-time-locale "en_US" )

(global-font-lock-mode t)

(use-package ethan-wspace
  :diminish "🄣"
  :config
  (progn
    ;; Turn off `mode-require-final-newline' since ethan-wspace
    ;; supersedes `require-final-newline'.
    (setq mode-require-final-newline nil)

    ;; Enable ethan-wspace globally
    (global-ethan-wspace-mode 1)

    ;; Prevent etha-wspace touch my TAB on makefile mode
    (add-hook 'makefile-mode-hook
              '(lambda()
                 (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))))

    ;; Ignore no trailing newline error
    (setq-default ethan-wspace-errors (remove 'no-nl-eof ethan-wspace-errors))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(toggle-truncate-lines t)

(setq visible-bell t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq select-enable-clipboard t
      select-enable-primary t)

(setq echo-keystrokes 0.1)

(delete-selection-mode 1)

(savehist-mode 1)
(setq history-length 1000)

(global-subword-mode 1)

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(use-package linum
  :init
  (add-hook 'prog-mode-hook
            '(lambda () (linum-mode 1))))

(setq column-number-mode t)

(use-package linum-relative
  :init
  (progn
    (linum-relative-mode)
    (setq linum-relative-current-symbol "")
    (setq linum-relative-format "%4s")))

(use-package highlight-numbers
  :init
  ;; json-mode has it's own highlight numbers method
  (add-hook 'prog-mode-hook '(lambda()
                               (if (not (derived-mode-p 'json-mode))
                                   (highlight-numbers-mode)))))

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

(use-package golden-ratio
  :diminish ""
  :init
    (golden-ratio-mode 1))

(use-package winner)

(use-package ace-window
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

(bind-key "C-x w" 'delete-frame)

(use-package popwin
  :init
  (progn
    (setq helm-popwin
          '(("*Flycheck errors*" :height 10)
            ("*Helm Find Files*" :height 0.3)
            ("^\*helm.+\*$" :regexp t :height 15))))
  :config
  (progn
    (popwin-mode 1)
    (push '("*helm semantic/imenu*" :width 0.382 :position left) popwin:special-display-config)
    (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
    ))

(defun create-scratch-buffer ()
  "create scratch buffer if it is not exist."

  (unless (get-buffer "*scratch*")
    (with-current-buffer
        (get-buffer-create "*scratch*")
      (insert initial-scratch-message)
      (lisp-interaction-mode))))

(defun switch-to-scratch-buffer ()
  "switch buffer to scratch if not exist then create new one"
  (interactive)
  (create-scratch-buffer)
  (switch-to-buffer "*scratch*"))

  ;; (run-with-idle-timer 1 t 'create-scratch-buffer)

(unbind-key "C-x C-c")
(bind-key "C-x C-c C-c" 'kill-emacs)

(bind-key "C-x C-c c" 'kill-other-buffers)
(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
  (delete-other-windows))

(bind-key "C-x C-c s" 'kill-all-buffer-but-scratch)
(defun kill-all-buffer-but-scratch ()
  (interactive)
  (switch-to-scratch-buffer)
  (kill-other-buffers)
  (delete-other-windows))

(bind-key "C-x k" 'kill-this-buffer)

(bind-key "C-c fn" 'my/file-info)

(defun my/file-info ()
  "Show current buffer information."
  (interactive)
  (if (buffer-file-name (current-buffer))
      (progn
        (let* ((file-name (buffer-file-name (current-buffer)))
               (f-attr (file-attributes file-name))
               (f-size (nth 7 f-attr))  ; ファイルサイズ
               (f-mode (nth 8 f-attr))  ; ファイル属性
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

  (let* ((decoders [utf-8 gb18030 big5])
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
(setq revert-without-query '(".*")) ;; disable revert query

(bind-key "C-x C-b" 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(add-hook 'ibuffer-hook '(lambda ()
                           (revert-buffer)))

(bind-key "M-o" 'mode-line-other-buffer)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (expand-file-name "recentf" user-cache-directory))
    (recentf-mode 1)
    (setq recentf-max-saved-items 500
          recentf-max-menu-items 10)))

(use-package saveplace
  :init
  (progn
    (setq save-place-file (expand-file-name "places" user-cache-directory))
    (save-place-mode 1)))

(use-package uniquify
:ensure nil
:init
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
)

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
  :config
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
           ("M-c" . my/minibuffer-switch-to-cluster)))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("s" . my/counsel-dired-sort)
              ("f" . dired-find-name-in-current-directory)
              ("C-x M-o" . dired-omit-and-remember))
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

;; (defun sort-dir (sort-item)
;; (dired-sort-other
;; (concat dired-listing-switches
;; (car (cdr (assoc sort-item sorting-action))))))

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
    :init (progn
            (diredp-toggle-find-file-reuse-dir 1)
))

(use-package async
  :init
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package openwith
  :init
  (progn
    (openwith-mode t)
    (setq openwith-associations
          (list (list (openwith-make-extension-regexp '("jpg" "jpeg" "png"))
                      "feh -FY" '(file))
                (list (openwith-make-extension-regexp '("flac" "mp3" "wav" "aiff" "m4a" "aac"))
                      "mpv" '(file))
                (list (openwith-make-extension-regexp '("avi" "flv" "mov" "mp4" "rmvb" "m2ts" "webm"
                                                        "mpeg" "mpg" "ogg" "wmv" "mkv"))
                      "mpv" '(file))
                ))))

(use-package peep-dired
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

(defun edit-current-file-as-root ()
  "Edit the file that is associated with the current buffer as root"
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
        (find-file file))
    (message "Current buffer does not have an associated file.")))
(bind-key "C-c fR" 'edit-current-file-as-root)

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
(bind-key "C-c fD" 'delete-current-buffer-file)

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
(bind-key "C-c fr"  'rename-current-buffer-file)

(defun clone-file-and-open (filename)
  "Clone the current buffer writing it into FILENAME and open it"
  (interactive "FClone to file: ")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) filename nil nil nil 'confirm))
  (find-file filename))
(bind-key "C-c fc"  'clone-file-and-open)

(defun set-file-executable()
  "Add executable permissions on current file."

  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))
(bind-key "C-c fx" 'set-file-executable)

(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files

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
  (async-wpub url nil t))

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

(defun send-url-or-org-to-kindle ()
  "Send content of current buffer to kindle"

  (interactive)
  (cond
   ((not (eq (eww-current-url) nil)) (eww-to-kindle-directly))
   ((eq (current-buffer-mode) 'org-mode) (org-to-kindle))
   ((eq (current-buffer-mode) 'elfeed-search-mode) (elfeed-search-to-kindle))
   ((eq (current-buffer-mode) 'elfeed-show-mode) (elfeed-show-to-kindle))
   (t (message "%s" "Can't send this page to kindle."))))

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
  (interactive)

  (let ((old-buffer-name (buffer-name))
        (new-directory-name (counsel-goto-recent-directory)))
    (message "open recent dir old:%s new:%s" old-buffer-name new-directory-name)
    (if (eq (count-windows) 1)
        (split-window-right))

    (switch-to-buffer old-buffer-name)
    (other-window 1)
    (golden-ratio)
    (find-file new-directory-name)))

(bind-key "C-c ff" 'find-file-usually-files)

(defun find-file-usually-files ()
  (interactive)
  (let ((usually-files (list user-emacs-init-org
                             emacs-init-el
                             "*scratch*"
                             "*Org Src emacs-init.org[ emacs-lisp ]*"
                             "*Messages*"
                             )))

    (ivy-read "find file:" usually-files
              :action #'find-file-or-buffer)))

(defun find-file-or-buffer (file-or-buffer-name)
  (if (get-buffer file-or-buffer-name)
      (switch-to-buffer file-or-buffer-name)
    (if (file-exists-p file-or-buffer-name)
        (find-file file-or-buffer-name)
      (message "Not found %s" file-or-buffer-name))
    ))

(use-package multi-term
  :demand popwin
  :bind (("C-x t" . multi-term-next)
         ("C-x T" . multi-term)
         ("C-!" . popwin-term:multi-term))
  :init
  (setq multi-term-program "/bin/zsh")
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
              (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
              (setq term-buffer-maximum-size 10000)
              (define-key term-raw-map (kbd "C-y") 'term-paste)
              ))
  (defun popwin-term:multi-term ()
    (interactive)
  
    (popwin:display-buffer-1
     (or (get-buffer "*terminal*")
         (save-window-excursion
           (call-interactively 'multi-term)))
     :default-config-keywords '(:position :bottom :height 12 :stick t))))

(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument. The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "endless/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))

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

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-history-directory-alist
          `(("." . ,(file-name-as-directory (expand-file-name "undo-tree" user-cache-directory)))))
    (setq undo-tree-auto-save-history t)))

(bind-key "M-SPC" 'set-mark-command)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(defun endless/comment-line (n)
  "Comment or uncomment current line and leave point after it.

With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."

  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

(bind-key "M-;" #'endless/comment-line)

(use-package smartparens-config
  :ensure smartparens
  :init
  (progn
    (smartparens-global-mode t)
    (show-smartparens-global-mode t))
  :config
  (progn
    (diminish 'smartparens-mode "ⓟ")
    ))

(use-package aggressive-indent
  :diminish " Ⓘ"
  :init
  (global-aggressive-indent-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ))

(use-package iedit)

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

(use-package ivy
  :diminish ""
  :bind
  (("C-c C-r" . ivy-resume)
   :map ivy-minibuffer-map
   ("M-." . ivy-next-history-element))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (setq completion-in-region-function 'ivy-completion-in-region)))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package counsel
  :ensure ag
  :bind
  (("M-y" . counsel-yank-pop)
   ("M-x" . counsel-M-x)
   ("C-x f" . counsel-recentf)
   ("C-x l" . counsel-locate)
   ("C-c k" . counsel-ag)
   ("C-c j" . counsel-git-grep)
   ("C-:" . counsel-company)
   ("C-x C-\\" . counsel-goto-recent-directory)
   ("C-x \\" . counsel-goto-recent-directory-other-window)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package flycheck
  :diminish (flycheck-mode . " ⓢ")
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (setq flycheck-indication-mode 'right-fringe)
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c v"))
    (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)))

(use-package ispell
  :config                               ;
  (progn
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args   '("--sug-mode=ultra"
                                  "--lang=en_US"
                                  "--ignore=2"
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
  :demand (flyspell ivy flyspell-correct)
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

(use-package company
  :diminish " ⓐ"
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (global-company-mode t)

    (setq company-idle-delay 0.1)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 2)
    (setq company-echo-delay 0)

    (add-to-list 'company-backends 'company-ispell t)))

(use-package company-quickhelp
  :demand company
  :init (company-quickhelp-mode 1))

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

(unbind-key "C-\\")
(unbind-key "C-z")
(unbind-key "C-x C-z")
(unbind-key "C-x m")

(use-package avy
  :bind (("M-g z" . avy-goto-char)
         ("M-g f" . avy-goto-line)))

(use-package ace-pinyin
  :demand avy
  :diminish ""
  :config
  (progn
    (ace-pinyin-global-mode +1)
    (setq ace-pinyin-simplified-chinese-only-p nil)))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."

  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(bind-key [home] 'smart-beginning-of-line)
(bind-key* "C-a" 'smart-beginning-of-line)

(use-package link-hint
  :bind ("C-c ol" . link-hint-open-link)
  :init
  (dolist (map (list help-mode-map Info-mode-map))
      (bind-key "f" 'link-hint-open-link map)))

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

(defun call-meld ()
  (interactive)
  (shell-command (format "meld %s" (buffer-file-name)))
  (winner-undo)
  )
(bind-key "C-c fd" 'call-meld)

(use-package diff-hl
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

(use-package eww
  :defer 0
  :bind (("C-c b" . browse-at-point))
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  
  (setq browse-url-generic-program (executable-find "google-chrome-stable")
        shr-external-browser 'browse-url-generic)
  (setq eww-search-prefix "https://www.google.com/search?q=")
  (setq shr-color-visible-luminance-min 70)
  (add-hook 'eww-mode-hook '(lambda ()
                              (toggle-truncate-lines nil)))
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
             ("ok" . eww-to-kindle-directly)
             ("oo" . eww-to-org)
             ("yy" . eww-copy-page-url)
             ("yt" . eww-copy-current-title)
             ("yo" . eww-copy-title-url-org)
             ("yu" . eww-copy-title-url)))

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
  "browse at point: selected region or url or single word
if cursor at whitespace then call eww wait for input some thing."

  (interactive "P")

  (if (consp arg)
      (call-interactively 'eww)
    (let ((thing (my/thing-at-point-to-eww)))
      (if (string= "" thing)
          (call-interactively 'eww)
        (eww thing)))))

(use-package org-eww
  :ensure nil
  :config
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
      )))



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
  (yank-url-or-tilte (org-make-link-string (eww-current-url) (eww-current-title))))

(defun eww-copy-current-source ()
  (interactive)

  (yank-url-or-title (eww-current-source)))

(use-package elfeed
  :bind ("C-c e" . elfeed)
  :config
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
  
  ;; (push '(it elfeed-search-it-title-face) elfeed-search-face-alist)
  ;; (push '(emacs elfeed-search-it-title-face) elfeed-search-face-alist)
  ;; (push '(finace elfeed-search-finace-title-face) elfeed-search-face-alist)
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  (push '(star elfeed-search-starred-title-face) elfeed-search-face-alist)
  (push '(mustread elfeed-search-mustread-title-face) elfeed-search-face-alist)
  ;; (push '(read elfeed-search-read-title-face) elfeed-search-face-alist)
  ;; (push '(junk elfeed-search-junk-title-face) elfeed-search-face-alist)
  
  ;; (theme-color
  ;; (background . "#1d1f21")
  ;; (current-line . "#282a2e")
  ;; (selection . "#373b41")
  ;; (foreground . "#c5c8c6")
  ;; (comment . "#969896")
  ;; (red . "#cc6666")
  ;; (orange . "#de935f")
  ;; (yellow . "#f0c674")
  ;; (green . "#b5bd68")
  ;; (aqua . "#8abeb7")
  ;; (blue . "#81a2be")
  ;; (purple . "#b294bb"))
  
  (setq elfeed-use-curl t)
  (add-hook 'elfeed-search-mode-hook
            '(lambda() (progn
                         (elfeed-update))))
  (add-hook 'elfeed-new-entry-hook
            '(lambda() (progn
                         (elfeed-make-tagger :before "1 week ago"
                                             :remove 'unread))))
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
             ("R" . elfeed-search-mark-all-read)
             ("B" . elfeed-search-external-browser)
             ("C-<return>" . elfeed-search-browse-url)
             ("yy" . copy-elfeed-search-url)
             ("yt" . copy-elfeed-search-title)
             ("yu" . copy-elfeed-search-title-url)
             ("yo" . copy-elfeed-search-org)
             ("w" . copy-elfeed-search-url)
             ("ok" . elfeed-search-to-kindle))
  
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
             ("yy" . copy-elfeed-show-url)
             ("yt" . copy-elfeed-show-title)
             ("yu" . copy-elfeed-show-title-url)
             ("yo" . copy-elfeed-show-org)
             ("w" . copy-elfeed-show-url)
             ("ok" . elfeed-show-to-kindle))
  (setq elfeed-search-filter "@1-weeks-ago +unread")
  (defun elfeed-search-mark-all-read ()
    "mark all feed as read in elfeed search mode"
  
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  
  (defun elfeed-entry-url (entry)
    (and entry (elfeed-entry-link entry)))
  
  (defun elfeed-link-title (entry)
    (and entry (elfeed-entry-title entry)))
  
  (defun elfeed-entry-title-url (entry)
    (concat (elfeed-link-title entry) "\n" (elfeed-entry-url entry)))
  
  (defun elfeed-entry-link-org (entry)
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
    "copy url to clipboard"
  
    (interactive)
    (yank-url-or-title (get-elfeed-show-url)))
  
  (defun copy-elfeed-show-title ()
    "copy url to clipboard"
  
    (interactive)
    (yank-url-or-title (get-elfeed-show-title)))
  
  (defun copy-elfeed-show-title-url ()
    "copy title and url to clipboard"
  
    (interactive)
    (yank-url-or-title (get-elfeed-show-title-url)))
  
  (defun copy-elfeed-show-org ()
    "copy title and url to clipboard"
  
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
  )

(defun elfeed-reset-to-default-filter ()
  (interactive)
  (elfeed-search-set-filter "@1-week-ago +unread"))

(defun my/cycle-elfeed-search-filter ()
  (interactive)

  (let* ((filters ["" "+emacs" "+it" "+finace" "-news" "+news1" "+news2"])
         (index-before
          (if (get 'my/cycle-elfeed-search-filter 'filter-index)
              (get 'my/cycle-elfeed-search-filter 'filter-index)
            0))
         (index-after (% (+ index-before 1) (length filters)))
         (next-filter (format "@1-week-ago %s +unread" (aref filters index-after))))

    (put 'my/cycle-elfeed-search-filter 'filter-index index-after)
    (elfeed-search-set-filter next-filter)
    (beginning-of-buffer)
    ))

(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup))

(use-package elfeed-org
  :init
  (progn
    (setq rmh-elfeed-org-files (list "~/.elfeed.org"))
    (elfeed-org)))

(use-package esqlite
  :config
  (progn
    (defun insert-stock-id-at-point ()
      "Insert stock ids at point."

      (interactive)

      (if (string= ".elfeed.org" (buffer-name))
          (let* ((db-name "/mnt/lvm-data/Programing/stock4/db/stock2.db")
                 (sql-fetch-stock-ids "select s.sid, i.name from summary as s, stock_info as i where s.sid=i.sid and s.userid=\"mariolong\""))

            (dolist (rec (esqlite-read db-name sql-fetch-stock-ids))
              (insert (format "*** [[http://news.wearn.com/rss/%s][%s-%s]]\n"
                              (s-trim (car rec))
                              (s-trim (car rec))
                              (s-trim (second rec))))))

        (message "%s" "Must in '.elfeed.org'")))

    (bind-key "C-c fi" 'insert-stock-id-at-point)
    ))

(use-package hangups
  :load-path
  :ensure adaptive-wrap
  :bind (("C-c h" . hangups-open-conversation-small-p)
         :map hangups-conv-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("l" . forward-char)
         ("h" . backward-char)
         ("d" . scroll-up-command)
         ("e" . scroll-down-command)
         ("o" . browse-at-point))
  :config
  (progn
    (defun hangups-open-conversation-small-p ()
      "Open conversation to small-p."
      (interactive)

      (hangups)
      (let ((name "小p_葉"))
        (switch-to-buffer (get-buffer-create (hangups/buffer-name name)))
        (hangups-conv-mode)
        (hangups-conversation name hangups-messages))

      (add-hook 'hangups-list-mode-hook
                (lambda () (run-with-timer 120 (* 1 60) 'hangups-list-refresh)))

      (spaceline-define-segment hangups-lighter
        "hangups conversations"
        (when (> hangups/convs-unread 0)
          "\u2706")
        :face font-lock-warning-face)

      (spaceline-spacemacs-theme '(hangups-lighter)))
    ))

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-src-fontify-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil
        org-startup-indented t)
  (add-to-list 'org-structure-template-alist
               '("T" "#+TITLE:?\n#+AUTHOR:\n#+DATE:"))
  (add-to-list 'org-structure-template-alist
               '("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
  (add-to-list 'org-structure-template-alist
               '("S" "#+BEGIN_SRC sh\n?\n#+END_SRC"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (ditaa . t)
     (dot . t)
     (js . t)
     (latex . t)
     (perl . t)
     (python . t)
     (ruby . t)
     (sh . t)
     (clojure . t)
     ))
  (setq org-agenda-window-setup 'current-window)
  (add-hook 'org-agenda-mode-hook 'hl-line-mode)
  
  (setq org-directory "~/Dropbox/GTD")
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-default-ideas-file (expand-file-name "0.Tasks.org" org-directory))
  
  (setq org-capture-templates
        '(("t" "Todo" entry (file org-default-ideas-file)
           "* TODO %?\n%T" :prepend t)
          ("e" "Emacs" entry (file+headline org-default-notes-file "emacs")
           "* TODO %?\n%T")
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?\n%T")
          ))
  
  (setq org-agenda-files (list org-default-notes-file
                               org-default-ideas-file))
  
  (setq org-refile-targets '((nil :maxlevel . 2)              ; refile to headings in the current buffer
                             (org-agenda-files :maxlevel . 1) ; refile to any of these files
                             ))
  
  (add-hook 'org-agenda-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-g") 'org-agenda-exit)))
  
  (bind-key "C-c l" 'org-store-link)
  (bind-key "C-c c" 'org-capture)
  (bind-key "C-c a" 'org-agenda)
  (unbind-key "C-c [")
  (unbind-key "C-c ]"))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package ox-reveal
  :ensure htmlize
  :config
  (progn
    (setq org-reveal-root "file:///mnt/lvm-data/github/reveal.js")))

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
  (interactive)

  (dolist (status '("/DONE" "/CANCELED"))
    (org-map-entries (lambda ()
                       (org-archive-subtree)
                       (setq org-map-continue-from (outline-previous-heading)))
                     status 'file)))

(use-package elpy
  :ensure t
  :demand (flycheck company yasnippet)
  :diminish "☀"
  :config
  (progn
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")

    (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
    (eval-after-load 'flycheck
      '(progn
         (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
         (add-hook 'elpy-mode-hook 'flycheck-mode)))

    (add-to-list 'elpy-modules 'elpy-module-company)
    (add-to-list 'elpy-modules 'elpy-module-yasnippet)

    (define-key python-mode-map (kbd "RET")
      'newline-and-indent)

    (add-hook 'python-mode-hook
              (lambda ()
                (set (make-local-variable 'comment-inline-offset) 2)))
    ))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
  (setq web-mode-engines-alist  '(("mako" . "\\.tmpl\\'")))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)

  (defun my-web-mode-hook ()
    "Hooks for Web mode."

    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 2))

  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package emmet-mode
  :ensure t
  :init
    (progn
      (add-hook 'sgml-mode-hook 'emmet-mode)
      (add-hook 'css-mode-hook  'emmet-mode)
      (add-hook 'emmet-mode-hook
                (lambda () (setq emmet-indentation 4)))))

(use-package js2-mode
  :ensure t
  :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
      (add-hook 'js2-mode-hook 'flycheck-mode)))

(use-package json-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines nil)))

(use-package systemd)

(use-package markdown-mode
  :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
      (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))))

(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))

(use-package haskell-mode
  :demand (flycheck flycheck-haskell)
  :mode "\\.hs$" "\\.l?hs$"
  :config
  (progn (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
         (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
         (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

         (defun my-haskell-hook ()
           (setq mode-name " λ ")

           (turn-on-haskell-doc)
           (diminish 'haskell-doc-mode "")
           (capitalized-words-mode)

           (diminish 'capitalized-words-mode "")
           (turn-on-eldoc-mode)

           (diminish 'eldoc-mode "")
           (turn-on-haskell-decl-scan)
           (setq evil-auto-indent nil))

         ;;(setq haskell-font-lock-symbols 'unicode)
         ;;(setq haskell-literate-default 'tex)
         ;;(setq haskell-stylish-on-save t)
         ;;(setq haskell-tags-on-save t)
         (add-hook 'haskell-mode-hook 'my-haskell-hook)))

(use-package flycheck-haskell
  :config (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package nginx-mode)

(bind-key "C-c d" 'kid-sdcv-to-buffer)

(defun kid-sdcv-to-buffer ()
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "Search the dictionary for (default %s): " word)
                            nil nil word))

    (set-buffer (get-buffer-create "*sdcv*"))
    (buffer-disable-undo)
    (erase-buffer)

    (let ((process (start-process-shell-command "sdcv" "*sdcv*" "sdcv" "-n" word)))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (string= (buffer-name) "*sdcv*")
             (setq kid-sdcv-window-configuration (current-window-configuration))
             (switch-to-buffer-other-window "*sdcv*")
             (local-set-key (kbd "d") 'kid-sdcv-to-buffer)
             (local-set-key (kbd "q") (lambda ()
                                        (interactive)
                                        (bury-buffer)
                                        (unless (null (cdr (window-list))) ; only one window
                                          (delete-window)))))
           (goto-char (point-min))))))))

(use-package moedict
  :load-path
  :ensure helm
  :ensure esqlite
  :bind ("C-c m" . moedict)
  :config
  
  (setq browse-url-chromium-program "google-chrome-stable")
  
  (defun counsel-moedict ()
    "moedict"
  
    (interactive)
  
    (let ((initial-input (word-at-point)))
      (ivy-read moedict-prompt
                '(lambda (x) (counsel-moedict-function x))
                :initial-input initial-input
                :dynamic-collection t
                :action #'moedict-lookup-and-show-in-buffer
                )))
  
  (defun counsel-moedict-function (&optional string)
    (if (null string)
        (setq string ""))
    (or (unless (string= "" string)
          (moedict-get-candidates-list string))
        (list nil))))

(use-package pdf-tools
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("H" . pdf-view-next-page)
              ("L" . pdf-view-previous-page)
              ("e" . pdf-view-scroll-down-or-previous-page)
          ("u" . pdf-view-scroll-down-or-previous-page)
              ("d" . pdf-view-scroll-up-or-next-page)
              ("=" . pdf-view-fit-width-to-window)
              ("-" . pdf-view-shrink)
              ("+" . pdf-view-enlarge))
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
    (setq pdf-view-continuous t)
    ))

(diminish 'overwrite-mode)
(diminish 'subword-mode)
(eval-after-load 'org-indent
  '(diminish 'org-indent-mode))
(diminish 'org-src-mode)

(use-package server
:config
  (unless (server-running-p)
    (server-start)))
