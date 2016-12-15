;; Simple make init.el config
;; copy from https://github.com/coldnew/coldnew-emacs
(setq user-emacs-directory (file-name-directory (or load-file-name (buffer-file-name))))
(set-default 'temporary-file-directory (expand-file-name "tmp" user-emacs-directory))
(defconst user-emacs-init-org (expand-file-name "emacs-init.org" user-emacs-directory))
(defconst user-emacs-init-el (expand-file-name "init.el" user-emacs-directory))

;; Functions
(defun find-tmpfile-path ()
  "Find suitable tmpfile path."
  (let ((my-ramdisk "/Volumes/ramdisk"))
    ;; I always create `/Volumes/ramdisk' for ramdisk usage on OSX
    (if (and (eq system-type 'darwin) (file-exists-p my-ramdisk))
        my-ramdisk
      temporary-file-directory)))

;; Generate init.el method
(defun make-init-el ()
  (require 'org)
  (setq org-confirm-babel-evaluate nil)
  (setq org-confirm-execute-src-block nil)

  ;; create a special dir for building my new init.el
  (let* ((pid (number-to-string (emacs-pid))) ; string
         (workdir (concat (find-tmpfile-path) "/.init.el/"))
         (lockfile (concat workdir "lock"))
         (init-el (concat workdir (concat "init.el." pid))))

    ;; create dir if not exist
    (if (not (file-exists-p workdir))
        (make-directory workdir :parents))

    ;; Write current pid for lock
    (with-temp-buffer
      (insert pid)
      (write-file lockfile))

    ;; tangle init.el
    (org-babel-tangle-file user-emacs-init-org init-el "emacs-lisp")

    ;; Since tnagle may use so many time, check if lock file still has
    ;; the same pid as this process
    (if (string= pid
                 (with-temp-buffer
                   (insert-file lockfile)
                   (buffer-string)))
        ;; when pid the same, create our init.el
        (rename-file init-el user-emacs-init-el t)
      ;; else just kill old one
      (delete-file init-el nil))))
