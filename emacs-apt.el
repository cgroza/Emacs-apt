(defvar cache "cache")
(defvar get "get")

(defun clear-buffer ()
  "Sets buffer-read-only to nil and deletes region point-min point-max"
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max)))

;; The apt-[task] functions take the necessary input and passes it to 
;; apt-command together with the respective module and task (install,
;; download etc).

(defun apt-search (names)
  (interactive "sapt-cache search ")
  (apt-command cache "search" names))

(defun apt-download (names)
  (interactive "apt-get download ")
  (apt-command get "download" names)  )

(defun apt-changelog (names)
  (interactive "sapt-get changelog ")
  (apt-command get "changelog" names))

(defun apt-source (names)
  (interactive "apt-get source ")
  (apt-command get "source" names))

(defun apt-showpkg (names)
  (interactive "sapt-cache showpkg ")
  (apt-command cache "showpkg" names))

(defun apt-stats ()
  (apt-command cache "apt-stats"))

(defun apt-showsrc (names)
  (apt-command cache "showsrc" names))

(defun apt-dump (names)
  (apt-command cache "dump"))

(defun apt-dumpavail (names)
  (apt-command cache "dumpvail"))

(defun apt-depends (names)
  (interactive "sapt-cache depends ")
  (apt-command cache "depends" names))

(defun apt-rdepends (names)
  (interactive "sapt-cache rdepends ")
  (apt-command cache "redepends" names))

(defun apt-pkgnames (prefix)
  (interactive "sapt-cache pkgnames ")
  (apt-command cache "pkgnames" prefix))

(defun apt-dotty (names)
  (interactive "sapt-cache dotty ")
  (apt-command cache "dotty" names))

(defun apt-xvcg (names)
  (interactive "sapt-cache xvcg ")
  (apt-command cache "xvcg" names))

(defun apt-policy (names)
  (interactive "sapt-cache policy ")
  (apt-command cache "policy" names))

(defun apt-madison (names)
  (interactive "sapt-cache madison ")
  (apt-command cache "madison" names))

(defun apt-command (module command &optional package-names) 
  (let ((buf (get-buffer-create (format "*APT: %s%s" package-names "*"))))
    (set-buffer buf)
    (clear-buffer)
    (apply 'start-process "emacs-apt"
		   buf			    ;output will be directed there
		   (format "apt-%s" module) ;construct apt command
		   command
		   ;; split package list and pass it as arguments
		   (split-string package-names "\s+"))
    (setq buffer-read-only t)
    (switch-to-buffer-other-window buf)
    buf))
