(defvar cache "cache")
(defvar get "get")

(defun apt-search (names)
  (interactive "sSearch for packages: ")
  (apt-command cache "search" names))

(defun apt-download (names)
  (interactive "sPackages to download: ")
  (apt-command get "download" names)  )

(defun apt-changelog (names)
  (interactive "sChangelog for packages: ")
  (apt-command get "changelog" names))

(defun apt-source (names)
  (interactive "sSource for packages: ")
  (apt-command get "source" names))

(defun apt-showpkg (names)
  (interactive "sPackage to show: ")
  (apt-command cache "showpkg" names))

(defun apt-stats ()
  (apt-command cache "stats"))

(defun apt-showsrc (names)
  (apt-command cache "showsrc" names))

(defun apt-dump (names)
  (apt-command cache "dump"))

(defun apt-dumpavail (names)
  (apt-command cache "dumpvail"))

(defun apt-depends (names)
  (interactive "sPackage names: ")
  (apt-command cache "depends" names))

(defun apt-rdepends (names)
  (interactive "sPackage names: ")
  (apt-command cache "redepends" names))

(defun apt-pkgnames (prefix)
  (interactive "sFilter prefix: ")
  (apt-command cache "pkgnames" prefix))

(defun apt-dotty (names)
  (interactive "sPackage names: ")
  (apt-command cache "dotty" names))

(defun apt-xvcg (names)
  (interactive "sPackage names: ")
  (apt-command cache "xvcg" names))

(defun apt-policy (names)
  (interactive "sPackage names: ")
  (apt-command cache "policy" names))

(defun apt-madison (names)
  (interactive "sPackage names: ")
  (apt-command cache "madison" names))

(defun apt-command (module command &optional package-names) 
  (let ((buf (get-buffer-create (format "*APT: %s%s" package-names "*"))))
    (set-buffer buf)
    (apply 'start-process "emacs-apt"
		   buf
		   (format "apt-%s" module)
		   command
		   (split-string package-names "\s+"))
    (setq buffer-read-only t)
    buf))
