(defconst cache "cache" 
  "For use inside functions to indicate apt module.")

(defconst get "get" 
  "For use inside functions to indicate apt module.")

(defvar apt-hi-color 
  "*Indicates color to be used for hightlighting.")

(defun apt-set-hi-color (hi-color)
  "apt-set-hi-color sets apt-hicolor to the value of hi-color."
  (set 'apt-hi-color hi-color))

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

(defun apt-stats (i)
  (interactive "i")
  (apt-command cache "stats"))

(defun apt-showsrc (i)
  (interactive "i")
  (apt-command cache "showsrc"))

(defun apt-dump (i)
  (interactive "i")
  (apt-command cache "dump"))

(defun apt-dumpavail (i)
  (interactive "i")
  (apt-command cache "dumpavail"))

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

(defun apt-command (module command &optional package-names high-light-function)
  "module - cahce or get
   command - apt command such as search or pkgnames
   package-names - string containing list of packages separated by spaces
   
   This function calls apt-cache or apt-get using call-process and returns
   the output in a buffer. Emacs may freeze until the command has finished."
  (let ((prev-buf (current-buffer))
	(package-list (split-string (or package-names "") "\s+"))
	(buf (get-buffer-create (format "*APT-%s %s %s%s" 
	      (upcase module) (upcase command) package-names "*"))))

    (set-buffer buf)
    (clear-buffer)
    (apply 'call-process
	   ;construct apt command
	   (format "apt-%s" module) 
	   nil	    ;/dev/null
	   buf      ;output will be directed there
	   nil
	   command  ;apt command
	   ;; split package list and pass it as arguments
	   package-list)

    (switch-to-buffer-other-window buf)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (if high-light-function 
	(funcall high-light-function)
      (highlight-regexp (regexp-opt package-list) apt-hi-color))
    (switch-to-buffer-other-window prev-buf)
    buf))