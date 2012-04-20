(defun apt-search (package-names)
  (interactive "sSearch for packages: ")
  (let ((buf (get-buffer-create (format "*APT-SEARCH: %s%s" package-names "*"))))
    (set-buffer buf)
    (start-process "apt-get" 
		   buf
		   "apt-cache"
		   "search"
		   package-names)
    (setq buffer-read-only t)
    (switch-to-buffer-other-window buf)
))
