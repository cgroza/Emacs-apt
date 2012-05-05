(defvar apt-mode-hook nil)

(defun pkg-at-point () (thing-at-point 'symbol))

(defun apt-mode-search ()
  (interactive)
  (apt-search (pkg-at-point)))

(defun apt-mode-download ()
  (interactive)
  (apt-download (pkg-at-point)))

(defun apt-mode-change-log ()
  (interactive)
  (apt-change-log (pkg-at-point)))

(defun apt-mode-source ()
  (interactive)
  (apt-source (pkg-at-point)))

(defun apt-mode-showpkg ()
  (interactive)
  (apt-showpkg (pkg-at-point)))

(defun apt-mode-depends ()
  (interactive)
  (apt-depends (pkg-at-point)))

(defun apt-mode-rdepends ()
  (interactive)
  (apt-rdepends (pkg-at-point)))

(defun apt-mode-pkgnames ()
  (interactive)
  (apt-pkgnames (pkg-at-point)))

(defun apt-mode-dotty ()
  (interactive)
  (apt-dotty (pkg-at-point)))

(defun apt-mode-xvcg ()
  (interactive)
  (apt-xvcg (pkg-at-point)))

(defun apt-mode-policy ()
  (interactive)
  (apt-policy (pkg-at-point)))

(defun apt-mode-madison ()
  (interactive)
  (apt-madison (pkg-at-point)))

(defvar apt-mode-map 
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'apt-mode-search)
    map)
  "Keymap for APT major mode.")

(defun apt-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map apt-mode-map)
  (setq major-mode 'apt-mode)
  (setq mode-name "APT")
  (run-hooks 'apt-mode-hook))

(provide 'apt-mode)