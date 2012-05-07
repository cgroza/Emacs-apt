(defvar apt-mode-hook nil)

(defun pkg-at-point () (thing-at-point 'symbol))

(defun apt-mode-search ()
  "Calls apt-search with symbol at point."
  (interactive)
  (apt-search (pkg-at-point)))

(defun apt-mode-download ()
  "Calls apt-download with symbol at point."
  (interactive)
  (apt-download (pkg-at-point)))

(defun apt-mode-change-log ()
  "Calls apt-change-log with symbol at point."
  (interactive)
  (apt-change-log (pkg-at-point)))

(defun apt-mode-source ()
  "Calls apt-source with symbol at point."
  (interactive)
  (apt-source (pkg-at-point)))

(defun apt-mode-showpkg ()
  "Calls apt-showpkgs with symbol at point."
  (interactive)
  (apt-showpkg (pkg-at-point)))

(defun apt-mode-depends ()
  "Calls apt-depends with symbol at point."
  (interactive)
  (apt-depends (pkg-at-point)))

(defun apt-mode-rdepends ()
  "Calls apt-rdepends with symbol at point."
  (interactive)
  (apt-rdepends (pkg-at-point)))

(defun apt-mode-pkgnames ()
  "Calls apt-pkgnames with symbol at point."
  (interactive)
  (apt-pkgnames (pkg-at-point)))

(defun apt-mode-dotty ()
  "Calls apt-dotty with symbol at point."
  (interactive)
  (apt-dotty (pkg-at-point)))

(defun apt-mode-xvcg ()
  "Calls apt-xvcg with symbol at point."
  (interactive)
  (apt-xvcg (pkg-at-point)))

(defun apt-mode-policy ()
  "Calls apt-policy with symbol at point."
  (interactive)
  (apt-policy (pkg-at-point)))

(defun apt-mode-madison ()
  "Calls apt-madison with symbol at point."
  (interactive)
  (apt-madison (pkg-at-point)))

(define-derived-mode apt-mode
  nil "APT"
  "Major mode for apt-get.
          \\{apt-mode-map}"
  (setq case-fold-search nil))

(define-key apt-mode-map "\C-cs" 'apt-mode-search)
(define-key apt-mode-map "\C-cS" 'apt-mode-source)
(define-key apt-mode-map "\C-cp" 'apt-mode-pkgnames)
(define-key apt-mode-map "\C-cP" 'apt-mode-policy)
(define-key apt-mode-map "\C-ch" 'apt-mode-showpkg)
(define-key apt-mode-map "\C-ct" 'apt-mode-dotty)
(define-key apt-mode-map "\C-cx" 'apt-mode-xvcg)
(define-key apt-mode-map "\C-cc" 'apt-mode-change-log)
(define-key apt-mode-map "\C-cd" 'apt-mode-depends)
(define-key apt-mode-map "\C-cD" 'apt-mode-download)
(define-key apt-mode-map "\c-cr" 'apt-mode-rdepends)
(define-key apt-mode-map "\C-cm" 'apt-mode-madison)

(defun apt-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map apt-mode-map)
  (setq major-mode 'apt-mode)
  (setq mode-name "APT")
  (run-hooks 'apt-mode-hook))

(provide 'apt-mode)