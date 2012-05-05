(defvar apt-mode-hook nil)

(defun apt-mode-search ()
  (interactive)
  (message "APT-MODE-SEARCH HAS BEEN CALLED"))

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
  (run-hooks 'apt-mode-hook)
  (provide 'apt-mode))