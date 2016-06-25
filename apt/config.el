(require 'apt-mode)

(defun apt/evilify-apt-mode ()
  "Sets up apt-mode maps"
  (evilified-state-evilify-map apt-mode-map
    :mode apt-mode))

(apt/evilify-apt-mode)
