;; Copyright (C) 2012 Groza Cristian
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(require 'emacs-apt)

;; syntactic sugar
(defun pkg-at-point () (thing-at-point 'symbol))

;; wrapers around the normal functions that call
;; equivalents with the symbol at point
(defun apt-mode-search ()
  "Calls apt-search with symbol at point."
  (interactive)
  (apt-search (pkg-at-point)))

(defun apt-mode-download (download-dir)
  "Calls apt-download with symbol at point."
  (interactive "DDownload location: ")
  (apt-download (pkg-at-point) download-dir))

(defun apt-mode-change-log ()
  "Calls apt-change-log with symbol at point."
  (interactive)
  (apt-change-log (pkg-at-point)))

(defun apt-mode-source (download-dir)
  "Calls apt-source with symbol at point."
  (interactive "DDownload location: ")
  (apt-source (pkg-at-point) download-dir))

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

;; define apt-mode
(define-derived-mode apt-mode
  text-mode "APT"
  "Major mode for apt-get.
          \\{apt-mode-map}"
  (setq case-fold-search nil))

;; User should define their own key bindings
;; mode key bindings
;; (define-key apt-mode-map "\C-cs" 'apt-mode-search)
;; (define-key apt-mode-map "\C-cS" 'apt-mode-source)
;; (define-key apt-mode-map "\C-cp" 'apt-mode-pkgnames)
;; (define-key apt-mode-map "\C-cP" 'apt-mode-policy)
;; (define-key apt-mode-map "\C-ch" 'apt-mode-showpkg)
;; (define-key apt-mode-map "\C-ct" 'apt-mode-dotty)
;; (define-key apt-mode-map "\C-cx" 'apt-mode-xvcg)
;; (define-key apt-mode-map "\C-cc" 'apt-mode-change-log)
;; (define-key apt-mode-map "\C-cd" 'apt-mode-depends)
;; (define-key apt-mode-map "\C-cD" 'apt-mode-download)
;; (define-key apt-mode-map "\C-cr" 'apt-mode-rdepends)
;; (define-key apt-mode-map "\C-cm" 'apt-mode-madison)

(provide 'apt-mode)
