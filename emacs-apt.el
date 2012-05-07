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

(require 'apt-mode)

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
  "Invokes apt-cache search {names} and outputs the result."
  (interactive "sapt-cache search ")
  (apt-command cache "search" names))

(defun apt-download (names)
  "Invokes apt-get download {names} and outputs the result."
  (interactive "apt-get download ")
  (apt-command get "download" names)  )

(defun apt-changelog (names)
  "Invokes apt-get changelog {names} and outputs the result."
  (interactive "sapt-get changelog ")
  (apt-command get "changelog" names))

(defun apt-source (names)
  "Invokes apt-get source {names} and outputs the result."
  (interactive "apt-get source ")
  (apt-command get "source" names))

(defun apt-showpkg (names)
  "Invokes apt-cache showpkg {names} and outputs the result."
  (interactive "sapt-cache showpkg ")
  (apt-command cache "showpkg" names))

(defun apt-stats (i)
  "Invokes apt-cache stats and outputs the result."
  (interactive "i")
  (apt-command cache "stats"))

(defun apt-showsrc (i)
  "Invokes apt-cache showsrc and outputs the result."
  (interactive "i")
  (apt-command cache "showsrc"))

(defun apt-dump (i)
  "Invokes apt-cache dump and outputs the result."
  (interactive "i")
  (apt-command cache "dump"))

(defun apt-dumpavail (i)
  "Invokes apt-cache dumpavail and outputs the result."
  (interactive "i")
  (apt-command cache "dumpavail"))

(defun apt-depends (names)
  "Invokes apt-cache depends {names} and outputs the result."
  (interactive "sapt-cache depends ")
  (apt-command cache "depends" names))

(defun apt-rdepends (names)
  "Invokes apt-cache rdepends {names} and outputs the result."
  (interactive "sapt-cache rdepends ")
  (apt-command cache "redepends" names))

(defun apt-pkgnames (prefix)
  "Invokes apt-cache pkgnames {prefix} and outputs the result."
  (interactive "sapt-cache pkgnames ")
  (apt-command cache "pkgnames" prefix))

(defun apt-dotty (names)
  "Invokes apt-cache dotty {names} and outputs the result."
  (interactive "sapt-cache dotty ")
  (apt-command cache "dotty" names))

(defun apt-xvcg (names)
  "Invokes apt-cache xvcg {names} and outputs the result."
  (interactive "sapt-cache xvcg ")
  (apt-command cache "xvcg" names))

(defun apt-policy (names)
  "Invokes apt-cache policy {names} and outputs the result."
  (interactive "sapt-cache policy ")
  (apt-command cache "policy" names))

(defun apt-madison (names)
  "Invokes apt-cache madison {names} and outputs the result."
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
    (apt-mode)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (if high-light-function 
	(funcall high-light-function)
      (highlight-regexp (regexp-opt package-list) apt-hi-color))
    (switch-to-buffer-other-window prev-buf)
    buf))