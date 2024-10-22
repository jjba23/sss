;;; autostarts.lisp - StumpWM Configurations

;; Copyright (C) 2024 Josep Bigorra

;; Author: Josep Bigorra <jjbigorra@gmail.com>
;; Maintainer: Josep Bigorra <jjbigorra@gmail.com>
;; URL: https://github.com/jjba23/sss

;; sss is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sss is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sss.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(asdf:load-system "stumpwm")
(in-package :stumpwm)

(defvar sss-emacs-started nil)

(defvar sss-picom-started nil)

(defvar sss-lxsession-started nil)

;; TODO implement run-shell-command-once

(defun sss-run-autostarts ()
  (progn
    ;; (display "\n>>=  applying wallpaper...\n")
    (run-shell-command "feh --bg-scale ~/Ontwikkeling/Persoonlijk/sss/resources/wallpapers/3nt5e7.png")
    ;;(display "\n>>=  starting Emacs daemon...\n")
    (run-shell-command "emacs --daemon")
    (setf sss-emacs-started t)
    ;;(display "\n>>=  starting Picom compositor...\n")
    (run-shell-command "picom -b")
    (setf sss-picom-started t)
    ;;(display "\n>>=  applying xmodmap keyboard configurations...\n")
    (run-shell-command "xmodmap ~/.xmodmap")
    ;;(display "\n>>=  cursor tweaks...\n")
    (run-shell-command "xsetroot -cursor_name left_ptr")
    ;;(display "\n>>=  start LXSession...\n")
    (run-shell-command "lxsession --de=StumpWM &")
    (setf sss-lxsession-started t)
    ;;(display "\n>>=  start Conky...\n")
    ;;(run-shell-command "conky -d")
    ))
