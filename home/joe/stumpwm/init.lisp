;;; init.lisp - StumpWM Configurations

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


(defvar user-name "Joe")
(defvar small-user-name "joe")
(defvar user-email "jjbigorra@gmail.com")

(defvar sss-guix-system-path "/run/current-system/profile/share/"
  "Define Guix System profile PATH.")

(defvar sss-stumpwm-module-dir (concat sss-guix-system-path "common-lisp/sbcl/")
  "Define the directory where StumpWM should load modules from.")

(defvar sss-mode-line-time-format
  (format nil "  %k:%M:%S %z - ~a @ SSS/Guix - %A, %d %B %Y   " small-user-name)
  "Date time format to be shown in the modeline, refer to the man pages for `date` for more information on the formats.")

(defvar sss-font-family "Intel One Mono"
  "Font family to be used as main for StumpWM")

;; Load StumpWM modules
(set-module-dir sss-stumpwm-module-dir)
(load-module "swm-gaps")
(load-module "cpu")
(load-module "mem")
(load-module "hostname")
(load-module "ttf-fonts")
(load-module "clx-truetype")
(load-module "stumptray")
(load-module "slynk")

;; Debugging
(defcommand sly-start-server () ()
  "Start a slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:create-server :dont-close t))))

(defcommand sly-stop-server () ()
  "Stop current slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:stop-server 4005))))

(defvar sss-guix-home-path "~/.guix-home/profile/share/"
  "Define Guix Home profile PATH.")

(defvar sss-stumpwm-data-dir (concat (getenv "XDG_CACHE_HOME") "/stumpwm/")
  "Define the directory where StumpWM should work with data.")

;; Load SSS modules
(load "~/.stumpwm.d/colors.lisp")
(load "~/.stumpwm.d/keymaps.lisp")

(setf *debug-level* 10)

;; Set prefix key, the leader key binding
(set-prefix-key (kbd "C-t"))

(defun show-ip-address ()
  (let ((ip (run-shell-command "ip addr | grep 'inet\s' | cut -d: -f2 | awk '{ print $2}' | xargs -n2 -d'\n'" t)))
    (substitute #\Space #\Newline ip)))

(defun show-battery-charge ()
  (let ((raw-battery (run-shell-command "acpi | cut -d, -f2" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun show-battery-state ()
  (let ((raw-battery (run-shell-command "acpi | cut -d: -f2 | cut -d, -f1" t)))
    (substitute #\Space #\Newline raw-battery)))

(defun show-kernel ()
  (let ((ip (run-shell-command "uname -r" t)))
    (substitute #\Space #\Newline ip)))

(defun show-current-volume ()
  (let ((vol (run-shell-command "" t)))
    (substitute #\Space #\Newline vol)))

(defun workspace-maker ()
  (progn
    (gnewbg " www")
    (gnewbg " console")
    (grename " emacs")
    (gnewbg " files")
    (gnewbg " priv")
    (gnewbg " audio")))

(defun sss-run-autostarts ()
  (progn
    ;; (display "\n>>=  applying wallpaper...\n")
    (run-shell-command "feh --bg-scale ~/Ontwikkeling/Persoonlijk/sss/resources/wallpapers/3nt5e7.png")
    ;;(display "\n>>=  starting Emacs daemon...\n")
    (run-shell-command "emacs --daemon")
    ;;(display "\n>>=  starting Picom compositor...\n")
    (run-shell-command "picom -b")
    ;;(display "\n>>=  applying xmodmap keyboard configurations...\n")
    (run-shell-command "xmodmap ~/.xmodmap")
    ;;(display "\n>>=  cursor tweaks...\n")
    (run-shell-command "xsetroot -cursor_name left_ptr")
    ;;(display "\n>>=  start LXSession...\n")
    (run-shell-command "lxsession --de=StumpWM &")
    ;;(display "\n>>=  start Conky...\n")
    ;;(run-shell-command "conky &")
    ))

(setf *mouse-focus-policy*    :click
      *float-window-modifier* :SUPER)

(setf *mode-line-position* :bottom)

(setq *colors*
      `(,sss-base   ;; 0 black
        ,sss-love  ;; 1 red
        ,sss-pine ;; 2 green
        ,sss-yellow  ;; 3 yellow
        ,sss-pine  ;; 4 blue
        ,sss-pine  ;; 5 magenta
        ,sss-foam   ;; 6 cyan
        ,sss-text)) ;; 7 white

(setf *normal-border-width*       0
      *float-window-border*       0
      *float-window-title-height* 15
      *window-border-style*       :none)

;; (load-module "battery-portable")
;; (load-module "wifi")

(setf *startup-message*
      (format nil "Master ~a | Welcome to StumpWM!" user-name))

;; Set visual preferences - color, border, etc.
(setf *mode-line-border-color* sss-surface
      *mode-line-border-width* 0
      *mode-line-pad-x* 6
      *mode-line-pad-y* 6)

(setf *mode-line-background-color* sss-base
      *mode-line-foreground-color* sss-text)

(set-focus-color sss-base)
(set-unfocus-color sss-base)
(set-float-focus-color sss-base)
(set-float-unfocus-color sss-base)

;; Date and time formats for the modeline
;; refer to the man pages for `date` for more information on the formats
(setf *time-modeline-string* sss-mode-line-time-format)

;; Modeline configuration
(setf *mode-line-timeout* 2)
(setf *screen-mode-line-format*
      (list
       '(:eval (show-hostname))
       "%d "
       " %g "
       "^>"
       "%M      "))

;; TODO? (:eval (format nil "vol: ~a" (show-current-volume)))

;; Font configurations
(setf xft:*font-dirs*
      (list (concat sss-guix-system-path "fonts/")
            (concat sss-guix-home-path "fonts/"))
      clx-truetype:+font-cache-filename+
      (concat (getenv "HOME")
              "/.local/share/fonts/"
              "font-cache.sexp"))
(xft:cache-fonts)
(set-font `(,(make-instance 'xft:font :family sss-font-family :subfamily "Regular" :size 11 :antialias t)))

(setf *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity*   :top)

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 6
      swm-gaps:*outer-gaps-size* 16)


(when *initializing*
  (progn
    (update-color-map (current-screen))
    (workspace-maker)
    (clear-window-placement-rules)
    (sss-run-autostarts)
    (swm-gaps:toggle-gaps)              
    (mode-line)
    (stumptray::stumptray)))


