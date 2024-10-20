;;; init.lisp - StumpWM Configurations

;; Copyright (C) 2024 Josep Bigorra

;; Version: 0.9.0
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

(defvar sss-guix-system-path "/run/current-system/profile/share/"
  "Define Guix System profile PATH.")

(defvar sss-stumpwm-module-dir (concat sss-guix-system-path "common-lisp/sbcl/")
  "Define the directory where StumpWM should load modules from.")

;; Load StumpWM modules
(set-module-dir sss-stumpwm-module-dir)
(load-module "swm-gaps")
(load-module "cpu")
(load-module "mem")
(load-module "hostname")
(load-module "ttf-fonts")
(load-module "clx-truetype")
(load-module "stumptray")
;; (load-module "slynk")

(defvar sss-guix-home-path "~/.guix-home/profile/share/"
  "Define Guix Home profile PATH.")

(defvar sss-stumpwm-data-dir (concat (getenv "XDG_CACHE_HOME") "/stumpwm/")
  "Define the directory where StumpWM should work with data.")

(defvar user-name "Joe")
(defvar small-user-name "joe")
(defvar user-email "jjbigorra@gmail.com")

(defun sss-define-keys (keys key-map command)
  (mapcar (lambda(x) (define-key key-map (kbd x) command )) keys))


;; Keymaps
(defvar sss-wm-keymap
  (let ((m (make-sparse-keymap)))
    (sss-define-keys '(":" "C-:" ) m "exec feh --bg-scale ~/Ontwikkeling/Persoonlijk/sss/resources/wallpapers/3nt5e7.png")
    (sss-define-keys '("l" "C-l") m "loadrc")
    (sss-define-keys '("r" "C-r") m "restart-hard")
    (sss-define-keys '("q" "C-q") m "quit")
    (sss-define-keys '("i" "C-i") m "which-key-mode")
    (sss-define-keys '("z" "C-z") m "iresize")
    m))

(defvar sss-end-session-keymap
  (let ((m (make-sparse-keymap)))
    (sss-define-keys '("s" "C-s" "s-s") m "exec loginctl suspend")
    (sss-define-keys '("h" "C-h" "s-h") m  "exec sudo halt")
    (sss-define-keys '("r" "C-r" "s-r") m "exec sudo reboot")
    m))

(defvar sss-reconfigure-keymap
  (let ((m (make-sparse-keymap)))    
    (sss-define-keys '("f" "C-f") m "exec cd ~/Ontwikkeling/Persoonlijk/sss && make fr")
    (sss-define-keys '("j" "C-j") m "exec cd ~/Ontwikkeling/Persoonlijk/sss && make jr")
    (sss-define-keys '("m" "C-m") m "exec cd ~/Ontwikkeling/Persoonlijk/sss && make mr")
    m))


(defvar sss-screenshot-keymap
  (let ((m (make-sparse-keymap)))
    (sss-define-keys '("d" "C-d" ) m "exec flameshot gui -d 3000")
    (sss-define-keys '("s" "C-s" "f" "C-f") m "exec flameshot full")
    (sss-define-keys '("g" "C-g" ) m "exec flameshot gui")
    m))

(defvar sss-application-keymap
  (let ((m (make-sparse-keymap)))
    (sss-define-keys '("e" "C-e" ) m "exec emacsclient -c")
    (sss-define-keys '("E" "C-E" ) m "exec emacs")
    (sss-define-keys '("RET" "C-RET" "t" "C-t") m "exec kitty")    
    (sss-define-keys '("w" "C-w" ) m "exec /home/joe/.nix-profile/bin/vivaldi")
    (sss-define-keys '("f" "C-f" ) m "exec thunar")
    (sss-define-keys '("m" "C-m" ) m "exec icedove")
    (sss-define-keys '("a" "C-a" ) m "exec pavucontrol")     
    (sss-define-keys '("/" "C-/" "q" "C-q" ) m "exec rofi -combi-modi drun,window -show combi")
    m))

(defvar sss-screen-config-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v") "run-shell-command ~/Ontwikkeling/Persoonlijk/sss/resources/screen-layouts/vandebron-macbook-single-screen.sh")
    (define-key m (kbd "r") "refresh-heads")
    m))

;; Colors 
(defvar sss-rose "#d38faf") 
(defvar sss-iris "#7fc500" )
(defvar sss-love "#ef6560" )
(defvar sss-yellow "#d4aa02" )
(defvar sss-foam "#5dc0aa" )
(defvar sss-pine "#3fb83f")
(defvar sss-text "#cfdfd5") 
(defvar sss-surface "#222522")
(defvar sss-base "#111111" )

;; Set prefix key, the leader key binding
(setf *debug-level* 10)
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

(setf *startup-message* (format nil "Master ~a | Welcome to StumpWM!" user-name))

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
(setf *time-modeline-string* (format nil "  %k:%M:%S %z - ~a @ SSS/Guix - %A, %d %B %Y   " small-user-name))

;; Modeline configuration
(setf *mode-line-timeout* 2)
(setf *screen-mode-line-format*
      (list
       '(:eval (show-hostname))
       "%d "
       " %g "
       "^>"
       '(:eval (format nil "Vol: ~a" (show-current-volume)))
       "%M      "
       )
      )


;; Font configurations
(setf xft:*font-dirs*
      (list (concat sss-guix-system-path "fonts/")
            (concat sss-guix-home-path "fonts/"))
      clx-truetype:+font-cache-filename+
      (concat (getenv "HOME")
              "/.local/share/fonts/"
              "font-cache.sexp"))
(xft:cache-fonts)
(set-font `(
            ,(make-instance 'xft:font :family "Intel One Mono" :subfamily "Regular" :size 11 :antialias t)
            )
          )

(setf *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity*   :top)

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 6
      swm-gaps:*outer-gaps-size* 16)


;; Group shortcuts
(sss-define-keys '("]" "C-]") *root-map* "gnext")
(sss-define-keys '("[" "C-[") *root-map* "gprev")


(sss-define-keys '("1") *root-map* "gselect 1")
(sss-define-keys '("2") *root-map* "gselect 2")
(sss-define-keys '("3") *root-map* "gselect 3")
(sss-define-keys '("4") *root-map* "gselect 4")
(sss-define-keys '("5") *root-map* "gselect 5")


;; (define-key *root-map* (kbd "C-1") "gmove 1")
;; (define-key *root-map* (kbd "C-2") "gmove 2")
;; (define-key *root-map* (kbd "C-3") "gmove 3")
;; (define-key *root-map* (kbd "C-4") "gmove 4")
;; (define-key *root-map* (kbd "C-5") "gmove 5")

;; Media control
(define-key *top-map*
            (kbd "XF86AudioPlay") "exec playerctl play-pause")
(define-key *top-map*
            (kbd "XF86AudioPause") "exec playerctl pause")
(define-key *top-map*
            (kbd "XF86AudioStop")
            "exec playerctl stop")
(define-key *top-map*
            (kbd "XF86AudioPrev")
            "exec playerctl previous")
(define-key *top-map*
            (kbd "XF86AudioNext")
            "exec playerctl next")
(define-key *top-map*
            (kbd "XF86AudioRewind")
            "exec playerctl position -1")
(define-key *top-map*
            (kbd "XF86AudioForward")
            "exec playerctl position +1")
(define-key *top-map*
            (kbd "XF86MonBrightnessDown")
            "exec xbacklight -perceived -dec 2")
(define-key *top-map*
            (kbd "XF86MonBrightnessUp")
            "exec xbacklight -perceived -inc 2")
(define-key *top-map*
            (kbd "Print")
            "exec spectacle -r")
(define-key *top-map*
            (kbd "XF86AudioRaiseVolume")
            "exec pamixer -i 5")
(define-key *top-map*
            (kbd "XF86AudioLowerVolume")
            "exec pamixer -d 5")
(define-key *top-map*
            (kbd "XF86AudioMute")
            "exec pamixer -m")

;; Lock screen
(define-key *root-map* (kbd "l") "exec i3lock-fancy")


;; Remap keys to Emacs style keys
(define-remapped-keys
 '(("(Firefox|Chrome|IceCat|GNU IceCat|Konqueror|Dolphin|Chromium|Google Chrome|Vivaldi)"
    ("C-n"   . "Down")
    ("C-p"   . "Up")
    ("C-f"   . "Right")
    ("C-b"   . "Left")
    ("C-v"   . "Next")
    ("M-v"   . "Prior")
    ("M-w"   . "C-c")
    ("C-w"   . "C-x")
    ("C-y"   . "C-v")
    ("M-<"   . "Home")
    ("M->"   . "End")
    ("C-M-b" . "M-Left")
    ("C-M-f" . "M-Right")
    ("C-k"   . ("C-S-End" "C-x")))))



(sss-define-keys '("w" "C-w") *root-map* 'sss-wm-keymap)
(sss-define-keys '("p" "C-p") *root-map* 'sss-end-session-keymap)
(sss-define-keys '("#" "C-#") *root-map* 'sss-reconfigure-keymap)
(sss-define-keys '("/" "C-/") *root-map* "exec rofi -combi-modi drun,window -show combi")
(sss-define-keys '("r" "C-r" "SPC" "C-SPC") *root-map* 'sss-application-keymap)
(sss-define-keys '("=" "C-=") *root-map* 'sss-screen-config-keymap)
(sss-define-keys '("." "C-.") *root-map* 'sss-screenshot-keymap)

(when *initializing*
  (progn
    (update-color-map (current-screen))
    (gnewbg " www")
    (gnewbg " console")
    (grename " emacs")
    (gnewbg " files")
    (gnewbg " priv")
    (gnewbg " audio")
    (clear-window-placement-rules)

    (sss-run-autostarts)
    
    (swm-gaps:toggle-gaps)              
    (mode-line)
    (stumptray::stumptray)
    )
  )


;; if debugging
;; (load-module "slynk")
;; (defcommand sly-start-server () ()
;;   "Start a slynk server for sly."
;;   (sb-thread:make-thread (lambda () (slynk:create-server :dont-close t))))
;; (defcommand sly-stop-server () ()
;;   "Stop current slynk server for sly."
;;   (sb-thread:make-thread (lambda () (slynk:stop-server 4005))))
