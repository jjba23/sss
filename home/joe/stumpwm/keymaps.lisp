;;; keymaps.lisp - StumpWM Configurations

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

;; SSS-DEFINE-KEYS is a function that provides a clever way to assign multiple key bindings to a command
(defun sss-define-keys (keys key-map command)
  (mapcar (lambda(x) (define-key key-map (kbd x) command)) keys))

;; Keymaps

;; Keymap for window Manager settings and actions, at: C-t w or C-t C-w
(defvar sss-wm-keymap
  (let ((m (make-sparse-keymap)))
    (sss-define-keys '(":" "C-:" ) m "exec feh --bg-scale ~/Ontwikkeling/Persoonlijk/sss/resources/wallpapers/3nt5e7.png")
    (sss-define-keys '("l" "C-l") m "loadrc")
    (sss-define-keys '("r" "C-r") m "restart-hard")
    (sss-define-keys '("q" "C-q") m "quit")
    (sss-define-keys '("i" "C-i") m "which-key-mode")
    (sss-define-keys '("z" "C-z") m "iresize")
    (sss-define-keys '("{" "C-{") m "sly-start-server")
    (sss-define-keys '("}" "C-}") m "sly-stop-server")
    (sss-define-keys '("c" "C-c") m "exec conky -d")
    (sss-define-keys '("z" "C-z") m "exec pkill conky")
    m))


;; Keymap for power, reboot, battery, at: C-t p or C-t C-p
(defvar sss-power-keymap
  (let ((m (make-sparse-keymap)))
    (sss-define-keys '("s" "C-s" "s-s") m "exec loginctl suspend")
    (sss-define-keys '("h" "C-h" "s-h") m  "exec sudo halt")
    (sss-define-keys '("r" "C-r" "s-r") m "exec sudo reboot")
    m))

;; Keymap for reconfiguring SSS, at: C-t # or C-t C-#
(defvar sss-reconfigure-keymap
  (let ((m (make-sparse-keymap)))
    ;; Full system rebuild
    (sss-define-keys '("f" "C-f") m "exec cd ~/Ontwikkeling/Persoonlijk/sss \
                                       && make fr")
    ;; Joe's home rebuild
    (sss-define-keys '("j" "C-j") m "exec cd ~/Ontwikkeling/Persoonlijk/sss \
                                       && make jr")
    ;; Manon's home rebuild
    (sss-define-keys '("m" "C-m") m "exec cd ~/Ontwikkeling/Persoonlijk/sss \
                                       && make mr")
    m))

;; Keymap for screenshot and recordings, at: C-t . or C-t C-.
(defvar sss-screenshot-keymap
  (let ((m (make-sparse-keymap)))
    (sss-define-keys '("d" "C-d" ) m "exec flameshot gui -d 3000")
    (sss-define-keys '("s" "C-s" "f" "C-f") m "exec flameshot full")
    (sss-define-keys '("g" "C-g" ) m "exec flameshot gui")
    m))

;; TODO: Keymap for daemon management (mu4e, emacs, etc) at leader + C-d ??

;; Keymap for launching applications, at: C-t r or C-t C-r
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

;; Keymaps for screen/monitor configurations, at: C-t = or C-t C-=
(defvar sss-screen-config-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "v") "run-shell-command ~/Ontwikkeling/Persoonlijk/sss/resources/screen-layouts/vandebron-macbook-single-screen.sh")
    (define-key m (kbd "r") "refresh-heads")
    m))


;; Bind keymaps to root map
(sss-define-keys '("w" "C-w") *root-map* 'sss-wm-keymap)
(sss-define-keys '("p" "C-p") *root-map* 'sss-power-keymap)
(sss-define-keys '("#" "C-#") *root-map* 'sss-reconfigure-keymap)
(sss-define-keys '("/" "C-/") *root-map* "exec rofi -combi-modi drun,window -show combi")
(sss-define-keys '("r" "C-r" "SPC" "C-SPC") *root-map* 'sss-application-keymap)
(sss-define-keys '("=" "C-=") *root-map* 'sss-screen-config-keymap)
(sss-define-keys '("." "C-.") *root-map* 'sss-screenshot-keymap)

;; Lock screen
(sss-define-keys '("l" "C-l") *root-map* "exec i3lock-fancy")

;; Group shortcuts
(sss-define-keys '("]" "C-]") *root-map* "gnext")
(sss-define-keys '("[" "C-[") *root-map* "gprev")

;; Move screen to workspace
(sss-define-keys '("1") *root-map* "gselect 1")
(sss-define-keys '("2") *root-map* "gselect 2")
(sss-define-keys '("3") *root-map* "gselect 3")
(sss-define-keys '("4") *root-map* "gselect 4")
(sss-define-keys '("5") *root-map* "gselect 5")

;; Move frame to workspace
(sss-define-keys '("C-1") *root-map* "gmove 1")
(sss-define-keys '("C-2") *root-map* "gmove 2")
(sss-define-keys '("C-3") *root-map* "gmove 3")
(sss-define-keys '("C-4") *root-map* "gmove 4")
(sss-define-keys '("C-5") *root-map* "gmove 5")

;; Media control
(define-key *top-map*
            (kbd "XF86AudioPlay") "exec playerctl play-pause")
(define-key *top-map*
            (kbd "XF86AudioPause") "exec playerctl pause")
(define-key *top-map*
            (kbd "XF86AudioStop")
            "exec playerctl stop")
(sss-define-keys '("XF86AudioPrev") *top-map* "exec playerctl previous")
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
