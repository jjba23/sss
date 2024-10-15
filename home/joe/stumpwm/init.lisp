
(asdf:load-system "stumpwm")

(in-package :stumpwm)

(defvar +guix-system-path+ "/run/current-system/profile/share/"
  "Define Guix System profile PATH.)")

(defvar +guix-home-path+ "~/.guix-home/profile/share/"
  "Define Guix Home profile PATH.")

(defvar +swm-data-dir+ (concat (getenv "XDG_CACHE_HOME") "/stumpwm/"))

(set-module-dir (concat +guix-system-path+ "common-lisp/sbcl/"))

(load-module "swm-gaps")
(load-module "cpu")
(load-module "mem")
(load-module "hostname")
(load-module "ttf-fonts")
(load-module "clx-truetype")
(load-module "stumptray")
;; (load-module "trivial-cltl2")
;; (load-module "slynk")

(setf xft:*font-dirs*
      (list (concat +guix-system-path+ "fonts/")
            (concat +guix-home-path+ "fonts/"))
      clx-truetype:+font-cache-filename+
      (concat (getenv "HOME")
              "/.local/share/fonts/"
              "font-cache.sexp"))

(defvar user-name "Joe")
(defvar small-user-name "joe")
(defvar user-email "jjbigorra@gmail.com")

(defvar rp-rose "#ebbcba") 
(defvar rp-iris "#c4a7e7" )
(defvar rp-love "#eb6f92" )
(defvar rp-yellow "#f6c177" )
(defvar rp-foam "#9ccfd8" )
(defvar rp-pine "#42859f")
(defvar rp-text "#e0def4") 
(defvar rp-surface "#222222")
(defvar rp-base "#111111" )

(stumpwm:set-prefix-key (stumpwm:kbd "C-t"))

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

;; (stumpwm:defcommand sly-start-server () ()
;;   "Start a slynk server for sly."
;;   (sb-thread:make-thread (lambda () (slynk:create-server :dont-close t))))

;; (stumpwm:defcommand sly-stop-server () ()
;;   "Stop current slynk server for sly."
;;   (sb-thread:make-thread (lambda () (slynk:stop-server 4005))))


(setf *mouse-focus-policy*    :click
      *float-window-modifier* :SUPER)

(setq *colors*
      `(,rp-base   ;; 0 black
        ,rp-love  ;; 1 red
        ,rp-pine ;; 2 green
        ,rp-yellow  ;; 3 yellow
        ,rp-pine  ;; 4 blue
        ,rp-love  ;; 5 magenta
        ,rp-foam   ;; 6 cyan
        ,rp-text)) ;; 7 white

;;(load-module "battery-portable")
;;(load-module "wifi")


(setf *normal-border-width*       0
      *float-window-border*       0
      *float-window-title-height* 15
      *window-border-style*       :none)



(setf *mode-line-timeout* 2)
;;(setf *time-modeline-string* "%F %H:%M")
(setf *window-format* "%n: %30t")

(setf *startup-message* (format nil "Welcome to StumpWM master ~a!" user-name))

(setf *mode-line-border-color* rp-surface
      *mode-line-border-width* 1
      *mode-line-pad-x* 4
      *mode-line-pad-y* 4)

(setf *mode-line-background-color* rp-base
      *mode-line-foreground-color* rp-text)



(set-focus-color rp-pine)
(set-unfocus-color rp-surface)
(set-float-focus-color rp-foam)
(set-float-unfocus-color rp-surface)

;; (setf *screen-mode-line-format*
;;       (list
;;        "[^B^3%n^b] ^4%W"
;;        "^>"
;;        ;;"%m"
;;        '(:eval (format nil "^5|Volume: ~D" (show-current-volume)))
;;        '(:eval (when (or (not (empty-directory-p "/sys/class/backlight")) (not (empty-directory-p "/dev/backlight"))) (format nil "^6|Backlight: ~D%" (show-brightness-value))))
;;        '(:eval (when (or (not (empty-directory-p "/sys/class/power_supply")) (not (eq 255 (parse-integer (remove #\Newline (run-shell-command "apm -l" t)))))) (format nil "^5|Battery:~D" (show-battery-charge))))
;;        '(:eval (when (or (not (empty-directory-p "/sys/class/power_supply")) (not (eq 255 (parse-integer (remove #\Newline (run-shell-command "apm -l" t)))))) (format nil " ~D" (show-battery-state))))
;;        "^6|%D" ;maildir
;;        "^5|%d"
;;        ))
(setf *time-modeline-string* "%A, %d %B %Y | %k:%M:%S %z")

(setf *screen-mode-line-format*
      (list
       '(:eval (show-hostname))
       (format nil " λ ~a @ GNU Guix | %d " small-user-name)
       "| %g "
        "^>"        
        ;; "bat: %B"
       ;;"CPU %C | "
       '(:eval (format nil "Vol: ~D" (show-current-volume)))
       "%M |     "
       )
      )

(xft:cache-fonts)
(set-font `(
            ,(make-instance 'xft:font :family "Intel One Mono" :subfamily "Regular" :size 10 :antialias t)
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
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "C-]") "gnext")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "C-[") "gprev")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "]") "gnext")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "[") "gprev")

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "1") "gselect 1")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "2") "gselect 2")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "3") "gselect 3")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "4") "gselect 4")
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "5") "gselect 5")


;; Media control
(define-key *top-map*
            (stumpwm:kbd "XF86AudioPlay") "exec playerctl play-pause")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioPause") "exec playerctl pause")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioStop")
            "exec playerctl stop")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioPrev")
            "exec playerctl previous")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioNext")
            "exec playerctl next")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioRewind")
            "exec playerctl position -1")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioForward")
            "exec playerctl position +1")
(define-key *top-map*
            (stumpwm:kbd "XF86MonBrightnessDown")
            "exec xbacklight -perceived -dec 2")
(define-key *top-map*
            (stumpwm:kbd "XF86MonBrightnessUp")
            "exec xbacklight -perceived -inc 2")
(define-key *top-map*
            (stumpwm:kbd "Print")
            "exec spectacle -r")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioRaiseVolume")
            "exec pamixer -i 5")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioLowerVolume")
            "exec pamixer -d 5")
(define-key *top-map*
            (stumpwm:kbd "XF86AudioMute")
            "exec pamixer -m")

;; Lock screen
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "l") "exec xsecurelock")


;; Remap keys to Emacs style keys
(define-remapped-keys
 '(("(Firefox|Chrome|IceCat|GNU IceCat|Konqueror|Dolphin|Chromium|Google Chrome)"
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


(defvar *my-end-session-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (stumpwm:kbd "q") "end-session")
    (define-key m (stumpwm:kbd "l") "logout")
    (define-key m (stumpwm:kbd "s") "suspend-computer")
    (define-key m (stumpwm:kbd "S") "shutdown-computer")
    (define-key m (stumpwm:kbd "r") "loadrc")
    (define-key m (stumpwm:kbd "R") "restart-hard")
    (define-key m (stumpwm:kbd "C-r") "restart-computer")
    m))


(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "p") '*my-end-session-keymap*)
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "/") "exec rofi -combi-modi drun,window -show combi")

(defvar *my-screenshot-keymap*
  (let ((m (make-sparse-keymap)))
    (define-key m (stumpwm:kbd "d") "exec flameshot gui -d 3000")
    (define-key m (stumpwm:kbd "s") "exec flameshot full")
    (define-key m (stumpwm:kbd "S") "exec flameshot gui")
    m))


(defvar *my-application-keymap*
  (let ((m (make-sparse-keymap)))

    (define-key m (kbd "e") "exec emacsclient -c")
    (define-key m (kbd "E") "exec emacs")
    (define-key m (kbd "[") "exec kitty -e \"emacsclient -t\"")
    (define-key m (kbd "w") "exec icecat")
    (define-key m (kbd "b") "exec pcmanfm")
    (define-key *root-map* (stumpwm:kbd "w") "exec icecat")
    (define-key *root-map* (stumpwm:kbd "m") "exec icedove")
    (define-key m (kbd "t") "exec kitty")
    (define-key m (kbd "f") "exec caja")
    (define-key m (kbd "a") "exec pavucontrol")
    ;;(define-key m (kbd "b") "exec bluedevil-wizard")
    (define-key m (kbd "/") "exec rofi -combi-modi drun,window -show combi")
    ;; (define-key m (stumpwm:kbd "1") "exec 1password")
    ;; (define-key m (stumpwm:kbd "C-1") "exec 1password")
    m))

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "r") '*my-application-keymap*)
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "C-r") '*my-application-keymap*)

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "x") "iresize")


(defvar *my-screen-config-keymap*
  (let ((m (make-sparse-keymap)))

    (define-key m (stumpwm:kbd "t") "run-shell-command ~/Ontwikkeling/Persoonlijk/sss/resources/screen-layouts/vandebron-macbook-single-screen.sh")
    (define-key m (stumpwm:kbd "r") "refresh-heads")
    m))

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "C-=") '*my-screen-config-keymap*)
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "=") '*my-screen-config-keymap*)

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "C-.") '*my-screenshot-keymap*)
(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd ".") '*my-screenshot-keymap*)


;; (defvar *my-keyboard-keymap*
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m (stumpwm:kbd "e") "exec setxkbmap -layout us -option ctrl:nocaps")
;;     m))

;; (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "C-z") '*my-keyboard-keymap*)
;; (stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "z") '*my-keyboard-keymap*)

;;(load-module "end-session")
;; Use loginctl instead of the default systemctl
;;(setf end-session:*end-session-command* "loginctl")

(when *initializing*
  (progn
    (update-color-map (current-screen))
    ;;(which-key-mode)
    (gnewbg "[WWW]")
    (gnewbg "[TERM]")
    (grename "[EMACS]")
    (gnewbg "[FILES]")
    (gnewbg "[PRIV]")
    (clear-window-placement-rules)

    (run-shell-command "feh --bg-scale ~/Ontwikkeling/Persoonlijk/sss/resources/wallpapers/3nt5e7.png")
    (run-shell-command "emacs --daemon")
    (run-shell-command "picom -b")
    (run-shell-command "xmodmap ~/.xmodmap")
    (run-shell-command "xsetroot -cursor_name left_ptr")
    
    (swm-gaps:toggle-gaps)              
    (mode-line)
    (stumptray::stumptray)
    )
  )

