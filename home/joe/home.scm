;;; home.scm

;; Copyright (C) 2024 Josep Jesus Bigorra Algaba

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

(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu packages emacs)
             (gnu packages admin)
             (gnu services)
             (guix gexp))
(use-modules (gnu home services ssh)
             (gnu home services gnupg)
             (gnu packages gnupg))
(use-modules (gnu home services sound)
             (gnu home services desktop) )

(load "./proc.scm")

(define sss-fancy-bash-service
  (simple-service
   'sss-fancy-bash
   home-bash-service-type
   (home-bash-extension
    (environment-variables '())
    (bashrc `(,(local-file "bash/bashrc.sh"))))))

(define (emacs-conf-file file)
  (format #f ".emacs.d/~a" file ))

(define sss-picom-conf-location
  "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/generated/picom.conf")

(define sss-picom-conf
  '((backend . "\"xrender\"")
    (corner-radius . "14")
    (fading . "true")
    (fade-in-step . "0.24")
    (fade-out-step . "0.24")
    (shadow . "false")))
;;(shadow-radius . "8")
;;(shadow-opacity . "0.65")

(define sss-kitty-conf-location
  "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/generated/kitty.conf")

(define sss-kitty-conf
  '((font_family . "Intel One Mono")
    (font_size . 12.0)
    (scrollback_lines . 10000000)
    (window_padding_width . 12)
    (background_opacity . "0.8")
    ("map alt+w" . copy_to_clipboard)
    ("map ctrl+y" . paste_from_clipboard)
    ("map alt+v" . scroll_page_up)
    ("map ctrl+v" . scroll_page_down)
    ("map alt+<" . scroll_home)
    ("map alt+>" . scroll_end)
    ))

(define sss-opengpg-conf
  (service home-gpg-agent-service-type
           (home-gpg-agent-configuration
            (pinentry-program
             (file-append pinentry-emacs "/bin/pinentry-emacs"))
            (ssh-support? #t))))

(define sss-xmodmap-conf-location
  "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/generated/xmodmap")

(define sss-xmodmap-conf
  '(("remove Lock" . "Caps_Lock")
    ("keysym Caps_Lock" . "Control_L")
    ("add Control" . "Control_L")
    ))


(define sss-gtk3-conf-location
  "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/generated/gtk-3.0-settings.ini")

(define sss-gtk3-conf
  '((gtk-icon-theme-name . "Delft-Green-Darkest")
    (gtk-theme-name . "Yaru-sage-dark")
    (gtk-font-name . "Liberation Sans 12")
    (gtk-key-theme-name . "Emacs")
    (gtk-enable-event-sounds . "0")
    (gtk-cursor-theme-name . "Yaru-sage-dark")
    (gtk-enable-input-feedback-sounds . "0")
    ))

(define sss-audio-service
  (service home-pipewire-service-type))

(define sss-home-vars-service
  (simple-service 'some-useful-env-vars-service
                  home-environment-variables-service-type
                  `(("SBCL_HOME" . "$HOME/.guix-home/profile/lib/sbcl")
                    ("GUIX_LOCPATH" . "$home/.guix-profile/lib/locale")                  
                    ("LANG" . "nl_NL.UTF-8")
                    ("LANGUAGE" . "nl_NL"))))



(define sss-ssh-service
  (service home-openssh-service-type
           (home-openssh-configuration
            (hosts
             (list (openssh-host (name "personal.github.com")
                                 (user "joe")
                                 (host-name "github.com")
                                 (identity-file "~/.ssh/gitlab_prive"))
                   (openssh-host (name "work.github.com")
                                 (user "joe")
                                 (host-name "github.com")
                                 (identity-file "~/.ssh/work_id"))))
            (authorized-keys '()))))

(display "\n>>= generating picom configuration...\n")
(mk-conf
 sss-picom-conf-location
 (lambda ()
   (write-picom-conf sss-picom-conf
                     sss-picom-conf-location) ))

(display "\n>>= generating kitty configuration...\n")
(mk-conf
 sss-kitty-conf-location
 (lambda ()
   (write-kitty-conf sss-kitty-conf
                     sss-kitty-conf-location) ))

(display "\n>>= generating xmodmap configuration...\n")
(mk-conf
 sss-xmodmap-conf-location
 (lambda ()
   (write-xmodmap-conf sss-xmodmap-conf
                       sss-xmodmap-conf-location) ))

(display "\n>>= generating GTK 3 configuration...\n")
(mk-conf
 sss-gtk3-conf-location
 (lambda ()
   (write-gtk3-conf sss-gtk3-conf
                    sss-gtk3-conf-location) ))


(display "\n>>= setting up nix env...\n")
(syscall "ln -sfv /nix/var/nix/profiles/per-user/joe/profile /home/joe/.nix-profile")

(display "\n>>= installing nix packages\n")
(for-each
 (lambda(x) (nix-profile-install x))
 '("vivaldi"
   "prismlauncher"
   "yaml-language-server"
   "jdt-language-server"
   "nil"
   "black"
   "pyright"
   "marksman"
   ))


(define sss-home-files-service
  (service home-files-service-type
	   `((".config/kitty/kitty.conf" ,(local-file  sss-kitty-conf-location))
             (".stumpwm.d/init.lisp" ,(local-file "stumpwm/init.lisp"))
             (".stumpwm.d/keymaps.lisp" ,(local-file "stumpwm/keymaps.lisp"))
             (".stumpwm.d/colors.lisp" ,(local-file "stumpwm/colors.lisp"))
             (".stumpwm.d/autostarts.lisp" ,(local-file "stumpwm/autostarts.lisp"))
             (".config/picom.conf" ,(local-file sss-picom-conf-location))
             (".xmodmap" ,(local-file  sss-xmodmap-conf-location))
             (".config/gtk-3.0/settings.ini" ,(local-file sss-gtk3-conf-location))
             (".config/guix/channels.scm" ,(local-file "channels.scm"))
             (".config/rofi/config.rasi" ,(local-file "rofi/config.rasi"))
             (".gitconfig" ,(local-file "git/gitconfig.ini"))
             (".gitconfig-personal" ,(local-file "git/gitconfig-personal.ini"))
             (".gitconfig-work" ,(local-file "git/gitconfig-work.ini"))
             (".gitignore-global" ,(local-file "git/gitignore-global"))
             (".gitmessage-personal" ,(local-file "git/gitmessage-personal"))
             (".gitmessage-work" ,(local-file "git/gitmessage-work"))
             (".config/dunst/dunstrc" ,(local-file "dunst/dunstrc"))
             (".config/nix/nix.conf" ,(local-file "nix/nix.conf"))
             (".config/nixpkgs/config.nix" ,(local-file "nix/nixpkgs.nix"))
             (".config/conky/conky.conf" ,(local-file "conky/conkyrc.lua"))
             (,(emacs-conf-file "init.el") ,(local-file "emacs/init.el"))
	     (,(emacs-conf-file "early-init.el") ,(local-file "emacs/early-init.el")))))

(display "\n>>= configuring home environment...\n")
(home-environment
 (packages
  (list pfetch)) 
 (services
  (list 
   sss-home-files-service
   sss-ssh-service
   sss-home-vars-service
   sss-fancy-bash-service
   sss-opengpg-conf
   (service home-dbus-service-type)
   sss-audio-service
   )))



