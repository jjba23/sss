;;; packages.scm

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
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (gnu)
	     (gnu packages admin)
	     (gnu packages screen)	     
	     (gnu packages emacs)
             (gnu packages xorg)
             (gnu packages display-managers)
             (gnu packages web-browsers) 
             (gnu packages docker)
             (gnu packages lisp-xyz)
             (gnu packages lisp)
             (gnu packages image-viewers)
             (gnu packages gnome-xyz)
             (gnu packages compton)
	     (gnu packages ssh)
	     (gnu packages version-control)
	     (gnu packages fonts)
             (gnu packages pulseaudio)
             (gnu services nix)
	     (gnu packages libreoffice)
             (gnu packages wm)
             (gnu packages ncurses)
             (gnu packages multiprecision)
             (gnu packages gcc)
             (gnu packages lxde)
             (gnu packages aspell)
	     (gnu packages rust-apps)
             (gnu packages inkscape)
	     (gnu packages gimp)
             (gnu packages python-web)
             (gnu packages xdisorg)
             (gnu packages imagemagick)
             (gnu packages curl)
             (gnu packages terminals)
             (gnu packages video)
             (gnu packages image)    
	     (gnu packages compression)
             (gnu packages sqlite)
             (gnu packages disk)
             (gnu packages glib)
             (gnu packages networking)
	     (gnu packages gnuzilla))

(use-modules (guix packages)
             (gnu packages base))
(use-modules (gnu packages linux))

(use-modules (gnu packages web))

(use-modules (gnu packages package-management))



(define jjba23-theme-packages
  (list yaru-theme))

(define jjba23-font-packages
  (list
   font-google-roboto
   font-jetbrains-mono
   font-google-noto))

(define jjba23-wm-packages
  (list
   stumpwm
   sbcl-stumpwm-screenshot
   sbcl-stumpwm-ttf-fonts
   sbcl-stumpwm-cpu
   sbcl-stumpwm-mem
   sbcl-stumpwm-net
   sbcl-stumpwm-hostname
   sbcl-zpb-ttf
   sbcl-alexandria
   sbcl-stumpwm-swm-gaps
   sbcl-zpng
   sbcl-salza2
   sbcl-trivial-gray-streams
   sbcl-cl-vectors
   sbcl-cl-fad
   sbcl-clx-truetype
   cl-asdf
   sbcl-asdf-system-connections
   sbcl-bordeaux-threads
   sbcl-global-vars
   sbcl-trivial-features
   sbcl-trivial-garbage
   sbcl-cl-store
   sbcl-stumpwm-stumptray
   sbcl-trivial-cltl2
   sbcl-clx-xembed
   sbcl-slynk
   sbcl-cl-environments
   stumpish
   picom
   ))

(define jjba23-dev-packages
  (list
   (specification->package "openjdk@21.0.2")

   ))

(define jjba23-other-system-packages
  (list
   htop
   emacs
   icecat
   icedove
   openssh
   dbus
   ncurses
   gmp
   gcc
   screen
   (specification->package "make")
   coreutils
   flatpak
   pipewire
   nginx
   watchexec
   git
   pcmanfm   
   pavucontrol

   nix
   
   ;; (if x86 tlp tlpui)
   xz
   ispell
   (specification->package "bind")
   wireshark
   kitty
   arandr
   httpie
   curl
   inkscape
   fzf
   vlc
   ripgrep
   sqlite
   (specification->package "ungoogled-chromium")
   obs
   libreoffice
   gimp
   imagemagick
   libwebp
   feh
   containerd
   xmodmap
   nyxt
   docker
   gparted
   xrandr
   rofi
   slim
   lm-sensors
   tmon
   zip
   unzip
   tar))

(define jjba23-system-packages
  (append
   jjba23-other-system-packages
   jjba23-wm-packages
   jjba23-font-packages
   jjba23-dev-packages
   jjba23-theme-packages))
