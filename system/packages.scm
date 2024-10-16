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
             (gnu packages databases)
             (gnu packages xorg)
             (gnu packages file-systems)
             (gnu packages display-managers)
             (gnu packages web-browsers) 
             (gnu packages docker)
             (gnu packages lisp-xyz)
             (gnu packages fontutils)
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
             (gnu packages xfce)
             (gnu packages multiprecision)
             (gnu packages gcc)
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

(use-modules (guix git-download))
(use-modules (guix build-system font))
(use-modules ((guix licenses) #:prefix license:))

(use-modules (guix packages)
             (gnu packages base))
(use-modules (gnu packages linux))

(use-modules (gnu packages web))

(use-modules (gnu packages package-management))

(define jjba23-theme-packages
  (list yaru-theme))

(define font-jjba23-intel-one-mono
  (package
   (name "font-jjba23-intel-one-mono")
   (version "1.4.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/intel/intel-one-mono")
                  (commit (string-append "V" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1snwxpcdxl62z1mcax19bmsmbw0hi6m0cqkxqz79ydynfch95sd0"))))
   ;;(outputs '("out" "ttf" "woff"))
   (build-system font-build-system)
   ;; (arguments
   ;;  `(#:phases %standard-phases))
   (home-page "https://github.com/intel/intel-one-mono")
   (synopsis "Expressive monospaced font family")
   (description
    "This package provides Intel One Mono, an expressive monospaced font
family that's built with clarity, legibility, and the needs of developers in
mind.")
   (license license:silofl1.1)))


(define jjba23-font-packages
  (list
   fontconfig
   font-google-roboto
   font-jetbrains-mono
   font-jjba23-intel-one-mono
   font-intel-one-mono
   font-liberation
   font-google-noto))

(define jjba23-wm-packages
  (list
   cl-asdf
   picom
   sbcl-alexandria
   sbcl-asdf-system-connections
   sbcl-bordeaux-threads
   sbcl-cl-environments
   sbcl-cl-fad
   sbcl-cl-store
   sbcl-cl-vectors
   sbcl-clx-truetype
   sbcl-clx-xembed
   sbcl-global-vars
   sbcl-salza2
   sbcl-slynk
   sbcl-stumpwm-cpu
   sbcl-stumpwm-hostname
   sbcl-stumpwm-mem
   sbcl-stumpwm-net
   sbcl-stumpwm-notify
   sbcl-stumpwm-stumptray
   sbcl-stumpwm-swm-gaps
   sbcl-stumpwm-ttf-fonts
   sbcl-stumpwm-wifi
   sbcl-trivial-cltl2
   sbcl-trivial-features
   sbcl-trivial-garbage
   sbcl-trivial-gray-streams
   sbcl-xml-emitter
   sbcl-zpb-ttf
   sbcl-zpng
   stumpish
   stumpwm
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
   ;;nyxt
   docker   
   gparted
   xrandr
   rofi
   slim
   lm-sensors
   exfatprogs
   exfat-utils
   fuse-exfat
   tmon
   zip
   flameshot
   unzip
   thunar
   xarchiver
   ristretto
   orage
   sqlitebrowser
   tar))

(define jjba23-system-packages
  (append
   jjba23-other-system-packages
   jjba23-wm-packages
   jjba23-font-packages
   jjba23-dev-packages
   jjba23-theme-packages))
