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
;; along with sss.  If not, see <https://www.gnu.org/licenses/>.

(define-module (sss-packages)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages screen)	     
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages music)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages web-browsers) 
  #:use-module (gnu packages docker)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu services nix)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages video)
  #:use-module (gnu packages image)    
  #:use-module (gnu packages compression)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages gnuzilla)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system copy)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages web)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages base)
  )

(define sss-theme-packages
  (list
   yaru-theme
   delft-icon-theme
   ))

(define font-sss-intel-one-mono
  (package
   (name "font-sss-intel-one-mono")
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
   (license silofl1.1)))


(define sss-font-packages
  (list
   fontconfig
   font-google-roboto
   font-google-noto-emoji
   font-jetbrains-mono
   font-sss-intel-one-mono
   font-intel-one-mono
   font-liberation
   font-google-noto))

(define sss-sbcl-packages
  (list
   sbcl-alexandria
   sbcl-asdf-system-connections
   sbcl-bordeaux-threads
   cl-asdf
   sbcl-local-time
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
   ))

(define sss-wm-packages
  (list
   cl-asdf
   picom
   
   stumpish
   (specification->package "stumpwm-with-slynk")
   ))


(define sss-treesitter-packages
  (list
   tree-sitter
   tree-sitter-bash
   tree-sitter-dockerfile
   tree-sitter-lua
   tree-sitter-haskell
   tree-sitter-css
   tree-sitter-html
   tree-sitter-nix
   tree-sitter-scala
   tree-sitter-markdown
   tree-sitter-typescript
   tree-sitter-scheme
   tree-sitter-java
   tree-sitter-javascript
   
   ))


(define sss-dev-packages
  (list
   (specification->package "openjdk@21.0.2")
   
   ))

(define sss-coreutils
  (list
   htop
   emacs
   openssh
   dbus
   ncurses
   screen
   tar zip unzip
   gmp
   gcc
   curl
   ripgrep
   net-tools
   dstat
   dconf-editor
   (specification->package "make")
   nix
   coreutils
   xinit
   xorg-server
   ))

(define sss-music-packages
  (list
   spotifyd
   lilypond
   ardour))

(define sss-other-system-packages
  (list
   icecat
   icedove
   
   flatpak
   pipewire
   nginx
   watchexec
   git
   pavucontrol   
   
   ;; (if x86 tlp tlpui)
   xz
   ispell
   (specification->package "bind")
   wireshark
   kitty
   arandr
   httpie

   inkscape
   dunst
   fzf
   vlc

   sqlite
   postgresql
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
   
   flameshot
   
   thunar
   xarchiver
   ristretto
   orage
   sqlitebrowser
   direnv
   openbox
   xf86-video-fbdev
   xf86-input-libinput
   lxsession
   pamixer
   lxappearance
   (specification->package "conky")
   i3lock-fancy
   mpv
   mpv-mpris
   fyi
   qbittorrent
   openvpn
   network-manager-applet
   network-manager-openconnect
   network-manager-openvpn
   xxd
   ))

(define sss-guile-packages
  (list
   (specification->package "guile-json@4.7.3")
   ))

(define-public sss-system-packages
  (append
   sss-other-system-packages
   sss-wm-packages
   sss-font-packages
   sss-sbcl-packages
   sss-dev-packages
   sss-guile-packages
   sss-treesitter-packages
   sss-coreutils
   sss-theme-packages))
