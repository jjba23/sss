;;; config.scm

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

(use-modules (gnu)
             (gnu packages display-managers))

(use-modules (nongnu packages linux)
             (nongnu system linux-initrd))

(use-modules (gnu system privilege))
(use-modules (gnu services xorg))
(use-modules (gnu services sddm))
(use-modules (gnu services))
(use-modules (gnu services shepherd))
(use-modules (gnu services base))
(use-modules (gnu services configuration))
(use-modules (gnu services dbus))
(use-modules (gnu services avahi))
(use-modules (gnu packages suckless))
(use-modules (gnu packages glib))
(use-modules (gnu packages admin))
(use-modules (gnu packages cups))
(use-modules (gnu packages freedesktop))
(use-modules (gnu packages gnome))
(use-modules (gnu packages kde))
(use-modules (gnu packages kde-frameworks))
(use-modules (gnu packages kde-plasma))
(use-modules (gnu packages pulseaudio))
(use-modules (gnu packages xfce))
(use-modules (gnu packages avahi))
(use-modules (gnu packages xdisorg))
(use-modules (gnu packages scanner))
(use-modules (gnu packages suckless))
(use-modules (gnu packages sugar))
(use-modules (gnu packages linux))
(use-modules (gnu packages libusb))
(use-modules (gnu packages lxqt))
(use-modules (gnu packages mate))
(use-modules (gnu packages nfs))
(use-modules (gnu packages enlightenment))
(use-modules (gnu services xorg))
(use-modules (gnu services networking))
(use-modules (gnu services sound))

(load "./per-host.scm")
(load "./system/packages.scm")

(use-service-modules networking desktop docker)
(use-package-modules certs)


(define sss-joe-user-account
  (user-account
   (name "joe")
   (group "users")
   (supplementary-groups '("wheel" "audio" "video" "docker"))))

(define sss-manon-user-account
  (user-account
   (name "manon")
   (group "users")
   (supplementary-groups '("audio" "video" "docker"))))

(define* (sss-desktop-services-for-system #:optional
                                          (system (or (%current-target-system)
                                                      (%current-system))))
  ;; List of services typically useful for a "desktop" use case.
  (cons*
   ;; i am a "no display manager" kinda person
   ;; otherwise add (service sddm-service-type)
   ;; or (service gdm-service-type) for x86_64

   ;; Screen lockers are a pretty useful thing and these are small.
   (service screen-locker-service-type
            (screen-locker-configuration
             (name "slock")
             (program (file-append slock "/bin/slock"))))
   (service screen-locker-service-type
            (screen-locker-configuration
             (name "xlock")
             (program (file-append xlockmore "/bin/xlock"))))

   ;; Add udev rules for MTP devices so that non-root users can access
   ;; them.
   (simple-service 'mtp udev-service-type (list libmtp))
   ;; Add udev rules for scanners.
   (service sane-service-type)
   ;; Add polkit rules, so that non-root users in the wheel group can
   ;; perform administrative tasks (similar to "sudo").
   polkit-wheel-service

   ;; Allow desktop users to also mount NTFS and NFS file systems
   ;; without root.
   (simple-service 'mount-setuid-helpers privileged-program-service-type
                   (map file-like->setuid-program
                        (list (file-append nfs-utils "/sbin/mount.nfs")
                              (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))

   ;; This is a volatile read-write file system mounted at /var/lib/gdm,
   ;; to avoid GDM stale cache and permission issues.
   ;; gdm-file-system-service

   ;; The global fontconfig cache directory can sometimes contain
   ;; stale entries, possibly referencing fonts that have been GC'd,
   ;; so mount it read-only.
   fontconfig-file-system-service

   ;; NetworkManager and its applet.
   (service network-manager-service-type)
   (service wpa-supplicant-service-type)    ;needed by NetworkManager
   (simple-service 'network-manager-applet
                   profile-service-type
                   (list network-manager-applet))
   (service modem-manager-service-type)
   (service usb-modeswitch-service-type)

   ;; The D-Bus clique.
   (service avahi-service-type)
   (service udisks-service-type)
   (service upower-service-type)
   (service accountsservice-service-type)
   (service cups-pk-helper-service-type)
   (service colord-service-type)
   (service geoclue-service-type)
   (service polkit-service-type)
   (service elogind-service-type)
   (service dbus-root-service-type)

   (service ntp-service-type)

   (service x11-socket-directory-service-type)

   (service pulseaudio-service-type)
   (service alsa-service-type)

   %base-services))

(define-syntax sss-desktop-services
  (identifier-syntax (sss-desktop-services-for-system)))



(operating-system
 (host-name "guixvm")
 (timezone "Europe/Amsterdam")

 ;; (locale "en_US.utf8")
 (locale "nl_NL.utf8")

 (bootloader (bootloader-configuration
	      (bootloader grub-efi-bootloader)
	      (targets '("/boot/efi"))))
 
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (kernel-arguments
  (list "console=ttyS0,115200"))
 
 (file-systems
  (append
   sss-filesystems
   %base-file-systems))
 
 (users
  (cons* sss-joe-user-account
         sss-manon-user-account
         %base-user-accounts))
 
 (packages
  (append sss-system-packages
          %base-packages))
 
 (services
  (cons* 
   (service nix-service-type)
   (service containerd-service-type)
   (service docker-service-type)
   sss-desktop-services
   )))



