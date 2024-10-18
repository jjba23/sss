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

(use-modules (gnu services xorg))

(load "./per-host.scm")
(load "./system/packages.scm")

(use-service-modules networking desktop docker)
(use-package-modules certs)


(define sss-joe-user-account
  (user-account
   (name "joe")
   (group "users")
   (supplementary-groups '("wheel" "audio" "video" "docker"))))

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
  (cons sss-joe-user-account
        %base-user-accounts))
 
 (packages
  (append sss-system-packages
          %base-packages))
 
 (services
  (cons* (service xfce-desktop-service-type)
         (service nix-service-type)
         (service containerd-service-type)
         (service docker-service-type)
         ;;(service slim-service-type)
         %desktop-services
         ;; (modify-services
         ;;  %desktop-services
         ;;  (delete gdm-service-type))
         )))



