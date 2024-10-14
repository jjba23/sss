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
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (gnu))

(use-modules (nongnu packages linux)
             (nongnu system linux-initrd))

(load "./system/packages.scm")

(use-service-modules networking desktop docker)
(use-package-modules certs)


(define jjba23-joe-user-account
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
   (list (file-system
	  (device "/dev/nvme0n1p3")
	  (mount-point "/")
	  (type "ext4"))
	 (file-system
	  (device "/dev/nvme0n1p1")
	  (mount-point "/boot/efi")
	  (type "vfat")))
   %base-file-systems))
 
 (users
  (cons jjba23-joe-user-account
        %base-user-accounts))
 
 (packages
  (append jjba23-system-packages
          %base-packages))
 
 (services
  (cons* (service xfce-desktop-service-type)
         (service nix-service-type)
         (service containerd-service-type)
         (service docker-service-type)
	 %desktop-services)))



