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

(define sss-audio-service
  (service home-pipewire-service-type))

(define sss-home-files-service
  (service home-files-service-type
	   `((".xinitrc" ,(local-file "xinit/xinitrc")))))

(display "configuring home environment...")
(home-environment
 (packages
  (list pfetch))
 (services
  (list 
   sss-home-files-service
   (service home-dbus-service-type)
   sss-audio-service
   )))



