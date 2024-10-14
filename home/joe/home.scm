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

(load "./proc.scm")

(define jjba23-fancy-bash-service
  (simple-service
   'jjba23-fancy-bash
   home-bash-service-type
   (home-bash-extension
    (environment-variables
     '(("PS1" . "\\u@\\h \\w${GUIX_ENVIRONMENT:+ [env]} λ ")))
    (aliases
     '(("ll" . "ls -lAh --group-directories-first")
       ("l" . "ls -lAh --group-directories-first")
       ("e" . "emacsclient -t")
       ("fr" . "cd ~/Ontwikkeling/Persoonlijk/sss && make fr")
       ("sr" . "cd ~/Ontwikkeling/Persoonlijk/sss && make sr")
       ("jr" . "cd ~/Ontwikkeling/Persoonlijk/sss && make jr")
       )))
   ))

(define (emacs-conf-file file)
  (format #f ".emacs.d/~a" file ))

(define jjba23-picom-conf
  '((backend . "\"xrender\"")
    (corner-radius . 12)))

(define jjba23-kitty-conf
  '((font_family . "\"JetBrains Mono\"")
    (font_size . 12.0)
    (scrollback_lines . 10000000)
    (window_padding_width . 12)
    (background_opacity . "0.8")
    ("map alt+w" . "copy_to_clipboard")
    ("map ctrl+y" . "paste_from_clipboard")
    ))

(define jjba23-xmodmap-conf '(
                              ("remove Lock" . "Caps_Lock")
                              ("keysym Caps_Lock" . "Control_L")
                              ("add Control" . "Control_L")
                              ))

(define jjba23-picom-conf-location
  "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/generated/picom.conf")

(define jjba23-kitty-conf-location
  "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/generated/kitty.conf")

(define jjba23-xmodmap-conf-location
  "/home/joe/Ontwikkeling/Persoonlijk/sss/home/joe/generated/xmodmap")


(define jjba23-home-files-service
  (service home-files-service-type
	   `((".config/kitty/kitty.conf" ,(local-file  jjba23-kitty-conf-location))
             (".stumpwm.d/init.lisp" ,(local-file "stumpwm/init.lisp"))
             (".config/picom.conf" ,(local-file  jjba23-picom-conf-location))
             (".xmodmap" ,(local-file  jjba23-xmodmap-conf-location))
             (,(emacs-conf-file "init.el")
              ,(local-file "emacs/init.el"))
	     (,(emacs-conf-file "early-init.el")
              ,(local-file "emacs/early-init.el")))))


(display "generating picom configuration...\n")
(mk-conf
 jjba23-picom-conf-location
 (lambda ()
   (write-picom-conf jjba23-picom-conf
                     jjba23-picom-conf-location) ))

(display "generating kitty configuration...\n")
(mk-conf
 jjba23-kitty-conf-location
 (lambda ()
   (write-kitty-conf jjba23-kitty-conf
                     jjba23-kitty-conf-location) ))

(display "generating xmodmap configuration...\n")
(mk-conf
 jjba23-xmodmap-conf-location
 (lambda ()
   (write-xmodmap-conf jjba23-xmodmap-conf
                       jjba23-xmodmap-conf-location) ))

(home-environment
 (packages
  (list pfetch))
 (services
  (list 
   jjba23-home-files-service
   jjba23-fancy-bash-service
   )))

