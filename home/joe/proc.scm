;;; proc.scm

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

(use-modules (ice-9 popen))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 time))
(use-modules (ice-9 format))
(use-modules (ice-9 string-fun))
(use-modules (ice-9 iconv))
(use-modules (ice-9 threads))

(define (syscall cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process)
    (display process-output)
    process-output))

(define-public (mk-conf file thunk) 
  (syscall (format #f "rm -rf ~a" file ))
  (syscall (format #f "touch ~a" file ))
  (thunk)
  )

(define (write-picom-conf kv file)
  (let* ((file-port
          (open-file file "a")))
    (with-output-to-port file-port
      (lambda ()
        (for-each
         (lambda(x)       
           (display (format #f "~a = ~a;\n" (car x) (cdr x))))     
         kv)
        ))
    (close file-port)))


(define (write-kitty-conf kv file)
  (let* ((file-port
          (open-file file "a")))
    (with-output-to-port file-port
      (lambda ()
        (for-each
         (lambda(x)       
           (display (format #f "~a ~a\n" (car x) (cdr x))))     
         kv)
        ))
    (close file-port)))

(define (write-xmodmap-conf kv file)
  (let* ((file-port
          (open-file file "a")))
    (with-output-to-port file-port
      (lambda ()
        (for-each
         (lambda(x)       
           (display (format #f "~a = ~a\n" (car x) (cdr x))))     
         kv)
        ))
    (close file-port)))
