#! /usr/bin/env gosh

(import (scheme base) (scheme file) (scheme sort) (scheme write) (srfi 13))
(import (gauche base) (file util))

(define (dribble . xs)
  (for-each (lambda (x) (display x (current-error-port))) xs)
  (newline (current-error-port)))

(define (srfi-number name)
  (let ((m (rxmatch #/^srfi-(\d+)$/ name)))
    (and m (string->number (rxmatch-substring m 1)))))

(define (srfi-directories)
  (list-sort (lambda (a b) (< (srfi-number a) (srfi-number b)))
             (directory-list "." :filter srfi-number)))

(define (srfi-html-files)
  (map (lambda (name) (string-append name "/" name ".html"))
       (srfi-directories)))

(define (convert-date match)
  (let* ((original (rxmatch-substring match))
         (old-date (rxmatch-substring match 1))
         (y (rxmatch-substring match 2))
         (m (rxmatch-substring match 3))
         (d (rxmatch-substring match 4))
         (suffix (rxmatch-substring match 5)))
    (if (equal? suffix "/") original ; Probably part of a URL.
        (let ((new-date
               (string-append (format "~4,'0d-~2,'0d-~2,'0d" y m d)
                              (if (string-null? suffix) "" "--"))))
          (dribble (string-pad-right old-date 10) " " new-date " ")
          new-date))))

(define (convert-html-string html-string)
  (regexp-replace-all
   (string->regexp "(((?:19|20)\\d{2})/(\\d{1,2})/(\\d{1,2}))(/| ?- ?|)")
   html-string convert-date))

(define (convert-html-file filename)
  (dribble filename)
  (let* ((new-filename (string-append filename ".new"))
         (old-contents (call-with-port (open-input-file filename)
                                       port->string))
         (new-contents (convert-html-string old-contents)))
    (with-output-to-file new-filename (lambda () (write-string new-contents)))
    (sys-rename new-filename filename)
    (dribble)))

(for-each convert-html-file (srfi-html-files))
