#lang racket/gui

(require mrlib/path-dialog)
(require net/url)
(require net/ftp)
(require "ftp_assistant.rkt")
(provide manager_dialog)

(define manager_dialog (new frame%
                   [label "Net Manager Dialog"]
                   [width 300]
                   [height 300]))
(define v-panel (new vertical-panel% [parent manager_dialog]))
(define user-panel (new vertical-panel% [parent v-panel]))
(define h-panel (new horizontal-panel% [parent v-panel]))

(define hostname-input (new text-field%
                            [label "Hostname"]
                            [parent user-panel]))

(define username-input (new text-field%
                            [label "username"]
                            [parent user-panel]))

(define password-input (new text-field%
                            [label "password"]
                            [parent user-panel]))

(define remote-dir-input (new text-field%
                            [label "remote-dir"]
                            [parent user-panel]))

(define object-file-input (new text-field%
                            [label "object-file-input"]
                            [parent user-panel]))

(define file-list-box (new list-box%
                           [parent user-panel]
                           [label "Files"]
                           [choices (list "")]
                           [style (list 'variable-columns
                                        'single
                                        'column-headers
                                        'clickable-headers)]
                           [columns (list "Files")]))

(define ftp-cd-button (new button%
                           [parent h-panel]
                           [label "Cd .."]
                           [callback
                            (lambda (button event) (change-directory))]))


(define ftp-download-button (new button%
                                [parent h-panel]
                                [label "Download"]
                                [callback
                                 (lambda (button event) (activate-downloading))]))

(define ftp-download-button-wd (new button%
                                [parent h-panel]
                                [label "Download wd"]
                                [callback
                                 (lambda (button event) (activate-downloading-with-dialog))]))

(define ftp-upload-button (new button%
                                [parent h-panel]
                                [label "Upload"]
                                [callback
                                 (lambda (button event) (activate-uploading-with-dialog))]))

(define ftp-upload-button-wd (new button%
                                [parent h-panel]
                                [label "Upload wd"]
                                [callback
                                 (lambda (button event) (activate-uploading-with-dialog))]))

(define ftp-list-files (new button%
                            [parent h-panel]
                            [label "List Files"]
                            [callback
                             (lambda (button event) (show-ftp-directory-list-fl))]
                            ))


(define exit-button (new button%
                         [parent h-panel]
                         [label "Exit"]
                         [callback
                          (lambda (button event) (send manager_dialog on-exit))]))

(define (activate-downloading)
  (let* ([usr (send username-input get-value)]
         [hstname (send hostname-input get-value)]
         [psswrd (send password-input get-value)]
         [object-file (send object-file-input get-value)]
         [rmt-dir (send remote-dir-input get-value)])
    (download-ftp-file hstname rmt-dir usr psswrd object-file)))


;; control
(define (activate-downloading-with-dialog)
  ;; (show-ftp-directory-list-fl)
    
  (let* ([elem-num (send file-list-box get-selection)]
         [elem-data-str (send file-list-box get-string elem-num)]
         [elem-list (string-split elem-data-str "\t")]
         [elem-file (list-ref elem-list 2)]
         [usr (send username-input get-value)]
         [hstname (send hostname-input get-value)]
         [psswrd (send password-input get-value)]
         [rmt-dir (send remote-dir-input get-value)]
         [conn (ftp-establish-connection hstname 21 usr psswrd)]
         ;; [object-file (get-file)])
         [object-file elem-file])
    (ftp-cd conn rmt-dir)    
    (download-ftp-file hstname rmt-dir usr psswrd object-file)
    (ftp-close-connection conn)))


(define (activate-uploading)
  (let* ([usr (send username-input get-value)]
         [hstname (send hostname-input get-value)]
         [psswrd (send password-input get-value)]
         [object-file (send object-file-input get-value)]
         [rmt-dir (send remote-dir-input get-value)])
    (upload-ftp-file hstname rmt-dir usr psswrd object-file)))

(define (activate-uploading-with-dialog)
  (let* ([usr (send username-input get-value)]
         [hstname (send hostname-input get-value)]
         [psswrd (send password-input get-value)]
         [object-file (get-file)]
         [rmt-dir (send remote-dir-input get-value)])
    (upload-ftp-file hstname rmt-dir usr psswrd object-file)))


(define (show-ftp-directory-list-fl)
  (let* ([*server* (send hostname-input get-value)]
         [*remote-dir* (send remote-dir-input get-value)]
         [*user* (send username-input get-value)]
         [*password* (send password-input get-value)]
         [conn (ftp-establish-connection
                *server*
                21
                *user*
                *password*)])
    (send file-list-box clear)
    (ftp-cd conn *remote-dir*)
    (map
     (lambda (elem) (send file-list-box append (string-join elem "\t")))
     (ftp-directory-list conn "."))
    (ftp-close-connection conn)))

;; control
(define (change-directory)
    (let* ([elem-num (send file-list-box get-selection)]
           [elem-data-str (send file-list-box get-string elem-num)]
           [elem-list (string-split elem-data-str "\t")]
           [elem-file (list-ref elem-list 2)]
           [elem-file-str (string-append elem-file "/")]
           [*server* (send hostname-input get-value)]
           [*remote-dir* (send remote-dir-input get-value)]
           [*user* (send username-input get-value)]
           [*password* (send password-input get-value)]
           [conn (ftp-establish-connection
                  *server*
                  21
                  *user*
                  *password*)])
      (send file-list-box clear)
      (ftp-cd conn *remote-dir*)
      (ftp-cd conn elem-file-str)
      (map
       (lambda (elem) (send file-list-box append (string-join elem "\t")))
       (ftp-directory-list conn "."))
      (ftp-close-connection conn)))

