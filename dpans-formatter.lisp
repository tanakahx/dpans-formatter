#|
exec /usr/local/bin/sbcl --noinform --non-interactive --load "$0" "$@"
|#

;;
;; Script for formatting draft proposed American National Standard (dpANS) on Common Lisp
;; so that each section has its own contents in place
;;
;; Usage: sbcl dpans-formatter.lisp DPANS_INDEX_HTML
;;
;;        DPANS_DIRECTORY is a top directory of Dpans.
;;

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :alexandria :silent t)
  (ql:quickload :cl-fad :silent t)
  (ql:quickload :cl-ppcre :silent t))

(defpackage dpans
  (:use cl)
  (:import-from :alexandria
                :read-file-into-string
                :write-string-into-file)
  (:import-from :cl-ppcre
                :scan
                :register-groups-bind
                :regex-replace
                :regex-replace-all)
  (:import-from :cl-fad
                :pathname-as-directory
                :pathname-directory-pathname
                :merge-pathnames-as-file
                :list-directory))

(in-package :dpans)

(defparameter *src-path* nil)   ; Source directory including original dpANS HTML documents
(defparameter *dst-path* nil)   ; Formatted dpANS HTML documents are saved in this directory
(defparameter *index-name* nil) ; Top HTML filename (e.g. index.html)

(defun scan-href (tag)
  "Pick up an href value of <A> tag."
  (register-groups-bind (tag)
      ("href=\"([^\"#]*)#.*\"" tag)
    tag))

(defun iterate-on (tag menu scanner)
  "Iterate on each tag"
  (labels ((rec (acc &key (start 0))
             (multiple-value-bind (begin end)
                 (scan tag menu :start start)
               (if (and begin end)
                   (rec (cons (subseq menu begin (length menu)) acc)
                        :start end)
                   (mapcar scanner (reverse acc))))))
    (rec nil)))

(defun body-content (src)
  "Return the contents of <body> in src"
  (register-groups-bind (content)
      ("(?s)<div class=\"node\">.*?</div>(.*)</body>" src)
    content))

(defun link-content (link)
  "Return the contents of a given link"
  (let* ((path (merge-pathnames-as-file *src-path* link)))
    (body-content (read-file-into-string path))))

(defun scan-menu-list (src)
  "Search a <ul> tag which is a menu list except for a dictionary list."
  (multiple-value-bind (begin end)
      (scan "(?s)<ul class=\"menu\">.*?</ul>" src)
    (let ((dict (scan "(?m)^<p>Dictionary$" src)))
      (if (or (null dict)
              (and dict (< begin dict)))
          (values begin end)))))

(defun replace-menu (src)
  "Replace menu part in src with its contents in place recursively."
  (multiple-value-bind (begin end)
      (scan-menu-list src)
    (if (and begin end)
        (let ((menu (subseq src begin end))
              (head (subseq src 0 begin))
              (tail (subseq src end (length src))))
          (apply #'concatenate 
                 (append (list 'string head "<hr>")
                         (mapcar #'replace-menu
                                 (mapcar #'link-content (iterate-on "<li>" menu #'scan-href)))
                         (list tail))))
        ;; a leaf document
        (concatenate 'string src "<hr>"))))

(defun file-size (path)
  "Return file size in byte"
  (with-open-file (in path)
    (file-length in)))

(defun set-params (index-path)
  "Set up global parameters."
  (setf *src-path* (pathname-directory-pathname index-path))
  (setf *dst-path* (pathname-as-directory
                    (concatenate 'string
                                 (subseq index-path 0 (1- (length (namestring *src-path*))))
                                 ".formatted")))
  (setf *index-name* (file-namestring index-path)))

(defun copy-all-files ()
  "Copy all files from *src-path* to *dst-path*.
A file named 'Index.html' and references to it are renamed to 'Indecies.html',
because this name is conflicting with 'index.html'. Besides, *index-name* is also renamed to 'index.html'.
This can be safely done, becase 'Index.html' is renamed to another name beforehand and no conflics are occured. "
  (flet ((replace-link-to-index (src)
           (regex-replace-all "([/\"])Index\\.html" src "\\1Indecies.html")))
    (format t "Making a directory ~a~%" *dst-path*)
    (ensure-directories-exist *dst-path*)
    (format t "Copying files from ~a to ~a~%" *src-path* *dst-path*)
    (dolist (i (list-directory *src-path*))
      (let ((out-path (cond ((scan "/Index\\.html$" (namestring i))
                             (merge-pathnames-as-file *dst-path* "Indecies.html"))
                            ((scan (format nil "~a$" *index-name*) (namestring i))
                             (merge-pathnames-as-file *dst-path* "index.html"))
                            (t
                             (merge-pathnames-as-file *dst-path* (file-namestring i)))))
            (src (replace-link-to-index (read-file-into-string i))))
        (write-string-into-file src out-path :if-exists :supersede)))))

(defun flatten-sections-on-chapters ()
  "Flatten sections which has an <A> tag on each chapter document."
  (let* ((in-path (merge-pathnames-as-file *dst-path* "index.html"))
         (index (read-file-into-string in-path)))
    (multiple-value-bind (begin end)
        (scan-menu-list index)
      (if (and begin end)
          (let* ((menu (subseq index begin end))
                 (chapter-list (iterate-on "<li>" menu #'scan-href)))
            (dolist (c chapter-list)
              (let ((in-path (merge-pathnames-as-file *dst-path* c))
                    (out-path (merge-pathnames-as-file *dst-path* c)))
                (write-string-into-file (replace-menu (read-file-into-string in-path))
                                        out-path :if-exists :supersede)
                (format t "Converting Ch. ~27a=> ~54a (~7d bytes)~%"
                        (regex-replace "\.html" c "") out-path (file-size out-path)))))))))

(defun main (args)
  "args must include pathname of an index HTML file."
  (when (< (length args) 1)
    (format t "USAGE: dpANS index HTML is required as an argument.~%")
    (return-from main))
  (set-params (car args))
  (copy-all-files)
  (flatten-sections-on-chapters))

(main uiop:*command-line-arguments*)
