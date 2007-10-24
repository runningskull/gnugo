;;; make-xpms-file.el --- create gnugo.el-support elisp from xpm files
;;; gnugo.el
;;;
;;; This is GNU Go, a Go program. Contact gnugo@gnu.org, or see
;;; http://www.gnu.org/software/gnugo/ for more information.   
;;;                                                            
;;; Copyright (C) 2003, 2004 by the Free Software Foundation.
;;;                                                            
;;; This program is free software; you can redistribute it and/
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation - version 3
;;; or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be    
;;; useful, but WITHOUT ANY WARRANTY; without even the implied 
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    
;;; PURPOSE.  See the GNU General Public License in file COPYING
;;; for more details.                                          
;;;                                                            
;;; You should have received a copy of the GNU General Public  
;;; License along with this program; if not, write to the Free 
;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,    
;;; Boston, MA 02111, USA.
;;; 
;;; This Emacs mode for GNU Go may work with Emacs 20.x but
;;; the graphical display requires Emacs 21.x.
;;;
;;; Maintainer: Thien-Thi Nguyen

;;; Commentary:

;; Usage: EBATCH -l make-xpms-file.el -f make-xpms-file OUTFILE [XPM ...]
;;        where EBATCH is: emacs -batch --no-site-file
;;
;; Write to OUTFILE emacs lisp that encapsulates each XPM file.

;;; Code:

(require 'pp)

(unless (fboundp 'delete-dups)
  (defun delete-dups (list)             ; from repo 2004-10-29
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
    (let ((tail list))
      (while tail
        (setcdr tail (delete (car tail) (cdr tail)))
        (setq tail (cdr tail))))
    list))

(defun make-xpms-file-usage ()
  (message "Usage: %s OUTFILE [XPM ...]" (car (command-line)))
  (error "Quit"))

(defun make-xpms-file-alist-entry (xpm)
  (let* ((stem (file-name-sans-extension (file-name-nondirectory xpm)))
         (bits (progn (find-file xpm)
                      (prog1 (buffer-string)
                        (kill-buffer (current-buffer)))))
         (nump (string-match "[0-9]$" stem))
         ;; 1 2 3
         ;; 4 5 6
         ;; 7 8 9
         (key (if (not nump)
                  (cons (intern stem) 5)
                (cons (intern (substring stem 0 -1))
                      (string-to-number (substring stem -1))))))
    (cons key bits)))

(defun make-xpms-file ()
  (unless noninteractive
    (error "Interactive use for make-xpms-file not supported, sorry"))
  (let ((outfile (car command-line-args-left))
        (xpms (cdr command-line-args-left))
        entries doc)
    (unless (and outfile xpms)
      (make-xpms-file-usage))
    (setq entries (mapcar 'make-xpms-file-alist-entry xpms)
          doc (concat
               "Alist of XPM images suitable for use by gnugo.el.\n"
               "Keys are (TYPE . PLACE), where TYPE is one of:\n"
               "  " (mapconcat 'symbol-name
                               (delete-dups (mapcar 'caar entries))
                               " ")
               "\n"
               "and PLACE is an integer describing a visible location:\n"
               "  1 2 3\n  4 5 6\n  7 8 9.\n"
               "The image values are the result of `find-image'."))
    (find-file outfile)
    (erase-buffer)
    (let ((standard-output (current-buffer)))
      (prin1 ";;; generated file --- do not edit!\n
;;; This is GNU Go, a Go program. Contact gnugo@gnu.org, or see
;;; http://www.gnu.org/software/gnugo/ for more information.
;;;
;;; Copyright (C) 2003, 2004 by the Free Software Foundation.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation - version 3
;;; or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License in file COPYING
;;; for more details.
;;;                        
;;; You should have received a copy of the GNU General Public
;;; License along with this program; if not, write to the Free
;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02111, USA.\n\n")
      (mapc 'pp `((defconst gnugo-xpms
                    (mapcar (lambda (pair)
                              (cons (car pair)
                                    (find-image
                                     (list (list :type 'xpm
                                                 :data (cdr pair)
                                                 :ascent 'center)))))
                            ',entries)
                    ,doc)
                  (provide 'gnugo-xpms))))
    (save-buffer)
    (kill-buffer (current-buffer))))


;;; make-xpms-file.el ends here
