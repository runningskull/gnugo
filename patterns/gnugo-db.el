;; This file is distributed with GNU Go, a Go program.
;;
;; Copyright 2003 by the Free Software Foundation.
;;
;; This program is free software; you can redistribute it and/
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation - version 2.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License in file COPYIN
;; for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111, USA.


;; GNU Emacs mode for editing pattern database files.
;;
;; Put this file to emacs/site-lisp directory and add
;;
;;	(require 'gnugo-db)
;;
;; to your ~/.emacs file. If you want gnugo-db-mode to be selected
;; automatically for every .db file, add these lines also:
;;
;;	(setq auto-mode-alist
;;	      (append
;;	       auto-mode-alist
;;	       '(("\\.db\\'" . gnugo-db-mode))))


(defvar gnugo-db-mode-map nil)
(unless gnugo-db-mode-map
  (setq gnugo-db-mode-map (make-sparse-keymap))
  (define-key gnugo-db-mode-map "\C-xp" 'gnugo-db-insert-pattern))

(defvar gnugo-db-mode-abbrev-table nil)
(define-abbrev-table 'gnugo-db-mode-abbrev-table ())

(defvar gnugo-db-mode-syntax-table nil)
(unless gnugo-db-mode-syntax-table
  (setq gnugo-db-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\# "<" gnugo-db-mode-syntax-table)
  (modify-syntax-entry ?\n ">#" gnugo-db-mode-syntax-table))

(defvar gnugo-db-font-lock-keywords (list "Pattern"
					  "goal_elements"
					  "callback_data"))


(defun gnugo-db-mode()
  "Major mode for editing pattern database files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map gnugo-db-mode-map)
  (setq local-abbrev-table gnugo-db-mode-abbrev-table)
  (set-syntax-table gnugo-db-mode-syntax-table)
  (set (make-local-variable 'paragraph-start) "Pattern")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (setq font-lock-defaults '(gnugo-db-font-lock-keywords nil nil ((?_ . "w"))))
  (set (make-local-variable 'indent-line-function) 'gnugo-db-indent-line)
  (setq mode-name "GNU Go pattern database")
  (setq major-mode 'gnugo-db-mode))


(defun gnugo-db-indent-line()
  "Indents a line of a constraint."
  (let ((current-point (point)))
    (beginning-of-line)
    (let ((line-beginning (point))
	  (first-char (char-after)))
      (unless (= first-char ?\;)
	(forward-line -1)
	(setq first-char (char-after)))
      (when (= first-char ?\;)
	(while (= (char-after) first-char)
	  (forward-line -1))
	(let ((paren-stack ()))
	  (while (< (point) line-beginning)
	    (let ((char (char-after)))
	      (if (or (= char 40) (= char 91))
		  (push (list char (current-column)) paren-stack)
		(let ((pop-paren (cond ((= char 41) 40)
				       ((= char 93) 91))))
		  (when pop-paren
		    (while (not (= (car (pop paren-stack)) pop-paren))
		      ())))))
	    (forward-char))

	  (let ((char (char-after)))
	    (when (= char first-char)
	      (forward-char))
	    (while (or (= (char-after) 32) (= (char-after) 9))
	      (forward-char))
	    (when (and paren-stack (= (char-after) (caar paren-stack)))
	      (pop paren-stack))
	    (let ((column (if paren-stack
			      (1+ (cadr (car paren-stack)))
			    2))
		  (skip-chars (max 0 (- current-point (point)))))
	      (when (or (not (= char first-char))
			(not (= column (current-column))))
		(delete-backward-char (- (point) line-beginning))
		(insert-char first-char 1)
		(insert-char 9 (/ column tab-width))
		(insert-char 32 (if (< column tab-width)
				    (1- column)
				  (% column tab-width)))
		(setq current-point (+ (point) skip-chars)))))))
      (goto-char current-point))))


(defun gnugo-db-insert-pattern()
  "Inserts a new pattern after the current one. Tries to pick up a
suitable name by incrementing numeric part of the previous pattern
name.

This function heavily depends on correctness of the current pattern."
  (interactive)
  (let ((first-name "")
	(middle-name "")
	(last-name ""))
    (end-of-line)
    (if (re-search-backward "^Pattern " 0 t)
	(progn
	  (forward-char 8)
	  (when (looking-at "\\([^0-9]+\\)\\([0-9]*\\)\\(.*\\)")
	    (setq first-name (match-string-no-properties 1)
		  middle-name (match-string-no-properties 2)
		  last-name (match-string-no-properties 3)))
	  (re-search-forward "^:" (buffer-size) t)
	  (forward-line 2)
	  (unless (memq (char-after) '(?# ?\n 32 9))
	    (re-search-forward "^[;>]" (buffer-size) t)
	    (while (looking-at "[;>]")
	      (forward-line))
	    (forward-line 2)
	    (when (looking-at "[;>]")
	      (while (looking-at "[;>]")
		(forward-line))
	      (forward-line 2)))
	  (forward-line))
      (re-search-forward "^Pattern " (buffer-size) t)
      (beginning-of-line))

    (insert "Pattern \n")
    (let ((move-to-point (1- (point))))
      (unless (string= first-name "")
	(let ((pattern-name
	       (if (string= last-name "")
		   (concat first-name
			   (number-to-string (1+ (string-to-number middle-name))))
		 (concat first-name middle-name
			 (char-to-string (1+ (string-to-char last-name)))))))
	  (when (string= last-name "")
	    (when (save-excursion
		    (re-search-forward "^Pattern " (buffer-size) t)
		    (or (looking-at pattern-name)
			(looking-at (concat first-name middle-name))))
	      (setq pattern-name (concat first-name middle-name "a"))))
	  (backward-char)
	  (insert pattern-name)
	  (forward-char)))
      (insert "\n")
      (unless (string= first-name "")
	(setq move-to-point (point)))
      (insert "\n\n:\n\n\n")
      (goto-char move-to-point))))


(provide 'gnugo-db)

