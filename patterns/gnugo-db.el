;; This file is distributed with GNU Go, a Go program.
;;
;; Copyright (C) 1999, 2000, 2002, 2003, 2004, 2005, 2006
;; 2007, 2008, 2009 and 2010 by the Free Software Foundation.
;;
;; This program is free software; you can redistribute it and/
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation - version 3
;; or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License in file COPYIN
;; for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
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
  (define-key gnugo-db-mode-map "\C-c\C-p" 'gnugo-db-insert-pattern)
  (define-key gnugo-db-mode-map "\C-c\C-c"
    'gnugo-db-copy-main-diagram-to-constraint))

(defvar gnugo-db-mode-abbrev-table nil)
(define-abbrev-table 'gnugo-db-mode-abbrev-table ())

(defvar gnugo-db-mode-syntax-table nil)
(unless gnugo-db-mode-syntax-table
  (setq gnugo-db-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\# "<" gnugo-db-mode-syntax-table)
  (modify-syntax-entry ?\n ">#" gnugo-db-mode-syntax-table))

(defvar gnugo-db-font-lock-keywords (list "Pattern"
					  "goal_elements"
					  "callback_data"
					  "attribute_map"))


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
  (set (make-local-variable 'indent-region-function) 'gnugo-db-indent-region)
  (setq mode-name "GNU Go pattern database")
  (setq major-mode 'gnugo-db-mode))


(defun gnugo-db-indent-line(&optional indenting-region)
  "Indents a line of a constraint or main diagram line with comment."
  (let ((return-point (point)))
    (beginning-of-line)
    (let ((line-beginning (point))
	  (first-char (char-after)))
      (unless (= first-char ?\;)
	(forward-line -1)
	(when (= (char-after) ?\;)
	  (setq first-char ?\;)))

      (let* ((column)
	     (indentation
	      (if (= first-char ?\;)
		  (progn
		    (while (and (= (char-after) ?\;)
				(= (forward-line -1) 0)))
		    (let ((paren-stack ()))
		      (while (search-forward-regexp "[][()]" line-beginning t)
			(let ((char (char-before)))
			  (if (memq char '(?\( ?\[))
			      (push (list char (current-column)) paren-stack)
			    (let ((pop-paren (cond ((= char ?\)) ?\()
						   ((= char ?\]) ?\[))))
			      (while (not (= (car (pop paren-stack)) pop-paren))
				())))))
		      (goto-char line-beginning)
		      (setq column (if paren-stack
				       (cadr (car paren-stack))
				     2)))
		    (concat ";"
			    (make-string (/ column tab-width) ?\t)
			    (make-string (if (< column tab-width)
					     (1- column)
					   (% column tab-width))
					 ? )))

		(goto-char line-beginning)
		(if (memq first-char '(?- ?+ ?| ?. ?X ?O ?x ?o
					  ?, ?! ?* ?? ?Y ?Q))
		    (progn
		      (let ((diagram-width 0))
			(while (not (memq (char-after) '(?  ?\t ?\n nil)))
			  (setq diagram-width (1+ diagram-width))
			  (forward-char))
			(if (< diagram-width 8)
			    (progn (setq column 12)
				   "\t    ")
			  (setq column (+ diagram-width 4))
			  "    ")))
		  nil))))

	(when indentation
	  (let ((indentation-point (point))
		(indentation-length (length indentation))
		(matched 0))
	    (while (and (< matched indentation-length)
			(eq (char-after) (aref indentation matched)))
	      (setq matched (1+ matched))
	      (forward-char))
	    (while (memq (char-after) '(?  ?\t))
	      (forward-char))
	    (unless (or (= (current-column) column)
			(and indenting-region (memq (char-after) '(?\n nil))))
	      (setq return-point (+ return-point
				    indentation-length
				    (- indentation-point (point))))
	      (delete-region (+ indentation-point matched) (point))
	      (when (< matched indentation-length)
		(insert (substring indentation matched))))
	    (when (< return-point (point))
	      (setq return-point (point)))))))

    (goto-char return-point)))


(defun gnugo-db-indent-region(start end)
  "Indents a region.  Indents in the same way as `gnugo-db-indent-line'."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (or (and (bolp) (eolp))
	  (gnugo-db-indent-line t))
      (forward-line))
    (move-marker end nil)))


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
	  (re-search-forward "^:" (1+ (buffer-size)) t)
	  (backward-char)
	  (forward-line 2)
	  (unless (memq (char-after) '(?\n ?  ?\t))
	    (re-search-forward "^[;>]" (1+ (buffer-size)) t)
	    (backward-char)
	    (while (looking-at "[;>]")
	      (forward-line))
	    (forward-line)
	    (when (looking-at "[;>]")
	      (while (looking-at "[;>]")
		(forward-line))
	      (forward-line)))
	  (when (= (forward-line) 1)
	    (end-of-line)
	    (insert "\n")))
      (re-search-forward "^Pattern " (1+ (buffer-size)) t)
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
		    (re-search-forward "^Pattern " (1+ (buffer-size)) t)
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


(defun gnugo-db-copy-main-diagram-to-constraint()
  "Copies pattern diagram to constraint and inserts a dummy constraint line"
  (interactive)
  (let ((start-point (point)))
    (end-of-line)
    (unless (re-search-backward "^Pattern " 0 t)
      (re-search-forward "^Pattern" (1+ (buffer-size)) t)
      (beginning-of-line))

    (forward-line)
    (while (not (looking-at "[-+|.XOxo,!*?YQ]"))
      (forward-line))

    (let ((diagram-beginning (point)))
      (while (looking-at "[-+|.XOxo,!*?YQ]")
	(forward-line))

      (let ((diagram (buffer-substring diagram-beginning  (point))))
	(re-search-forward "^:" (1+ (buffer-size)) t)
	(backward-char)
	(forward-line)
	(while (looking-at "#")
	  (forward-line))
	(when (memq (char-after) '(?\n ?  ?\t))
	  (forward-line))

	(when (looking-at "[-+|.XOxo,!*?YQ;>]")
	  (goto-char start-point)
	  (error "Pattern already seems to have a constraint"))

	(let ((constraint-diagram-beginning (point)))
	  (insert diagram)
	  (let ((constraint-diagram-end (point)))
	    (goto-char constraint-diagram-beginning)
	    (while (not (= (point) constraint-diagram-end))
	      (while (not (memq (char-after) '(?\n ?  ?\t)))
		(forward-char))
	      (unless (= (char-after) ?\n)
		(let ((diagram-line-end (point)))
		  (end-of-line)
		  (setq constraint-diagram-end
			(- constraint-diagram-end (- (point) diagram-line-end)))
		  (delete-region diagram-line-end (point))))
	      (forward-char))

	    (insert "\n; \n\n")
	    (goto-char constraint-diagram-beginning)))))))


(provide 'gnugo-db)
