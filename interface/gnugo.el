;;; This is GNU Go, a Go program. Contact gnugo@gnu.org, or see
;;; http://www.gnu.org/software/gnugo/ for more information.   
;;;                                                            
;;; Copyright 1999, 2000, 2001 by the Free Software Foundation.            
;;;                                                            
;;; This program is free software; you can redistribute it and/
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation - version 2.  
;;;                                                            
;;; This program is distributed in the hope that it will be    
;;; useful, but WITHOUT ANY WARRANTY; without even the implied 
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    
;;; PURPOSE.  See the GNU General Public License in file COPYIN
;;; for more details.                                          
;;;                                                            
;;; You should have received a copy of the GNU General Public  
;;; License along with this program; if not, write to the Free 
;;; Software Foundation, Inc., 59 Temple Place - Suite 330,    
;;; Boston, MA 02111, USA.
;;; 
;;; This Emacs mode for GNU Go requires Emacs 2.4 or later.
;;;
;;; ID: gnugo.el,v 1.10 2000/02/05 07:08:41 ttn Exp
;;;
;;;---------------------------------------------------------------------------
;;; Variables

(defvar gnugo-board-mode-map nil
  "Keymap for GNUGO Board mode.")

(defvar gnugo-option-history '("--mode=emacs --quiet")
  "History of GNUGO command-line options.")

;;;---------------------------------------------------------------------------
;;; Support functions

(defun gnugo-sentinel (proc string)
  (let ((status (process-status proc)))
    (when (or (eq status 'exit)
              (eq status 'signal))
      (switch-to-buffer (get 'gnugo 'cbuf))
      (delete-other-windows)
      (delete-process proc)
      (put 'gnugo 'proc nil))))

(defun gnugo-insertion-filter (proc string)
  (let ((console-string nil)
	(board-string nil))
    (if (get 'gnugo 'console-p)
	(if (string-match "EMACS1" string)
	    (progn
	      (setq console-string (substring string 0 (match-beginning 0))
		    board-string (substring string (match-end 0)))
	      (if (string-match "EMACS0" board-string)
		  (setq console-string
			(concat console-string
				(substring board-string (match-end 0)))
			board-string
			(substring board-string 0 (match-beginning 0)))
	      (put 'gnugo 'console-p nil)))
	  (setq console-string string))
      (if (string-match "EMACS0" string)
	  (progn
	    (setq board-string (substring string 0 (match-beginning 0))
		  console-string (substring string (match-end 0)))
	    (put 'gnugo 'console-p t))
	(setq board-string string)))
    (when console-string
      (let ((cbuf (get 'gnugo 'cbuf)))
	(with-current-buffer cbuf
	  (goto-char (process-mark proc))
	  (insert console-string)
	  (set-marker (process-mark proc) (point))
	  (set-window-point (get-buffer-window cbuf) (point))
	  (display-buffer cbuf)
	  (recenter -1))))
    (when board-string
      (with-current-buffer (get 'gnugo 'bbuf)
	(goto-char (point-max))
	(insert board-string)
	(recenter -1)
	(let ((last-pos (get 'gnugo 'last)))
	  (when (> (point-max) last-pos)
	    (goto-char last-pos)))))))

(defun gnugo-progn (pair-list)
  (mapcar (lambda (pair)
            (condition-case error
                (let ((it (get 'gnugo (car pair))))
                  (when it
                    (funcall (cdr pair) it)))
              (error t)))
	  pair-list))

(defun gnugo-cleanup ()
  "Kill gnugo process and buffers.  Reset internal state."
  (gnugo-progn '((proc . delete-process)
                 (cbuf . kill-buffer)
                 (bbuf . kill-buffer)
                 (orig . switch-to-buffer)))
  (delete-other-windows)
  (setplist 'gnugo nil))

(defun gnugo-position ()
  "Examine buffer, returning position based on point.
The position is expressed as a string: (concat LETTER NUMBER)"
  (let* ((letter (save-excursion
		   (let ((col (current-column)))
		     (re-search-forward "^\\s-+A B C")
		     (move-to-column col)
		     (buffer-substring (point) (1+ (point))))))
	 (number (save-excursion
		   (beginning-of-line)
		   (looking-at "\\s-+\\([0-9]+\\)")
		   (match-string 1)))
	 (pos (concat letter number)))
    (if (string-match "[A-T][0-9][0-9]*" pos)
	pos
      (error "Not a proper position point"))))

(defun gnugo-send (string)
  (process-send-string (get 'gnugo 'proc) string))

(defun gnugo-move ()
  "Make a move"
  (let ((pos (concat (gnugo-position) "\n")))
    (put 'gnugo 'last (point))
    (with-current-buffer (get 'gnugo 'cbuf)
      (goto-char (point-max))
      (insert pos)
      (set-marker (process-mark (get 'gnugo 'proc)) (point))
      (recenter -1))
    (with-current-buffer (get 'gnugo 'bbuf)
      (erase-buffer))
    (gnugo-send pos)))

(defun gnugo-refresh (&optional board-buf console-buf)
  (let ((bbuf (or board-buf (get 'gnugo 'bbuf)))
	(cbuf (or console-buf (get 'gnugo 'cbuf))))
    (delete-other-windows)
    (switch-to-buffer bbuf)
    (split-window-horizontally (get 'gnugo 'board-cols))
    (other-window 1)
    (switch-to-buffer cbuf)
    (other-window -1)))

(defun gnugo-bury ()
  (gnugo-progn '((orig . switch-to-buffer)))
  (delete-other-windows))

(defun gnugo-board-mode ()
  "In this mode, keys do not self insert.
Here are the default keybindings:

  RET or SPC	Select point as the next move.  An error is signalled
		for invalid locations.  Illegal locations, on the other
		hand, show up in the GNUGO Console buffer.

  q or Q	Quit.  Both Board and Console buffers are deleted.

  R		Resign.

  C-l		Refresh.  Includes restoring default window configuration.

  M-_		Bury both Board and Console buffers (when the boss is near).

  p		Pass; i.e., select no location for your move.

  :		Extended command.  Type in a string to be passed
		directly to the inferior GNUGO process."
  (kill-all-local-variables)
  (use-local-map gnugo-board-mode-map)
  (setq major-mode 'gnugo-board-mode)
  (setq mode-name "GNUGO Board"))

(defun gnugo-interpret-action (action)
  (if (stringp action)
      (gnugo-send (concat action "\n"))
    (funcall action)))

(defun gnugo-command ()
  (gnugo-interpret-action (read-string "Command: ")))

;;;---------------------------------------------------------------------------
;;; Entry point

;;;###autoload
(defun gnugo ()
  "Run gnugo in a buffer, or resume a game in progress.
You are queried for command-line options, which must include
\"--mode=emacs --quiet\".  If there is already a game in progress
you may resume it instead of starting a new one.
See function `gnugo-board-mode' for more info."
  (interactive)
  (if (and (get 'gnugo 'proc)
	   (y-or-n-p "GNU Go game in progress, resume play? "))
      (progn
	(put 'gnugo 'orig (current-buffer))	; update
	(switch-to-buffer (get 'gnugo 'bbuf))
	(gnugo-refresh (get 'gnugo 'bbuf) (get 'gnugo 'cbuf)))
    (gnugo-cleanup)
    (put 'gnugo 'orig (current-buffer))
    (put 'gnugo 'console-p t)
    (put 'gnugo 'last 1)
    (let* ((name "gnugo")
           (args (read-string "GNU Go options: "
                              (car gnugo-option-history)
                              'gnugo-option-history 1))
           (proc (apply 'start-process name nil name (split-string args)))
           (cbuf (generate-new-buffer "*gnugo console*"))
           (bbuf (generate-new-buffer "*gnugo board*")))
      (put 'gnugo 'board-cols
           (let ((board-size (if (string-match "--boardsize" args)
                                 (let ((start (match-end 0)))
                                   (string-match "[1-9]+" args start)
                                   (string-to-number (match-string 0 args)))
                               19)))
             (+ 8 (* 2 board-size))))
      (mapcar (lambda (sym)
		(put 'gnugo sym (eval sym)))
	      '(proc cbuf bbuf))
      (set-process-coding-system proc 'latin-1-unix 'latin-1-unix)
      (with-current-buffer cbuf
	(set-marker (process-mark proc) (point)))
      (set-process-filter proc 'gnugo-insertion-filter)
      (set-process-sentinel proc 'gnugo-sentinel)
      (gnugo-refresh bbuf cbuf))
    (gnugo-board-mode)))

;;;---------------------------------------------------------------------------
;;; Load-time actions

(unless gnugo-board-mode-map
  (setq gnugo-board-mode-map (make-sparse-keymap))
  (suppress-keymap gnugo-board-mode-map)
  (mapcar (lambda (pair)
	    (define-key gnugo-board-mode-map (car pair)
	      `(lambda () (interactive)
		 (gnugo-interpret-action ',(cdr pair)))))
	  '(("\C-m"	. gnugo-move)
	    (" "	. gnugo-move)
	    ("q"	. gnugo-cleanup)
	    ("Q"	. gnugo-cleanup)
	    ("R"	. "resign")
	    ("\C-l"	. gnugo-refresh)
	    ("\M-_"	. gnugo-bury)
	    ("p"	. "pass")
	    ("P"	. "pass")
	    (":"	. gnugo-command))))

(provide 'gnugo)

;;; gnugo.el,v1.10 ends here ends here

