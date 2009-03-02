;;; ID: $Id: gnugo.el,v 1.1.1.1 2008/12/21 18:47:58 bump Exp $
;;;
;;; This is GNU Go, a Go program. Contact gnugo@gnu.org, or see
;;; http://www.gnu.org/software/gnugo/ for more information.   
;;;                                                            
;;; Copyright 1999, 2000, 2001 by the Free Software Foundation.            
;;;                                                            
;;; This program is free software; you can redistribute it and/
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation - version 3,
;;; or (at your option) any later version.
;;;                                                            
;;; This program is distributed in the hope that it will be    
;;; useful, but WITHOUT ANY WARRANTY; without even the implied 
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR    
;;; PURPOSE.  See the GNU General Public License in file COPYIN
;;; for more details.                                          
;;;                                                            
;;; You should have received a copy of the GNU General Public  
;;; License along with this program; if not, write to the Free 
;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,    
;;; Boston, MA 02111, USA.

;;; Description: Run GNU Go in a buffer.

;;; Commentary:

;; This is an interface to GNU Go using the Go Text Protocol.  Interaction
;; with the gnugo subprocess is synchronous except for `gnugo-get-move'.  This
;; means you can use Emacs to do other things while gnugo is thinking about
;; its move.  (Actually, all interaction with the subprocess is inhibited
;; during thinking time -- really, trying to distract your opponent is poor
;; sportsmanship. :-)
;;
;; Customization is presently limited to `gnugo-animation-string', q.v.
;;
;; This code was tested with Emacs 20.7 on a monochrome 80x24 terminal.

;;; Code:

(require 'cl)                           ; use the source luke!

;;;---------------------------------------------------------------------------
;;; Variables

(defvar gnugo-board-mode-map nil
  "Keymap for GNU Go Board mode.")

(defvar gnugo-option-history nil
  "History of additional GNU Go command-line options.")

(defvar gnugo-animation-string
  (let ((jam "*#") (blink " #") (spin "-\\|/") (yada "*-*!"))
    (concat jam jam jam jam jam
            ;; "SECRET MESSAGE HERE"
            blink blink blink blink blink blink blink blink
            ;; Playing go is like fighting ignorance: when you think you have
            ;; surrounded something by knowing it very well it often turns
            ;; out that in the time you spent deepening this understanding,
            ;; other areas of ignorance have surrounded you.
            spin spin spin spin spin spin spin spin spin
            ;; Playing go is not like fighting ignorance: what one person
            ;; knows many people may come to know; knowledge does not build
            ;; solely move by move.  Wisdom, on the other hand...
            yada yada yada))
  "*String whose individual characters are used for animation.
Specifically, the `gnugo-worm-stones' and `gnugo-dragon-stones' commands
render the stones in their respective (computed) groups as the first
character in the string, then the next, and so on until the string (and/or
the viewer) is exhausted.")

;;;---------------------------------------------------------------------------
;;; Support functions

(defun gnugo-other (color)
  (if (string= "black" color) "white" "black"))

(defun gnugo-gate ()
  (unless (eq (current-buffer) (get 'gnugo 'bbuf))
    (error "Wrong buffer -- try M-x gnugo"))
  (when (eq 'waiting (get 'gnugo 'get-move-state))
    (error "Not your turn yet -- please wait"))
  (when (eq 'game-over (get 'gnugo 'last-move))
    (error "Sorry, game over")))

(defun gnugo-sentinel (proc string)
  (let ((status (process-status proc)))
    (when (or (eq status 'exit)
              (eq status 'signal))
      (switch-to-buffer (get 'gnugo 'bbuf))
      (delete-other-windows)
      (delete-process proc)
      (put 'gnugo 'proc nil))))

(defun gnugo-send-line (line)
  (process-send-string (get 'gnugo 'proc) (concat line "\n")))

(defun gnugo-synchronous-send/return (message)
  "Return (TIME . STRING) where TIME is that returned by `current-time' and
STRING omits the two trailing newlines.  See also `gnugo-query'."
  (when (eq 'waiting (get 'gnugo 'get-move-state))
    (error "sorry, still waiting for %s to play" (get 'gnugo 'gnugo-color)))
  (put 'gnugo 'sync-return "")
  (let ((proc (get 'gnugo 'proc)))
    (set-process-filter
     proc #'(lambda (proc string)
              (let* ((so-far (get 'gnugo 'sync-return))
                     (start  (max 0 (- (length so-far) 2))) ; backtrack a little
                     (full   (put 'gnugo 'sync-return (concat so-far string))))
                (when (string-match "\n\n" full start)
                  (put 'gnugo 'sync-return
                       (cons (current-time) (substring full 0 -2)))))))
    (gnugo-send-line message)
    (let (rv)
      ;; type change => break
      (while (stringp (setq rv (get 'gnugo 'sync-return)))
        (accept-process-output proc))
      (put 'gnugo 'sync-return "")
      rv)))

(defun gnugo-query (message)
  "Return cleaned-up value of a call to `gnugo-synchronous-send/return', q.v.
The TIME portion is omitted as well as the first two characters of the STRING
portion (corresponding to the status indicator in the Go Text Protocol).  Use
this function when you are sure the command cannot fail."
  (substring (cdr (gnugo-synchronous-send/return message)) 2))

(defun gnugo-goto-pos (pos)
  "Move point to board position POS, a letter-number string."
  (goto-char (point-min))
  (search-forward (substring pos 0 1))
  (let ((col (1- (current-column))))
    (re-search-forward (concat "^\\s-*" (substring pos 1) "\\s-"))
    (move-to-column col)))

;;;---------------------------------------------------------------------------
;;; Game play actions

(defun gnugo-showboard ()
  (interactive)
  (let ((board (cdr (gnugo-synchronous-send/return "showboard")))
        white-captures black-captures)
    (with-current-buffer (get 'gnugo 'bbuf)
      (delete-region (point-min) (point-max))
      (insert (substring board 3))      ; omit "= \n"
      (goto-char (point-min))
      (while (re-search-forward "\\s-*\\(WH\\|BL\\).*capt.*\\([0-9]+\\).*$"
                                (point-max) t)
        (if (string= "WH" (match-string 1))
            (setq white-captures (match-string 2))
          (setq black-captures (match-string 2)))
        (replace-match ""))
      (goto-char (point-max))
      (move-to-column-force (get 'gnugo 'board-cols))
      (delete-region (point) (point-max))
      (let (pos)
        (insert
         (case (get 'gnugo 'last-move)
           ((nil) "(black to play)")
           ((game-over) "(t toggle, ! score, q quit)")
           (t (let* ((last-move (get 'gnugo 'last-move))
                     (color (car last-move))
                     (move (cdr last-move)))
                (setq pos (and (not (string= "PASS" move)) move))
                (format "%s: %s (%s to play)\n%scaptures: black %s white %s"
                        color move (gnugo-other color)
                        (make-string (get 'gnugo 'board-cols) 32) ; space
                        black-captures white-captures)))))
        (when pos
          (gnugo-goto-pos pos)
          (delete-char -1) (insert "(")
          (forward-char 1) (delete-char 1) (insert ")")))
      (goto-char (get 'gnugo 'last)))))

(defun gnugo-get-move-insertion-filter (proc string)
  (let* ((so-far (get 'gnugo 'get-move-string))
         (full   (put 'gnugo 'get-move-string (concat so-far string))))
    (when (string-match "^= \\(.+\\)\n\n" full)
      (let ((pos (match-string 1 full)))
        (put 'gnugo 'get-move-string nil)
        (put 'gnugo 'get-move-state nil)
        (put 'gnugo 'last-move (cons (get 'gnugo 'gnugo-color) pos))
        (gnugo-showboard)
        (put 'gnugo 'passes
             (if (string= "PASS" pos)
                 (1+ (get 'gnugo 'passes))
               0))
        (when (= 2 (get 'gnugo 'passes))
          (put 'gnugo 'last-move 'game-over))))))

(defun gnugo-get-move (color)
  (put 'gnugo 'get-move-state 'waiting)
  (set-process-filter (get 'gnugo 'proc) 'gnugo-get-move-insertion-filter)
  (gnugo-send-line (concat "genmove " color))
  (accept-process-output))

(defun gnugo-cleanup (&optional quietly)
  "Kill gnugo process and *gnugo board* buffer.  Reset internal state."
  (interactive)
  (let ((proc (get 'gnugo 'proc)))
    (when proc
      (delete-process proc)))
  (let ((bbuf (get 'gnugo 'bbuf)))
    (when (and bbuf (get-buffer bbuf))
      (kill-buffer bbuf)))
  (unless quietly
    (message "Thank you for playing GNU Go."))
  (setplist 'gnugo nil))

(defun gnugo-position ()
  (let* ((letter (ignore-errors
                   (save-excursion
                     (let ((col (current-column)))
                       (re-search-forward "^\\s-+A B C")
                       (move-to-column col)
                       (buffer-substring (point) (1+ (point)))))))
 (number (save-excursion
   (beginning-of-line)
   (looking-at "\\s-*\\([0-9]+\\)")
   (match-string 1)))
 (pos (concat letter number)))
    (if (string-match "^[A-T][1-9][0-9]*$" pos)
pos
      (error "Not a proper position point"))))

(defun gnugo-move ()
  "Make a move on the *gnugo board* buffer.
The position is computed from current point.
Signal error if done out-of-turn or if game-over.
To start a game try M-x gnugo."
  (interactive)
  (gnugo-gate)
  (let* ((pos (gnugo-position))
         (move (format "play %s %s" (get 'gnugo 'user-color) pos))
         (accept (cdr (gnugo-synchronous-send/return move)))
         (status (substring accept 0 1)))
    (cond ((string= "=" status)
           (put 'gnugo 'last (point))
           (put 'gnugo 'last-move (cons (get 'gnugo 'user-color) pos))
           (put 'gnugo 'passes 0)
           (gnugo-showboard))
          (t (error accept)))
    (gnugo-get-move (get 'gnugo 'gnugo-color))))

(defun gnugo-mouse-move (e)
  "Do `gnugo-move' at mouse location."
  (interactive "@e")
  (mouse-set-point e)
  (when (looking-at "[.+]")
    (gnugo-move)))

(defun gnugo-pass ()
  "Make a pass on the *gnugo board* buffer.
Signal error if done out-of-turn or if game-over.
To start a game try M-x gnugo."
  (interactive)
  (gnugo-gate)
  (let ((passes (1+ (get 'gnugo 'passes))))
    (put 'gnugo 'passes passes)
    (put 'gnugo 'last-move
         (if (= 2 passes)
             'game-over
           (cons (get 'gnugo 'user-color) "PASS")))
    (gnugo-showboard)
    (unless (= 2 passes)
      (gnugo-get-move (get 'gnugo 'gnugo-color)))))

(defun gnugo-mouse-pass (e)
  "Do `gnugo-pass' at mouse location."
  (interactive "@e")
  (mouse-set-point e)
  (gnugo-pass))

(defun gnugo-refresh ()
    "Display *gnugo board* buffer and update it with the current board state.
During normal play, parenthesize the last-played stone (no parens for pass),
and display at bottom-right corner a message describing the last-played
position, who played it (and who is to play), and the number of stones
captured thus far by each player."
  (interactive)
  (switch-to-buffer (get 'gnugo 'bbuf))
  (gnugo-showboard))

(defun gnugo-animate-group (command)
  (message "Computing %s ..." command)
  (let ((stones (cdr (gnugo-synchronous-send/return
                      (format "%s %s" command (gnugo-position))))))
    (if (not (string= "=" (substring stones 0 1)))
        (error stones)
      (setq stones (split-string (substring stones 1)))
      (message "Computing %s ... %s in group." command (length stones))
      (dolist (c (string-to-list gnugo-animation-string))
        (save-excursion
          (dolist (pos stones)
            (gnugo-goto-pos pos)
            (delete-char 1)
            (insert c)))
        (sit-for 0.08675309))           ; jenny jenny i got your number...
      (sit-for 5)
      (let ((p (point)))
        (gnugo-showboard)
        (goto-char p)))))

(defun gnugo-display-group-data (command buffer-name)
  (message "Computing %s ..." command)
  (let ((data (cdr (gnugo-synchronous-send/return
                    (format "%s %s" command (gnugo-position))))))
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (insert data))
  (message "Computing %s ... done." command))

(defun gnugo-worm-stones ()
  "In the *gnugo board* buffer, animate \"worm\" at current position.
Signal error if done out-of-turn or if game-over.
See variable `gnugo-animation-string' for customization."
  (interactive)
  (gnugo-gate)
  (gnugo-animate-group "worm_stones"))

(defun gnugo-worm-data ()
  "Display in another buffer data from \"worm\" at current position.
Signal error if done out-of-turn or if game-over."
  (interactive)
  (gnugo-gate)
  (gnugo-display-group-data "worm_data" "*gnugo worm data*"))

(defun gnugo-dragon-stones ()
  "In the *gnugo board* buffer, animate \"dragon\" at current position.
Signal error if done out-of-turn or if game-over.
See variable `gnugo-animation-string' for customization."
  (interactive)
  (gnugo-gate)
  (gnugo-animate-group "dragon_stones"))

(defun gnugo-dragon-data ()
  "Display in another buffer data from \"dragon\" at current position.
Signal error if done out-of-turn or if game-over."
  (interactive)
  (gnugo-gate)
  (gnugo-display-group-data "dragon_data" "*gnugo dragon data*"))

(defun gnugo-snap ()
  (save-excursion
    (let ((letters (progn
                     (goto-char (point-min))
                     (end-of-line)
                     (split-string (buffer-substring (point-min) (point)))))
          (maxnum (read (current-buffer)))
          snap)
      (dolist (letter letters)
        (do ((number maxnum (1- number)))
            ((= 0 number))
          (let* ((pos (format "%s%d" letter number))
                 (color (gnugo-query (format "color %s" pos))))
            (unless (string= "empty" color)
              (setq snap (cons (cons pos color) snap))))))
      snap)))

(defun gnugo-toggle-dead-group ()
  "In a *gnugo board* buffer, during game-over, toggle a group as dead.
The group is selected from current position (point).
Signal error if not in game-over or if there is no group at that position."
  (interactive)
  (unless (eq 'game-over (get 'gnugo 'last-move))
    (error "Sorry, game still in play"))
  (let* ((snap (or (get 'gnugo 'snap) (put 'gnugo 'snap (gnugo-snap))))
         (pos (gnugo-position))
         (color (gnugo-query (format "color %s" pos)))
         (morgue (get 'gnugo 'morgue)))
    (if (string= "empty" color)
        (let ((already-dead (find-if '(lambda (group)
                                        (member pos (cdr group)))
                                     morgue)))
          (unless already-dead
            (error "No group at that position"))
          (put 'gnugo 'morgue (delete already-dead morgue))
          (setq color (car already-dead))
          (save-excursion
            (let ((c (if (string= color "black") "X" "O")))
              (dolist (stone (cdr already-dead))
                (gnugo-synchronous-send/return
                 (format "play %s %s" color stone))
                (gnugo-goto-pos stone) (delete-char 1) (insert c)))))
      (let ((stones (sort (split-string
                           (gnugo-query (format "worm_stones %s" pos)))
                          'string<)))
        (let ((newly-dead (cons color stones)))
          (unless (member newly-dead morgue)
            (setq morgue (put 'gnugo 'morgue (cons newly-dead morgue)))))
        ;; clear and add back everything except the dead -- yuk!
        (gnugo-synchronous-send/return "clear_board")
        (let ((all-dead (apply 'append (mapcar 'cdr morgue))))
          (dolist (pos-color snap)
            (unless (member (car pos-color) all-dead)
              (gnugo-synchronous-send/return
               (format "play %s %s" (cdr pos-color) (car pos-color))))))
        (let ((p (point)))
          ;;(gnugo-showboard)
          (dolist (worm morgue)
            (let ((c (if (string= "black" (car worm)) "x" "o")))
              (dolist (stone (cdr worm))
                (gnugo-goto-pos stone)
                (delete-char 1) (insert c))))
          (goto-char p))))))

(defun gnugo-estimate-score ()
  "Display estimated score of a game of GNU Go.
Output includes number of stones on the board and number of stones
captured by each player, and the estimate of who has the advantage (and
by how many stones)."
  (interactive)
  (message "Est.score ...")
  (let ((black (length (split-string (gnugo-query "list_stones black"))))
        (white (length (split-string (gnugo-query "list_stones white"))))
        (black-captures (gnugo-query "captures black"))
        (white-captures (gnugo-query "captures white"))
        (est (gnugo-query "estimate_score")))
    (message "Est.score ... B %s %s | W %s %s | %s"
             black black-captures white white-captures est)))

;;;---------------------------------------------------------------------------
;;; Command properties and gnugo-command

;; A direct gtp command can easily confuse gnugo.el, so we allow for
;; interpretation of any command (and still become confused when the
;; heuristics fail ;-).  Both control and data paths are are influenced by
;; these properties:
;;
;;  gnugo-full -- completely interpret the command string; the value is a
;;                func that takes the list of words derived from splitting the
;;                command string (minus the command) and handles everything.
;;
;;  gnugo-rinse -- function taking raw output string and returning a
;;                 (possibly filtered) replacement, the only one able
;;                 to set the `gnugo-post-function' property (below).
;;                 value may also be a list of such functions.
;;
;;  gnugo-output -- symbol specifying the preferred output method.
;;                     message -- show output in minibuffer
;;                     discard -- sometimes you just don't care
;;                  default is to switch to buffer "*gnugo command output*"
;;                  if the output has a newline, otherwise use `message'.
;;
;;  gnugo-post-function -- function or list of functions to call after the
;;                         command (also after all output processing); only
;;                         settable by a `gnugo-rinse' function.

(defun gnugo-command (command)
  "During a GNU Go game, send Go Text Protocol COMMAND to the subprocess."
  (interactive "sCommand: ")
  (if (string= "" command)
      (message "(no command given)")
    (let* ((split (split-string command))
           (cmd (intern (car split)))
           (full (get cmd 'gnugo-full))
           (last-message nil))
      (if full
          (funcall full (cdr split))
        (message "Doing %s ..." command)
        (let* ((ans (cdr (gnugo-synchronous-send/return command)))
               (rinse (get cmd 'gnugo-rinse))
               (where (get cmd 'gnugo-output)))
          (put cmd 'gnugo-post-function nil)
          (when rinse
            (cond ((functionp rinse) (setq ans (funcall rinse ans)))
                  ((listp rinse) (while rinse
                                   (setq ans (funcall (car rinse) ans)
                                         rinse (cdr rinse))))
                  (t (error "bad gnugo-rinse property: %s" rinse))))
          (if (string-match "unknown.command" ans)
              (message ans)
            (cond ((eq 'discard where) (message ""))
                  ((or (eq 'message where)
                       (not (string-match "\n" ans)))
                   (message ans))
                  (t (switch-to-buffer "*gnugo command output*")
                     (erase-buffer)
                     (insert ans)
                     (message "Doing %s ... done." command)))
            (let ((pf (get cmd 'gnugo-post-function)))
              (when pf
                (cond ((functionp pf) (funcall pf))
                      ((listp pf) (while pf
                                    (progn (funcall (car pf))
                                           (setq pf (cdr pf)))))
                      (t (error "bad gnugo-post-function property: %s"
                                pf)))
                (put cmd 'gnugo-post-function nil)))))))))

;;;---------------------------------------------------------------------------
;;; Major mode for interacting with a GNU Go subprocess

(defun gnugo-board-mode ()
  "In this mode, keys do not self insert.
Here are the default keybindings:

  ?             View this help.

  RET or SPC    Select point as the next move.
                An error is signalled for invalid locations.

  q or Q        Quit (the latter without confirmation).

  R             Resign.

  C-l           Refresh board.

  _ or M-_      Bury the Board buffer (when the boss is near).

  P             Pass; i.e., select no location for your move.

  w             Animate current position's worm stones.
  d             Animate current position's dragon stones.
                See variable `gnugo-animation-string'.

  W             Display current position's worm data in another buffer.
  D             Display current position's dragon data in another buffer.

  t             Toggle dead groups (when the game is over).

  !             Estimate score (at any time).

  : or ;        Extended command.  Type in a string to be passed (quite
                indirectly) to the GNU Go subprocess.  Output and emacs
                behavior depend on which command is given.  Try `help'
                to get a list of all commands.  Note that some commands
                may confuse gnugo.el."
  (kill-all-local-variables)
  (use-local-map gnugo-board-mode-map)
  (setq major-mode 'gnugo-board-mode)
  (setq mode-name "GNU Go Board"))

;;;---------------------------------------------------------------------------
;;; Entry point

;;;###autoload
(defun gnugo ()
  "Run gnugo in a buffer, or resume a game in progress.
You are queried for additional command-line options (Emacs supplies
\"--mode gtp --quiet\" automatically).  Here is a list of options
that gnugo.el understands and handles specially:

    --boardsize num   Set the board size to use (5--19)
    --color <color>   Choose your color ('black' or 'white')
    --handicap <num>  Set the number of handicap stones (0--9)

If there is already a game in progress you may resume it instead of
starting a new one.  See `gnugo-board-mode' documentation for more info.
See also variable `gnugo-option-history'."
  (interactive)
  (if (and (get 'gnugo 'proc)
           (y-or-n-p "GNU Go game in progress, resume play? "))
      (progn
        (switch-to-buffer (get 'gnugo 'bbuf))
        (gnugo-refresh))
    (gnugo-cleanup t)
    (put 'gnugo 'last 1)
    (let* ((name "gnugo")
           (args (read-string "GNU Go options: "
                              (car gnugo-option-history)
                              'gnugo-option-history))
           (proc (apply 'start-process name nil name
                        "--mode" "gtp" "--quiet"
                        (split-string args)))
           (bbuf (generate-new-buffer "*gnugo board*"))
           (board-cols (+ 8 (* 2 (if (string-match "--boardsize" args)
                                     (let ((start (match-end 0)))
                                       (string-match "[1-9]+" args start)
                                       (string-to-number (match-string 0 args)))
                                   19))))
           (user-color (if (string-match "--color" args)
                           (let ((start (match-end 0)))
                             (string-match "\\(black\\|white\\)" args start)
                             (match-string 0 args))
                         "black"))
           (gnugo-color (gnugo-other user-color))
           (handicap (if (string-match "--handicap" args)
                         (let ((start (match-end 0)))
                           (string-match "[0-9]+" args start)
                           (string-to-number (match-string 0 args)))
                       0))
           (passes 0)
           snap morgue)
      (mapcar '(lambda (sym)
                 (put 'gnugo sym (eval sym)))
              '(proc bbuf board-cols user-color gnugo-color handicap passes
                     snap morgue))
      (unless (= 0 handicap)
        (gnugo-synchronous-send/return (concat "fixed_handicap " handicap)))
      (set-process-sentinel proc 'gnugo-sentinel)
      (gnugo-refresh))
    ;; set it all up
    (gnugo-board-mode)
    ;; first move
    (when (or (and (string= "black" (get 'gnugo 'user-color))
                   (< 1 (get 'gnugo 'handicap)))
              (and (string= "black" (get 'gnugo 'gnugo-color))
                   (< (get 'gnugo 'handicap) 2)))
      (gnugo-get-move (get 'gnugo 'gnugo-color)))))

;;;---------------------------------------------------------------------------
;;; Load-time actions

(unless gnugo-board-mode-map
  (setq gnugo-board-mode-map (make-sparse-keymap))
  (suppress-keymap gnugo-board-mode-map)
  (mapcar '(lambda (pair)
             (define-key gnugo-board-mode-map (car pair) (cdr pair)))
          '(("?"        . describe-mode)
            ("\C-m"     . gnugo-move)
            (" "        . gnugo-move)
            ("P"        . gnugo-pass)
            ("R"        . (lambda () (interactive)
                            (if (y-or-n-p "Resign? ")
                                (gnugo-cleanup)
                              (message "(not resigning)"))))
            ("q"        . (lambda () (interactive)
                            (if (y-or-n-p "Quit? ")
                                (gnugo-cleanup)
                              (message "(not quitting)"))))
            ("Q"        . gnugo-cleanup)
            ("\C-l"     . gnugo-refresh)
            ("\M-_"     . bury-buffer)
            ("_"        . bury-buffer)
            ("w"        . gnugo-worm-stones)
            ("W"        . gnugo-worm-data)
            ("d"        . gnugo-dragon-stones)
            ("D"        . gnugo-dragon-data)
            ("t"        . gnugo-toggle-dead-group)
            ("!"        . gnugo-estimate-score)
            (":"        . gnugo-command)
            (";"        . gnugo-command)
            ;; mouse
            ([(down-mouse-1)] . gnugo-mouse-move)
            ([(down-mouse-3)] . gnugo-mouse-pass))))

(put 'help 'gnugo-full
     '(lambda (sel)
        (info "(gnugo)GTP command reference")
        (if (not sel)
            (message "(you can also try \"help COMMAND\" next time)")
          (let ((topic (intern (car sel))))
            (goto-char (point-min))
            (when (search-forward (concat "* " (car sel) "\n") (point-max) t)
              (let (buffer-read-only)
                (when (get topic 'gnugo-full)
                  (insert "[NOTE: fully handled by gnugo.el]\n"))
                (when (get topic 'gnugo-rinse)
                  (insert "[NOTE: output rinsed by gnugo.el]\n"))))))))

(mapc '(lambda (command)
         (put command 'gnugo-output 'discard)
         (put command 'gnugo-rinse
              '(lambda (ans)
                 (put cmd 'gnugo-post-function 'gnugo-refresh)
                 ans)))
      '(clear_board
        fixed_handicap))

(provide 'gnugo)

;;; $RCSfile: gnugo.el,v $$Revision: 1.1.1.1 $ ends here
