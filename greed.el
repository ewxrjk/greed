;;; greed.el --- RGTP-based GROGGS reader for GNU Emacs

;; Author: Gareth Rees <gdr11@cl.cam.ac.uk>
;;         Owen Dunn <owen@greenend.org.uk>
;;         Richard Kettlewell <rjk@greenend.org.uk>
;;         Peter Maydell <pmaydell@chiark.greenend.org.uk>
;; Created: 1 Oct 1995
;; Version: 1.4.1
;; Keywords: news

;; LCD Archive Entry:
;; greed|Gareth Rees|gdr11@cl.cam.ac.uk|
;; GREED: an RGTP-based GROGGS reader|
;; ??-???-1997|Version: 1.3|~/packages/greed-1.3.tar.gz|

;;; Copyright:

;; Copyright (C) 1995-1997 Gareth Rees, 2000 Owen Dunn,
;; 2002 Richard Kettlewell, 2005-2007 Peter Maydell

;; GREED is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; GREED is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;;; Commentary:

;; GREED stands for "GROGGS Reader/Editor with Emacs Display", and
;; GROGGS for "General-purpose Reverse-Ordered Gossip-Gathering System".
;; GROGGS was originally a bulletin board running on Cambridge
;; University's IBM 3074 mainframe, but now refers to any system
;; offering an RGTP server.  (RGTP is "Reverse Gossip Transfer
;; Protocol", a client-server protocol running over a
;; connection-oriented bytestream such as TCP/IP).

;; See the web page <http://www.groggs.group.cam.ac.uk/groggs/> for a
;; description of the project and software, and the page
;; <http://www.groggs.group.cam.ac.uk/groggs/protocol.txt> for the
;; details of the protocol.

;; GREED is a GROGGS client for GNU Emacs.  It uses a split-window
;; display like MH-E or GNUS, with a list of items in one window, and
;; the selected item in the other.  It knows about the `MD5'
;; authentication protocol and deals with user registration.

;; For instructions on installing and running GREED, read the Info file
;; whose Texinfo source comes with the GREED distribution.

;;; Code:

(defconst greed-version "1.4.1")


;;; Requirements: -------------------------------------------------------------

(eval-and-compile
  (defvar greed-xemacs-p
    (let ((case-fold-search t))
      (string-match "xemacs" emacs-version))
    "Non-NIL if Greed is running under XEmacs."))

;; The TCP package provides network support using a separate program.
;; Modern versions of GNU Emacs have this functionality built in.
(if (not (fboundp 'open-network-stream)) (require 'tcp))

(require 'greed-md5)
(require 'calendar)
(if greed-xemacs-p
    (require 'itimer)
  (require 'timer))

;; In early versions of Emacs 19, the `help-macro' package was called
;; `help-screen'; the name was changed because of 14-character filename
;; limits on some systems.
(eval-when-compile
  (if greed-xemacs-p
      (load-library "greed-help-macro.el")
    (unwind-protect
	(require 'help-macro)
      (if (not (featurep 'help-macro))
	  (require 'help-screen)))))

;; Events are opaque objects in XEmacs 21, but you have to interpret
;; their structure in other versions.
(defun greed-button-test (e)
  (if (functionp 'button-press-event-p)
      (button-press-event-p event)
    (eq (car e) 'mouse-2)))


;;; Variables -----------------------------------------------------------------

;; User options that are too esoteric to be on the options menu or saved
;; in the startup file.

(defvar greed-servers
  '(("Cambridge Groggs server" "rgtp-serv.groggs.group.cam.ac.uk" 1431
     "~/.groggsrc.el" "~/.groggsecret")
    ("Experimental Groggs server" "rgtp-serv.groggs.group.cam.ac.uk" 1432
     "~/.groggsrcx.el" "~/.groggsecretx"))
  "*List of (name server port startup-file secret-file) lists.")
(defvar greed-save-old-versions t
  "*If non-NIL, old versions of startup files are saved.")
(defvar greed-paranoid nil
  "*If this is non-nil, the client tries to authenticate the server,
and the user is prompted before their password is saved.")
(defvar greed-no-save-grognames nil
  "*If non-NIL, grognames aren't saved in `greed-grogname-list'.")
(defvar greed-reply-check-blank t
  "*If non-NIL, then GREED will query before sending a blank reply.")
(defvar greed-debug nil
  "*If non-NIL, then buffers will not be killed on exit.")
(defvar greed-timer-secs 300
  "*Number of seconds between automatic index refreshes.")
(defvar greed-continued-item-indent "+ "
  "*String with which to indent continued items.")
(defvar greed-show-sequence-numbers t
  "*If non-NIL, include the sequence number of each reply in its header
in the item buffer.")

;; These four parameters are the components of the selected member of
;; the variable `greed-servers':

(defvar greed-rgtp-server nil
  "*The name of the RGTP server.")
(defvar greed-rgtp-service nil
  "*RGTP service port number.")
(defvar greed-password-file nil
  "*The file containing the user's password on the RGTP server.")
(defvar greed-startup-file nil
  "*File from which to read LISP commands on startup.")

;; The following options are saved in the startup file and controlled by
;; the options menu.

(defvar greed-ask-for-grogname nil
  "*If non-NIL, then a grogname is prompted for before posting a reply.
If NIL, the value of `greed-grogname' is used.")
(defvar greed-choose-random-grogname nil
  "*If non-NIL, and `greed-ask-for-grogname' is NIL, then GREED chooses
a random grogname from `greed-grogname-list' for each posting.")
(defvar greed-index-height 10
  "*Number of lines in the Index window.")
(defvar greed-index-show-itemids nil
  "*If non-NIL, itemids appear in the index and mode lines.")
(defvar greed-index-show-first nil
  "*If non-NIL, userid of first reply appears in index, else userid of last.")
(defvar greed-index-reverse-order t
  "*If non-NIL, then latest item is at top of index, otherwise at bottom.")
(defvar greed-index-sort-by-last t
  "*If non-NIL, sort by last contribution to item, otherwise by first.")
(defvar greed-index-show-threads t
  "*If non-NIL, show continuation threads in index.")
(defvar greed-novice-user t
  "*If non-NIL, more help and extra confirmation prompts will be given.")
(defvar greed-quit-without-prompting nil
  "*If non-NIL, quit without prompting even if greed-novice-user is t.")
(defvar greed-save-directory nil
  "*Directory in which to save items.")
(defvar greed-grogname ""
  "*Grogname \(an identifying string appearing on each reply\).")
(defvar greed-grogname-list nil
  "*Association list of grognames user has used.")
(defvar greed-userid nil
  "*Userid on the RGTP server (typically an e-mail address).")
(defvar greed-timer-p nil
  "*If non-NIL, the index will be refreshed every `greed-timer-secs' seconds.")
(defvar greed-password nil
  "*Password on the RGTP server.
Typically a random number represented as a string of hex digits.")

;; Information about an item is stored in a vector with the following
;; fields:

(defconst greed-data-itemid 0)
(defconst greed-data-nreplies 1)
(defconst greed-data-first-id 2)
(defconst greed-data-first-seq 3)
(defconst greed-data-last-id 4)
(defconst greed-data-last-seq 5)
(defconst greed-data-subject 6)
(defconst greed-data-read-to 7)
(defconst greed-data-killed 8)
(defconst greed-data-exists 9)
(defconst greed-data-contfrom 10)
(defconst greed-data-contin 11)
(defconst greed-data-first-date 12)
(defconst greed-data-last-date 13)
(defconst greed-data-updated 14)

;; For translation between hexadecimal and ASCII (isn't there any LISP
;; code to do this?).

(defconst greed-hex-digits
  '((?0 . 0) (?1 . 1) (?2 . 2) (?3 . 3) (?4 . 4) (?5 . 5)
    (?6 . 6) (?7 . 7) (?8 . 8) (?9 . 9) (?a . 10) (?b . 11)
    (?c . 12) (?d . 13) (?e . 14) (?f . 15) (?A . 10) (?B . 11)
    (?C . 12) (?D . 13) (?E . 14) (?F . 15)))

;; These are the names of the buffers used by Greed.

(defvar greed-index-buffer "*Index*")
(defvar greed-output-buffer " *greed-output*")
(defvar greed-data-buffer " *greed-data*")
(defvar greed-log-buffer "*greed-log*")
(defvar greed-reply-buffer "*composition*")

;; These variables track state which needs to be saved or killed.

(defvar greed-item-buffers nil
  "A list of all item buffers that have been created.")
(defvar greed-buffer-list
  (list greed-index-buffer greed-output-buffer
	greed-data-buffer greed-reply-buffer)
  "GREED buffer names which should be killed when exiting.")
(defvar greed-save-variables-list
  (list 'greed-index-reverse-order 'greed-index-sort-by-last
	'greed-ask-for-grogname 'greed-choose-random-grogname
	'greed-index-height 'greed-index-show-itemids
	'greed-index-show-first 'greed-index-show-threads
	'greed-novice-user 'greed-quit-without-prompting
	'greed-save-directory 'greed-grogname 'greed-timer-p
	'greed-userid)
  "GREED variable names which should be saved to the startup file.
They should all be variables whose PRINC representation is the same as
their read syntax.")

;; Variables associated with starting, logging in and access control.

(defvar greed-startup-window-config nil) ; Initial indow configuration
(defvar greed-login-done nil)		; Login was completed successfully?
(defvar greed-access-level 0)		; 0 none 1 read 2 write 3 edit.
(defvar greed-edit-enabled nil)		; Enabled to use the editing commands?
(defvar greed-password-changed nil)	; Password has changed this session?
(defvar greed-lock nil)			; Edit in progress?

;; Variables associated with display characteristics.

(defvar greed-index-userid-length 10
  "*Length of field for userid, in characters.")
(defvar greed-item-subject-width 45
  "*Length of the subject field in the mode line.
Will be less if the itemid is printed too.")
(defvar greed-index-display-killed-items nil) ; Killed items displayed too?

;; These variables track the items GREED knows about.  Each item in the
;; index is a vector whose fields are given by the `greed-data-'
;; constants.

(defvar greed-index-number-items 0)	; Number of items in the index.
(defvar greed-index-last-seq nil)	; Last seq number used by the server.
(defvar greed-index-list nil)		; List of items in the index.
(defvar greed-displayed-index-list nil)	; List of items actually displayed.

;; Keymap variables.

(defvar greed-options-map nil)		; Keymap for the options menu.
(defvar greed-index-options-menu nil)	; Keymap for options pull-down menu.
(defvar greed-index-actions-menu nil)	; Keymap for actions pull-down menu.
(defvar greed-index-edit-menu nil)	; Keymap for editing pull-down menu.
(defvar greed-index-mode-map nil)	; Keymap for Index Mode.
(defvar greed-item-mode-map nil)	; Keymap for Item mode.
(defvar greed-reply-mode-map nil)	; Keymap for Reply Mode.
(defvar greed-reply-mode-menu nil)	; Menu for Reply Mode.
(defvar greed-reply-full-map nil)	; Keymap for replying to a full item.
(defvar greed-reply-continued-map nil)	; Keymap for replying to cont item.
(defvar greed-noedit-map nil)		; Editing keymap (user not editor).
(defvar greed-edit-map nil)		; Editing keymap (user is editor).
(defvar greed-login-map nil)		; Keymap for login/registration.

;; Hook variables.

(defvar greed-startup-hook nil
  "*Hook runs on entry to GREED.")
(defvar greed-index-mode-hook nil
  "*Hook runs on entry to GREED Index Mode.")
(defvar greed-item-mode-hook nil
  "*Hook runs on entry to GREED Item Mode.")
(defvar greed-item-prepare-hook nil
  "*Hook called after item is prepared in the Item buffer.")
(defvar greed-reply-mode-hook nil
  "*Hook run when starting to compose a reply or newitem.")
(defvar greed-index-redraw-hook nil
  "*Hook run whenever the index, or a part thereof, is redrawn.")

;; Timer variables

(defvar greed-timer nil)		; Timer used for refreshing the index.

;; Variables associated with the connection to the server.

(defvar greed-server-buffer nil)	; Buffer for RGTP server process.
(defvar greed-server-process nil)	; The RGTP server process.
(defvar greed-response-code 0)		; Most recent response code.
(defvar greed-response-text "")		; Text of most recent response.
(defvar greed-reconnect-possible t)	; Reconnection possible?

;; These variables are buffer-local in item buffers.

(defvar greed-item-data nil)		; Vector of item information.
(defvar greed-first-unread nil)		; First unread reply in item.

;; These variables are buffer-local in reply buffers.

(defvar greed-reply-to-item nil)	; Which item this is a reply to.
(defvar greed-reply-sent nil)		; Whether reply has been sent.
(defvar greed-old-window-config nil)	; Window config before reply composed.
(defvar greed-action-to-be nil)		; What action to take (reply/new/etc).


;;; XEmacs compatibility ------------------------------------------------------

;; The `mode-line-' variables have become `modeline-' variables in
;; XEmacs.  These macros are just to shut the byte-compiler up.
(defmacro greed-set-buffer-identification (bi)
  (list 'setq (if greed-xemacs-p 'modeline-buffer-identification
		'mode-line-buffer-identification)
	bi))

(defmacro greed-set-process (p)
  (list 'setq (if greed-xemacs-p 'modeline-process 'mode-line-process) p))

(defun greed-set-modified (m)
  (list 'setq (if greed-xemacs-p 'modeline-modified 'mode-line-modified) m))

;; The menu interface is very different in XEmacs.  The function
;; `greed-make-menu' builds a menu data structure and the function
;; `greed-install-menus' installs menus on the menubar.

;; Make a menu, given the menu name and the list of items in it.  Each
;; item is a list consisting of a distinctive symbol, some text to
;; appear in the menu, a callback to call when the item is selected,
;; and a form which is evaluated to tell if the item is active.  The
;; items must be in reverse order.
(defun greed-make-menu (name items)
  (let ((menu (if greed-xemacs-p nil (make-sparse-keymap name))))
    (while items
      (let ((symbol (nth 0 (car items)))
	    (text (nth 1 (car items)))
	    (callback (nth 2 (car items)))
	    (active-p (nth 3 (car items))))
	(if greed-xemacs-p
	    (setq menu (cons (vector text callback ':active (or active-p t))
			     menu))
	  (define-key menu (vector symbol) (cons text callback))
	  (if active-p
	      (put callback 'menu-enable active-p))))
      (setq items (cdr items)))
    (if greed-xemacs-p (cons name menu)
      menu)))

;; Install menus on the menubar.  The menus appear in the order they
;; are given.
(defun greed-install-menus (&rest menus)
  (if greed-xemacs-p
      ;; Need to create a local menubar in XEmacs.
      (set-buffer-menubar (copy-sequence current-menubar))
    ;; GNU Emacs installs menus in reverse order.
    (setq menus (reverse menus)))
  (while menus
    (if greed-xemacs-p
	(add-submenu nil (car menus))
      (local-set-key (vector 'menu-bar
			     (intern (concat "greed-menu-" (car (reverse (car menus))))))
		     (cons (car (reverse (car menus))) (car menus))))
    (setq menus (cdr menus))))


;;; Customization -------------------------------------------------------------

(defgroup greed nil
  "Customization of Greed"
  :group 'applications
  )

(defface greed-unread-face
  '(
    (((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "blue"))
    (t
     (:bold t))
    )
  "Face used for items with unread gossip"
  :group 'greed
  )
(defvar greed-unread-face 'greed-unread-face
  "Face used for items with unread gossip")

(defface greed-item-header-face
  '(
    (((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "green"))
    (t
     (:bold t))
    )
  "Face used for item headers"
  :group 'greed)
(defvar greed-item-header-face 'greed-item-header-face
  "Face used for item headers")

(defface greed-reply-separator-face
  '(
    (((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t
     (:bold t))
    )
  "Face used to for reply separators within items"
  :group 'greed)
(defvar greed-reply-separator-face 'greed-reply-separator-face
  "Face used to for reply separators within items")


;;; Miscellaneous utilities ---------------------------------------------------

;; Print STRING to the buffer of logged messages.
;; Runs only if `greed-debug' is non-NIL.
(defun greed-log-message (string)
  (if greed-debug
      (save-excursion
	(set-buffer (get-buffer-create greed-log-buffer))
	(goto-char (point-max))
	(insert string))))

;; Configure GREED windows.
;; If optional argumet OTHER-BUFFER is non-NIL, use that buffer instead
;; of the current item buffer.  If optional argument DONT-SHOW-CURR is
;; non-NIL, it won't force the current item buffer to be visible if it
;; exists.
(defun greed-configure-windows (&optional other-buffer dont-show-curr)
  (let* ((idxwin (get-buffer-window greed-index-buffer))
	 (itmbufname (or other-buffer (greed-current-item-buffer-name)))
	 (itmwin (get-buffer-window itmbufname))
	 (split-window-keep-point t)
	 win
	 (bufs greed-item-buffers))
    (while (and bufs (not win))
      (setq win (get-buffer-window (car bufs))
	    bufs (cdr bufs)))

    ;; Don't do any reconfiguring if (i) there's no Index buffer, or
    ;; (ii) both the Index buffer is visible *and* the necessary other
    ;; buffer (if it exists and is is required) is visible.
    (if (or (null (get-buffer greed-index-buffer))
	    (and idxwin
		 (or itmwin
		     (and (not win)
			  (or (null (get-buffer itmbufname))
			      dont-show-curr)))))
	nil

      ;; Select the Index window and make it full screen
      (select-window (or idxwin itmwin (selected-window)))
      (delete-other-windows)
      (switch-to-buffer greed-index-buffer)
      (if (get-buffer itmbufname)
	  (progn
	    (if (> (window-height (get-buffer-window greed-index-buffer))
		   greed-index-height)
		(split-window-vertically greed-index-height)
	      (split-window-vertically))
	    (other-window 1)
	    (switch-to-buffer itmbufname)
	    (other-window 1))))))

;; Visit the Info documentation node (greed)NODE.
(defun greed-info-node (node)
  (let ((b (string= (buffer-name (current-buffer)) greed-index-buffer)))
    (require 'info)
    (Info-goto-node (concat "(greed)" node))
    (if b (delete-other-windows))
    (message (substitute-command-keys
	      "Type \\[Info-exit] to return to GREED."))))

;; Return the sequence number following SEQ (an 8-character string of
;; hexadecimal digits).  The new sequence number is returned as an
;; 8-character string of hex digits.  The addition wraps around from
;; FFFFFFFF to 00000000.
(defun greed-seq-add-one (seq)
  (let ((v (greed-hex-decode seq))
	(i 3) (c 1))
    (while (and (eq c 1) (>= i 0))
      (let ((d (+ c (aref v i))))
	(if (> d 255)
	    (aset v i 0)
	  (aset v i d)
	  (setq c 0)))
      (setq i (1- i)))
    (greed-hex-encode v)))

;; Return STRING, but with spaces removed.
;; This should work using the four-argument version of replace-match,
;; but some earlier Emacses don't have that.
(defun greed-remove-spaces (string)
  (let ((new "") (i 0) (l (length string)))
    (while (< i l)
      (if (/= (aref string i) ? )
	  (setq new (concat new (char-to-string (aref string i)))))
      (setq i (1+ i)))
    new))

;; Return T if SEQ1 (string of 8 hex digits) comes before SEQ2 (ditto).
;; The reason why I can't just do ordinary string comparison is that
;; sequence numbers are allowed to be gappy, and they are allowed to
;; wrap around so long as the sequence numbers in use at a given time
;; span a range smaller than 2^31.
(defun greed-seq< (seq1 seq2)
  (let ((d1 (assq (aref seq1 0) greed-hex-digits))
	(d2 (assq (aref seq2 0) greed-hex-digits)))
    (if (and d1 d2)
	(let ((diff (- (car d2) (car d1))))
	  (cond ((< diff -8) t)
		((or (eq diff -8) (eq diff 8))
		 (string< (upcase (substring seq2 1))
			  (upcase (substring seq1 1))))
		((and (> diff -8) (< diff 0)) nil)
		((eq diff 0) (string< (upcase seq1) (upcase seq2)))
		((and (> diff 0) (< diff 8)) t)
		(t nil)))
      nil)))

;; Causes an error if access level is less than N.
(defun greed-check-access-level (n)
  (if (< greed-access-level n)
      (error "You don't have %s permission at the server."
	     (nth (1- n) '("read" "write" "edit")))))

;; Any inline functions have to be placed here so that they appear
;; before they are used.  The point of making these functions inline is
;; that `greed-read-index' (the main bottleneck in startup) runs a bit
;; quicker.

;; Returns T if N is a valid item number, NIL otherwise.
(defsubst greed-valid-item-number (n)
  (and (>= n 1) (<= n (length greed-displayed-index-list))))

;; Returns T if current item number is valid, NIL otherwise.
(defsubst greed-valid-current-item-number ()
  (greed-valid-item-number (greed-index-current-item)))

;; Returns data for item number N, or NIL if none.
(defsubst greed-index-data (n)
  (elt greed-displayed-index-list (1- n)))

;; Returns the data for the current item, or NIL if none.
(defsubst greed-index-current-data ()
  (greed-index-data (greed-index-current-item)))

;; Buffer substring from START to END with trailing spaces trimmed.
(defsubst greed-trimmed-substring (start end)
  (buffer-substring start
		    (save-excursion
		      (goto-char end)
		      (skip-chars-backward " ")
		      (point))))

;; Select the member of `greed-index-list' corresponding to ITEMID.
(defsubst greed-index-entry (itemid)
  (let ((l greed-index-list) (v nil))
    (while l
      (if (string= (aref (car l) 0) itemid)
	  (setq v (car l)
		l nil)
	(setq l (cdr l))))
    (if (null v)
	(setq v (vector itemid 0 nil nil nil nil nil nil nil
			nil nil nil nil nil nil)
	      greed-index-list (cons v greed-index-list)))
    v))

;; As `y-or-n-p', but clears the echo area afterwards.
(defsubst greed-y-or-n-p (prompt)
  (prog1 (y-or-n-p prompt) (message nil)))

;; This is a wrapper for `make-help-screen' from `help-macro.el'; it
;; sets the variable `three-step-help' to T around every call to the
;; help screen, so that the prompt line always displays.
(eval-when-compile
  (if greed-xemacs-p
      (defmacro greed-make-help-screen (fname help-line help-text helped-map)
	` (make-help-screen , fname , help-line , help-text 
			     , helped-map))
    (defmacro greed-make-help-screen (fname help-line help-text helped-map)
      (let ((real-fn (intern (concat (symbol-name fname) "-real"))))
	` (progn (defun , fname ()
		    (interactive)
		    (let ((three-step-help t)) (, real-fn)))
		  (make-help-screen
		   , real-fn , help-line , help-text 
		   , helped-map))))))


;;; Date functions ------------------------------------------------------------

;; Returns a string describing the date SECS (# secs since 1 Jan 1970,
;; given as a string of 8 hexadecimal digits).
(defun greed-format-date (secs)
    (apply 'greed-do-format-date (greed-decode-time secs)))

;; Turn date/time (as for `decode-time') into string for printing in
;; index.  Note that there are two versions of the date string (dates
;; this calendar year include the time of day; dates previously include
;; the year instead) and they should to be the same length so that the
;; index lines up properly.
(defun greed-do-format-date (sec minute hour day month year dow dst zone)
  (if (numberp month)
      (setq month (substring (calendar-month-name month) 0 3)))
  (if (/= year (nth 2 (calendar-current-date)))
      (format "%2d %3s %4d " day month year)
    (format "%2d %3s %2d:%02d" day month hour minute)))

;; Wrapper for `decode-time'.  SECS is # secs since 1 Jan 1970, given
;; as a string of 8 hexadecimal digits.  If `decode-time' is not
;; defined (it was new in 19.28) then use the more cumbersome
;; `current-time-string' instead.
(defun greed-decode-time (secs)
  (let* ((n1 (+         (cdr (assq (aref secs 7) greed-hex-digits))
	        (*   16 (cdr (assq (aref secs 6) greed-hex-digits)))
	        (*  256 (cdr (assq (aref secs 5) greed-hex-digits)))
	        (* 4096 (cdr (assq (aref secs 4) greed-hex-digits)))))
	 (n2 (+         (cdr (assq (aref secs 3) greed-hex-digits))
		(*   16 (cdr (assq (aref secs 2) greed-hex-digits)))
		(*  256 (cdr (assq (aref secs 1) greed-hex-digits)))
		(* 4096 (cdr (assq (aref secs 0) greed-hex-digits)))))
	 (time (cons n2 n1)))
    (if (fboundp 'decode-time)
	(decode-time time)
      (let* ((time-string (current-time-string time))
	     (sec (string-to-number (substring time-string 17 19)))
	     (minute (string-to-number (substring time-string 14 16)))
	     (hour (string-to-number (substring time-string 11 13)))
	     (day (string-to-number (substring time-string 8 10)))
	     (month-name (substring time-string 4 7))
	     (year (string-to-number (substring time-string 20))))
	;; Don't care about last three return values.
	(list sec minute hour day month-name year 0 nil 0)))))


;;; Starting up and shutting down: --------------------------------------------

;;;###autoload
(defun greed (&optional arg)
  "Read GROGGS.
If called with prefix argument, prompt for server from `greed-servers'.
If called like `C-u C-u M-x greed', don't read startup files."
  (interactive "P")
  (setq greed-startup-window-config (current-window-configuration))
  (if (get-buffer greed-index-buffer)
      (switch-to-buffer greed-index-buffer)
    (let* ((completion-ignore-case t)
	   (chosen-server
	    (if arg (completing-read (format "Server (default \"%s\"): "
					     (car (car greed-servers)))
				     greed-servers nil t)
	      ""))
	   (server (if (string= chosen-server "")
		       (car greed-servers)
		     (assoc chosen-server greed-servers))))
      (setq greed-rgtp-server (nth 1 server)
	    greed-rgtp-service (nth 2 server)
	    greed-startup-file (nth 3 server)
	    greed-password-file (nth 4 server)))
    (setq greed-startup-window-config (current-window-configuration))
    (switch-to-buffer greed-index-buffer)
    (greed-startup-message)
    (if (not (equal arg '(16)))
	(greed-read-startup-files))
    (setq greed-login-done nil
	  greed-password-changed nil)
    (unwind-protect
	(greed-connect-to-server)
      (if (not (and (greed-server-opened) greed-login-done))
	  (greed-clear-system)
	(greed-index-mode)
	(greed-get-index)
	(greed-index-list-items)
	(if greed-novice-user
	    (greed-index-describe-briefly))
	(add-hook 'kill-emacs-query-functions 'greed-kill-hook)
	(if greed-timer-p
	    (greed-timer-start))
	(run-hooks 'greed-startup-hook)))))

;; Insert startup message into the current buffer.
(defun greed-startup-message ()
  (let ((buffer-read-only nil))
    (erase-buffer)
    (insert (concat "GREED " greed-version
		    "\n\nRGTP-based GROGGS reader for GNU Emacs and XEmacs\n"
		    "\nGREED is self-documenting: once started, type"
                    "\na capital I to read the Info documentation.\n"
		    "\nGareth Rees\ngdr11@cl.cam.ac.uk"
		    "\nOwen Dunn\nowend@chiark.greenend.org.uk"
		    "\nRichard Kettlewell\nrjk@greenend.org.uk"
		    "\nPeter Maydell\npmaydell@chiark.greenend.org.uk"))
    (goto-char (point-min))
    (let ((fill-column (window-width)))
      (while (not (eobp))
	(center-line)
	(forward-line 1)))
    (goto-char (point-min))
    (insert-char ?\n (/ (max (- (window-height) 8) 0) 2))
    (sit-for 0)))

;; Read `greed-startup-file' and `greed-password-file'.
(defun greed-read-startup-files ()
  (if (file-exists-p greed-startup-file)
      (save-excursion
	(set-buffer (get-buffer-create greed-data-buffer))
	(erase-buffer)
	(insert-file-contents greed-startup-file)
	(eval-buffer nil)
	(kill-buffer (current-buffer))))
  (if (file-exists-p greed-password-file)
      (save-excursion
	(set-buffer (get-buffer-create greed-data-buffer))
	(erase-buffer)
	(insert-file-contents greed-password-file)
	(goto-char (point-min))
	(if (re-search-forward
	     "^md5-secret \\([a-fA-F0-9]+\\)$" nil t)
	    (setq greed-password (buffer-substring (match-beginning 1)
						   (match-end 1))))
	(kill-buffer (current-buffer)))))

;; Open network stream to remote RGTP server.
(defun greed-connect-to-server ()
  (if (greed-server-opened)
      nil
    (message "Connecting to RGTP server on %s..." greed-rgtp-server)
    (if (greed-open-server)
	(progn
	  (message nil)
	  (greed-login))
      (error "Cannot open RGTP server on %s." greed-rgtp-server))))

;; Quit reading GROGGS.
;; If optional argument NOSAVE is non-NIL, the startup files are not
;; saved.  If optional argument NOWIN is non-NIL, the window config is
;; not restored.
(defun greed-quit (&optional nosave nowin)
  (if (greed-server-opened)
      (greed-close-server))

  ;; Possibly save options
  (if (not nosave) (greed-save-startup-files))

  ;; Delete buffers, etc.
  (greed-clear-system)

  ;; Restore windows
  (set-window-configuration greed-startup-window-config))

;; Save the startup file and the password file.
(defun greed-save-startup-files ()
  (save-excursion
    (if (and greed-save-old-versions
	     (file-exists-p greed-startup-file))
	(copy-file greed-startup-file (concat greed-startup-file ".old") t))
    (set-buffer (get-buffer-create greed-data-buffer))
    (make-local-variable 'make-backup-files)
    (setq make-backup-files nil)
    (erase-buffer)
    (mapcar (function
	     (lambda (v)
	       (insert (format "(setq %s %S)\n" (symbol-name v)
			       (symbol-value v)))))
	    greed-save-variables-list)
    (insert "(setq greed-grogname-list '(")
    (let ((grognames greed-grogname-list))
      (while grognames
	(insert (format "(\"%s\")" (car (car grognames))))
	(setq grognames (cdr grognames))))
    (insert "))\n")
    (insert (format "(setq greed-index-last-seq %S)\n" greed-index-last-seq))
    (mapcar (function
	     (lambda (i)
	       (if (aref i greed-data-exists)
		   (insert (greed-make-restore-command i)))))
	    greed-index-list)
    (write-file greed-startup-file)
    ;; Save the password file only if it doesn't already exist.
    (if (and greed-password-changed
	     greed-password
	     (or (not greed-paranoid)
		 (y-or-n-p "Save password? ")))
	(progn
	  (if (and greed-save-old-versions
		   (file-exists-p greed-password-file))
	      (copy-file greed-password-file
			 (concat greed-password-file ".old") t))
	  (erase-buffer)
	  (insert "md5-secret " greed-password "\n")
	  (write-file greed-password-file)
	  (set-file-modes greed-password-file 384)))
    (kill-buffer (current-buffer))))

;; Restore partial item information: ITEMID, READ-TO and KILLED.
;; (Keep this function for compatibility with .groggsrc.el files from
;; Greed 1.2 and previous).
(defun greed-read-item (itemid read-to killed)
  (let ((v (greed-index-entry itemid)))
    (aset v greed-data-read-to read-to)
    (aset v greed-data-killed killed)))

;; Build a command to restore item; return the command as a string.
;; All the fields can be written directly as strings, except the
;; continuation pointers, which point to other item vectors, so we
;; replace those with their itemids.
(defun greed-make-restore-command (item)
  (let* ((v (copy-sequence item))
	 (contfrom (aref v greed-data-contfrom))
	 (contin (aref v greed-data-contin)))
    (if contfrom
	(aset v greed-data-contfrom (aref contfrom greed-data-itemid)))
    (if contin
	(aset v greed-data-contin (aref contin greed-data-itemid)))
    (let ((l (mapcar (function (lambda (x) x)) v)))
      (format "%S\n" (cons 'greed-restore-item l)))))

;; Restore complete item information.  Everything restores
;; automatically except for the continuation pointers, which must be
;; restored from their itemids.
(defun greed-restore-item (itemid &rest item-data)
  (let ((v (greed-index-entry itemid))
	(i 1))
    (while item-data
      (aset v i (car item-data))
      (setq i (1+ i))
      (setq item-data (cdr item-data)))
    (let ((contfrom (aref v greed-data-contfrom))
	  (contin (aref v greed-data-contin)))
      (if contfrom
	  (aset v greed-data-contfrom (greed-index-entry contfrom)))
      (if contin
	  (aset v greed-data-contin (greed-index-entry contin))))))

;; Delete buffers, clear variables, etc. on shutdown
(defun greed-clear-system ()
  (remove-hook 'kill-emacs-query-functions 'greed-kill-hook)
  (greed-timer-stop)
  (if (get-buffer greed-index-buffer)
      (progn
	(set-buffer greed-index-buffer)
	(remove-hook 'kill-buffer-query-functions 'greed-index-quit)))
  (if (not greed-debug)
      (progn
	(mapcar (function (lambda (b) (if (get-buffer b) (kill-buffer b))))
		(append greed-buffer-list greed-item-buffers))
	(setq greed-index-list nil
	      greed-index-last-seq nil
	      greed-password nil
	      greed-userid nil
	      greed-access-level nil
	      greed-lock nil
	      greed-edit-enabled nil
	      greed-displayed-index-list nil
	      greed-timer-p nil))))

;; Function runs when Emacs is killed; prompts to quit GREED.
(defun greed-kill-hook ()
  (greed-quit (not (and (get-buffer greed-index-buffer)
			(y-or-n-p "Save GREED startup files? "))) t)
  t)


;;; Index mode ----------------------------------------------------------------

(if greed-options-map
    nil
  (setq greed-options-map (make-sparse-keymap))
  (define-key greed-options-map "?" 'greed-options-help)
  (define-key greed-options-map "p" 'greed-options-grogname-prompt)
  (define-key greed-options-map "r" 'greed-options-grogname-random)
  (define-key greed-options-map "d" 'greed-options-grogname-default)
  (define-key greed-options-map "g" 'greed-options-change-grogname)
  (define-key greed-options-map "h" 'greed-options-change-height)
  (define-key greed-options-map "a" 'greed-options-auto-toggle)
  (define-key greed-options-map "i" 'greed-options-itemids-toggle)
  (define-key greed-options-map "o" 'greed-options-order-toggle)
  (define-key greed-options-map "s" 'greed-options-sort-toggle)
  (define-key greed-options-map "t" 'greed-options-threads-toggle)
  (define-key greed-options-map "n" 'greed-options-novice-toggle)
  (define-key greed-options-map "f" 'greed-options-save-directory)
  (define-key greed-options-map "I" 'greed-options-info)
  (define-key greed-options-map "\C-c\C-i" 'greed-options-info))

(greed-make-help-screen greed-options-help
  "Prompt/Rnd/Default Grogname Files Height Auto Id Novice Order Sort Threads ?"
  "You have typed `o', the GREED options key. Type an option:

I  read Info documentation about these options.

p  always Prompt for a grogname before sending a reply or new item.
d  never prompt, but use the Default grogname `greed-grogname'.
r  never prompt, but pick a Random grogname from `greed-grogname-list'.
   (Any grognames you type in are added to this list.)

f  change the directory in which items will be saved to Files.
g  change your default Grogname.
h  change the Height of the index window.

a  toggle whether the index is Automatically updated regularly.
i  toggle whether or not Itemids are displayed in the Index.
n  toggle whether user is a Novice or experienced with GREED.
   (If the former, then more prompts will be given).
o  toggle the Order of the Index (earliest at top/last at top).
s  toggle the field on which the Index is Sorted (time of first
   contribution/time of last reply).
t  toggle whether item Threads are displayed."
  greed-options-map)

;; Options menu
(setq greed-index-options-menu (greed-make-menu "G)Options"
  '((info     "GREED options manual       (o I)" greed-options-info)
    (sep2     "--")
    (novice   "Toggle novice status       (o n)" greed-options-novice-toggle)
    (threads  "Toggle threads display     (o t)" greed-options-threads-toggle)
    (sort     "Sort by item/reply         (o s)" greed-options-sort-toggle)
    (order    "Reverse order              (o o)" greed-options-order-toggle)
    (itemids  "Show/hide itemids          (o i)" greed-options-itemids-toggle)
    (auto     "Toggle automatic update    (o a)" greed-options-auto-toggle)
    (height   "Change index height        (o h)" greed-options-change-height)
    (direct   "Change save directory      (o f)" greed-options-save-directory)
    (sep1     "--")
    (grogname "Change grogname            (o g)" greed-options-change-grogname)
    (random   "Choose Grogname randomly   (o r)" greed-options-grogname-random)
    (prompt   "Always prompt for Grogname (o p)" greed-options-grogname-prompt)
    (default  "Use the default Grogname   (o d)" greed-options-grogname-default))))

;; Actions menu
(setq greed-index-actions-menu (greed-make-menu "GREED"
  '((info    "GREED manual" greed-index-info)
    (collate "Collate Groggs"      greed-index-collate)
    (sep4    "--")
    (nosave  "Quit without saving" greed-index-quit-nosave)
    (quit    "Quit GREED"          greed-index-quit)
    (suspend "Suspend GREED"       greed-index-suspend)
    (sfsave  "Save startup files"  greed-index-save)
    (sep3    "--")
    (hide    "Hide killed items"   greed-index-list-items-brief
	     greed-index-display-killed-items)
    (list    "List killed items"   greed-index-list-all-items
	     (not greed-index-display-killed-items))
    (unkill  "Unkill item"         greed-index-unkill-item
	     (and (greed-valid-current-item-number)
		  (aref (greed-index-current-data) greed-data-killed)))
    (kill    "Kill item"           greed-index-kill-item
	     (and (greed-valid-current-item-number)
		  (not (aref (greed-index-current-data) greed-data-killed))))
    (sep2    "--")
    (save    "Save item"           greed-index-save-item
	     (greed-valid-current-item-number))
    (reply   "Reply to item"       greed-index-reply
	     (and (greed-valid-current-item-number) (> greed-access-level 1)))
    (newi    "Start a new item"    greed-index-newitem
	     (and (greed-valid-current-item-number) (> greed-access-level 1)))
    (sep1    "--" nil)
    (motd    "Message of the day"  greed-index-read-motd)
    (back    "Chain backward"      greed-index-chain-backward
             (and (greed-valid-current-item-number)
		  (aref (greed-index-current-data) greed-data-contfrom)))
    (forw    "Chain forwards"      greed-index-chain-forward
	     (and (greed-valid-current-item-number)
		  (aref (greed-index-current-data) greed-data-contin)))
    (read    "Read item"           greed-index-scroll-item-forward
	     (greed-valid-current-item-number)))))

(if greed-index-mode-map
    nil
  (setq greed-index-mode-map (make-keymap))
  (suppress-keymap greed-index-mode-map)
  (define-key greed-index-mode-map "o" 'greed-options-help)
  (define-key greed-index-mode-map " " 'greed-index-scroll-item-forward)
  (define-key greed-index-mode-map "\177" 'greed-index-scroll-item-backward)
  (define-key greed-index-mode-map [backspace] 'greed-index-scroll-item-backward)
  (define-key greed-index-mode-map "\r" 'greed-index-scroll-item-one-line)
  (define-key greed-index-mode-map "n" 'greed-index-next-item)
  (define-key greed-index-mode-map "p" 'greed-index-prev-item)
  (define-key greed-index-mode-map "\C-n" 'greed-index-next-item)
  (define-key greed-index-mode-map "\C-p" 'greed-index-prev-item)
  (define-key greed-index-mode-map [down] 'greed-index-next-item)
  (define-key greed-index-mode-map [up] 'greed-index-prev-item)
  (define-key greed-index-mode-map "<" 'greed-index-beginning-of-item)
  (define-key greed-index-mode-map ">" 'greed-index-end-of-item)
  (define-key greed-index-mode-map "," 'greed-index-beginning-of-item)
  (define-key greed-index-mode-map "." 'greed-index-end-of-item)
  (define-key greed-index-mode-map "^" 'greed-index-chain-backward)
  (define-key greed-index-mode-map "a" 'greed-index-reply)
  (define-key greed-index-mode-map "b" 'greed-index-chain-backward)
  (define-key greed-index-mode-map "c" 'greed-index-catch-up)
  (define-key greed-index-mode-map "f" 'greed-index-chain-forward)
  (define-key greed-index-mode-map "g" 'greed-index-get-new-items)
  (define-key greed-index-mode-map "i" 'greed-index-newitem)
  (define-key greed-index-mode-map "I" 'greed-index-info)
  (define-key greed-index-mode-map "\C-c\C-i" 'greed-index-info)
  (define-key greed-index-mode-map "k" 'greed-index-kill-item)
  (define-key greed-index-mode-map "l" 'greed-index-list-items-brief)
  (define-key greed-index-mode-map "L" 'greed-index-list-all-items)
  (define-key greed-index-mode-map "m" 'greed-index-read-motd)
  (define-key greed-index-mode-map "q" 'greed-index-quit)
  (define-key greed-index-mode-map "Q" 'greed-index-quit-nosave)
  (define-key greed-index-mode-map "r" 'greed-index-reply)
  (define-key greed-index-mode-map "s" 'greed-index-save-item)
  (define-key greed-index-mode-map "S" 'greed-index-save)
  (define-key greed-index-mode-map "u" 'greed-index-unkill-item)
  (define-key greed-index-mode-map "v" 'greed-index-version)
  (define-key greed-index-mode-map "z" 'greed-index-suspend)
  (define-key greed-index-mode-map [button2] 'greed-index-mouse-item)
  (define-key greed-index-mode-map [mouse-2] 'greed-index-mouse-item)
  (define-key greed-index-mode-map "?" 'describe-mode))

(defun greed-index-mode ()
  "GREED Index Mode: a major mode for reading GROGGS.

SPC     If current item is not displayed, display it.
        If item is displayed, scroll to the next page.
DEL     Scroll to the previous page of the current item.
RET     Scroll down one line of the current item.
n       Move to the next line in the Index.
p       Move to the previous line.
C-n     Move to the next line.
C-p     Move to the previous line.
< or ,  Move point to the beginning of the current item
> or .  Move point to the start of the first unread reply, if any,
        or to the end of the current item, if none.
^ or b  Backward chain \(go to the parent item of this item\).
c       Catch up \(mark all items as read\).
e       Edit menu: type `e ?' for description of options.
f       Forward chain \(go to the continuation of this item\).
g       Get new items and replies.
i       Start a new item.
k       Kill this item.
l       List items.
L       List all items, including killed items.
m       Read the server's message of the day.
o       Options menu: type `o ?' for description of options.
q       Quit reading GROGGS.
Q       Quit reading GROGGS without saving the startup files.
r       Reply to the current item.
s       Save the current item to a file.
S       Save startup files but don't quit reading GROGGS.
u       Un-kill this item.
v       Show the version number of GREED.
z       Suspend GREED (without quitting)
?       Describe Index mode.
I       Read Info documentation.

Many features of GREED can be configured using the options menu
\(type `o ?' for help\).  In addition, the following user-customisable
variables control the less commonly-needed features \(see the manual for
further details\):

 greed-servers
    A list of servers offering GROGGS bulletin boards. This variable
    should be a list with one element for each server, in the form \(NAME
    HOST PORT STARTUP-FILE PASSWORD-FILE\), where NAME is a string
    describing the server, HOST is the Internet host on which it runs,
    PORT is its service number, STARTUP-FILE is the file in which
    options and state should be saved, and PASSWORD-FILE is the file in
    which the user's password is kept. \(It needs to be different from
    `greed-startup-file' because it should be unreadable to everyone
    else.\)

    If `greed' is called with a prefix argument, then the user is
    offered a choice from this list; if called without, then the first
    server on the list is chosen.

 greed-debug
    If non-NIL, then GREED saves a log of its RGTP session in the buffer
    `*greed-log*'; it also fails to delete its buffers when you quit
    \(these features are for debugging purposes\).  By default it is NIL.

 greed-no-save-grognames
    If non-NIL, grognames are not kept in `greed-grogname-list'.
    By default it is NIL.

 greed-paranoid
    If non-nil, then GREED attempts to authenticate the server, and
    you are asked for confirmation before your password is saved.
    By default it is NIL.

 greed-reply-check-blank
    If this is non-NIL, then GREED requires confirmation before it will
    allow the posting of a blank reply.  By default it is T.

 greed-save-old-versions
    If non-NIL, then copies of the old versions of the startup file and
    password file are made. By default it is T.

 greed-timer-secs
    Number of seconds between automatic index refreshes \(if you have
    this option turned on with `o a'\). By default it is 300."
  (interactive)
  (kill-all-local-variables)
  (use-local-map greed-index-mode-map)
  (buffer-disable-undo (current-buffer))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'greed-index-quit)
  (setq major-mode 'greed-index-mode
	mode-name "Index"
	buffer-read-only t
	truncate-lines t)
  (greed-set-buffer-identification "GREED: list of items")
  (greed-set-process nil)
  (greed-set-modified "--- ")
  (greed-install-menus greed-index-actions-menu greed-index-options-menu
		       greed-index-edit-menu)
  (run-hooks 'greed-index-mode-hook))

;; Select item N in the index buffer, where N starts at 1.
(defun greed-index-point-at-item (n)
  (if (greed-valid-item-number n)
      (let ((w (get-buffer-window greed-index-buffer))
	    (p (progn
		 (set-buffer greed-index-buffer)
		 (goto-line n)
		 (point))))
	(if w
	    (set-window-point w p)))))

;; Return number of current item in index buffer.
(defun greed-index-current-item ()
  (save-excursion
    (set-buffer greed-index-buffer)
    (+ (count-lines (point-min) (point))
       (if (= (current-column) 0) 1 0))))

;; List the current items in the Index buffer.
;; List all items if `greed-index-display-killed-items' is non-NIL.
(defun greed-index-list-items ()
  (let ((old-item (greed-index-current-data)))
    (setq greed-displayed-index-list (sort (greed-index-copy-items)
					   'greed-index-compare)
	  greed-index-number-items (length greed-displayed-index-list))
    (set-buffer greed-index-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (mapcar 'greed-index-format-item greed-displayed-index-list))
    (run-hooks 'greed-index-redraw-hook)
    ;; Try to preserve the item at which the cursor was pointing
    (let ((curr-item 1))
      (if old-item
	  (let ((new-item (memq old-item greed-displayed-index-list)))
	    (if new-item
		(setq curr-item (1+ (- (length greed-displayed-index-list)
				       (length new-item)))))))
      (setq curr-item (min (max curr-item 1)
			   (length greed-displayed-index-list)))
      (greed-index-point-at-item curr-item))))

;; Returns a copy of `greed-index-list' containing items fit to display.
;; Leaves out items that don't exist, or which are killed.
(defun greed-index-copy-items ()
  (let ((copy nil))
    (mapcar (function
	     (lambda (i)
	       (if (and (aref i greed-data-exists)
			(or greed-index-display-killed-items
			    (not (aref i greed-data-killed))))
		   (setq copy (cons i copy)))))
	    greed-index-list)
    copy))

;; Insert a line describing ITEM into the current buffer.
(defun greed-index-format-item (item)
  (let* ((itemid (aref item greed-data-itemid))
	 (nreplies (aref item greed-data-nreplies))
	 (id (substring (concat (aref item (if greed-index-show-first
					       greed-data-first-id
					     greed-data-last-id))
				(make-string greed-index-userid-length ? ))
			0 greed-index-userid-length))
	 (date (greed-format-date (aref item (if greed-index-show-first
						 greed-data-first-date
					       greed-data-last-date))))
	 (subject (aref item greed-data-subject))
	 (killed (aref item greed-data-killed))
	 (read-to (aref item greed-data-read-to))
	 (last-seq (aref item greed-data-last-seq))
	 (continued (aref item greed-data-contin))
	 (unread (or (null read-to) (greed-seq< read-to last-seq)))
         (linestart (point)))
    (insert
     (if unread "*" " ")
     (if greed-index-display-killed-items
	 (if killed "K" " ")
       "")
     (if greed-index-show-itemids " " "")
     (if greed-index-show-itemids itemid "")
     (format " %3d " nreplies)
     id " " date " "
     (if (and greed-index-show-threads continued)
	 greed-continued-item-indent
       "")
     subject)
    (if unread (put-text-property linestart (point) 'face greed-unread-face))
    (insert "\n")))


;;; Options -------------------------------------------------------------------

(defun greed-options-threads-toggle ()
  "Toggle display of item threads."
  (interactive)
  (message (if (setq greed-index-show-threads (not greed-index-show-threads))
	       "Threads displayed."
	     "Threads hidden."))
  (greed-index-list-items))

(defun greed-options-sort-toggle ()
  "Toggle sorting in order of first/last contribution to items."
  (interactive)
  (message (if (setq greed-index-sort-by-last (not greed-index-sort-by-last))
	       "Items ordered by time of last reply."
	     "Items ordered by time of contribution."))
  (setq greed-index-show-first (not greed-index-sort-by-last))
  (greed-index-list-items))

(defun greed-options-order-toggle ()
  "Exchange order of items: earliest at top or at bottom?"
  (interactive)
  (message (if (setq greed-index-reverse-order (not greed-index-reverse-order))
	       "Items listed in reverse order (last at top)."
	     "Items listed in usual order (first at top)."))
  (greed-index-list-items))

(defun greed-options-grogname-prompt ()
  "Always prompt for a grogname before sending a reply or newitem."
  (interactive)
  (setq greed-ask-for-grogname t)
  (message "Will always prompt for grogname before sending reply."))

(defun greed-options-grogname-random ()
  "Choose a random grogname from `greed-grogname-list' for each reply."
  (interactive)
  (setq greed-ask-for-grogname nil
	greed-choose-random-grogname t)
  (message "Will choose a random grogname for each reply."))

(defun greed-options-grogname-default ()
  "Never ask for grogname, but use default in `greed-grogname'."
  (interactive)
  (setq greed-ask-for-grogname nil
	greed-choose-random-grogname nil)
  (message "Will use the default grogname for each reply."))

(defun greed-options-novice-toggle ()
  "Toggle between novice and experienced status."
  (interactive)
  (message (if (setq greed-novice-user (not greed-novice-user))
	       "Novice user."
	     "Experienced user.")))

(defun greed-options-auto-toggle ()
  "Toggle automatic refreshing of the index."
  (interactive)
  (if (setq greed-timer-p (not greed-timer-p))
      (progn
	(greed-timer-start)
	(message "Automatic refreshing of index turned on."))
    (greed-timer-stop)
    (message "No automatic refreshing of index.")))

;; Automatic refreshing of the index.
(defun greed-timer-function ()
  (if (and greed-timer-p
	   (get-buffer greed-index-buffer))
      (greed-get-index)
    (greed-timer-stop)))

;; Turn timer on.
(defun greed-timer-start ()
  (if (not greed-timer)
      (setq greed-timer
	    (if greed-xemacs-p
		(start-itimer "Greed" 'greed-timer-function greed-timer-secs greed-timer-secs)
	      (run-at-time greed-timer-secs greed-timer-secs
			   'greed-timer-function)))))

;; Turn timer off.
(defun greed-timer-stop ()
  (if greed-timer
      (progn
	(if greed-xemacs-p
	    (delete-itimer greed-timer)
	  (cancel-timer greed-timer))
	(setq greed-timer nil))))

(defun greed-options-itemids-toggle ()
  "Toggle display of itemids."
  (interactive)
  (message (if (setq greed-index-show-itemids (not greed-index-show-itemids))
	       "Showing itemids in the Index."
	     "Omitting itemids from the Index."))
  (mapcar
   (function
    (lambda (buffer)
      (if (get-buffer buffer)
	  (progn
	    (set-buffer buffer)
	    (greed-set-buffer-identification
		  (format "GREED: %s"
			  (greed-item-identification greed-item-data)))))))
   greed-item-buffers)
  (greed-index-list-items))

(defun greed-options-change-grogname ()
  "Change default grogname."
  (interactive)
  (let ((greed-completion-map (copy-keymap minibuffer-local-completion-map)))
    (define-key greed-completion-map " " 'self-insert-command)
    (let* ((minibuffer-local-completion-map greed-completion-map)
	   (new-grogname
	    (completing-read "Grogname: " greed-grogname-list nil
			     nil greed-grogname nil)))
      (if new-grogname
	  (progn
	    (setq greed-grogname new-grogname)
	    (if (and (not greed-no-save-grognames)
		     (null (assoc greed-grogname greed-grogname-list)))
		(setq greed-grogname-list
		      (cons (list greed-grogname)
			    greed-grogname-list))))))))

(defun greed-options-save-directory (new-save-dir)
  "Change the directory to save items to."
  (interactive "DDirectory in which to save items: ")
  (if (file-directory-p new-save-dir)
      (progn
	(setq greed-save-directory new-save-dir)
	(message "Save directory now %s." greed-save-directory))
    (error "Not a directory: %s." new-save-dir)))

(defun greed-options-change-height (new-height)
  "Change the height of the index window."
  (interactive "nNew height of index window: ")
  (if (and (integerp new-height)
	   (> new-height 0))
      (progn
	(setq greed-index-height new-height)
	(message "Default Index window height now %d." greed-index-height)
	(switch-to-buffer greed-index-buffer)
	(delete-other-windows)
	(greed-configure-windows))
    (error "Not appropriate height: %s." new-height)))

(defun greed-options-info ()
  "Read Info documentation about options."
  (interactive)
  (greed-info-node "Reading options"))


;;; Index Mode commands -------------------------------------------------------

(defun greed-index-collate (omit-killed)
  "Collate all the Groggs items available at the server and display the result.
With a prefix argument, omit any killed items."
  (interactive "P")
  (let ((item-list (if omit-killed
		       greed-displayed-index-list
		     greed-index-list)))
    (save-excursion
      (set-buffer (get-buffer-create greed-output-buffer))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapcar
       (function
	(lambda (item)
	  (unwind-protect
	      (if (eq 2 (greed-index-select-item item))
		  nil
		(insert-buffer-substring (greed-item-buffer-name item))
		(insert ?\n)))))
       item-list)
      (greed-item-mode nil "all Groggs" "Collate"))
    (greed-configure-windows greed-output-buffer)
    (select-window (get-buffer-window greed-output-buffer))
    (message (substitute-command-keys
	      "Type \\[greed-item-show-index] to return to the index."))))

(defun greed-index-stats ()
  "Compile posting statistics for the current server.
It may be illegal to use this function in the UK on servers registered
under the terms of the Data Protection Act.  Consult a lawyer for advice."
  (interactive)
  (let ((b (get-buffer-create greed-output-buffer)))
    (set-buffer b)
    (setq buffer-read-only nil)
    (message "Fetching index...")
    (sit-for 0)
    (greed-send-command "INDX")
    (greed-wait-for-response 250)
    (greed-wait-for-data b t)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((p (point)))
	(if (not (memq (char-after (+ p 103)) '(?R ?F ?C ?I)))
	    (delete-region p (+ p 200))
	  (delete-region (+ p 102) (+ p 199))
	  (delete-region p (+ p 27))
	  (forward-line 1))))
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (sort-lines nil (point-min) (point-max))
    (push-mark)				; workaround for bug in 19.29
    (shell-command-on-region (point-min) (point-max) "uniq -c" nil t)
    (sort-lines t (point-min) (point-max))
    (goto-char (point-min))
    (greed-item-mode nil "posting statistics" "Stats")
    (greed-configure-windows b)
    (sit-for 0)
    (save-window-excursion
      (select-window (get-buffer-window b))
      (if (> (point-max) (window-end))
	  (message (substitute-command-keys "Type \\[scroll-other-window] to scroll the statistics."))))))

(defun greed-index-next-item ()
  "Move to the next item in index.
Returns T if successful, NIL otherwise."
  (interactive)
  (if (>= (greed-index-current-item) greed-index-number-items)
      (progn
	(message "No more items.")
	nil)
    (greed-index-point-at-item (1+ (greed-index-current-item)))
    t))

(defun greed-index-prev-item ()
  "Move to the previous item in index.
Returns T if successful, NIL otherwise."
  (interactive)
  (if (<= (greed-index-current-item) 1)
      (progn
	(message "No previous items.")
	nil)
    (greed-index-point-at-item (1- (greed-index-current-item)))
    t))

(defun greed-index-quit ()
  "Quit reading GROGGS.
Returns T if GREED quit, NIL otherwise."
  (interactive)
  (if (or (not greed-novice-user)
	  greed-quit-without-prompting
	  (y-or-n-p "Do you really want to quit GREED? "))
      (progn
	(greed-quit)
	t)
    nil))

(defun greed-index-quit-nosave ()
  "Quit reading GROGGS."
  (interactive)
  (if (or (not greed-novice-user)
	  (y-or-n-p "Really quit GREED without saving options? "))
      (greed-quit t)))

(defun greed-index-save ()
  "Save startup files, including which items have been read and killed."
  (interactive)
  (with-temp-message "Saving GREED startup files..."
    (greed-save-startup-files)))

(defun greed-index-list-items-brief ()
  "List only non-killed items."
  (interactive)
  (setq greed-index-display-killed-items nil)
  (greed-index-list-items))

(defun greed-index-list-all-items ()
  "List all items, including killed items."
  (interactive)
  (setq greed-index-display-killed-items t)
  (greed-index-list-items))

(defun greed-index-get-new-items ()
  "Get new items and replies."
  (interactive)
  (greed-get-index)
  (greed-index-list-items))

(defun greed-index-version ()
  "Show the version number of GREED."
  (interactive)
  (message "GREED version %s by Gareth Rees." greed-version))

;; Select item with data ITEM into item buffer.  Returns:
;; 0 if item was already displayed
;; 1 if item was fetched, but not visible
;; 2 if the item did not exist
;; 3 if the item was displayed.
(defun greed-index-select-item (item)
  (save-excursion
    (if (null item)
	;; Can't display non-existent item.
	2
      (let* ((bn (greed-item-buffer-name item))
	     (b (get-buffer bn))
	     (rt (aref item greed-data-read-to))
	     (out-of-date
	      (or (null rt) (greed-seq< rt (aref item greed-data-last-seq)))))
	(if (and b (not out-of-date))
	    (if (get-buffer-window bn)
		;; Item is already displayed
		0
	      ;; Item has been fetched, but isn't visible
	      1)
	  (if (not (member bn greed-item-buffers))
	      (setq greed-item-buffers (cons bn greed-item-buffers)))
	  (set-buffer (get-buffer-create bn))
	  (greed-item-mode item)
	  (message "Requesting item %s..." (aref item greed-data-itemid))
	  (greed-send-command "ITEM" (aref item greed-data-itemid))
	  (if (eq (greed-wait-for-response 250 410) 410)
	      (error "Couldn't fetch item: %s" greed-response-text))
	  (let ((buffer-read-only nil))
	    (greed-wait-for-data (current-buffer) t)
	    (greed-item-prepare))

	  ;; Success
	  (message nil)

	  ;; Update `read-to' data, and redisplay this line in the index
	  (aset item greed-data-read-to (aref item greed-data-last-seq))
	  (let ((n (memq item greed-displayed-index-list)))
	    (if n
		(let ((line (1+ (- (length greed-displayed-index-list)
				   (length n)))))
		  (set-buffer greed-index-buffer)
		  (goto-line line)
		  (let ((p (point))
			(buffer-read-only nil))
		    (forward-line 1)
		    (delete-region p (point))
		    (greed-index-format-item item)
		    (run-hooks 'greed-index-redraw-hook))
		  (goto-line line))))
	  3)))))

;; Scroll item forward by LINES (backward if BACKP non-NIL).
;; If LINES is NIL then the height of the item window will be used.
;; Returns NIL if successful, T if at end of item and scrolling down.
(defun greed-index-scroll-item (lines &optional backp)
  (if (greed-valid-current-item-number)
      (let ((i (prog1
		   (greed-index-select-item (greed-index-current-data))
		 (greed-configure-windows))))
	(cond ((= i 1)
	       (greed-index-beginning-of-item)
	       nil)
	      ((= i 0)
	       (select-window
		(get-buffer-window (greed-current-item-buffer-name)))
	       (prog1
		   (greed-item-scroll lines backp)
		 (select-window (get-buffer-window greed-index-buffer))))
	      (t nil)))))

(defun greed-index-scroll-item-forward (&optional lines)
  "Scroll a page forward. With prefix arg, scrolls that number of lines."
  (interactive "P")
  (if (and (greed-index-scroll-item lines)
	   (greed-index-next-item))
      (greed-index-scroll-item 0)))

(defun greed-index-scroll-item-backward (&optional lines)
  "Scroll a page backward. With prefix arg, scrolls that number of lines."
  (interactive "P")
  (greed-index-scroll-item lines t))

(defun greed-index-scroll-item-one-line (&optional lines)
  "Scroll item by one line. With prefix arg, scrolls that number of lines."
  (interactive "p")
  (greed-index-scroll-item lines))

(defun greed-index-mouse-item (event)
  "Jump to an item, scroll it if already there"
  (interactive "e")
  (cond ((greed-button-test event)
	 (mouse-set-point event)
	 (let ((s (greed-index-select-item (greed-index-current-data))))
	   (cond ((eq s 0)		;item is already displayed
		  (greed-index-scroll-item nil)
		  )
		 ((eq s 1)		;fetched but invisible (still)
		  (greed-configure-windows)
		  (greed-index-beginning-of-item)
		  )
		 ((eq s 3)		;we displayed it
		  (greed-index-beginning-of-item)
		  ))))))

(defun greed-index-describe-briefly ()
  "Describe Index mode commands briefly."
  (interactive)
  (message (substitute-command-keys
	    "\\[greed-index-scroll-item-forward]:Select  \\[greed-index-next-item]:Forward  \\[greed-index-prev-item]:Backward  \\[greed-index-get-new-items]:Get new items  \\[greed-index-quit]:Quit  \\[describe-mode]:Help  \\[greed-index-info]:info")))

(defun greed-index-beginning-of-item ()
  "Go to beginning of item body."
  (interactive)
  (if (greed-valid-current-item-number)
      (progn
	(greed-index-select-item (greed-index-current-data))
	(greed-configure-windows)
	(select-window (get-buffer-window (greed-current-item-buffer-name)))
	(goto-char (point-min))
	(select-window (get-buffer-window greed-index-buffer)))))

(defun greed-index-end-of-item ()
  "Go to end of item body, or first unseen reply (if any)."
  (interactive)
  (if (greed-valid-current-item-number)
      (progn
	(greed-index-select-item (greed-index-current-data))
	(greed-configure-windows)
	(select-window (get-buffer-window (greed-current-item-buffer-name)))
	(if greed-first-unread
	    (progn
	      (goto-char greed-first-unread)
	      (setq greed-first-unread nil))
	  (goto-char (point-max))
	  (message (substitute-command-keys "End of item. Press \\[greed-index-scroll-item-forward] for next item.")))
	(select-window (get-buffer-window greed-index-buffer)))))

(defun greed-index-kill-item ()
  "Kill current item."
  (interactive)
  (if (greed-valid-current-item-number)
      (let ((v (greed-index-current-data))
	    (curr-item (greed-index-current-item)))
	(if (aref v greed-data-killed)
	    (error "Item already killed.")
	  (aset v greed-data-killed t)
	  (goto-line (greed-index-current-item))
	  (let ((p (point))
		(buffer-read-only nil))
	    (forward-line 1)
	    (delete-region p (point))
	    (if greed-index-display-killed-items
		(greed-index-format-item v)
	      (setq greed-displayed-index-list
		    (delq v greed-displayed-index-list)
		    greed-index-number-items (1- greed-index-number-items)
		    curr-item (max 1 (min curr-item
					  greed-index-number-items))))
	    (run-hooks 'greed-index-redraw-hook)))
	(goto-line curr-item))))

(defun greed-index-unkill-item ()
  "Unkill current item."
  (interactive)
  (if (greed-valid-current-item-number)
      (let ((v (greed-index-current-data)))
	(if (not (aref v greed-data-killed))
	    (error "Item not killed.")
	  (aset v greed-data-killed nil)
	  (goto-line (greed-index-current-item))
	  (let ((curr-item (greed-index-current-item))
		(p (point))
		(buffer-read-only nil))
	    (forward-line 1)
	    (delete-region p (point))
	    (greed-index-format-item v)
	    (goto-line curr-item))))))

(defun greed-index-reply ()
  "Reply to current item."
  (interactive)
  (greed-check-access-level 2)
  (if (greed-valid-current-item-number)
      (greed-index-do-reply 'reply (greed-index-current-data))))

(defun greed-index-newitem ()
  "Start a new item."
  (interactive)
  (greed-check-access-level 2)
  (greed-index-do-reply 'newitem))

(defun greed-index-save-item (&optional filename)
  "Save item to file, which may be given as prefix argument."
  (interactive "P")
  (if (and (greed-valid-current-item-number)
	   (/= (greed-index-select-item (greed-index-current-data)) 2))
      (progn
	(if (null filename)
	    (let ((itemid (aref (greed-index-current-data)
				greed-data-itemid)))
	      (progn
		(setq filename
		      (expand-file-name
		       (read-file-name "Save in file: " greed-save-directory
				       itemid nil itemid))))))
	(save-excursion
	  (set-buffer (greed-current-item-buffer-name))
	  (append-to-file (point-min) (point-max) filename)))))

(defun greed-index-chain-forward ()
  "Select the continuation of the current item."
  (interactive)
  (if (greed-valid-current-item-number)
      (let* ((v (greed-index-current-data))
	     (ci (aref v greed-data-contin)))
	(if (null ci)
	    (error "Item not continued.")
	  (let ((x (memq ci greed-displayed-index-list)))
	    (if (null x)
		(error "Continuation has been killed.")
	      (greed-index-point-at-item
	       (1+ (- (length greed-displayed-index-list) (length x))))
	      (greed-index-scroll-item 0)))))))

(defun greed-index-chain-backward ()
  "Select the item of which the current is a continuation."
  (interactive)
  (if (greed-valid-current-item-number)
      (let* ((v (greed-index-current-data))
	     (cf (aref v greed-data-contfrom)))
	(if (null cf)
	    (error "Item is not a continuation.")
	  (let ((x (memq cf greed-displayed-index-list)))
	    (if (null x)
		(error "Parent item has been killed.")
	      (greed-index-point-at-item
	       (1+ (- (length greed-displayed-index-list) (length x))))
	      (greed-index-scroll-item 0)))))))

(defun greed-index-read-motd ()
  "Display the message of the day."
  (interactive)
  (greed-send-command "MOTD")
  (if (eq 410 (greed-wait-for-response 250 410))
      (error "No message of the day.")
    (set-buffer (get-buffer-create greed-output-buffer))
    (let ((buffer-read-only nil))
      (greed-wait-for-data (get-buffer greed-output-buffer) t)
      (delete-region 1 19)
      (greed-item-mode nil "message of the day" "MOTD")
      (greed-configure-windows greed-output-buffer)
      (sit-for 0)
      (save-window-excursion
	(select-window (get-buffer-window greed-output-buffer))
	(if (> (point-max) (window-end))
	    (message (substitute-command-keys "Type \\[scroll-other-window] to scroll the message of the day.")))))))

(defun greed-index-catch-up ()
  "Mark all items as read, including killed items."
  (interactive)
  (if (y-or-n-p "Mark all items as read? ")
      (progn
	(mapcar (function (lambda (i) (aset i greed-data-read-to
					    (aref i greed-data-last-seq))))
		greed-index-list)
	(greed-index-list-items))))

(defun greed-index-info ()
  "Read Info documentation."
  (interactive)
  (greed-info-node "Top"))

(defun greed-index-suspend ()
  "Bury Index buffer and restore old window config."
  (interactive)
  (set-window-configuration greed-startup-window-config))


;;; Reading the index ---------------------------------------------------------

;; Get the index from the server and set up data structures.
;; If optional argument NOREFRESH is non-NIL, superseded buffers aren't
;; killed.
(defun greed-get-index (&optional norefresh)
  (save-excursion
    (message "Fetching index...")
    (sit-for 0)
    (let ((data-buffer (get-buffer-create greed-data-buffer))
	  (refetching t) (newgroggs nil))
      (set-buffer data-buffer)
      (if greed-index-last-seq
	  (greed-send-command
	   "INDX"
	   (concat "#" (greed-seq-add-one greed-index-last-seq)))
	;; We're fetching the entire index from the server, so set the
	;; `exists' field to nil for all items.
	(setq refetching nil)
	(mapcar (function (lambda (i) (aset i greed-data-exists nil)))
		greed-index-list)
	(greed-send-command "INDX"))
      (greed-wait-for-response 250)
      (greed-wait-for-data data-buffer t)
      (message "Reading index...")
      (sit-for 0)
      (setq newgroggs (greed-read-index))
      (message (cond ((not newgroggs) "No new GROGGS.")
		     (refetching "You have new GROGGS.")
		     (t "Reading index...done")))
      (if (and newgroggs (not norefresh))
	  (mapcar
	   (function
	    (lambda (ibn)
	      (let ((ibw (get-buffer-window ibn)))
		(if ibw
		    (progn
		      (set-buffer ibn)
		      (if (or (null (aref greed-item-data greed-data-read-to))
			      (greed-seq<
			       (aref greed-item-data greed-data-read-to)
			       (aref greed-item-data greed-data-last-seq)))
			  (progn
			    (greed-index-select-item greed-item-data)
			    (if greed-first-unread
				(progn
				  (set-window-point ibw greed-first-unread)
				  (setq greed-first-unread nil))))))))))
	   greed-item-buffers)))))

;; Kill any item buffers that have new replies at the server.
(defun greed-kill-superseded-buffers ()
  (mapcar (function
	   (lambda (i)
	     (let* ((read-to (aref i greed-data-read-to))
		    (last-seq (aref i greed-data-last-seq))
		    (buf-name (greed-item-buffer-name i)))
	       (if (and read-to
			(greed-seq< read-to last-seq)
			(get-buffer buf-name))
		   (kill-buffer buf-name)))))
	  greed-index-list))

;; Parse the index entries in the current buffer into the variable
;; `greed-index-list'.  Returns T if there were new replies, NIL if
;; none.

;; This code has been completely written since 0.6, and runs more than
;; twice as quickly.  The first version went through the index forwards,
;; and so had to store information for each reply (which was typically
;; overwritten by the information from the next reply!).  This version
;; goes backwards through the index, needing only to store information
;; the first time it comes across a reply.  Some effort is made to
;; generate as little garbage as possible (e.g., by calling
;; `buffer-substring' only when we know we will store the resulting
;; string)..
(defun greed-read-index ()
  (if (eobp)
      nil

    ;; Most recently used sequence number is on last line in index.
    (goto-char (point-max))
    (forward-line -1)
    (setq greed-index-last-seq (buffer-substring (point) (+ (point) 8)))

    ;; Set the `updated' fields of all items to NIL.
    (mapcar (function (lambda (i) (aset i greed-data-updated nil)))
	    greed-index-list)

    ;; Now scan backward through the index, updating data structures.
    (goto-char (point-max))
    (let (contfrom new-groggs e p v code seq date userid)
      (while (not (bobp))
	(setq e (point))
	(forward-line -1)
	(setq p (point))
	(if (not (and (eq (- e p) 200)
		      (memq (setq code (char-after(+ p 103))) '(?R ?F ?C ?I))))
	    nil

	  ;; The index entry is relevant to us (i.e., it's not an edit),
	  ;; so do some initialisation of structures appropriate to both
	  ;; replies and new items.  We need to read in the sequence
	  ;; number, date and userid for new items, and also for replies
	  ;; (but only if it's the first time we've come across the
	  ;; reply in the index).
	  (setq v (greed-index-entry (buffer-substring(+ p 18)(+ p 26))))
	  (aset v greed-data-nreplies (1+ (aref v greed-data-nreplies)))
	  (if (or (not (aref v greed-data-updated)) (eq code ?C) (eq code ?I))
	      (progn
		(setq seq (buffer-substring p (+ p 8)))
		(setq date (buffer-substring (+ p 9) (+ p 17)))
		(setq userid (greed-trimmed-substring (+ p 27) (+ p 102)))))
	  (if (not (aref v greed-data-updated))
	      (progn
		(aset v greed-data-updated t)
		(aset v greed-data-last-seq seq)
		(aset v greed-data-last-date date)
		(aset v greed-data-last-id userid)))

	  ;; If the index entry is a new item or a continuation, we set
	  ;; up the information about its first reply; also its subject.
	  (cond
	   ((or (eq code ?C) (eq code ?I))
	    (setq new-groggs t)
	    (aset v greed-data-exists t)
	    (aset v greed-data-first-id userid)
	    (aset v greed-data-first-seq seq)
	    (aset v greed-data-first-date date)
	    (aset v greed-data-subject
		  (greed-trimmed-substring (+ p 105) (+ p 199)))
	    (if (and contfrom (eq code ?C))
		(progn
		  (aset v greed-data-contfrom contfrom)
		  (aset contfrom greed-data-contin v)
		  (setq contfrom nil))))

	   ;; If the index entry is a forwarding pointer, remember the
	   ;; item that was continued (so when we reach the continuation
	   ;; item itself we can set up pointers accordingly).
	   ((eq code ?F)
	    (setq contfrom v))

	   ;; If the index entry is a reply, set new-groggs to t if the
	   ;; updated item is visible.
	   ((eq code ?R)
	    (if (and (not new-groggs)
		     (or (not (aref v greed-data-killed))
			 greed-index-display-killed-items))
		(setq new-groggs t))))))

      ;; Return value indicates whether any visible items have received
      ;; new groggs.
      new-groggs)))

;; Return T if index entry E1 comes after index entry E2.  Order is
;; controlled by the variables `greed-index-reverse-order',
;; `greed-index-sort-by-last' and `greed-index-show-threads'.
(defun greed-index-compare (e1 e2)
  (let ((n1 0) (n2 0) ci)
    (if greed-index-show-threads
	(progn
	  (while (setq ci (aref e1 greed-data-contin))
	    (setq e1 ci n1 (1+ n1)))
	  (while (setq ci (aref e2 greed-data-contin))
	    (setq e2 ci n2 (1+ n2)))))
    (let* ((field (if greed-index-sort-by-last
		      greed-data-last-seq
		    greed-data-first-seq))
	   (order (if (eq e1 e2)
		      (< n2 n1)
		    (greed-seq< (aref e1 field) (aref e2 field)))))
      (if greed-index-reverse-order
	  (not order)
	order))))

;; Make a STAT call for item ITEMID and update data structures.  This
;; call may result in the `last-seq' field being out of synch with the
;; `last-id' field.
(defun greed-stat (itemid)
  (greed-send-command "STAT" itemid)
  (if (eq 211 (greed-wait-for-response 211 410))
      (let ((item (greed-index-entry itemid))
	    (contfrom (substring greed-response-text 0 8))
	    (contin (substring greed-response-text 9 17)))
	(if (string-match "^[A-Z][0-9]+$" contfrom)
	    (aset item greed-data-contfrom (greed-index-entry contfrom)))
	(if (string-match "^[A-Z][0-9]+$" contin)
	    (aset item greed-data-contin (greed-index-entry contin)))
	(aset item greed-data-last-seq (substring greed-response-text 27 35))
	(aset item greed-data-subject (substring greed-response-text 36)))))


;;; Item mode -----------------------------------------------------------------

(if greed-item-mode-map
    nil
  (setq greed-item-mode-map (make-sparse-keymap))
  (suppress-keymap greed-item-mode-map)
  (define-key greed-item-mode-map " " 'greed-item-scroll-forward)
  (define-key greed-item-mode-map "\177" 'greed-item-scroll-backward)
  (define-key greed-item-mode-map "\r" 'greed-item-scroll-one-line)
  (define-key greed-item-mode-map "?" 'greed-item-describe-briefly)
  (define-key greed-item-mode-map "^" 'greed-item-chain-backward)
  (define-key greed-item-mode-map "b" 'greed-item-chain-backward)
  (define-key greed-item-mode-map "f" 'greed-item-chain-forward)
  (define-key greed-item-mode-map "i" 'greed-item-show-index)
  (define-key greed-item-mode-map "r" 'greed-item-reply)
  (define-key greed-item-mode-map "<" 'beginning-of-buffer)
  (define-key greed-item-mode-map ">" 'end-of-buffer)
  (define-key greed-item-mode-map "," 'beginning-of-buffer)
  (define-key greed-item-mode-map "." 'end-of-buffer))

(defun greed-item-mode (item &optional id mn)
  "Major mode for browsing through an item.
All normal editing commands are turned off.
Instead, these commands are available:
\\{greed-item-mode-map}

Various hooks for customisation:
 greed-item-mode-hook
    Entry to this mode calls the value with no arguments, if that
    value is non-nil.

 greed-item-prepare-hook
    Called with no arguments after an item is prepared for reading,
    if that value is non-nil."
  (kill-all-local-variables)
  (make-variable-buffer-local 'greed-item-data)
  (make-variable-buffer-local 'greed-first-unread)
  (greed-set-modified "--- ")
  (greed-set-process nil)
  (setq greed-item-data nil
	major-mode 'greed-item-mode
	buffer-read-only t
	greed-first-unread 1)
  (if item
      (progn
	(setq greed-item-data item
	      mode-name "Item")
	(greed-set-buffer-identification
	      (format "GREED: %s"
		      (greed-item-identification greed-item-data))))
    (greed-set-buffer-identification (format "GREED: %s" id))
    (setq mode-name mn))
  (use-local-map greed-item-mode-map)
  (buffer-disable-undo (current-buffer))
  (run-hooks 'greed-item-mode-hook))

;; Returns appropriate identification string for item data ITEM.  The
;; subject is put into quotes if QUOTESP is non-nil.
(defun greed-item-identification (item &optional quotesp)
  (let* ((itemid (aref item greed-data-itemid))
	 (subject (aref item greed-data-subject))
	 (subj-len (- greed-item-subject-width
		      (if greed-index-show-itemids 15 0)))
	 (trunc-subj (if (> (length subject) subj-len)
			(concat (substring subject 0 (- subj-len 3)) "...")
		      subject))
	 (quote (if quotesp "\"" "")))
    (if greed-index-show-itemids
	(format "item %s (%s)" itemid trunc-subj)
      (format "%s%s%s" quote trunc-subj quote))))

;; Scroll item forward by LINES (backward if BACKP non-NIL).  If LINES
;; is NIL then the height of the item window will be used.  Returns NIL
;; if successful, T if at end of item and scrolling down.
(defun greed-item-scroll (lines &optional backp)
  (if backp
      (progn
	(move-to-window-line 0)
	(condition-case ()
	    (scroll-down lines)
	  (beginning-of-buffer (goto-char (point-min)))))
    (if (eobp)
	t
      (move-to-window-line -1)
      (condition-case ()
	  (scroll-up lines)
	(end-of-buffer (goto-char (point-max))
		       (message (substitute-command-keys "End of item. Type \\[greed-item-scroll-forward] for next item."))))
      nil)))

(defun greed-item-chain-forward ()
  "Select the continuation of the current item."
  (interactive)
  (greed-item-check)
  (let* ((v greed-item-data)
	 (ci (aref v greed-data-contin)))
    (if (null ci)
	(error "Item not continued.")
      (let ((x (memq ci greed-displayed-index-list)))
	(if (null x)
	    (error "Continuation has been killed.")
	  (greed-index-point-at-item
	   (1+ (- (length greed-displayed-index-list) (length x))))
	  (greed-index-scroll-item 0)
	  (select-window
	   (get-buffer-window (greed-current-item-buffer-name))))))))

(defun greed-item-chain-backward ()
  "Select the item of which the current is a continuation."
  (interactive)
  (greed-item-check)
  (let* ((v greed-item-data)
	 (cf (aref v greed-data-contfrom)))
    (if (null cf)
	(error "Item is not a continuation.")
      (let ((x (memq cf greed-displayed-index-list)))
	(if (null x)
	    (error "Parent item has been killed.")
	  (greed-index-point-at-item
	   (1+ (- (length greed-displayed-index-list) (length x))))
	  (greed-index-scroll-item 0)
	  (select-window
	   (get-buffer-window (greed-current-item-buffer-name))))))))

(defun greed-item-scroll-forward (&optional lines)
  "Scroll a page forward. With prefix arg, scrolls that number of lines."
  (interactive "P")
  (if (and (greed-item-scroll lines)
	   (greed-index-next-item))
      (greed-index-scroll-item 0)))

(defun greed-item-scroll-backward (&optional lines)
  "Scroll a page forward. With prefix arg, scrolls that number of lines."
  (interactive "P")
  (greed-item-scroll lines t))

(defun greed-item-scroll-one-line ()
  "Scroll item window by one line."
  (interactive)
  (greed-item-scroll 1))

(defun greed-item-show-index ()
  "Reconfigure windows to show Index buffer and current item buffer."
  (interactive)
  (greed-configure-windows)
  (select-window (get-buffer-window greed-index-buffer)))

(defun greed-item-describe-briefly ()
  "Describe Item mode commands briefly."
  (interactive)
  (message (substitute-command-keys "\\[greed-item-scroll-forward]:Next page  \\[greed-item-scroll-backward]:Previous page  \\[greed-item-chain-backward]:chain Back  \\[greed-item-chain-forward]:chain Forward  \\[greed-item-show-index]:show Index")))

;; Prepare item for viewing. Leaves mark at start of first unseen
;; reply.
(defun greed-item-prepare ()
  (let ((buffer-read-only nil)
	(mark-next nil)
        (is-first t))
    (goto-char (point-min))
    (while (re-search-forward "^\\^\\([0-9A-Fa-f]+\\) \\([0-9A-Fa-f]+\\).*$" nil t)
      (let ((cookie-start (match-beginning 0))
	    (header-start (1+ (match-end 0)))
	    (seq (buffer-substring (match-beginning 1) (match-end 1)))
	    (time (greed-decode-time (buffer-substring (match-beginning 2) (match-end 2))))
	    (now (calendar-current-date)))
	(goto-char header-start)
	(if (and (or (< (nth 5 time) (1- (nth 2 now)))
		     (and (eq (nth 5 time) (1- (nth 2 now)))
			  (<= (nth 4 time) (nth 1 now))))
		 (looking-at ".* \\(on \\)\\([A-Z][a-z][a-z] \\)[0-9]+ [A-Z][a-z][a-z]$"))
	    (let ((w (- (match-end 0) (match-beginning 0))))
	      (if (> w 75)
		  (delete-region (match-beginning (if (eq w 79) 1 2))
				 (match-end 2)))
	      (end-of-line)
	      (insert (format " %4d" (nth 5 time)))
	      (beginning-of-line)))
        (if greed-show-sequence-numbers
            (progn
              (end-of-line)
              (insert " [#" (substring seq (string-match "[^0]" seq)) "]")
              (beginning-of-line)))
	(if (re-search-forward "^$" nil t)
	    (put-text-property header-start (match-beginning 0)
			       'face (if is-first greed-item-header-face greed-reply-separator-face)))
        (setq is-first nil)
	(delete-region cookie-start header-start)
	(if mark-next
	    (setq mark-next nil
		  greed-first-unread (point)))
	(if (string= seq (aref greed-item-data greed-data-read-to))
	    (setq mark-next t
		  greed-first-unread nil))))
    (goto-char (point-min))
    (while (re-search-forward "^\\^\\^" nil t)
      (replace-match "^" t t))
    (goto-line 2)			; First line is item data
    (delete-region (point-min) (point))
    (run-hooks 'greed-item-prepare-hook)))

(defun greed-item-reply ()
  "Reply to the item in the current buffer."
  (interactive)
  (greed-item-check)
  (greed-check-access-level 2)
  (greed-index-do-reply 'reply greed-item-data))

;; Returns buffer name for item given by vector ITEM.
(defun greed-item-buffer-name (item)
  (concat "*Item-" (aref item greed-data-itemid) "*"))

;; Returns name of the buffer for the current item.
(defun greed-current-item-buffer-name ()
  (greed-item-buffer-name (greed-index-current-data)))

;; Checks that the item buffer really contains the item text.
(defun greed-item-check ()
  (if (null greed-item-data)
      (error)))


;;; Reply mode ----------------------------------------------------------------

;; Reply to ITEM, start new item, edit ITEM/index/motd, as ACTION is
;; 'reply, 'newitem, 'edit, 'edix, 'mots.
(defun greed-index-do-reply (action &optional item)
  (let ((exists-reply-buf (get-buffer greed-reply-buffer))
	(reply-buf (get-buffer-create greed-reply-buffer))
	(win-config (current-window-configuration)))
    (set-buffer reply-buf)
    (make-variable-buffer-local 'greed-reply-to-item)
    (make-variable-buffer-local 'greed-reply-sent)
    (make-variable-buffer-local 'greed-old-window-config)
    (make-variable-buffer-local 'greed-action-to-be)
    (if (and (memq action '(reply newitem edit edix mots))
	     (or (not exists-reply-buf)
		 greed-reply-sent
		 (y-or-n-p "Unsent composition in progress: really erase? ")))
	(progn
	  (kill-all-local-variables)
	  (erase-buffer)
	  (setq greed-old-window-config win-config
		greed-reply-sent nil
		greed-action-to-be action)
	  (cond ((eq action 'reply)
		 (setq greed-reply-to-item item)
		 (message "Replying to item %s."
			  (greed-item-identification greed-reply-to-item t))
		 (greed-index-select-item greed-reply-to-item)
		 (switch-to-buffer (greed-item-buffer-name item))
		 (goto-char (point-max)))
		((eq action 'edit)
		 (setq greed-reply-to-item item)
		 (message "Editing item %s."
			  (greed-item-identification greed-reply-to-item t))
		 (switch-to-buffer greed-index-buffer))
		(t (message (cond ((eq action 'edix) "Editing index.")
				  ((eq action 'newitem) "Composing new item.")
				  (t "Editing message of the day.")))
		 (switch-to-buffer greed-index-buffer)))
	  (delete-other-windows)
	  (pop-to-buffer reply-buf)
	  (greed-reply-mode)))))

(if greed-reply-mode-map
    nil
  (setq greed-reply-mode-map (copy-keymap text-mode-map))
  (define-key greed-reply-mode-map "\C-c\C-c" 'greed-reply-post)
  (define-key greed-reply-mode-map "\C-c\C-q" 'greed-reply-quit)
  (define-key greed-reply-mode-map "\C-c\C-i" 'greed-reply-info)
  (define-key greed-reply-mode-map "\C-c\C-p" 'greed-reply-prompt))

(setq greed-reply-mode-menu (greed-make-menu "GREED"
  '((info   "Read manual"                  greed-reply-info)
    (sep    "--")
    (quit   "Abandon"                      greed-reply-quit)
    (prompt "Post and prompt for Grogname" greed-reply-prompt)
    (post   "Post to Groggs"               greed-reply-post))))

(defun greed-reply-mode ()
  "Major mode for editing an item or reply to be posted to GROGGS.
Like Text Mode but with these additional commands:

C-c C-c  Post the item, reply or edit.
C-c C-p  Like C-c C-c, but prompt for grogname for this reply only.
C-c C-q  Abandon the item, reply or edit.
C-c C-i  Read Info documentation.

The hook `greed-reply-mode-hook' is available for customisation."
  (set-syntax-table text-mode-syntax-table)
  (use-local-map greed-reply-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table
	major-mode 'greed-reply-mode
	mode-name "GREED Reply")
  (greed-set-buffer-identification
	(cond ((eq greed-action-to-be 'reply)
	       (format "GREED: reply to %s"
		       (greed-item-identification greed-reply-to-item t)))
	      ((eq greed-action-to-be 'newitem)
	       "GREED: new item")
	      ((eq greed-action-to-be 'edix)
	       "GREED: editing the index")
	      ((eq greed-action-to-be 'edit)
	       (format "GREED: editing %s"
		       (greed-item-identification greed-reply-to-item t)))
	      ((eq greed-action-to-be 'mots)
	       "GREED: editing MOTD")))
  (greed-install-menus greed-reply-mode-menu)
  (if greed-novice-user
      (greed-reply-describe-briefly))
  (run-hooks 'text-mode-hook 'greed-reply-mode-hook))

(if greed-reply-full-map
    nil
  (setq greed-reply-full-map (make-sparse-keymap))
  (define-key greed-reply-full-map "c" 'greed-reply-full-continue)
  (define-key greed-reply-full-map "C" 'greed-reply-full-continue)
  (define-key greed-reply-full-map "e" 'greed-reply-full-edit)
  (define-key greed-reply-full-map "E" 'greed-reply-full-edit)
  (define-key greed-reply-full-map "r" 'greed-reply-full-retry)
  (define-key greed-reply-full-map "R" 'greed-reply-full-retry)
  (define-key greed-reply-full-map "a" 'greed-reply-quit)
  (define-key greed-reply-full-map "A" 'greed-reply-quit)
  (define-key greed-reply-full-map "?" 'greed-reply-full)
  (define-key greed-reply-full-map "h" 'greed-reply-full)
  (define-key greed-reply-full-map "H" 'greed-reply-full))

(greed-make-help-screen greed-reply-full
  "Item is full. (C)ontinue (E)dit (R)etry (A)bandon (?)help"
  "The item you attempted to post to was full.
Type one of the following:

C  Post your reply as the start of a continuation item.
   You will be prompted for the subject line of the new item.
E  Go back to editing your reply. You may then type `C-c C-c'
   to post it as the start of a continuation item.
R  Go back to editing your reply. You may then type `C-c C-c'
   to attempt to reply to the same item again. Choose this
   option rather than `E' if you intend to slim down your reply.
A  Abandon the reply altogether."
  greed-reply-full-map)

(if greed-reply-continued-map
    nil
  (setq greed-reply-continued-map (make-sparse-keymap))
  (define-key greed-reply-continued-map "r" 'greed-reply-post)
  (define-key greed-reply-continued-map "R" 'greed-reply-post)
  (define-key greed-reply-continued-map "e" 'greed-reply-edit)
  (define-key greed-reply-continued-map "E" 'greed-reply-edit)
  (define-key greed-reply-continued-map "a" 'greed-reply-quit)
  (define-key greed-reply-continued-map "A" 'greed-reply-quit)
  (define-key greed-reply-continued-map "?" 'greed-reply-continued)
  (define-key greed-reply-continued-map "h" 'greed-reply-continued)
  (define-key greed-reply-continued-map "H" 'greed-reply-continued))

(greed-make-help-screen greed-reply-continued
  "Item has been continued. (R)eply (E)dit (A)bandon (?)help"
  "The item you attempted to reply to was continued by someone else.
Type one of the following:

R  Post your reply to the continuation item.
E  Go back to editing your reply. You may then type `C-c C-c'
   to post it as a reply to the continuation item.
A  Abandon the reply altogether."
  greed-reply-continued-map)

(defun greed-reply-edit ()
  "Abort attempt to post and go back to editing the reply."
  (interactive))

(defun greed-reply-post ()
  "Post current buffer to GROGGS."
  (interactive)
  (if (and greed-lock (memq greed-action-to-be '(newitem reply)))
      (error "Edit is in progress. You must finish it first."))
  (if greed-reply-sent
      (error "Message already been sent."))

  ;; Warn user if they've been gazumped by another reply.
  (if (eq greed-action-to-be 'reply)
      (let ((old-seq (aref greed-reply-to-item greed-data-last-seq)))
	(greed-stat (aref greed-reply-to-item greed-data-itemid))
	(if (and (greed-seq< old-seq
			     (aref greed-reply-to-item greed-data-last-seq))
		 (not
		  (y-or-n-p
		   "Item has been replied to. Continue with your reply? ")))
	    (error "Not sent."))))

  ;; Check message for 80-columns
  (if (not (eq greed-action-to-be 'edix))
      (progn
	(goto-char (point-min))
	(while (not (eobp))
	  (let ((p (point)))
	    (end-of-line)
	    (if (> (- (point) p) 80)
		(error "Line longer than 80 characters will be rejected."))
	    (forward-line 1)))))

  (let ((edit-reason "") (action greed-action-to-be) (grogname ""))
    (save-excursion

      ;; Double-dot encode item in separate buffer (so composition buffer
      ;; looks ok if we abort the posting)
      (let ((reply-buffer (current-buffer)))
	(set-buffer (get-buffer-create greed-data-buffer))
	(erase-buffer)
	(insert-buffer-substring reply-buffer))
      (goto-char (point-min))
      (while (re-search-forward "^\\." nil t)
	(replace-match ".." t t))

      ;; Remove whitespace at end of message (just in case the user has
      ;; `next-line-add-newlines' set to T).
      (goto-char (point-max))
      (if (re-search-backward "[^\n\t ]" nil t)
	  (delete-region (match-end 0) (point-max)))

      ;; Check that reply isn't blank (though the user can send a blank
      ;; reply if they want, just in case the whole of the text is in the
      ;; grogname).
      (if (and greed-reply-check-blank
	       (eq (point-min) (point-max))
	       (not (y-or-n-p "Buffer has no text!  Really send it? ")))
	  (error "Not sent."))

      ;; Possibly prompt for grogname
      (if (memq action '(reply newitem cont))
	  (progn
	    (if greed-ask-for-grogname
		(greed-options-change-grogname))
	    (setq grogname
		  (if (and (not greed-ask-for-grogname)
			   greed-choose-random-grogname
			   (> (length greed-grogname-list) 0))
		      (car (elt greed-grogname-list
				(random (length greed-grogname-list))))
		    greed-grogname))))

      ;; If an edit, check that it is really meant.
      (if (and (memq action '(edix edit mots))
	       (not (yes-or-no-p "Really send edit? ")))
	  (error "Not sent."))

      ;; If an edit, prompt for reason.
      (if (memq action '(edit edix))
	  (while (string= edit-reason "")
	    (setq edit-reason (read-string "Reason for edit: "))))

      ;;Send it
      (greed-send-command "DATA")
      (greed-wait-for-response 150)
      (if (memq action '(reply newitem cont))
	  (greed-send-command grogname))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((p (point)))
	  (end-of-line)
	  (greed-send-command (buffer-substring p (point)))
	  (forward-line 1)))
      (greed-send-command ".")
      (greed-wait-for-response 350 423 425))

    (cond
     ((/= greed-response-code 350)
      (error "Unable to send data: %s" greed-response-text))

     ((eq greed-action-to-be 'newitem)
      (let ((subject ""))
	(while (string= subject "")
	  (setq subject (read-string "Enter subject: ")))
	(greed-send-command "NEWI" subject))
      (greed-wait-for-response 120 424 423 425)
      (if (eq greed-response-code 120)
	  (greed-wait-for-response 220))
      (if (/= greed-response-code 220)
	  (error "Unable to start item: %s" greed-response-text)
	(message "New item started.")
	(greed-reply-quit)))

     ((eq greed-action-to-be 'cont)
      (let ((subject ""))
	(while (string= subject "")
	  (setq subject (read-string "Enter subject: ")))
	(greed-send-command "CONT" subject))
      (greed-wait-for-response 120 122 410 423 425 520)
      (cond

       ;; The continuation was sent succesfully.
       ((eq greed-response-code 120)
	(greed-wait-for-response 220)
	(message "Continuation sent.")
	(greed-reply-quit))

       ;; The item was already continued.
       ((eq greed-response-code 122)
	(let ((new-id (substring greed-response-text 0 8)))
	  (greed-wait-for-response 422)
	  (greed-stat new-id)
	  (setq greed-reply-to-item (greed-index-entry new-id))
	  (setq greed-action-to-be 'reply)
	  (greed-set-buffer-identification
		(format "GREED: reply to %s"
			(greed-item-identification
			 greed-reply-to-item t)))
	  (greed-reply-continued)))

       ;; Various errors
       (t (error "Unable to post continuation: %s" greed-response-text))))

     ((eq greed-action-to-be 'reply)
      (greed-send-command "REPL" (aref greed-reply-to-item greed-data-itemid))
      (greed-wait-for-response 220 410 421 122 423 425)
      (cond

       ;; Reply was send successfully
       ((eq greed-response-code 220)
	(message "Reply sent.")
	(greed-reply-quit))

       ;; Item was full, but not continued
       ((eq greed-response-code 421)
	(greed-reply-full))

       ;; Item was previously continued
       ((eq greed-response-code 122)
	(let ((new-id (substring greed-response-text 0 8)))
	  (greed-wait-for-response 422)
	  (greed-stat new-id)
	  (setq greed-reply-to-item (greed-index-entry new-id))
	  (greed-set-buffer-identification
		(format "GREED: reply to %s"
			(greed-item-identification
			 greed-reply-to-item t)))
	  (greed-reply-continued)))

       ;; Various errors
       (t (error "Unable to reply: %s" greed-response-text))))

     ;; Editing an item
     ((memq greed-action-to-be '(edit edix))
      (greed-send-command "EDCF" edit-reason)
      (if (/= 220 (greed-wait-for-response 220 410 423 434 500))
	  (error "Edit refused: %s" greed-response-text)
	(message "Success: %s" greed-response-text)
	(greed-reply-quit)))

     ;; Editing the message of the day
     ((eq greed-action-to-be 'mots)
      (greed-send-command "MOTS")
      (if (/= 220 (greed-wait-for-response 220 423 531 530))
	  (error "Edit refused: %s" greed-response-text)
	(message "Success: %s" greed-response-text)
	(greed-reply-quit))))))

(defun greed-reply-full-edit ()
  "Change reply buffer data to show that reply is now a continuation."
  (interactive)
  (setq greed-action-to-be 'cont)
  (greed-set-buffer-identification
	(format "GREED: continuation of %s"
		(greed-item-identification
		 greed-reply-to-item t))))

(defun greed-reply-full-continue ()
  "Immediately post reply as a continuation."
  (interactive)
  (greed-reply-full-edit)
  (greed-reply-post))

(defun greed-reply-full-retry ()
  "Go back to editing reply in attempt to slim it to fit."
  (interactive))

(defun greed-reply-quit ()
  "Abandon the reply and restore window configuration."
  (interactive)
  (setq greed-reply-sent t)
  (if (memq greed-action-to-be '(edix edit))
      (progn
	(greed-send-command "EDAB")
	(greed-wait-for-response 200 500)
	(greed-edit-unlock)))
  (let ((old-win-config greed-old-window-config))
    (bury-buffer)
    (set-window-configuration old-win-config)))

(defun greed-reply-prompt ()
  "Send reply, prompting for a grogname for this reply only."
  (interactive)
  (let ((greed-ask-for-grogname t)
	(grogname greed-grogname))
    (unwind-protect
	(greed-reply-post)
      (setq greed-grogname grogname))))

(defun greed-reply-describe-briefly ()
  "Print short descriptive message of Reply-mode commands."
  (interactive)
  (message (substitute-command-keys (format "\\[greed-reply-post]:send %s\\[greed-reply-quit]:abandon \\[greed-reply-info]:info"
	   (if (memq greed-action-to-be '(reply newitem))
	       "\\[greed-reply-prompt]:send+prompt " "")))))

(defun greed-reply-info ()
  "Read Info documentation."
  (interactive)
  (greed-info-node (if (memq greed-action-to-be '(newitem reply))
		       "Replying"
		     "Editing")))


;;; Editing commands ----------------------------------------------------------

(if greed-noedit-map
    nil
  (setq greed-noedit-map (make-sparse-keymap))
  (define-key greed-noedit-map "?" 'greed-noedit-help)
  (define-key greed-noedit-map "d" 'greed-edit-noaccess)
  (define-key greed-noedit-map "e" 'greed-edit-noaccess)
  (define-key greed-noedit-map "i" 'greed-edit-noaccess)
  (define-key greed-noedit-map "I" 'greed-edit-noaccess)
  (define-key greed-noedit-map "m" 'greed-edit-noaccess)
  (define-key greed-noedit-map "u" 'greed-edit-noaccess)
  (define-key greed-noedit-map "w" 'greed-edit-noaccess)
  (define-key greed-noedit-map "x" 'greed-edit-noaccess)
  (define-key greed-noedit-map "l" 'greed-edit-log))

(greed-make-help-screen greed-noedit-help
  "Log ?"
  "You have typed `e', the GREED editing key.
Since you do not have editing permission, you have only one option:

l  view the edit Log"
  greed-noedit-map)

(if greed-edit-map
    nil
  (setq greed-edit-map (make-sparse-keymap))
  (define-key greed-edit-map "?" 'greed-edit-help)
  (define-key greed-edit-map "d" 'greed-edit-diffs)
  (define-key greed-edit-map "e" 'greed-edit-enable)
  (define-key greed-edit-map "i" 'greed-edit-item)
  (define-key greed-edit-map "I" 'greed-edit-info)
  (define-key greed-edit-map "l" 'greed-edit-log)
  (define-key greed-edit-map "m" 'greed-edit-motd)
  (define-key greed-edit-map "u" 'greed-edit-udbm)
  (define-key greed-edit-map "w" 'greed-edit-withdraw)
  (define-key greed-edit-map "x" 'greed-edit-index))

(greed-make-help-screen greed-edit-help
  "Diffs Enable Item Log Motd Udbm Withdraw indeX ?"
  "You have typed `e', the GREED editing key. Type an option:

l  view the edit Log
e  Enable/disable editing
   (editing must be enabled before using the editing commands)
d  view Diffs for the current item
   (`C-u e d' to view index diffs)
i  edit the current Item
m  edit the Message of the day
u  User database management
x  edit the indeX
I  read the Info documentation"
  greed-edit-map)

(setq greed-index-edit-menu (greed-make-menu "G)Edit"
  '((info "GREED edit manual       (e I)" greed-edit-info)
    (udbm "User database           (e u)" greed-edit-udbm
	  (and (> greed-access-level 2) greed-edit-enabled))
    (sep3 "--")
    (mots "Edit message of the day (e m)" greed-edit-motd
	  (and (> greed-access-level 2) greed-edit-enabled))
    (with "Withdraw item           (e w)" greed-edit-withdraw
	  (and (> greed-access-level 2) greed-edit-enabled
	       (greed-valid-current-item-number)))
    (edit "Edit item               (e i)" greed-edit-item
	  (and (> greed-access-level 2) greed-edit-enabled
	       (greed-valid-current-item-number)))
    (edix "Edit index              (e x)" greed-edit-index
	  (and (> greed-access-level 2) greed-edit-enabled))
    (sep2 "--")
    (itdf "Item diffs              (e d)" greed-edit-diffs
	  (and (> greed-access-level 2) (greed-valid-current-item-number)))
    (ixdf "Index diffs         (C-u e d)" greed-edit-index-diffs
	  (> greed-access-level 2))
    (log  "Log of edits            (e l)" greed-edit-log)
    (sep1 "--")
    (enbl "Enable/disable editing  (e e)" greed-edit-enable
	  (> greed-access-level 2)))))

(defun greed-edit-noaccess ()
  "Report that user doesn't have the required editing access."
  (interactive)
  (greed-check-access-level 3))

(defun greed-edit-enable ()
  "Toggle enabling of editing."
  (interactive)
  (if (not greed-edit-enabled)
      (greed-check-access-level 3))
  (message (if (setq greed-edit-enabled (not greed-edit-enabled))
	       "Editing enabled."
	     "Editing disabled.")))

;; Check that editing is enabled; cause an error otherwise.
(defun greed-edit-check-enabled ()
  (greed-check-access-level 3)
  (if (not greed-edit-enabled)
      (error "Editing is disabled at the moment. Type `e e' to enable it.")))

;; Lock the message area. Returns if successful; causes error if fails.
(defun greed-edit-lock ()
  (greed-send-command "EDLK")
  (if (/= 200 (greed-wait-for-response 200 411 531 530))
      (error "Permission refused: %s" greed-response-text))
  (setq greed-lock t))

;; Unlock the message area. Returns if successful; causes error if fails.
(defun greed-edit-unlock ()
  (greed-send-command "EDUL")
  (if (/= 200 (greed-wait-for-response 200 500 532 530))
      (error "Can't unlock: %s" greed-response-text))
  (setq greed-lock nil))

(defun greed-edit-item ()
  "Edit the current item."
  (interactive)
  (greed-edit-check-enabled)
  (if greed-lock
      (error "Edit is already in progress. Finish it first."))
  (if (greed-valid-current-item-number)
      (progn
	(greed-edit-lock)
	(greed-index-do-reply 'edit (greed-index-current-data))
	(greed-send-command
	 "EDIT"
	 (aref (greed-index-current-data) greed-data-itemid))
	(if (/= 250 (greed-wait-for-response 250 410 532 530))
	    (let ((response greed-response-text))
	      (greed-edit-unlock)
	      (set-buffer greed-reply-buffer)
	      (greed-reply-quit)
	      (error "Can't edit item: %s" response))
	  (greed-wait-for-data (get-buffer greed-reply-buffer) t)))))

(defun greed-edit-withdraw ()
  "Withdraw the current item."
  (interactive)
  (greed-edit-check-enabled)
  (let ((edit-reason "") (success nil))
    (if (not (greed-valid-current-item-number))
	nil
      (if (not (yes-or-no-p "Really withdraw the item? "))
	  (error "Not withdrawn."))
      (greed-edit-lock)
      (unwind-protect
	  (progn
	    (greed-send-command
	     "EDIT"
	     (aref (greed-index-current-data) greed-data-itemid))
	    (if (/= 250 (greed-wait-for-response 250 410 532 530))
		(error "Can't withdraw item: %s" greed-response-text))
	    (unwind-protect
		(progn
		  ;; Discard the item data
		  (greed-wait-for-data (get-buffer greed-data-buffer))
		  (while (string= edit-reason "")
		    (setq edit-reason (read-string "Reason for edit: ")))
		  (greed-send-command "EDCF" edit-reason)
		  (if (/= 220 (greed-wait-for-response 220 410 423 424 500))
		      (error "Withdraw failed: %s" greed-response-text))
		  (setq success t)
		  (message "Success: %s" greed-response-text))
	      (if (not success)
		  (progn
		    (greed-send-command "EDAB")
		    (greed-wait-for-response 200 500)))))
	(greed-edit-unlock)))))

(defun greed-edit-index ()
  "Edit the index."
  (interactive)
  (greed-edit-check-enabled)
  (if greed-lock
      (error "Edit is already in progress. Finish it first."))
  (greed-edit-lock)
  (greed-index-do-reply 'edix)
  (greed-send-command "EDIX")
  (set-buffer greed-reply-buffer)
  (if (/= 250 (greed-wait-for-response 250 532 530))
      (progn
	(greed-edit-unlock)
	(greed-reply-quit)
	(error "Can't edit index: %s" greed-response-text)))
  (greed-wait-for-data (get-buffer greed-reply-buffer) t)
  (auto-fill-mode -1))

(defun greed-edit-motd ()
  "Edit the message of the day."
  (interactive)
  (greed-edit-check-enabled)
  (greed-index-do-reply 'mots)
  (greed-send-command "MOTD")
  (if (eq 250 (greed-wait-for-response 250 410))
      (greed-wait-for-data (get-buffer greed-reply-buffer) t)))

(defun greed-edit-log ()
  "Display the edit log."
  (interactive)
  (greed-send-command "ELOG")
  (if (eq 531 (greed-wait-for-response 250 531 530))
      (error "Permission refused: %s" greed-response-text)
    (set-buffer (get-buffer-create greed-output-buffer))
    (let ((buffer-read-only nil))
      (greed-wait-for-data (get-buffer greed-output-buffer) t)
      (greed-item-mode nil "edit log" "ELOG")
      (greed-configure-windows greed-output-buffer)
      (sit-for 0)
      (save-window-excursion
	(select-window (get-buffer-window greed-output-buffer))
	(if (> (point-max) (window-end))
	    (message (substitute-command-keys "Type \\[scroll-other-window] to scroll the edit log.")))))))

(defun greed-edit-diffs (&optional arg)
  "Display the editing diffs for the current item.
If called with prefix argument, index diffs will be fetched."
  (interactive "P")
  (greed-check-access-level 3)
  (if (greed-valid-current-item-number)
      (progn
	(greed-send-command
	 "DIFF"
	 (if arg ""
	   (aref (greed-index-current-data) greed-data-itemid)))
	(if (/= 250 (greed-wait-for-response 250 410 531 530))
	    (error "Permission refused: %s" greed-response-text)
	  (set-buffer (get-buffer-create greed-output-buffer))
	  (let ((buffer-read-only nil))
	    (greed-wait-for-data (get-buffer greed-output-buffer) t)
	    (greed-item-mode nil
			     (if arg
				 "index diffs"
			       (format "diffs for %s"
				       (greed-item-identification
					(greed-index-current-data))))
			     "DIFFS")
	    (greed-configure-windows greed-output-buffer)
	    (sit-for 0)
	    (save-window-excursion
	      (select-window (get-buffer-window greed-output-buffer))
	      (if (> (point-max) (window-end))
		  (message (substitute-command-keys "Type \\[scroll-other-window] to scroll the edit diffs.")))))))))

(defun greed-edit-index-diffs ()
  "Display the index diffs."
  (interactive)
  (greed-edit-diffs 1))

(defun greed-edit-udbm (commands)
  "Run User Database Management Program with COMMANDS as args."
  (interactive "sArguments: ")
  (greed-edit-check-enabled)
  (greed-send-command "UDBM" commands)
  (if (/= 250 (greed-wait-for-response 250 531 530))
      (error "Permission refused.")
    (set-buffer (get-buffer-create greed-output-buffer))
    (let ((buffer-read-only nil))
      (greed-wait-for-data (get-buffer greed-output-buffer) t)
      (greed-item-mode nil "UDBM output" "UDBM")
      (greed-configure-windows greed-output-buffer)
      (sit-for 0)
      (save-window-excursion
	(select-window (get-buffer-window greed-output-buffer))
	(if (> (point-max) (window-end))
	    (message (substitute-command-keys "Type \\[scroll-other-window] to scroll the UDBM output.")))))))

(defun greed-edit-info ()
  "Read Info documentation on editing commands."
  (interactive)
  (greed-info-node "Editing"))


;;; RGTP Internals ------------------------------------------------------------

;; Close the connection to the server and report an error.
(defun greed-error (&rest strings)
  (greed-close-server)
  (apply 'error strings))

;; Close the connection to the server and use the server's error message.
(defun greed-server-error ()
  (greed-error "Server error: %s" greed-response-text))

;; Open RGTP server on `greed-rgtp-server' and log in.  Returns NIL if
;; it cannot, T if it can.
(defun greed-open-server ()
  (save-excursion
    (setq greed-server-buffer (get-buffer-create " *rgtp*"))
    (set-buffer greed-server-buffer)
    (buffer-disable-undo greed-server-buffer)
    (erase-buffer)
    (kill-all-local-variables)
    (setq greed-server-process (open-network-stream "rgtp" greed-server-buffer
						    greed-rgtp-server
						    greed-rgtp-service))
    (if (null greed-server-process)
	(progn (greed-close-server)
	       nil)
      ; Emacs 22 would like:
      ; (set-process-query-on-exit-flag greed-server-process nil)
      ; and issues a warning for process-kill-without-query.  However
      ; Emacs21 does not know set-process-query-on-exit-flag, so we persist
      ; with the old function for now.
      (process-kill-without-query greed-server-process)

      ;; We must specify that the CRLF termination of protocol lines is not
      ;; converted to LF by emacs20...
      ;; (was 'no-conversion 'no-conversion
      (if (fboundp 'set-process-coding-system)
	  (set-process-coding-system greed-server-process 'iso-8859-1-unix 'iso-8859-1-unix))
	

      ;; Wait for banner response; either "230 you must log in" or "231
      ;; Read access only".  In either case we have to log in because we
      ;; want posting access.
      (greed-wait-for-response 230 231)
      t)))

(if greed-login-map
    nil
  (setq greed-login-map (make-sparse-keymap))
  (define-key greed-login-map "?" 'greed-login-help)
  (define-key greed-login-map "h" 'greed-login-help)
  (define-key greed-login-map "H" 'greed-login-help)
  (define-key greed-login-map "r" 'greed-login-register)
  (define-key greed-login-map "R" 'greed-login-register)
  (define-key greed-login-map "l" 'greed-login-login)
  (define-key greed-login-map "L" 'greed-login-login))

(greed-make-help-screen greed-login-help
  "Need userid and password to login. (R)egister, (L)ogin, or (?)help"
  "In order to login to the Groggs server, I need a userid and a password.
Choose one of the following options:

R  To Register as a new user on the Groggs server.
   You will be asked for your e-mail address and the server will send
   you a password by e-mail. When the arrives, run GREED again, and
   select the `Login' option.
L  To Login to the Groggs server.
   You will be asked for your e-mail address and your password.
   If you do not have a password yet, choose the `Register' option.
   You only have to enter your password once; GREED will remember it for
   future sessions."
  greed-login-map)

(defun greed-login-register ()
  "Register as a new user on the Groggs server."
  (interactive)
  (greed-send-command "REGU")
  (if (eq 100 (greed-wait-for-response 100 250 482))
      (greed-wait-for-response 250 482))
  (if (eq greed-response-code 482)
      (error "Registration refused: %s" greed-response-text)
    (let ((buffer-read-only nil))
      (greed-wait-for-data (get-buffer greed-index-buffer) t)
      (switch-to-buffer greed-index-buffer)
      (goto-char (point-min))
      (keep-lines "^ ")
      (goto-char (point-min))
      (while (re-search-forward "^ " nil t)
	(replace-match "" t t)))
    (if (y-or-n-p "Continue with registration? ")
	(progn
	  (setq greed-userid "")
	  (while (string= greed-userid "")
	    (setq greed-userid (read-string "E-mail address: "
					    (and (boundp 'user-mail-address)
						 user-mail-address))))
	  (greed-send-command "USER" greed-userid)
	  (if (eq 280 (greed-wait-for-response 280 482))
	      (greed-error "Server will send you e-mail. When you receive it, run GREED again.")
	    (greed-error "Registration refused: %s" greed-response-text)))))
  (greed-error "Registration aborted."))

(defun greed-login-login ()
  "First login ever: user types in e-mail address and password, then login."
  (interactive)
  (if (null greed-userid)
      (setq greed-userid (read-string "E-mail address: "
				      (and (boundp 'user-mail-address)
					   user-mail-address))))
  (if (null greed-password)
      (setq greed-password (greed-remove-spaces (read-string "Password: "))
	    greed-password-changed t))
  (greed-login))

;; Assumes an open connection; logs in the user.
(defun greed-login ()
  (if (not (and greed-userid greed-password))
      (greed-login-help)

    ;; Send userid
    (greed-send-command "USER" greed-userid)
    (greed-wait-for-response 130 231 232 233 482)
    (if (eq greed-response-code 482)
	(greed-error "Login refused."))

    ;; Server requires authentication
    (if (eq greed-response-code 130)
	(progn
	  (if (not (equal (substring greed-response-text 0 4) "MD5 "))
	      (greed-error "Authentication method not known: %s"
			   greed-response-text))

	  ;; Followed by "333 <server-nonce>"
	  (greed-wait-for-response 333 335)
	  (if (not (eq (length greed-response-text) 32))
	      (greed-error "Expected server nonce to be 32 characters long."))

	  ;; Send authentication data
	  (greed-authenticate greed-response-text)

	  ;; Expect to be allowed on successfully
	  (greed-wait-for-response 231 232 233))

      ;; Some people don't trust servers they can't authenticate
      (if (and greed-paranoid
	       (not (y-or-n-p "No authentication took place. Proceed? ")))
	  (greed-error "Login aborted.")))

    ;; Login successful
    (setq greed-access-level (- greed-response-code 230))
    (define-key greed-index-mode-map "e"
      (if (eq greed-access-level 3) 'greed-edit-help 'greed-noedit-help))
    (define-key greed-index-mode-map [menu-bar greed-edit]
      (if (eq greed-access-level 3) (cons "Edit" greed-index-edit-menu) nil))
    (setq greed-login-done t)))

;; Sends authentication data to server based on hex string SNONCE.
(defun greed-authenticate (snonce)
  (let* ((server-nonce (greed-hex-decode snonce))
	 (client-nonce (greed-generate-nonce 16))
	 (shared-secret (greed-hex-decode greed-password))
	 (invert-secret (apply 'vector
			       (mapcar (function (lambda (x) (- 255 x)))
				       shared-secret)))
	 (userid (greed-string-to-vector greed-userid 16)))

    ;; Send the authentication data to the server
    (greed-send-command
     "AUTH"
     (greed-md5-encode (vconcat client-nonce server-nonce
				userid invert-secret))
     (greed-hex-encode client-nonce))

    ;; Expecting 133 with <server-hash>
    (greed-wait-for-response 133)

    ;; Check the server for correctness?
    (if greed-paranoid
	(if (and (not (string= (upcase greed-response-text)
			       (greed-md5-encode
				(vconcat server-nonce client-nonce
					 userid shared-secret))))
		 (not (y-or-n-p "Server failed its authentication. Proceed anyway? ")))
	    (greed-error "Could not validate server.")))))

(defun greed-md5-encode (v)
  "Return the MD5 hash of vector V as a string of 32 hex digits."
;;  (require 'md5)
  (greed-hex-encode (md5-encode v)))

;; Close RGTP server.
(defun greed-close-server ()
  (unwind-protect
      (if (greed-server-opened)
	  (greed-send-command "QUIT"))
    (if greed-server-process (delete-process greed-server-process))
    (if (not greed-debug)
	(progn
	  (if greed-server-buffer
	      (kill-buffer greed-server-buffer))
	  (setq greed-server-buffer nil)))
    (setq greed-server-process nil)))

;; Wait for RGTP server to reply; return response code.  Arguments
;; RESPONSES are a list of valid response codes.  Other codes cause an
;; error.
(defun greed-wait-for-response (&rest responses)
  (save-excursion
    (set-buffer greed-server-buffer)
    (let ((wait t))
      (while wait
	(goto-char (point-min))
	(if (looking-at "^\\([12345][0-9][0-9]\\) \\(.*\\)\r$")
	    (setq wait nil)
	  (greed-accept-response))))
    (forward-line 1)
    (setq greed-response-text (buffer-substring (match-beginning 2)
						(match-end 2))
	  greed-response-code (string-to-number (buffer-substring
						 (match-beginning 1)
						 (match-end 1))))
    (if greed-debug
	(greed-log-message (buffer-substring (point-min) (point))))
    (delete-region (point-min) (point))
    (if (memq greed-response-code responses)
	greed-response-code
      (greed-server-error))))

;; Wait for RGTP server to reply with data in BUFFER.
;; Carriage-return/new-line pairs are replaced with new-lines only.  If
;; optional argument DOT is non-nil, double-dot-decode the data.  The
;; previous contents of BUFFER are deleted.  Return NIL if it copied
;; successfully, T if it failed.
(defun greed-wait-for-data (buffer &optional dot)
  (save-excursion
    (set-buffer greed-server-buffer)
    (let ((wait t))
      (while wait
	(goto-char (- (point-max) 3))
	(if (looking-at "^\\.\r\n")
	    (setq wait nil)
	  (greed-accept-response))))
    (let ((start (point-min))
	  (end (- (point-max) 3)))
      (delete-region end (point-max))
      (prog1
	  (save-excursion
	    (if (not (bufferp buffer))
		nil
	      (set-buffer buffer)
	      (erase-buffer)
	      (insert-buffer-substring greed-server-buffer start end)
	      (goto-char (point-min))
	      (while (search-forward "\r\n" nil t) (replace-match "\n" t t))
	      (if dot
		  (progn
		    (goto-char (point-min))
		    (while (re-search-forward "^\\.\\." nil t)
		      (replace-match "." t t))))
	      t))
	(if greed-debug
	    (greed-log-message (buffer-substring start end)))
	(erase-buffer)))))

;; Read response of server.
(defun greed-accept-response ()
  (if (memq (process-status greed-server-process) '(open run))
      (accept-process-output greed-server-process)
    (error "RGTP: Connection closed.")))

;; Send STRINGS to RGTP server.  Make sure not to get caught in an
;; infinite loop!
(defun greed-send-command (&rest strings)
  (if (not (greed-server-opened))
      (if greed-reconnect-possible
	  (let ((greed-reconnect-possible nil))
	    (greed-connect-to-server))
	(error "Unable to reconnect to RGTP server on %s" greed-rgtp-server)))
  (let ((command ""))
    (while strings
      (setq command (concat command (car strings)
			    (if (cdr strings) " " "\r\n"))
	    strings (cdr strings)))
    (greed-log-message command)
    (process-send-string greed-server-process command)))

;; Return server process status, T or NIL.
;; If the stream is opened, return T, otherwise return NIL.
(defun greed-server-opened ()
  (and greed-server-process
       (memq (process-status greed-server-process) '(open run))))


;;; Authentication functions --------------------------------------------------

;; Encode the vector V as a string of hexademical digits.
;; The resulting string has two hex digits for each byte in V.
(defun greed-hex-encode (v)
  (let ((i 0) (s ""))
    (while (< i (length v))
      (setq s (concat s (format "%02x" (aref v i))) i (1+ i)))
    s))

;; Decode STRING into a vector of bytes.
;; STRING must consist of hexadecimal digits (upper or lower case).
;; Each byte of the result corresponds to two hex digits in STRING.
;; Returns the vector if successful, NIL otherwise.
(defun greed-hex-decode (string)
  (let* ((l (/ (length string) 2))
	 (v (make-vector l 0))
	 (i 0)
	 (j 0)
	 (flag t))
    (while (< i l)
      (let ((d1 (assq (aref string j) greed-hex-digits))
	    (d2 (assq (aref string (1+ j)) greed-hex-digits)))
	(if (and d1 d2)
	    (aset v i (+ (* (cdr d1) 16) (cdr d2)))
	  (setq flag nil)))
      (setq i (1+ i) j (+ j 2)))
    (if flag v nil)))

;; Convert STRING into a vector of length N containing its characters.
;; If STRING is longer than N, succeeding characters are dropped.  If
;; STRING is shorter than N, the vector is padded out with zeros.
(defun greed-string-to-vector (string n)
  (let ((v (make-vector n 0)) (i 0) (l (min (length string) n)))
    (while (< i l)
      (aset v i (aref string i))
      (setq i (1+ i)))
    v))

;; Return a vector of N bytes.  The idea is to return a different value
;; each time it is called, to prevent the possibility of replay attacks.
;; This implementation uses the unix time for the first four bytes of
;; the nonce, followed by pseudorandom numbers initialised from the
;; current time and process id.
(defun greed-generate-nonce (n)
  (let ((v (make-vector n 0))
	(i 4)
	(time (current-time)))
    (if (> n 0) (aset v 0 (/ (car time) 256)))
    (if (> n 1) (aset v 1 (% (car time) 256)))
    (if (> n 2) (aset v 2 (/ (car (cdr time)) 256)))
    (if (> n 3) (aset v 3 (% (car (cdr time)) 256)))
    (random t)
    (while (< i n)
      (aset v i (random 256))
      (setq i (1+ i)))
    v))


(provide 'greed)

;;; greed.el ends here
