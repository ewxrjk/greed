;;; greed-help-macro.el -- Make command line help similar to help-for-help

;; Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1997 Gareth Rees

;; Author: Gareth Rees <gareth@persimmon.co.uk>
;; Created: Thu Apr 10 08:47:54 BST 1997

;;; Commentary:

;; This is a replacement for the help-macro.el feature of GNU Emacs
;; that works in XEmacs.  It is based on the definition of
;; help-for-help in help.el for XEmacs.

;;; Code:

(defmacro make-help-screen (fname help-line help-text helped-map)
  "Construct help-menu function name FNAME.
When invoked, FNAME shows HELP-LINE and reads a command using HELPED-MAP.
If the command is the help character, FNAME displays HELP-TEXT
and continues trying to read a command using HELPED-MAP.
When FNAME finally does get a command, it executes that command
and then returns."
  `(defun ,fname ()
     ,help-text
     (interactive)
     (let ((help-key (copy-event last-command-event))
	   event char)
       (message ,help-line)
       (setq event (next-command-event)
	     char (event-to-character event))
       (if (or (equal event help-key)
	       (eq char ??)
	       (eq 'help-command (key-binding event)))
	   (save-window-excursion
	     (switch-to-buffer "*Help*")
	     (delete-other-windows)
	     (let ((buffer-read-only nil))
	       (erase-buffer)
	       (insert (documentation (quote ,fname))))
	     (goto-char (point-min))
	     (while (or (equal event help-key)
			(eq char ??)
			(eq 'help-command (key-binding event))
			(eq char ? )
			(eq 'scroll-up (key-binding event))
			(eq char ?\177)
			(eq 'scroll-down (key-binding event)))
	       (if (or (eq char ? )
		       (eq 'scroll-up (key-binding event)))
		   (scroll-up))
	       (if (or (eq char ?\177)
		       (eq 'scroll-down (key-binding event)))
		   (scroll-down))
;	       (if (pos-visible-in-window-p (point-max))
;		   (message "A B C F I K L M N P S T V W C-c C-d C-n C-w C-i C-k C-f: ")
;		 (message "A B C F I K L M N P S T V W C-c C-d C-n C-w C-i C-k C-f or Space to scroll: "))
	       (let ((cursor-in-echo-area t))
		 (setq event (next-command-event event)
		       char (or (event-to-character event) event))))))
       (let ((defn (or (lookup-key ,helped-map (vector event))
		       (and (numberp char)
			    (lookup-key ,helped-map 
					(make-string 1 (downcase char)))))))
	 (message nil)
	 (if defn
	     (call-interactively defn)
	   (ding))))))
