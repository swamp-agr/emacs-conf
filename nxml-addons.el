
;;; nxml-addons.el --- XML analyzing tools

;; Copyright (C) 2014 Andrey Prokopenko <persiantiger@yandex.ru>

;; Version: 0.1.1
;; Keywords: xml analysis tool, XPath, XML Filter
;; Author: Andrey Prokopenko <persiantiger@yandex.ru>
;; URL: 

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; This plugin requires `nxhtml` plugin to work correctly
;; You can download it from: http://ourcomments.org/cgi-bin/emacsw32-dl-latest.pl

(load "~/.emacs.d/nxhtml/autostart.el")
;; Please read README files for any relevant help.

;;; Thanks to
;; Alexander Syromyatnikov, Alexander Kovalev, Alexander Lesnyakov, 
;; Daniil Chernopolsky, Irina Kuznetsova, Andrew Safronov 
;; and especially thanks to my wife.
;;
;; All of You motivated me to write this stuff.


(defun nxml-where ()
  "Display the XPath-proto from the current point."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
	(widen)
	(while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
		    (condition-case nil
			(progn
			  (nxml-backward-up-element) ; always returns nil
			  t)
		      (error nil)))
	  (setq path (cons (xmltok-start-tag-local-name) path)))
	(if (called-interactively-p t)
	    (progn (message "/%s" (mapconcat 'identity path "/"))
		   ;; added result xpath-proto to clipboard
		   (kill-new 
		    (format "/%s" (mapconcat 'identity path "/"))))
	  (format "/%s" (mapconcat 'identity path "/")))))))


(defun nxml-filter (node-string attr-string)
  "Display filtering XML by main element and one of child elements:
here element can be anything: container, node with or without value, with or without any attributes.
We search any elements in any parent container' elements and returns concurrences if they exist.
"
  (interactive "sInput string of parent element: \nsInput string of child element: ")
  ;; begin
  (let (start end parent-buffer node-close-tag attr-close-tag is-node-closed is-attr-closed level pattern)
    ;; create new buffer
    (setq parent-buffer (get-buffer-create "temp")) 
    ;; erase new buffer (repeating eval fixed)
    (save-current-buffer
      (set-buffer parent-buffer)
      (erase-buffer))
    ;; close-tag
    (if (string-match ".*?<\\([^> ]*\\).*?>" node-string)
	(progn 
	  (setq node-close-tag (format "</%s>" (match-string 1 node-string)))
	  (if (string-match node-close-tag node-string)
	      (setq is-node-closed 1)
	    (setq is-node-closed 0))))

    (if (string-match ".*?<\\([^> ]*\\).*?>" attr-string)
	(progn 
	  (setq attr-close-tag (format "</%s>" (match-string 1 attr-string)))
	  (if (string-match attr-close-tag attr-string)
	      (setq is-attr-closed 1)
	    (setq is-attr-closed 0))))

    ;; setting level of regexp
    (setq level 
	  (cond ((and (= is-node-closed 0) (= is-attr-closed 0)) 1)
		((and (= is-node-closed 0) (= is-attr-closed 1)) 2)
		((and (= is-node-closed 1) (= is-attr-closed 0)) 3)
		((and (= is-node-closed 1) (= is-attr-closed 1)) 4)))

    (setq pattern 
	  (cond ((= level 1) (concat "\\(" (regexp-quote node-string)
				     "\\|" (regexp-quote attr-string)
				     "\\|" (regexp-quote node-close-tag)
				     "\\|" (regexp-quote attr-close-tag)
				     "\\)"))
		((= level 2) (concat "\\(" (regexp-quote node-string)
				     "\\|" (regexp-quote attr-string)
				     "\\|" (regexp-quote node-close-tag)
				     "\\)"))
		((= level 3) (concat "\\(" (regexp-quote node-string)
				     "\\|" (regexp-quote attr-string)
				     "\\|" (regexp-quote attr-close-tag)
				     "\\)"))
		((= level 4) (concat "\\(" (regexp-quote node-string)
				     "\\|" (regexp-quote attr-string)
				     "\\)"))))
	  
    ;; main part
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
	  (progn (beginning-of-line)
		 (setq start (point))
		 (end-of-line)
		 (setq end (point))
		 (message attr-string)
		 (insert-outer-buffer-substr parent-buffer start end)))

      (message "Done!")
      ;; popping result
      (pop-to-buffer "temp")
      ))

(defun insert-outer-buffer-substr (buffer start end)
  "insert substring from region of current buffer to buffer"
	(let ((oldbuf (current-buffer)))
	  (save-current-buffer
	    (set-buffer buffer)
	    (insert-buffer-substring oldbuf start end)
	    (newline)
	    )))

;; key bindings
(global-set-key (kbd "C-x w") 'nxml-where)
(global-set-key (kbd "C-x f") 'nxml-filter)

(provide 'nxml-addons)


