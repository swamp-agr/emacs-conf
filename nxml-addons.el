
;;; xml-analyzer.el --- XML analyzing tools

;; Copyright (C) 2014 Andrey Prokopenko <persiantiger@yandex.ru>

;; Version: 0.0.1
;; Keywords: xml analysis tool
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
;; This plugin requires `nxhtml` plugin to work correctly

(load "~/.emacs.d/nxhtml/autostart.el")
;; Please read README files for any relevant help.

;;; Thanks

;; All the users motivated me to write this stuff.


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
	    (message "/%s" (mapconcat 'identity path "/"))
	  (format "/%s" (mapconcat 'identity path "/")))))))

(defun nxml-filter (node-string attr-string)
  "Display filtering XML by main element and one of child elements:
here element can be anything: container, node with or without value, with or without any attributes.
We search any elements in any parent container' elements and returns concurrences if they exist.
"
  (interactive "sEnter node name: \nsEnter attribute name: ")
  ;; begin
  (let (start end parent-buffer close-tag error-s)
    ;; create new buffer
    (setq parent-buffer (get-buffer-create "temp")) 
    ;; erase new buffer (repeating eval fixed)
    (save-current-buffer
      (set-buffer parent-buffer)
      (erase-buffer))
    ;; close-tag
    (string-match ".*?<\\([^> ]*\\).*?>" node-string)
    (setq close-tag (concat "</" (format "%s" (match-string 1 node-string)) ">"))

    ;; main part
    (goto-char (point-min))
    (save-excursion
      (while (search-forward node-string nil t)
    	(beginning-of-line)
    	(setq start (point))
    	(next-line)
    	(setq end (point))
    	(insert-outer-buffer-substr parent-buffer start end)
	
    	(while (search-forward close-tag nil t)
    	  (while (search-forward attr-string nil t)
    	    (beginning-of-line)
    	    (setq start (point))
    	    (next-line)
    	    (setq end (point))
    	    (insert-outer-buffer-substr parent-buffer start end))
    	  (beginning-of-line)
    	  (setq start (point))
    	  (next-line)
    	  (setq end (point))
    	  (insert-outer-buffer-substr parent-buffer start end))	  
	(message "Done!")
	;; popping result
	(pop-to-buffer "temp")
      ))))

(defun insert-outer-buffer-substr (buffer start end)
  "insert substring from region of current buffer to buffer"
	(let ((oldbuf (current-buffer)))
	  (save-current-buffer
	    (set-buffer buffer)
	    (insert-buffer-substring oldbuf start end)
	    )))
  

(provide 'nxml-addons)
