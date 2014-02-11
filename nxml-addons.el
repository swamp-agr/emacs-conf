
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
  "Display the hierarchy of XML elements the point is on as a path."
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
  "Display filtering XML by parent node and one of child nodes"
  (interactive "sEnter node name: \nsEnter attribute name: ")
  (let (start end parent-buffer start-region end-region
	      error-msg t-child-name error-s)
    ;; create new buffer
    (setq parent-buffer (get-buffer-create "temp")) 
    ;; erase new buffer (repeating eval fixed)
    (save-current-buffer
      (set-buffer parent-buffer)
      (erase-buffer))
    ;; main part
    (goto-char (point-min))
    (save-excursion
      (while (search-forward node-string nil t)
	(goto-char (match-end 0))
	(nxml-backward-up-element) ;; point to the begin of element
	(setq start-region (point)) ;; mark start-point point
	(forward-char) ;; move up to one character
	(nxml-up-element) ;; point to the-end-tag
	(setq end-region (point))
	(nxml-backward-element)
	(beginning-of-line)
	(setq start (point))
	(end-of-line)
	(setq end (point))
	(insert-outer-buffer-substr parent-buffer start end)
	(save-excursion
	  (save-restriction
	    (narrow-to-region start-region end-region)
	    (goto-char (point-min))
	    (while (search-forward attr-string nil t)
	      (goto-char (match-end 0))
	      (nxml-backward-up-element)
	      (beginning-of-line)
	      (setq start (point))
	      (end-of-line)
	      (setq end (point))
	      (insert-outer-buffer-substr parent-buffer start end))))
	(goto-char end-region)
	(beginning-of-line)
	(setq start (point))
	(end-of-line)
	(setq end (point))
	(insert-outer-buffer-substr parent-buffer start end)
      ;; TODO open new frame and show "temp" buffer
      ))))

(defun insert-outer-buffer-substr (buffer start end)
  "insert substring from region of current buffer to buffer"
	(let ((oldbuf (current-buffer)))
	  (save-current-buffer
	    (set-buffer buffer)
	    (insert-buffer-substring oldbuf start end)
	    (newline))))
  

(provide 'nxml-addons)
