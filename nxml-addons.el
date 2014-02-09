
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

(defun nxml-filter (node-name attr-name)
  "Display filtering XML by two level structure"
  (interactive "sEnter node name: \nsEnter attribute name: ")
  (let (start end parent-buffer 
	      child-buffer error-msg 
	      t-parent-name error-s)
    (setq parent-buffer (get-buffer-create "temp"))
    (goto-char (point-min))
    (save-excursion
      (while (< (point) (point-max))
	(search-forward (string-tag node-name))
	;; searching buffer
	(nxml-backward-up-element) ;; point to the begin of element
	(setq start (point)) ;; mark start point
	(forward-char) ;; move up to one character
	(setq t-parent-name (xmltok-start-tag-local-name))
	(nxml-up-element) ;; point to the-end-tag
	(setq end (point))
	;; adding region to buffer
	(let ((oldbuf (current-buffer)))
	  (save-current-buffer
	    (set-buffer parent-buffer)
	    (insert-buffer-substring oldbuf start end)))))))


(defun string-tag (str)
  "Geting tag element from string"
  (concat "<" str ">"))

;;+ 0.1 создаю пустой буфер
;;+ 0.2 иду в начало документа

					; 1. ищу первый родительский элемент


(provide 'nxml-addons)
