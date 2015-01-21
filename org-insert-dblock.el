;;; org-inset-dblock.el --- Wizzard to insert a dynamic block
;; -*- coding:utf-8;-*-

;; Copyright (C) 2013, 2014, 2015  Thierry Banel

;; Author: Thierry Banel
;; Version: 0.2
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: org, table

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A wizzard to insert Org-mode dynamic blocks.
;; The toplevel wizzard calls specialized wizzards.
;; Specialized wizzards are functions matching org-insert-dblock:*
;; Right now, the following are available:
;;   org-insert-dblock:columnview   (calls org-insert-columns-dblock)
;;   org-insert-dblock:clocktable   (calls org-clock-report)
;;   org-insert-dblock:propview
;;   org-insert-dblock:invoice
;;   org-insert-dblock:aggregate
;;   org-insert-dblock:transpose
;;   org-insert-dblock:join
;;
;; The toplevel wizzards extends the C-c C-x i keybinding.
;; (The C-c C-x i binding was limited to org-insert-columns-dblock,
;; which can be invoqued by answering "columnview"
;; at the toplevel wizzard prompt)

;;; Code:

(require 'easymenu)
(require 'org)

;; ------------------------------------
;; A few adapters need to be defined
;; to make present wizzards compliant with
;; the org-insert-dblock:* pattern naming

;;;###autoload
(defun org-insert-dblock:columnview ()
  "Adapter function for inserting a column view."
  (interactive)
  (org-insert-columns-dblock))

;;;###autoload
(defun org-insert-dblock:clocktable ()
  "Adapter function to insert a clock-table."
  (interactive)
  (org-clock-report))

;;;###autoload
(defun org-insert-dblock:propview ()
  "Adapter function to insert a property view."
  (interactive)
  (org-create-dblock
   (list
    :name "propview"
    :id ""
    :cols ()
    :inherit 'no
    :conds t
    :match nil
    :scope ()
    :noquote t
    :colnames ()
    :defaultval "aa"
    :content "")))

;;;###autoload
(defun org-insert-dblock:invoice ()
  "Adapter function to insert an invoce block."
  (interactive)
  (org-create-dblock
   (list
    :name "invoice"
    :scope :tree1
    :prices t
    :headers t
    :summary t)))

;; The top-level wizzard collects sub-wizzards by looking
;; for functions named following the org-insert-dblock:* pattern
;; The wizzard can find any loaded or auto-loadable sub-wizzard
;; It is up to each sub-wizzard to do whatever completion they need.

;;;###autoload
(defun org-insert-dblock ()
  "Insert an org table dynamic block.
This is a dispatching function which prompts for the type
of dynamic block to insert.  It dispatches to functions
which names matches the pattern \\[org-insert-dblock:*]"
  (interactive)
  (let ((fun
	 (intern
	  (format
	   "org-insert-dblock:%s"
	   (org-icompleting-read
	    "Kind of dynamic block: "
	    (mapcar (lambda (x)
		      (replace-regexp-in-string
		       "^org-insert-dblock:"
		       ""
		       (symbol-name x)))
		    (apropos-internal "^org-insert-dblock:")))))))
    (if (functionp fun)
	(funcall fun)
      (message "No such dynamic block: %s" fun))))

;;;###autoload
(defun org-insert-dblock-bindings ()
  "Setup key-binding.
This function can be called in your .emacs. It will extend the
C-c C-x i key-binding for inserting any dynamic block, not only
\\[org-insert-columns-dblock]"
  (org-defkey org-mode-map "\C-c\C-xi" 'org-insert-dblock)
  (easy-menu-add-item
   org-org-menu '()
   ["Insert Dynamic Block" org-insert-dblock t] "Agenda Command..."))

(provide 'org-insert-dblock)
;;; org-insert-dblock.el ends here
