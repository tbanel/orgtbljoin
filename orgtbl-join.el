;;; orgtbl-join.el --- join columns from another table

;; Copyright (C) 2014-2015  Thierry Banel

;; Author: Thierry Banel tbanelwebmin at free dot fr
;; Contributors:
;; Version: 0.1
;; Keywords: org, table, join, filtering
;; Package-Requires: ((cl-lib "0.5"))

;; orgtbl-join is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; orgtbl-join is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; A master table is enriched with columns coming from a reference
;; table.  For enriching a row of the master table, matching rows from
;; the reference table are selected.  The matching succeeds when the
;; key cells of the master row and the reference row are equal.
;;
;; Full documentation here:
;;   https://github.com/tbanel/orgtbljoin/blob/master/README.org

;;; Requires:
(require 'org-table)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(defun orgtbl--join-colname-to-int (col table)
  "Convert the column name into an integer (first column is numbered 0)
COL may be:
- a dollar form, like $5 which is converted to 4
- a number, like 5 which is converted to 4
- an alphanumeric name which appears in the column header (if any)
When COL does not match any actual column, an error is generated.
TABLE is an Org mode table passed as a list of lists of cells.
It is used to check COL against TABLE header."
  ;; skip first hlines if any
  (while (not (listp (car table)))
    (setq table (cdr table)))
  (if (symbolp col)
      (setq col (symbol-name col)))
  (cond ((numberp col)
	 t)
	((string-match "^\\$?\\([0-9]+\\)$" col)
	 (setq col (string-to-number (match-string 1 col))))
	(t
	 ;; TABLE has no header, COL does not make sense
	 (unless (memq 'hline table)
	   (user-error "No header on the table, and no such column '%s'" col))
	 ;; iterate over first line of header to find COL
	 (let ((i 0)
	       (n))
	   (mapc (lambda (c)
		   (setq i (1+ i))
		   (if (equal col c)
		       (setq n i)))
		 (car table))
	   (unless n (user-error "No such column '%s'" col))
	   (setq col n))))
  (setq col (1- col))
  (if (or (< col 0) (>= col (length (car table))))
      (user-error "Column %s outside table" col))
  col)

(defun orgtbl--join-query-column (prompt table)
  "Interactively query a column.
PROMPT is displayed to the user to explain what answer is expected.
TABLE is the org mode table from which a column will be choosen
by the user.  Its header is used for column names completion.  If
TABLE has no header, completion is done on generic column names:
$1, $2..."
  (while (eq 'hline (car table))
    (setq table (cdr table)))
  (org-icompleting-read
    prompt
    (if (memq 'hline table) ;; table has a header
	(car table)
      (let ((i 0))
	(mapcar (lambda (x) (format "$%s" (setq i (1+ i))))
		(car table))))))

(defun orgtbl--join-convert-to-hashtable (table col)
  "Convert an Org-mode TABLE into a hash table.
The purpose is to provide fast lookup to TABLE's rows.  The COL
column contains the keys for the hashtable entries.  Return a
cons, the car contains the header, the cdr contains the
hashtable."
  ;; skip heading horinzontal lines if any
  (while (eq (car table) 'hline)
    (setq table (cdr table)))
  ;; split header and body
  (let ((head)
	(body (memq 'hline table))
	(hash (make-hash-table :test 'equal :size (+ 20 (length table)))))
    (if (not body)
	(setq body table)
      (setq head table)
      ;; terminate header with nil
      (let ((h head))
	(while (not (eq (cadr h) 'hline))
	  (setq h (cdr h)))
	(setcdr h nil)))
    ;; fill-in the hashtable
    (mapc (lambda (row)
	    (when (listp row)
	      (let ((key (nth col row)))
		(puthash key (nconc (gethash key hash) (list row)) hash))))
	  body)
    (cons head hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions are borrowed
;; from the orgtbl-aggregate package.

(defun orgtbl-list-local-tables ()
  "Search for available tables in the current file."
  (interactive)
  (let ((tables))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*#\\+\\(tbl\\)?name:[ \t]*\\(.*\\)" nil t)
	  (let ((text (match-string 2)))
	    (set-text-properties 0 (length text) () text)
	    (setq tables (cons text tables))))))
    tables))

(defun orgtbl-get-distant-table (name-or-id)
  "Find a table in the current buffer named NAME-OR-ID.
Returns it as a list of lists of cells.  An horizontal line is
translated as the special symbol `hline'."
  (unless (stringp name-or-id)
    (setq name-or-id (format "%s" name-or-id)))
  (let (buffer loc id-loc tbeg form)
    (save-excursion
      (save-restriction
	(widen)
	(save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward
	       (concat "^[ \t]*#\\+\\(tbl\\)?name:[ \t]*"
		       (regexp-quote name-or-id)
		       "[ \t]*$")
	       nil t)
	      (setq buffer (current-buffer) loc (match-beginning 0))
	    (setq id-loc (org-id-find name-or-id 'marker))
	    (unless (and id-loc (markerp id-loc))
	      (error "Can't find remote table \"%s\"" name-or-id))
	    (setq buffer (marker-buffer id-loc)
		  loc (marker-position id-loc))
	    (move-marker id-loc nil)))
	(with-current-buffer buffer
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char loc)
	      (forward-char 1)
	      (unless (and (re-search-forward "^\\(\\*+ \\)\\|[ \t]*|" nil t)
			   (not (match-beginning 1)))
		(user-error "Cannot find a table at NAME or ID %s" name-or-id))
	      (setq tbeg (point-at-bol))
	      (org-table-to-lisp))))))))

(defun orgtbl-insert-elisp-table (table)
  "Insert TABLE in current buffer at point.
TABLE is a list of lists of cells.  The list may contain the
special symbol 'hline to mean an horizontal line."
    (while table
      (let ((row (car table)))
	(setq table (cdr table))
	(cond ((consp row)
	       (insert "|")
	       (insert (mapconcat #'identity row "|")))
	      ((eq row 'hline)
	       (insert "|-"))
	      (t (error "Bad row in elisp table")))
	(insert "\n")))
    (delete-char -1)
    (org-table-align))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In-place mode

;;;###autoload
(defun orgtbl-join ()
  "Add material from a reference table to the current table.
Rows from the reference table are appended to rows of the current
table.  For each row of the current table, matching rows from the
reference table are searched and appended.  The matching is
performed by testing for equality of cells in the current column,
and a joining column in the reference table.  If a row in the
current table matches several rows in the reference table, then
the current row is duplicated and each copy is appended with a
different reference row.  If no matching row is found in the
reference table, then the current row is kept, with empty cells
appended to it."
  (interactive)
  (org-table-check-inside-data-field)
  (let* ((col (1- (org-table-current-column)))
	 (tbl (org-table-to-lisp))
	 (ref (orgtbl-get-distant-table
	       (org-icompleting-read
		"Reference table: "
		(orgtbl-list-local-tables))))
	 (dcol (orgtbl--join-colname-to-int
		(orgtbl--join-query-column "Reference column: " ref)
		ref))
	 (refhead)
	 (refhash))
    (setq ref (orgtbl--join-convert-to-hashtable ref dcol)
	  refhead (car ref)
	  refhash (cdr ref))
    (goto-char (org-table-begin))
    ;; Skip any hline a the top of tbl.
    (while (eq (car tbl) 'hline)
      (setq tbl (cdr tbl))
      (forward-line 1))
    ;; is there a header on tbl ? append the ref header (if any)
    (when (memq 'hline tbl)
      ;; for each line of header in tbl, add a header from ref
      ;; if ref-header empties too fast, continue with nils
      ;; if tbl-header empties too fast, ignore remaining ref-headers
      (while (listp (pop tbl))
	(end-of-line)
	(when refhead
	  (orgtbl--join-insert-ref-row (car refhead) dcol)
	  (setq refhead (cdr refhead)))
	(forward-line 1))
      (forward-line 1))
    ;; now the body of the tbl
    (mapc (lambda (masline)
	    (if (listp masline)
		(let ((done))
		  ;; if several ref-lines match, all of them are considered
		  (mapc (lambda (refline)
			  (end-of-line)
			  (when done ;; make a copy of the current row
			    (open-line 1)
			    (forward-line 1)
			    (insert "|")
			    (mapc (lambda (y) (insert y) (insert "|"))
				  masline))
			  (orgtbl--join-insert-ref-row refline dcol)
			  (setq done t))
			(gethash (nth col masline) refhash))))
	    (forward-line 1))
	  tbl))
  (forward-line -1)
  (org-table-align))

(defun orgtbl--join-insert-ref-row (row dcol)
  "Insert a distant ROW in the buffer.
The DCOL columns (joining column) is skipped."
  (let ((i 0))
    (while row
      (unless (equal i dcol)
	(insert (car row))
	(insert "|"))
      (setq i (1+ i))
      (setq row (cdr row)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PULL & PUSH engine

(defun orgtbl--join-append-mas-ref-row (masrow refrow refcol)
  "Concatenate master and reference rows, skiping the reference column.
MASROW is a list of cells from the master table.  REFROW is a
list of cells from the reference table.  REFCOL is the position,
numbered from zero, of the column in REFROW that should not be
appended in the result, because it is already present in MASROW."
  (let ((result (reverse masrow))
	(i 0))
    (while refrow
      (unless (equal i refcol)
	(setq result (cons (car refrow) result)))
      (setq refrow (cdr refrow))
      (setq i (1+ i)))
    (reverse result)))

(defun orgtbl--create-table-joined (mastable mascol reftable refcol)
  "Join a master table with a reference table.
MASTABLE is the master table, as a list of lists of cells.
MASCOL is the name of the joining column in the master table.
REFTABLE is the reference table.
REFCOL is the name of the joining column in the reference table.
Returns MASTABLE enriched with material from REFTABLE."
  (let ((result)  ;; result built in reverse order
	(refhead)
	(refhash))
    ;; skip any hline a the top of both tables
    (while (eq (car mastable) 'hline)
      (setq result (cons 'hline result))
      (setq mastable (cdr mastable)))
    (while (eq (car reftable) 'hline)
      (setq reftable (cdr reftable)))
    ;; convert column-names to numbers
    (setq mascol (orgtbl--join-colname-to-int mascol mastable))
    (setq refcol (orgtbl--join-colname-to-int refcol reftable))
    ;; convert reference table into fast-lookup hashtable
    (setq reftable (orgtbl--join-convert-to-hashtable reftable refcol)
	  refhead (car reftable)
	  refhash (cdr reftable))
    ;; iterate over master table header if any
    ;; and join it with reference table header if any
    (if (memq 'hline mastable)
	(while (listp (car mastable))
	  (setq result
		(cons (orgtbl--join-append-mas-ref-row
		       (car mastable)
		       (and refhead (car refhead))
		       refcol)
		      result))
	  (setq mastable (cdr mastable))
	  (if refhead
	      (setq refhead (cdr refhead)))))
    ;; create the joined table
    (mapc (lambda (masline)
	    (if (not (listp masline))
		(setq result (cons masline result))
	      (let ((result0 result))
		;; if several ref-lines match, all of them are considered
		(mapc (lambda (refline)
			(setq result
			      (cons
			       (orgtbl--join-append-mas-ref-row
				masline
				refline
				refcol)
			       result)))
		      (gethash (nth mascol masline) refhash))
		;; if no ref-line matches, add the non-matching master-line anyway
		(if (eq result result0)
		    (setq result (cons masline result))))))
	  mastable)
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUSH mode

;;;###autoload
(defun orgtbl-to-joined-table (table params)
  "Enrich the master TABLE with lines from a reference table.

PARAMS contains pairs of key-value with the following keys:

:ref-table   the reference table.
             Lines from the reference table will be added to the
             master table.

:mas-column  the master joining column.
             This column names one of the master table columns.

:ref-column  the reference joining column.
             This column names one of the reference table columns.

Columns names are either found in the header of the table, if the
table have a header, or a dollar form: $1, $2, and so on.

The destination must be specified somewhere in the
same file with a bloc like this:
#+BEGIN RECEIVE ORGTBL destination_table_name
#+END RECEIVE ORGTBL destination_table_name"
  (interactive)
  (orgtbl-to-generic
   (orgtbl--create-table-joined
    table
    (plist-get params :mas-column)
    (orgtbl-get-distant-table (plist-get params :ref-table))
    (plist-get params :ref-column))
   (org-combine-plists
    (list :sep "|" :hline "|-" :lstart "|" :lend "|")
    params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PULL mode

;;;###autoload
(defun org-insert-dblock:join ()
  "Wizard to interactively insert a joined table as a dynamic block."
  (interactive)
  (let* ((localtables (orgtbl-list-local-tables))
	 (mastable
	  (org-icompleting-read
	   "Master table: "
	   localtables))
	 (mascol
	  (orgtbl--join-query-column
	   "Master joining column: "
	   (orgtbl-get-distant-table mastable)))
	 (reftable
	  (org-icompleting-read
	   "Reference table: "
	   localtables))
	 (refcol
	  (orgtbl--join-query-column
	   "Reference joining column: "
	   (orgtbl-get-distant-table reftable))))
    (org-create-dblock
     (list :name "join"
	   :mas-table mastable :mas-column mascol
	   :ref-table reftable :ref-column refcol))
    (org-update-dblock)))

;;;###autoload
(defun org-dblock-write:join (params)
  "Create a joined table out of a master and a reference table.

PARAMS contains pairs of key-value with the following keys:

:mas-table   the master table.
             This table will be copied and enriched with material
             from the reference table.

:ref-table   the reference table.
             Lines from the reference table will be added to the
             master table.

:mas-column  the master joining column.
             This column names one of the master table columns.

:ref-column  the reference joining column.
             This column names one of the reference table columns.

Columns names are either found in the header of the table, if the
table have a header, or a dollar form: $1, $2, and so on.

The
#+BEGIN RECEIVE ORGTBL destination_table_name
#+END RECEIVE ORGTBL destination_table_name"
  (interactive)
  (orgtbl-insert-elisp-table
   (orgtbl--create-table-joined
    (orgtbl-get-distant-table (plist-get params :mas-table))
    (plist-get params :mas-column)
    (orgtbl-get-distant-table (plist-get params :ref-table))
    (plist-get params :ref-column))))

;;;###autoload
(defun orgtbl-join-setup-keybindings ()
  "Setup key-binding and menu entry.
This function can be called in your .emacs. It will add the `C-c
C-x j' key-binding for calling the orgtbl-join wizard, and a menu
entry under Tbl > Column > Join with another table."
  (eval-after-load 'org
    '(progn
       (org-defkey org-mode-map "\C-c\C-xj" 'orgtbl-join)
       (easy-menu-add-item
	org-tbl-menu '("Column")
	["Join with another table" orgtbl-join t]))))

(provide 'orgtbl-join)
;;; orgtbl-join.el ends here
