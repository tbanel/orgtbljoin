;;; orgtbl-join.el --- Join columns from other Org Mode tables -*- lexical-binding: t;-*-

;; Copyright (C) 2014-2025  Thierry Banel

;; Author: Thierry Banel tbanelwebmin at free dot fr
;; Contributors:
;; Version: 0.1
;; Keywords: data, extensions
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/tbanel/orgtbljoin/blob/master/README.org

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
;; table.
;;
;; We want to enrich this table:
;; | type     | quty |
;; |----------+------|
;; | onion    |   70 |
;; | tomato   |  120 |
;; | eggplant |  300 |
;; | tofu     |  100 |
;;
;; We have a reference table:
;; | type     | Fiber | Sugar | Protein | Carb |
;; |----------+-------+-------+---------+------|
;; | eggplant |   2.5 |   3.2 |     0.8 |  8.6 |
;; | tomato   |   0.6 |   2.1 |     0.8 |  3.4 |
;; | onion    |   1.3 |   4.4 |     1.3 |  9.0 |
;; | egg      |     0 |  18.3 |    31.9 | 18.3 |
;; | rice     |   0.2 |     0 |     1.5 | 16.0 |
;; | bread    |   0.7 |   0.7 |     3.3 | 16.0 |
;; | orange   |   3.1 |  11.9 |     1.3 | 17.6 |
;; | banana   |   2.1 |   9.9 |     0.9 | 18.5 |
;; | tofu     |   0.7 |   0.5 |     6.6 |  1.4 |
;; | nut      |   2.6 |   1.3 |     4.9 |  7.2 |
;; | corn     |   4.7 |   1.8 |     2.8 | 21.3 |
;;
;; We get the resulting joined table:
;; | type     | quty | Fiber | Sugar | Protein | Carb |
;; |----------+------+-------+-------+---------+------|
;; | onion    |   70 |   1.3 |   4.4 |     1.3 |  9.0 |
;; | tomato   |  120 |   0.6 |   2.1 |     0.8 |  3.4 |
;; | eggplant |  300 |   2.5 |   3.2 |     0.8 |  8.6 |
;; | tofu     |  100 |   0.7 |   0.5 |     6.6 |  1.4 |
;;
;; Full documentation here:
;;   https://github.com/tbanel/orgtbljoin/blob/master/README.org

;;; Requires:
(require 'org)
(require 'org-table)
(eval-when-compile (require 'cl-lib))
(require 'rx)
(require 'json)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creating long lists in the right order may be done
;; - by (nconc)  but behavior is quadratic
;; - by (cons) (nreverse)
;; a third way involves keeping track of the last cons of the growing list
;; a cons at the head of the list is used for housekeeping
;; the actual list is (cdr ls)

(defsubst orgtbl-join--list-create ()
  "Create an appendable list."
  (let ((x (cons nil nil)))
    (setcar x x)))

(defmacro orgtbl-join--list-append (ls value)
  "Append VALUE at the end of LS in O(1) time."
  `(setcar ,ls (setcdr (car ,ls) (cons ,value nil))))

(defmacro orgtbl-join--list-get (ls)
  "Return the regular Lisp list from LS."
  `(cdr ,ls))

(defmacro orgtbl-join--pop-simple (place)
  "Like (pop PLACE), but without returning (car PLACE)."
  `(setq ,place (cdr ,place)))

(defmacro orgtbl-join--pop-leading-hline (table)
  "Remove leading hlines from TABLE, if any."
  `(while (not (listp (car ,table)))
     (orgtbl-join--pop-simple ,table)))

(defun orgtbl-join--plist-get-remove (params prop)
  "Like `plist-get', but also remove PROP from PARAMS."
  (let ((v (plist-get params prop)))
    (if v
        (setcar (memq prop params) nil))
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The function (org-table-to-lisp) have been greatly enhanced
;; in Org Mode version 9.4
;; To benefit from this speedup in older versions of Org Mode,
;; this function is copied here with a slightly different name
;; It has also undergone near 3x speedup,
;; - by not using regexps
;; - achieving the shortest bytecode
;; Furthermore, this version avoids the
;; inhibit-changing-match-data and looking-at
;; incompatibilities between Emacs-27 and Emacs-30

(defun orgtbl-join--table-to-lisp (&optional txt)
  "Convert the table at point to a Lisp structure.
The structure will be a list.  Each item is either the symbol `hline'
for a horizontal separator line, or a list of field values as strings.
The table is taken from the parameter TXT, or from the buffer at point."
  (if txt
      (with-temp-buffer
	(buffer-disable-undo)
        (insert txt)
        (goto-char (point-min))
        (orgtbl-join--table-to-lisp))
    (save-excursion
      (goto-char (org-table-begin))
      (let (table)
        (while (progn (skip-chars-forward " \t")
                      (eq (following-char) ?|))
	  (forward-char)
	  (push
	   (if (eq (following-char) ?-)
	       'hline
	     (let (row)
	       (while (progn (skip-chars-forward " \t")
                             (not (eolp)))
                 (let ((q (point)))
                   (skip-chars-forward "^|\n")
                   (goto-char
                    (let ((p (point)))
                      (unless (eolp) (setq p (1+ p)))
                      (skip-chars-backward " \t" q)
                      (push
                       (buffer-substring-no-properties q (point))
                       row)
                      p))))
	       (nreverse row)))
	   table)
	  (forward-line))
	(nreverse table)))))

;; There is no CSV parser bundled with Emacs. In order to avoid a
;; dependency on a package, here is an implementation of a parser.  It
;; is made of the same technology as `orgtbl-aggregate--table-to-lisp'
;; (which is now integrated into the newest versions of Emacs). It is
;; probably as fast as can be in Emacs-Lisp byte-code.

(defun orgtbl-join--csv-to-lisp (header colnames)
  "Convert current buffer in CSV to Lisp.
It recognize cells protected by double quotes, and cells not protected.
When a cell is not protected, blanks are kept.
When a cell is protected, blanks before the first double quote are ignored.
Double double quotes are recognized within a cell double-quoted.
The last line may or may not end in a newline.
Separators are comma, semicolon, or TAB. They can be mixed.
If a row is empty, it is considered as a separator, and translated
to `hline', the Org table horizontal separator.
HEADER non nil means that the first row must be interpreted as a header.
COLNAMES, if not nil, is a list of column names."
  (goto-char (point-min))
  (let (table)
    (while (not (eobp))
      (let (row)
        (while (not (eolp))
          (let ((p (point)))
            (skip-chars-forward " ")
            (if (eq (following-char) ?\")
                (let (dquote)
                  (forward-char 1)
                  (setq p (point))
                  (while
                      (progn
                        (skip-chars-forward "^\"")
                        (forward-char 1)
                        (if (eq (following-char) ?\")
                            (progn (forward-char 1)
                                   (setq dquote t)))))
                  (push
                   (let ((cell
                          (buffer-substring-no-properties p (1- (point)))))
                     (if dquote
                         (string-replace "\"\"" "\"" cell)
                       cell))
                   row)
                  (skip-chars-forward " "))
              (skip-chars-forward "^,;\t\n")
              (push
               (buffer-substring-no-properties p (point))
               row))
            (skip-chars-forward ",;\t" (1+ (point)))))
        (push
         (if row (nreverse row) 'hline)
         table)
        (or (eobp) (forward-char 1))))
    (setq table (nreverse table))
    (if header
        (setcdr table (cons 'hline (cdr table))))
    (if colnames
        (setq table (cons colnames (cons 'hline table))))
    table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A few rx abbreviations
;; each time a bit of a regexp is used twice or more,
;; it makes sense to define an abbrev

(eval-when-compile ;; not used at runtime

  ;; search for table name, such as:
  ;; #+tablename: mytable
  (rx-define tblname
    (seq bol (* blank) "#+" (? "tbl") "name:" (* blank)))

  ;; skip lines beginning with # in order to reach the start of table
  (rx-define skipmetatable (firstchars)
    (seq point
         (0+ (0+ blank) (? firstchars (0+ any)) "\n")
         (0+ blank) "|"))

  ;; just to get ride of a few parenthesis
  (rx-define notany (&rest list)
    (not (any list)))

  ;; match quoted column names, like
  ;; 'col a' "col b" colc
  (rx-define quotedcolname (&rest bare)
    (or
     (seq ?'  (* (notany ?' )) ?' )
     (seq ?\" (* (notany ?\")) ?\")
     bare))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here is a bunch of useful utilities,
;; generic enough to be detached from the orgtbl-join package.
;; For the time being, they are here.

(defun orgtbl-join--list-local-tables (file)
  "Search for available tables in FILE.
If FILE is nil, use current buffer."
  (interactive)
  (with-current-buffer
      (if file (find-file-noselect file) (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (cl-loop
         while
         (re-search-forward
          (rx tblname (group (*? any)) (* blank) eol)
          nil t)
         collect (match-string-no-properties 1))))))

(defun orgtbl-join--table-from-babel (name-or-id)
  "Retrieve an input table as the result of running a Babel block.
NAME-OR-ID is the usual Org convention for pointing to a distant reference.
Examples: babel, file:babel, file:babel[1:3,2:5], file:babel(p1=…,p2=…)
This function could work also for a table,
but this has already been short-circuited."
  ;; A user error is generated in case no Babel block is found
  (let ((table (org-babel-ref-resolve name-or-id)))
    (and
     table
     (consp table)
     (or (eq (car table) 'hline)
         (consp (car table)))
     table)))

(defun orgtbl-join--table-from-csv (file params)
  "Parse a CSV formatted table located in FILE.
The cell-separator is currently guessed.
Currently, there is no header."
  (let ((header) (colnames))
    (cl-loop
     for p on (cdr (read params))
     do
     (cond
      ((eq (car p) 'header)
       (setq header t))
      ((eq (car p) 'colnames)
       (setq p (cdr p))
       (setq colnames (car p)))
      (t
       (message "parameter %S not recognized" (car p)))))
    (with-temp-buffer
      (insert-file-contents file)
      (orgtbl-join--csv-to-lisp header colnames))))

(defun orgtbl-join--table-from-json (file _params)
  "Parse a JSON formatted table located in FILE.
FILE is a filename with possible relative or absolute path.
Currently, the accepted format is
[[\"COL1\",\"COL2\",…]
 \"hline\"
 [\"VAL11\",\"COL12\",…]
 [\"VAL21\",\"COL22\",…]
 [\"VAL31\",\"COL32\",…]
Numbers do not need to be quoted.
 …"
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (let ((json (json-read-file file)))
      (cl-loop
       for row in json
       if (stringp row)
       collect (intern row)
       else
       collect (append row ())))))

(defun orgtbl-join--table-from-name (file name)
  "Parse an Org table named NAME in a ditant Org file named FILE.
FILE is a filename with possible relative or absolute path.
If FILE is nil, look in the current buffer."
  (with-current-buffer
      (if file
          (find-file-noselect file)
        (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (when (let ((case-fold-search t))
	      (re-search-forward
	       (rx tblname (literal name) (* blank) eol)
	       nil t))
        (re-search-forward (rx (skipmetatable "#")) nil t)
        (orgtbl-join--table-to-lisp)))))

(defun orgtbl-join--table-from-id (id)
  "Parse a table following a header in a distant Org file.
The header have an ID property equal to ID in a PROPERTY drawer."
  (let ((id-loc (org-id-find id 'marker)))
    (when (and id-loc (markerp id-loc))
      (with-current-buffer (marker-buffer id-loc)
        (save-excursion
          (goto-char (marker-position id-loc))
          (move-marker id-loc nil)
          (and
           (re-search-forward (rx (skipmetatable (any "*#:"))) nil t)
           (orgtbl-join--table-to-lisp)))))))

(defun orgtbl-join-table-from-any-ref (name-or-id)
  "Find a table referenced by NAME-OR-ID.
The reference is all the accepted Org references,
and additionally pointers to CSV or JSON files.
The pointed to object may also be a Babel block, which when executed
returns an Org table. Parameters may be passed to the Babel block
in parenthesis.
A slicing may be applied to the table, to select rows or columns.
The syntax for slicing is like [1:3] or [1:3,2:5].
Return it as a Lisp list of lists.
An horizontal line is translated as the special symbol `hline'."
  (unless (stringp name-or-id)
    (setq name-or-id (format "%s" name-or-id)))
  (unless
      (string-match
       (rx
        bos
        (* space)
        (? (group-n 1 (* (notany ":"))) ":")
        (* space)
        (   group-n 2 (* (notany "[]():")))
        (* space)
        (? (group-n 3 "(" (* any) ")"))
        (* space)
        (? (group-n 4 "[" (* any) "]"))
        (* space)
        eos)
       name-or-id)
    (user-error "Malformed table reference %S" name-or-id))
  (let ((file   (match-string 1 name-or-id))
        (name   (match-string 2 name-or-id))
        (params (match-string 3 name-or-id))
        (slice  (match-string 4 name-or-id)))
    (if (eq (length file) 0)
        (setq file nil))
    (if (eq (length name) 0)
        (setq name nil))
    (unless (or file name)
      (user-error "Malformed table reference %S" name-or-id))
    (let
        ((table
          (cond
           ;; name-or-id = "file:(csv …)"
           ((and file (not name)
                 (string-match (rx bos "(csv") params))
            (orgtbl-join--table-from-csv file params))
           ;; name-or-id = "file:(json …)"
           ((and file (not name)
                 (string-match (rx bos "(json") params))
            (orgtbl-join--table-from-json file params))
           ;; name-or-id = "babel(p=…)" or "file:babel(p=…)"
           ((and params
                 (orgtbl-join--table-from-babel
                  (if file
                      (format "%s:%s%s" file name params)
                    (format "%s%s" name params)))))
           ;;name-or-id = "table" or "file:table"
           ((orgtbl-join--table-from-name file name))
           ;; name-or-id = "babel" or "file:babel"
           ((orgtbl-join--table-from-babel
             (if file
                 (format "%s:%s" file name)
               name)))
           ;; name-or-id = "34cbc63a-c664-471e-a620-d654b26ffa31"
           ;; pointing to a header in a distant org file, followed by a table
           ((and (not file) name (not params)
                 (orgtbl-join--table-from-id name)))
           ;; everything failed
           (t
            (user-error
             "Cannot find table or babel block with reference %S"
             name-or-id)))))
      (if slice
          (org-babel-ref-index-list slice table)
        table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(defun orgtbl-join--split-string-with-quotes (string)
  "Like (split-string STRING), but with quote protection.
Single and double quotes protect space characters,
and also single quotes protect double quotes
and the other way around."
  (let ((l (length string))
	(start 0)
	(result (orgtbl-join--list-create)))
    (save-match-data
      (while (and (< start l)
		  (string-match
                   (rx
                    (* blank)
                    (group (+ (quotedcolname (notany " '\"")))))
		   string start))
	(orgtbl-join--list-append result (match-string 1 string))
	(setq start (match-end 1))))
    (orgtbl-join--list-get result)))

(defun orgtbl-join--colname-to-int (colname table &optional err)
  "Convert the COLNAME into an integer.
COLNAME is a column name of TABLE.
The first column is numbered 1.
COLNAME may be:
- a dollar form, like $5 which is converted to 5
- an alphanumeric name which appears in the column header (if any)
- the special symbol `hline' which is converted into 0
If COLNAME is quoted (single or double quotes),
quotes are removed beforhand.
When COLNAME does not match any actual column,
an error is generated if ERR optional parameter is true
otherwise nil is returned."
  (if (symbolp colname)
      (setq colname (symbol-name colname)))
  (if (string-match
       (rx
	bos
	(or
	 (seq ?'  (group-n 1 (* (not (any ?' )))) ?' )
	 (seq ?\" (group-n 1 (* (not (any ?\")))) ?\"))
	eos)
       colname)
      (setq colname (match-string 1 colname)))
  ;; skip first hlines if any
  (orgtbl-join--pop-leading-hline table)
  (cond ((string= colname "")
	 (and err (user-error "Empty column name")))
	((string= colname "hline")
	 0)
	((string-match (rx bos "$" (group (+ (any "0-9"))) eos) colname)
	 (let ((n (string-to-number (match-string 1 colname))))
	   (if (<= n (length (car table)))
	       n
	     (if err
		 (user-error "Column %s outside table" colname)))))
	((and
          (memq 'hline table)
	  (cl-loop
	   for h in (car table)
	   for i from 1
	   thereis (and (string= h colname) i))))
        (err
	 (user-error "Column %s not found in table" colname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp table to string conversion

(defun orgtbl-join--insert-make-spaces (n spaces-cache)
  "Make a string of N spaces.
Caches results into SPACES-CACHE to avoid re-allocating
again and again the same string."
  (if (< n (length spaces-cache))
      (or (aref spaces-cache n)
	  (aset spaces-cache n (make-string n ? )))
    (make-string n ? )))

;; Time optimization: surprisingly,
;; (insert (concat a b c)) is faster than
;; (insert a b c)
;; Therefore, we build a the Org Mode representation of a table
;; as list of strings which get concatenated into a huge string.
;; This is faster and less garbage-collector intensive than
;; inserting bits one at a time in a buffer.
;;
;; benches:
;; insert a large 3822 rows × 16 columns table
;; - one row at a time or as a whole
;; - with or without undo active
;; repeat 10 times
;;
;; with undo, one row at a time
;;  (3.587732240 40 2.437140552)
;;  (3.474445440 39 2.341087725)
;;
;; without undo, one row at a time
;;  (3.127574093 33 2.001691096)
;;  (3.238456106 33 2.089536034)
;;
;; with undo, single huge string
;;  (3.030763545 30 1.842303196)
;;  (3.012367879 30 1.841319998)
;;
;; without undo, single huge string
;;  (2.499138596 21 1.419285666)
;;  (2.403039955 21 1.338347655)
;;       ▲       ▲      ▲
;;       │       │      ╰──╴CPU time for GC
;;       │       ╰─────────╴number of GC
;;       ╰─────────────────╴overall CPU time

(defun orgtbl-join--elisp-table-to-string (table)
  "Convert TABLE to a string formatted as an Org Mode table.
TABLE is a list of lists of cells.  The list may contain the
special symbol `hline' to mean an horizontal line."
  (let* ((nbcols (cl-loop
		  for row in table
		  maximize (if (listp row) (length row) 0)))
	 (maxwidths  (make-list nbcols 1))
	 (numbers    (make-list nbcols 0))
	 (non-empty  (make-list nbcols 0))
	 (spaces-cache (make-vector 100 nil)))

    ;; compute maxwidths
    (cl-loop for row in table
	     do
	     (cl-loop for cell on row
		      for mx on maxwidths
		      for nu on numbers
		      for ne on non-empty
		      for cellnp = (car cell)
		      do (cond ((not cellnp)
				(setcar cell (setq cellnp "")))
		       	       ((not (stringp cellnp))
		      		(setcar cell (setq cellnp (format "%s" cellnp)))))
		      if (string-match-p org-table-number-regexp cellnp)
		      do (setcar nu (1+ (car nu)))
		      unless (equal cellnp "")
		      do (setcar ne (1+ (car ne)))
		      if (< (car mx) (string-width cellnp))
		      do (setcar mx (string-width cellnp))))

    ;; change meaning of numbers from quantity of cells with numbers
    ;; to flags saying whether alignment should be left (number alignment)
    (cl-loop for nu on numbers
	     for ne in non-empty
	     do
	     (setcar nu (< (car nu) (* org-table-number-fraction ne))))

    ;; creage well padded and aligned cells
    (let ((bits (orgtbl-join--list-create)))
      (cl-loop for row in table
	       do
	       (if (listp row)
		   (cl-loop for cell in row
			    for mx in maxwidths
			    for nu in numbers
			    for pad = (- mx (string-width cell))
                            do
			    (orgtbl-join--list-append bits "| ")
			    (cond
			     ;; no alignment
                             ((<= pad 0)
			      (orgtbl-join--list-append bits cell))
			     ;; left alignment
			     (nu
			      (orgtbl-join--list-append bits cell)
                              (orgtbl-join--list-append
                               bits
                               (orgtbl-join--insert-make-spaces pad spaces-cache)))
			     ;; right alignment
                             (t
			      (orgtbl-join--list-append
                               bits
                               (orgtbl-join--insert-make-spaces pad spaces-cache))
			      (orgtbl-join--list-append bits cell)))
			    (orgtbl-join--list-append bits " "))
		 (cl-loop for bar = "|" then "+"
			  for mx in maxwidths
                          do
			  (orgtbl-join--list-append bits bar)
			  (orgtbl-join--list-append bits (make-string (+ mx 2) ?-))))
	       (orgtbl-join--list-append bits "|\n"))
      ;; remove the last \n because Org Mode re-adds it
      (setcar (car bits) "|")
      (mapconcat #'identity (orgtbl-join--list-get bits) ""))))

(defun orgtbl-join--insert-elisp-table (table)
  "Insert TABLE in current buffer at point.
TABLE is a list of lists of cells.  The list may contain the
special symbol `hline' to mean an horizontal line."
  ;; inactivating jit-lock-after-change boosts performance a lot
  (cl-letf (((symbol-function 'jit-lock-after-change) (lambda (_a _b _c)) ))
    (insert (orgtbl-join--elisp-table-to-string table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Org Table Join package really begins here

;; The *this* variable is accessible to the user.
;; It refers to the joined table before it is "printed"
;; into the buffer, so that it can be post-processed.
(defvar *this*)

(defun orgtbl-join--post-process (table post)
  "Post-process the joined TABLE according to the :post header.
POST might be:
- a reference to a babel-block, for example:
  :post \"myprocessor(inputtable=*this*)\"
  and somewhere else:
  #+name: myprocessor
  #+begin_src language :var inputtable=
  ...
  #+end_src
- a Lisp lambda with one parameter, for example:
  :post (lambda (table) (append table \\'(hline (\"total\" 123))))
- a Lisp function with one parameter, for example:
  :post my-lisp-function
- a Lisp expression which will be evaluated
  the *this* variable will contain the TABLE
In all those cases, the result must be a Lisp value compliant
with an Org Mode table."
  (cond
   ((null post) table)
   ((functionp post)
    (apply post table ()))
   ((stringp post)
    (let ((*this* table))
      (condition-case err
	  (org-babel-ref-resolve post)
	(error
	 (message "error: %S" err)
	 (orgtbl-join--post-process table (read post))))))
   ((listp post)
    (let ((*this* table))
      (eval post)))
   (t (user-error ":post %S header could not be understood" post))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wizard

(defun orgtbl-join--display-help (explain &rest args)
  "Display help for each field the wizard queries."
  (with-current-buffer "*orgtbl-join-help*"
    (erase-buffer)
    (insert (apply #'format explain args))
    (goto-char (point-min))))

;; This variable contains the history of user entered answers,
;; so that they can be entered again or edited.
(defvar orgtbl-join-history-cols ())

(defun orgtbl-join--join-query-column
    (help prompt table default allcolumns &optional keepanswer)
  "Interactively query a column.
HELP & PROMPT are displayed to the user to explain what answer is expected.
TABLE is the Org Mode table from which a column will be choosen
by the user.  Its header is used for column names completion.  If
TABLE has no header, completion is done on generic column names:
$1, $2...
DEFAULT is a proposed column name.
ALLCOLUMNS is an accumulation list of all columns seen through all
invocations of this function.
KEEPANSWER should be true to keep the user's answer into ALLCOLUMNS."
  (orgtbl-join--pop-leading-hline table)
  (let ((completions
	 (if (memq 'hline table) ;; table has a header
	     (car table)
	   (cl-loop ;; table does not have a header
	    for _row in (car table)
	    for i from 1
	    collect (format "$%s" i)))))
    (orgtbl-join--display-help
     help
     (mapconcat (lambda (x) (format " ~%s~" x)) completions))
    (let ((answer
           (completing-read
            prompt
            completions
            nil 'confirm
            (and (member default completions) default))))
      (setcdr allcolumns
              (append
               (cdr allcolumns)
               (if keepanswer
                   completions
                 (delete answer completions))))
      answer)))

(defun orgtbl-join--wizard-query-table (table typeoftable)
  "Query the 3 fields composing a generalized table: file:name:slice.
If TABLE is not nil, it is decomposed into file:name:slice, and each
of those 3 fields serve as default answer when prompting.
TYPEOFTABLE is a qualifier: t for master, nil for reference."
  (let (file slice tablenames)
    (when (and
           table
           (string-match
            (rx bos
                (? (group (+ (not (any ":[]")))) ":")
                (group (+ (not "[")))
                (?  (group "[" (* any) "]"))
                eos)
            table))
      (setq file  (match-string 1 table))
      (setq slice (match-string 3 table))
      (setq table (match-string 2 table)))

    (orgtbl-join--display-help
     (if typeoftable
         "* In which file is the master table?
The master table may be in another file.
The master table is the one we want to enrich with material from other tables.
Leave answer empty to mean that the table is in the current buffer."
       "* In which file is the reference table?
A reference table is a storage of knowledge where orgtbl-join will pick
selected rows. The flow of information is from the reference tables to the
master table to be enriched.
Leave answer empty to mean that the table is in the current buffer."))
    (let ((insert-default-directory nil))
      (setq file
            (read-file-name "File (RET for current buffer): "
                            nil
                            nil
                            nil
                            file)))

    (if (equal file "") (setq file nil))

    (setq tablenames (orgtbl-join--list-local-tables file))
    (if file
        (setq tablenames (nconc tablenames '("(csv)" "(csv header)" "(json)"))))

    (and
     file
     (not table)
     (cond
      ((string-match (rx ".csv"  eos) file)
       (setq table "(csv)"))
      ((string-match (rx ".json" eos) file)
       (setq table "(json)"))))

    (orgtbl-join--display-help
     "* The input %s table may be:
- a regular Org table,
- a Babel block whose output will be the input table,
- a ~CSV~ or ~JSON~ formatted file.
Org table & Babel block names are available at completion (type ~TAB~).
Alternately, it may be an Org ID pointing to a table or Babel block
  (no completion).
For a Babel block, the name of the Babel may be followed by
  parameters in parenthesis. Example: ~mybabel(p=\"a\",quty=12)~
for a ~CSV~ table, type ~(csv params…)~
  currently only ~(cvs header)~ is recognized: first row is a header
for a ~JSON~ table, type ~(json params…)~
  currently no parameters are recognized."
     (if typeoftable "master" "reference"))
    (setq table
          (completing-read
           "Table, Babel, ID, (csv…), (json…): "
           tablenames
           nil
           nil ;; user is free to input anything
           table))

    (unless (string-match-p (rx bos (* space) eos) table)
      (orgtbl-join--display-help "* Slicing
Slicing is an Org Mode feature allowing to cut the input table.
It applies to any input: Org table, Babel output, CSV, JSON.
Leave empty for no slicing.
** Examples:
- ~mytable[0:5]~     retains only the first 6 rows of the input table
- ~mytable[*,0:1]~   retains only the first 2 columns
- ~mytable[0:5,0:1]~ retains 5 rows and 2 columns")
      (setq slice
            (read-string
             "Input slicing (optional): "
             slice
             'orgtbl-join-history-cols))

      (concat
       (or file "")
       (if file ":" "")
       table
       (or slice "")))))

(defun orgtbl-join--wizard-create-update (mastable params)
  "Interactively query tables and joining columns.
PARAMS is a plist (possibly empty) where user answers accumulate.
The updated PARAMS is returned."
  (let ((mascol (orgtbl-join--plist-get-remove params :mas-column))
        (reftable)
        (refcol)
        (full)
        (allcolumns (list nil))
        (newparams))
    (save-window-excursion
      (save-selected-window
        (split-window nil 15 'above)
        (switch-to-buffer "*orgtbl-join-help*")
        (org-mode))
      (unless mastable
        (setq mastable
              (orgtbl-join--wizard-query-table
               (orgtbl-join--plist-get-remove params :mas-table)
               t))
        (setq newparams `(,@newparams ,:mas-table ,mastable))
        (setq mastable
              (orgtbl-join-table-from-any-ref mastable)))

      (cl-loop
       do
       (setq reftable
             (orgtbl-join--wizard-query-table
              (orgtbl-join--plist-get-remove params :ref-table)
              nil))
       (setq mascol
	     (orgtbl-join--join-query-column
              "* Which master column?
One of the columns in the master table will be used to search
for a selection of matching rows in the reference table.
Candidates are:
  %s"
	      "Master column: "
	      mastable
              (or
               (orgtbl-join--plist-get-remove params :mas-column)
	       mascol)
              allcolumns
              t))
       (setq refcol
	     (orgtbl-join--join-query-column
              "* Which reference column?
One of the columns in the reference table will be matched
in order to collect some rows and add them to the master table.
Candidates are:
  %s"
	      "Reference column: "
	      (orgtbl-join-table-from-any-ref reftable)
	      (or
               (orgtbl-join--plist-get-remove params :ref-column)
               mascol)
              allcolumns))
       (orgtbl-join--display-help "* Which table should be kept entirely?
Possible answers are:
- =mas=: the master table is kept entirely.
  This is the standard case. Only matching rows are picked from
  the reference tables.
- =ref=: the reference table is kept entirely.
  This option should probably never be used.
- =mas+ref=: master & reference tables are kept entirely.
  Useful in cases where both tables are somehow symmetric.
  Missing rows from one or the other table will be present in the result
  with some empty cells.
- =none=: only matching rows will be kept.
  the resulting enriched table may therefore be shorter than both tables.")
       (setq full
	     (completing-read
	      "Which table should appear entirely? "
	      '("mas" "ref" "mas+ref" "none")
	      nil nil
              (or
               (orgtbl-join--plist-get-remove params :full)
               full
               "mas")))
       (setq newparams
             `(,@newparams
               ,:ref-table ,reftable
               ,:mas-column ,mascol
               ,:ref-column ,refcol
               ,:full ,full))
       (orgtbl-join--display-help
        "* Another reference table?
Up to now, the reference tables used in the joining are:
%s"
        (cl-loop
         for pair on newparams
         if (eq (car pair) :ref-table)
         concat (format " ~%s~" (cadr pair))
         do (setq pair (cdr pair))))
       while
       (y-or-n-p "Another reference table? "))

      (pop allcolumns)

      (when nil ;; not active
        ;; The :cols parameter is not yet ready for interactive query.
        ;; This is because there are ambiguities with $1 $2 $3 names.
        ;; Do they refer to columns in the master table or in any of
        ;; the references tables?
        ;; There also might be duplicate column names which create ambiguity.
        (orgtbl-join--display-help
         "* Columns re-arrangement
The natural ordering of columns puts the master columns first,
then each of the reference columns.
This order may be changed by specifying the list of output columns.
Columns may also be ignored by this way.
Candidates are:
  %s"
         (mapconcat
          (lambda (x) (format " ~%s~" x))
          allcolumns))
        (let ((cols
               (read-string
                "(Optional) specify output columns: "
                (or
                 (orgtbl-join--plist-get-remove params :cols)
                 (mapconcat #'identity allcolumns " "))
                'orgtbl-join-history-cols)))
          (unless (string-match-p (rx bos (* space) eos) cols)
            (setq newparams
                  `(,@newparams
                    :cols
                    ,cols)))))
      )

    ;; recover parameters not taken into account by the wizard
    (cl-loop
     for pair on params
     if (car pair)
     do (nconc newparams `(,(car pair) ,(cadr pair)))
     do (setq pair (cdr pair)))
    `(:name "join" ,@newparams)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backend engine

(defun orgtbl-join--join-append-mas-ref-row (masrow refrow refcol)
  "Concatenate master and reference rows, skiping the reference column.
MASROW is a list of cells from the master table.  REFROW is a
list of cells from the reference table.  REFCOL is the position,
numbered from zero, of the column in REFROW that should not be
appended in the result, because it is already present in MASROW."
  (let ((result (orgtbl-join--list-create)))
    (cl-loop for cell in masrow
	     do (orgtbl-join--list-append result cell))
    (cl-loop for cell in refrow
	     for i from 0
	     unless (equal i refcol)
	     do (orgtbl-join--list-append result cell))
    (orgtbl-join--list-get result)))

(defun orgtbl-join--to-string (x)
  "Convert X to a string,
be it a number, a symbol, or a string (unchanged)."
  (cond
   ((stringp x) x)
   ((symbolp x) (symbol-name x))
   ((numberp x) (number-to-string x))
   (t (format "%s" x))))

(defun orgtbl-join--create-table-joined (mastable mascol reftable refcol full)
  "Join a master table with a reference table.
MASTABLE is the master table, as a list of lists of cells.
MASCOL is the name of the joining column in the master table.
REFTABLE is the reference table.
REFCOL is the name of the joining column in the reference table.
FULL is a flag to specify whether or not tables should be fully extracted
to the result:
if it contains \"mas\" then the master    table will appear entirely
if it contains \"ref\" then the reference table will appear entirely
if it contains both (like \"mas+ref\") then both table will appear
entirely.
COLS is the list of columns that must appear in the result
if COLS is nil, all columns appear in the result
Returns MASTABLE enriched with material from REFTABLE."
  (unless full (setq full "mas")) ;; default value is "mas"
  (unless (stringp full)
    (setq full (format "%s" full)))
  (let ((result (orgtbl-join--list-create))
	(width
	 (cl-loop for row in mastable
		  maximize (if (listp row) (length row) 0)))
	(refhead)
	(refbody)
	(refhash (make-hash-table :test 'equal)))
    ;; make master table rectangular if not all rows
    ;; share the same number of cells
    (cl-loop for row on mastable
	     if (listp (car row)) do
	     (let ((n (- width (length (car row)))))
	       (if (> n 0)
		   (setcar
		    row
		    (nconc (car row) (make-list n ""))))))
    ;; skip any hline at the top of both tables
    (while (eq (car mastable) 'hline)
      (orgtbl-join--list-append result 'hline)
      (orgtbl-join--pop-simple mastable))
    (orgtbl-join--pop-leading-hline reftable)
    ;; convert column-names to numbers
    (setq mascol (1- (orgtbl-join--colname-to-int mascol mastable t)))
    (setq refcol (1- (orgtbl-join--colname-to-int refcol reftable t)))
    ;; split header and body
    (setq refbody (memq 'hline reftable))
    (if (not refbody)
	(setq refbody reftable)
      (setq refhead reftable)
      ;; terminate header with nil
      (cl-loop for h on reftable
	       until (eq (cadr h) 'hline)
	       finally (setcdr h nil)))
    ;; convert reference table into fast-lookup hashtable
    ;; an entry in the hastable for a key KEY is:
    ;; (0 row1 row2 row3)
    ;; where row1, row2, row3 are rows in the reftable with the KEY column
    ;; the leading 0 will be how many times this particular
    ;; hashtable entry has been consumed
    (cl-loop for row in refbody
	     if (listp row) do
	     (let* ((key (orgtbl-join--to-string (nth refcol row)))
		    (hashentry (gethash key refhash)))
	       (if hashentry
		   (nconc hashentry (list row))
		 (puthash key (list 0 row) refhash))))
    ;; iterate over master table header if any
    ;; and join it with reference table header if any
    (if (memq 'hline mastable)
	(while (listp (car mastable))
	  (orgtbl-join--list-append
	   result
	   (orgtbl-join--join-append-mas-ref-row
	    (car mastable)
	    (car refhead) ;; nil if refhead is nil
	    refcol))
	  (orgtbl-join--pop-simple mastable)
	  (if refhead (orgtbl-join--pop-simple refhead))))
    ;; create the joined table
    (cl-loop
     with full-mas = (string-match "mas" full)
     for masrow in mastable
     do
     (if (not (listp masrow))
	 (orgtbl-join--list-append result masrow)
       (let ((hashentry
              (gethash (orgtbl-join--to-string (nth mascol masrow)) refhash)))
	 (if (not hashentry)
	     ;; if no ref-line matches, add the non-matching master-line anyway
	     (if full-mas (orgtbl-join--list-append result masrow ))
	   ;; if several ref-lines match, all of them are considered
	   (cl-loop
	    for refrow in (cdr hashentry)
	    do
	    (orgtbl-join--list-append
	     result
	     (orgtbl-join--join-append-mas-ref-row masrow refrow refcol)))
	   ;; ref-table rows were consumed, increment counter
	   (setcar hashentry (1+ (car hashentry)))))))
    ;; add rows from the ref-table not consumed
    (if (string-match "ref" full)
	(cl-loop
	 for refrow in refbody
	 if (listp refrow) do
	 (let ((hashentry
                (gethash (orgtbl-join--to-string (nth refcol refrow)) refhash)))
	   (if (equal (car hashentry) 0)
	       (let ((fake-masrow (make-list width "")))
		 (setcar (nthcdr mascol fake-masrow)
			 (nth refcol (cadr hashentry)))
		 (orgtbl-join--list-append
		  result
		  (orgtbl-join--join-append-mas-ref-row
		   fake-masrow
		   refrow
		   refcol)))))
	 else do
	 (orgtbl-join--list-append result 'hline)))
    (setq result (orgtbl-join--list-get result))
    result))

(defun orgtbl-join--join-rearrange-columns (table cols)
  "Rearrange the joined TABLE to select columns.
COLS contains a user-specified list of columns as given
in the :cols parameter.  This function rearranges
TABLE so that it contains only COLS, in the same order."
  (if (stringp cols)
      (setq cols (orgtbl-join--split-string-with-quotes cols)))
  (setq cols
	(cl-loop
	 for c in cols
	 collect (1- (orgtbl-join--colname-to-int c table t))))
  (cl-loop
   for rrow on table
   for row = (car rrow)
   if (listp row)
   do (setcar
       rrow
       (cl-loop
	for c in cols
	collect (nth c row))))
  table)

(defun orgtbl-join--join-all-ref-tables (mas-table params)
  "Repeatedly join reference tables found in PARAMS to MAS-TABLE.
Destructively modify PARAMS."
  (let (ref-table ref-column mas-column full)
    (while (setq ref-table (orgtbl-join--plist-get-remove params :ref-table))
      (unless (setq
               ref-column
               (orgtbl-join--plist-get-remove params :ref-column))
        (user-error "Missing :ref-column for :ref-table %s" ref-table))
      (unless (setq
               mas-column
               (or
                (orgtbl-join--plist-get-remove params :mas-column)
                mas-column))
        (user-error "Missing :mas-column for :ref-table %s" ref-table))
      (setq
       full
       (or (orgtbl-join--plist-get-remove params :full)
           full))
      (setq
       mas-table
       (orgtbl-join--create-table-joined
        mas-table
        mas-column
        (orgtbl-join-table-from-any-ref ref-table)
        ref-column
        full)))
    (if (setq ref-column (plist-get params :ref-column))
        (user-error "Missing :ref-table for :ref-column %s" ref-column))
    (if (setq mas-column (plist-get params :mas-column))
        (user-error "Missing :ref-table for :mas-column %s" mas-column))
    (if (setq full (plist-get params :full))
        (user-error "Missing :ref-table for :full %s" full)))
  (let ((cols (plist-get params :cols)))
    (if cols (orgtbl-join--join-rearrange-columns mas-table cols)))
  (orgtbl-join--post-process mas-table (plist-get params :post)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IN-PLACE mode

;;;###autoload
(defun orgtbl-join (&optional params)
  "Add material from a reference table to the current table.

Optional PARAMS gives reference tables information.
If it is nil, then this information is queried interactively.

Rows from the reference table are appended to rows of the current
table.  For each row of the current table, matching rows from the
reference table are searched and appended.  The matching is
performed by testing for equality of cells in the current column,
and a joining column in the reference table.

If a row in the current table matches several rows in the
reference table, then the current row is duplicated and each copy
is appended with a different reference row.

If no matching row is found in the reference table, then the
current row is kept, with empty cells appended to it."
  (interactive)
  (org-table-check-inside-data-field)
  (let ((col (org-table-current-column))
	(tbl (orgtbl-join--table-to-lisp))
	(pt (line-number-at-pos))
	(cn (- (point) (line-beginning-position))) )
    (unless params
      (setq params
            (list
             :mas-column
             (if (memq 'hline tbl)
                 (nth (1- col) (car tbl))
               (format "$%s" col))))
      (setq params (orgtbl-join--wizard-create-update tbl params)))
    (let ((b (org-table-begin))
	  (e (org-table-end)))
      (save-excursion
	(goto-char e)
	(orgtbl-join--insert-elisp-table
         (orgtbl-join--join-all-ref-tables tbl params))
        (insert "\n")
        (delete-region b e)))
    (goto-char (point-min))
    (forward-line (1- pt))
    (forward-char cn)))

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
table has a header, or a dollar form: $1, $2, and so on.

The destination must be specified somewhere in the
same file with a bloc like this:
#+BEGIN RECEIVE ORGTBL destination_table_name
#+END RECEIVE ORGTBL destination_table_name

Note:
 The name `orgtbl-to-joined-table' follows the Org Mode standard
 with functions like `orgtbl-to-csv', `orgtbl-to-html'..."
  (interactive)
  (orgtbl-join--elisp-table-to-string
   (orgtbl-join--join-all-ref-tables table params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PULL mode

(defun orgtbl-join--table-recalculate (content formula)
  "Wrapper arround `org-table-recalculate'.
The computed table may have formulas which need to be recomputed.
This function adds a #+TBLFM: line at the end of the table.
It merges old formulas (if any) contained in CONTENT,
with new formulas (if any) given in the `formula' directive.
The standard `org-table-recalculate' function is slow because
it must handle lots of cases. Here the table is freshely created,
therefore a lot of special handling and cache updates can be
safely bypassed. Moreover, the alignment of the resulting table
is delegated to orgtbl-join, which is fast.
The result is a speedup up to x6, and a memory consumption
divided by up to 5. It makes a difference for large tables."
  (let ((tblfm
         ;; Was there already a #+tblfm: line ? Recover it.
         (and content
	      (let ((case-fold-search t))
	        (string-match
	         (rx bol
                     (* (any " \t"))
                     (group "#+tblfm:" (* not-newline)))
	         content))
              (match-string 1 content))))
    (if (stringp formula)
        ;; There is a :formula directive. Add it if not already there
        (if tblfm
	    (unless (string-match (rx-to-string formula) tblfm)
	      (setq tblfm (format "%s::%s" tblfm formula)))
	  (setq tblfm (format "#+TBLFM: %s" formula))))

    (when tblfm
      ;; There are formulas. They need to be evaluated.
      (end-of-line)
      (insert "\n" tblfm)
      (forward-line -1)

      (let ((old (symbol-function 'org-table-goto-column)))
        (cl-letf (((symbol-function 'org-fold-core--fix-folded-region)
                   (lambda (_a _b _c)))
                  ((symbol-function 'jit-lock-after-change)
                   (lambda (_a _b _c)))
                  ;; Warning: this org-table-goto-column trick fixes a bug
                  ;; in org-table.el around line 3084, when computing
                  ;; column-count. The bug prevents single-cell formulas
                  ;; creating the cell in some rare cases.
                  ((symbol-function 'org-table-goto-column)
                   (lambda (n &optional on-delim _force)
                     ;;                            △
                     ;;╭───────────────────────────╯
                     ;;╰╴parameter is forcibly changed to t╶─╮
                     ;;                      ╭───────────────╯
                     ;;                      ▽
                     (funcall old n on-delim t))))
          (condition-case nil
              (org-table-recalculate t t)
            ;;                       △ △
            ;; for all lines╶────────╯ │
            ;; do not re-align╶────────╯
            (args-out-of-range nil))))

      ;; Realign table after org-table-recalculate have changed or added
      ;; some cells. It is way faster to re-read and re-write the table
      ;; through orgtbl-join routines than letting org-mode do the job.
      (let* ((table (orgtbl-join--table-to-lisp))
             (width
              (cl-loop for row in table
                       if (consp row)
                       maximize (length row))))
        (cl-loop
         for row in table
         if (and (consp row) (< (length row) width))
         do (nconc row (make-list (- width (length row)) nil)))
        (delete-region (org-table-begin) (org-table-end))
        (insert (orgtbl-join--elisp-table-to-string table) "\n")))))

(defun orgtbl-join--parse-header-arguments ()
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (if (string-match
         (rx bos (* blank) "#+begin:" (* blank) "join")
         line)
        (cdr (org-babel-parse-header-arguments line t)))))

;;;###autoload
(defun orgtbl-join-insert-dblock-join ()
  "Wizard to interactively insert a joined table as a dynamic block."
  (interactive)
  (let* ((oldline (flatten-list (orgtbl-join--parse-header-arguments)))
         (params (orgtbl-join--wizard-create-update nil oldline)))
    (when oldline
      (org-mark-element)
      (delete-region (region-beginning) (1- (region-end))))
    (org-create-dblock params)
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
table has a header, or a dollar form: $1, $2, and so on.

The
#+BEGIN RECEIVE ORGTBL destination_table_name
#+END RECEIVE ORGTBL destination_table_name"
  (interactive)
  (let ((formula (plist-get params :formula))
	(content (plist-get params :content)))
    (if (and content
	     (let ((case-fold-search t))
	       (string-match
		(rx bos
                    (+
                     (* (any " \t")) "#+" (* not-newline) "\n"))
		content)))
	(insert (match-string 0 content)))
    (orgtbl-join--insert-elisp-table
     (orgtbl-join--join-all-ref-tables
      (orgtbl-join-table-from-any-ref (plist-get params :mas-table))
      params))
    (orgtbl-join--table-recalculate content formula)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings, menu, wizard

;;;###autoload
(defun orgtbl-join-setup-keybindings ()
  "Setup key binding and menu entry.
This function can be called in your .emacs.  It will add the
\\<org-tbl-menu> & \\[orgtbl-join] key binding for calling the
`orgtbl-join' wizard,
and a menu entry under Tbl > Column > Join with other tables."

  (declare (obsolete "Look at
https://github.com/tbanel/orgtbljoin/blob/master/README.org
for how to make the key binding and menu binding in .emacs"
                     "[2023-01-21 Sat]"))

  (message "Function orgtbl-join-setup-keybindings is obsolete
as of [2023-01-21 Sat]. See:
https://github.com/tbanel/orgtbljoin/blob/master/README.org
or put this in your .emacs:
(use-package orgtbl-join
  :after (org)
  :bind (\"C-c j\" . orgtbl-join)
  :init
  (easy-menu-add-item
   org-tbl-menu '(\"Column\")
   [\"Join with other tables\" orgtbl-join (org-at-table-p)]))")

  (eval-after-load 'org
    '(progn
       (org-defkey org-mode-map "\C-c\C-xj" 'orgtbl-join)
       (easy-menu-add-item
	org-tbl-menu '("Column")
	["Join with other tables" orgtbl-join t]))))

;; Insert a dynamic bloc with the C-c C-x x dispatcher
;;;###autoload
(eval-after-load 'org
  '(when (fboundp 'org-dynamic-block-define)
     (org-dynamic-block-define "join" #'orgtbl-join-insert-dblock-join)))

(provide 'orgtbl-join)
;;; orgtbl-join.el ends here
