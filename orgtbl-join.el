;;; orgtbl-join.el --- Join columns from other Org Mode tables -*- lexical-binding: t;-*-

;; Copyright (C) 2014-2026  Thierry Banel

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
(require 'org-id)
(eval-when-compile (require 'cl-lib))
(require 'rx)
(require 'json)

;;; Code:

;; [bazilo synchronize orgtbl-αggregate & orgtbl-joιn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; creating long lists in the right order may be done
;; - by (nconc)  but behavior is quadratic
;; - by (cons) (nreverse)
;; a third way involves keeping track of the last cons of the growing list
;; a cons at the head of the list is used for housekeeping
;; the actual list is (cdr ls)
;;
;; A list with 4 elements:
;; ╭─┬─╮ ╭────┬─╮ ╭────┬─╮ ╭────┬─╮ ╭────┬─╮
;; │◦│◦┼▶┤val1│◦┼▶┤val2│◦┼▶┤val3│◦┼▶┤val4│◦┼▶╴nil
;; ╰┼┴─╯ ╰────┴─╯ ╰────┴─╯ ╰────┴─╯ ╰─┬──┴─╯
;;  │                                 ▲
;;  ╰─────────────────────────────────╯
;;
;; A newly created, empty list
;; ╭─┬─╮
;; │◦│◦┼▶─nil
;; ╰┼┴┬╯
;;  │ ▲
;;  ╰─╯

(eval-when-compile

  (defmacro orgtbl-join--list-create ()
    "Create an appendable list."
    `(let ((x (cons nil nil)))
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
  )

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
;; is made of the same technology as `orgtbl-join--table-to-lisp'
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

  ;; match a column name not protected by quotes
  (rx-define nakedname
    (+ (any "$._#@" word)))
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

(defun orgtbl-join--block-from-name (file name)
  "Parse an Org table named NAME in a distant Org file named FILE.
FILE is a filename with possible relative or absolute path.
If FILE is nil, look in the current buffer."
  (with-current-buffer
      (if file
          (find-file-noselect file)
        (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (if (re-search-forward
             (rx ;; a single regexp :)
              tblname (literal name) (* blank) "\n"
              (0+ blank) "#+begin" (0+ any) "\n"
              (group (*? (or any "\n")))
              bol (* space) "#+end")
             nil t)
            (match-string-no-properties 1))))))

(defun orgtbl-join--table-from-csv (file name params)
  "Parse a CSV formatted table located in FILE.
If NAME is nil, then FILE is supposed to contain just one CSV table.
If NAME is given, it is supposed to be an Org block name which contains
a CSV table.
The cell-separator is currently guessed.
Currently, there is no header."
  (let ((header) (colnames) (block))
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
    (if name
        (setq block (orgtbl-join--block-from-name file name)))
    (with-temp-buffer
      (if name
          (insert block)
        (insert-file-contents file))
      (orgtbl-join--csv-to-lisp header colnames))))

(defun orgtbl-join--table-from-json (file name _params)
  "Parse a JSON formatted table located in FILE.
FILE is a filename with possible relative or absolute path.
Currently, the accepted format is
[[\"COL1\",\"COL2\",…]
 \"hline\"
 [\"VAL1\",\"VAL2\",…]
 {\"key1\":\"val1\", \"key2\":\"val2\",…},
 …
]
Numbers do not need to be quoted.
Horizontal lines may be: \"hline\", null, [], {}.
A mixture of vector and hash-objects is allowed.
Therfore the styles vector-of-vectors and vector-of-hash-objects
are supported.
A header containing the column names may be given as the first row,
(which must be a vector) followed by an horizontal line.
Keys not found in the header (or if there is no header), are added
to the column names."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (let ((json
           (if name
               (json-read-from-string (orgtbl-join--block-from-name file name))
             (json-read-file file)))
          (colnames ())
          (result))
      (when (and (cddr json)                ;; at least 2 rows
                 (consp (car json))         ;; first row is a vector
                 (not (consp (cadr json)))) ;; second row is an hline
        (setq colnames (car json)) ;; then first row contains column names
        (setq json (cddr json)))
      (setq
       result
       (cl-loop
        for row in json
        if (not row)                    ;; [], {}, null
        collect 'hline                  ;; are 'hline
        else if (stringp row)           ;; "symbol"
        collect (intern row)            ;; becomes 'symbol
        else if (and (consp row) (consp (car row)))
        collect                         ;; case of an hash-object
        (progn
          (cl-loop
           for icell in row
           if (and (consp icell) (not (member (car icell) colnames)))
           do (setq colnames `(,@colnames ,(car icell))))
          (let ((vec (make-list (length colnames) nil)))
            (cl-loop
             for icell in row
             do (cl-loop
                 for colname in colnames
                 for ocell on vec
                 if (equal colname (car icell))
                 do (setcar ocell (cdr icell))))
            vec))
        else                            ;; case of a vector
        collect row))
      (if colnames
          `(,colnames hline ,@result)
        result))))

(defun orgtbl-join--table-from-name (file name)
  "Parse an Org table named NAME in a distant Org file named FILE.
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

(defun orgtbl-join--nil-if-empty (field)
  (and
   field
   (not (string-match-p (rx bos (* blank) eos) field))
   field))

(defun orgtbl-join--parse-locator (locator)
  "Parse LOCATOR, a description of where to find the input table.
The result is a vector containing:
[
  FILE   ; optional file where the table/Babel/CSV/JSON may be found
  NAME   ; name of table/Babel denoted by #+name:
  ORGID  ; Org Mode id in a property drawer (exclusive with file+name)
  PARAMS ; optional parameters to pass to babel/CSV/JSON
  SLICE  ; optional slicing of the resultin table, like [0:7]
]
If LOCATOR looks like NAME(params…)[slice] or just NAME, then NAME
is searched in the Org Mode database, and if found it is interpreted
as an Org Id and put in the `orgid' field."
  (unless locator (setq locator ""))
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
       locator)
    (user-error "Malformed table reference %S" locator))
  (let ((file   (orgtbl-join--nil-if-empty (match-string 1 locator)))
        (name   (orgtbl-join--nil-if-empty (match-string 2 locator)))
        (orgid                                                           )
        (params (orgtbl-join--nil-if-empty (match-string 3 locator)))
        (slice  (orgtbl-join--nil-if-empty (match-string 4 locator))))
    (when (and
           (not file)
           (progn
             (unless org-id-locations (org-id-locations-load))
             (and org-id-locations
	          (hash-table-p org-id-locations)
	          (gethash name org-id-locations))))
      (setq orgid name)
      (setq name nil))
    (vector file name orgid params slice)))

(defun orgtbl-join--assemble-locator (file name orgid params slice)
  "Assemble fields of a locator as a string.
FILE NAME ORGID PARAMS SLICE are the 5 fields composing a locator.
Many of them are optional.
The result is a locator suitable for orgtbl-join and Org Mode."
  (unless params (setq params ""))
  (unless slice  (setq slice  ""))
  (setq file  (orgtbl-join--nil-if-empty file ))
  (setq orgid (orgtbl-join--nil-if-empty orgid))
  (cond
   (orgid (format "%s%s%s"         orgid        params slice))
   (file  (format "%s:%s%s%s" file (or name "") params slice))
   (t     (format "%s%s%s"         name         params slice))))

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
  (let*
      ((struct (orgtbl-join--parse-locator name-or-id))
       (file   (aref struct 0))
       (name   (aref struct 1))
       (orgid  (aref struct 2))
       (params (aref struct 3))
       (slice  (aref struct 4))
       (table
        (cond
         ;; name-or-id = "file:(csv …)"
         ((and params (string-match (rx bos "(csv") params))
          (orgtbl-join--table-from-csv file name params))
         ;; name-or-id = "file:(json …)"
         ((and params (string-match (rx bos "(json") params))
          (orgtbl-join--table-from-json file name params))
         ;; name-or-id = "34cbc63a-c664-471e-a620-d654b26ffa31"
         ;; pointing to a header in a distant org file, followed by a table
         (orgid
          (orgtbl-join--table-from-id orgid))
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
         ;; everything failed
         (t
          (user-error
           "Cannot find table or babel block with reference %S"
           name-or-id)))))
      (if slice
          (org-babel-ref-index-list slice table)
        table)))

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
	 (seq ?'  (group-n 1 (* (notany ?' ))) ?' )
	 (seq ?\" (group-n 1 (* (notany ?\"))) ?\"))
	eos)
       colname)
      (setq colname (match-string 1 colname)))
  ;; skip first hlines if any
  (orgtbl-join--pop-leading-hline table)
  (cond ((string= colname "")
	 (and err (user-error "Empty column name")))
	((string= colname "hline")
	 0)
	((string-match (rx bos "$" (group (+ digit)) eos) colname)
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
;; Therefore, we build the Org Mode representation of a table
;; as a list of strings which get concatenated into a huge string.
;; This is faster and less garbage-collector intensive than
;; inserting cells one at a time in a buffer.
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
		      unless (string= cellnp "")
		      do (setcar ne (1+ (car ne)))
		      if (< (car mx) (string-width cellnp))
		      do (setcar mx (string-width cellnp))))

    ;; change meaning of numbers from quantity of cells with numbers
    ;; to flags saying whether alignment should be left (number alignment)
    (cl-loop for nu on numbers
	     for ne in non-empty
	     do
	     (setcar nu (< (car nu) (* org-table-number-fraction ne))))

    ;; create well padded and aligned cells
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
      (mapconcat #'identity (orgtbl-join--list-get bits)))))

(defun orgtbl-join--insert-elisp-table (table)
  "Insert TABLE in current buffer at point.
TABLE is a list of lists of cells.  The list may contain the
special symbol `hline' to mean an horizontal line."
  ;; inactivating jit-lock-after-change boosts performance a lot
  (cl-letf (((symbol-function 'jit-lock-after-change) (lambda (_a _b _c)) ))
    (insert (orgtbl-join--elisp-table-to-string table))))

(defun orgtbl-join--cell-to-string (cell)
  "Convert CELL (a cell in the input table) to a string if it is not already."
  (cond
   ((not cell) cell)
   ((stringp cell) cell)
   ((numberp cell) (number-to-string cell))
   ((symbolp cell) (symbol-name cell))
   (t (error "cell %S is not a number neither a string" cell))))

(defun orgtbl-join--get-header-table (table &optional asstring)
  "Return the header of TABLE as a list of column names.
When ASSTRING is true, the result is a string which concatenates the
names of the columns.  TABLE may be a Lisp list of rows, or the
name or id of a distant table.  The function takes care of
possibly missing headers, and in this case returns a list
of $1, $2, $3... column names.
Actual column names which are not fully alphanumeric are quoted."
  (unless (consp table)
    (setq table
          (condition-case _err
              (orgtbl-join-table-from-any-ref table)
            (error
             '(("$1" "$2" "$3" "…") hline)))))
  (orgtbl-join--pop-leading-hline table)
  (let ((header
	 (if (memq 'hline table)
	     (cl-loop for x in (car table)
                      do (setq x (orgtbl-join--cell-to-string x))
		      collect
		      (if (string-match-p
                           (rx bos nakedname eos)
                           x)
			  x
			(format "\"%s\"" x)))
	   (cl-loop for _x in (car table)
		    for i from 1
		    collect (format "$%s" i)))))
    (if asstring
	(mapconcat #'identity header " ")
      header)))

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
         (condition-case err2
	     (orgtbl-join--post-process
              table
              (thing-at-point--read-from-whole-string post))
           (error
            (user-error
             ":post %S ends in an error
- as a Babel block: %s
- not a valid Lisp expression: %s"
             post err err2)))))))
   ((listp post)
    (let ((*this* table))
      (eval post)))
   (t (user-error ":post %S header could not be understood" post))))

(defun orgtbl-join--alist-get-remove (key alist)
  "A variant of alist-get which removes an entry once read.
ALIST is a list of pairs (key . value).
Search ALIST for a KEY. If found, replace the key in (key . value)
by nil, and return value. If nothing is found, return nil."
  (let ((x (assq key alist)))
    (when x
      (setcar x nil)
      (cdr x))))

(defun orgtbl-join--recalculate-fast ()
  "Wrapper arround `org-table-recalculate'.
The standard `org-table-recalculate' function is slow because
it must handle lots of cases. Here the table is freshely created,
therefore a lot of special handling and cache updates can be
safely bypassed. Moreover, the alignment of the resulting table
is delegated to orgtbl-join, which is fast.
The result is a speedup up to x6, and a memory consumption
divided by up to 5. It makes a difference for large tables."
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
        (args-out-of-range nil)))))

(defun orgtbl-join--table-recalculate (content formula)
  "Update the #+TBLFM: line and recompute all formulas.
The computed table may have formulas which need to be recomputed.
This function adds a #+TBLFM: line at the end of the table.
It merges old formulas (if any) contained in CONTENT,
with new formulas (if any) given in the `formula' directive."
  (let ((tblfm
         ;; Was there already a #+tblfm: line ? Recover it.
         (and content
	      (let ((case-fold-search t))
	        (string-match
	         (rx bol (* blank) (group "#+tblfm:" (* any)))
	         content))
              (match-string 1 content))))
    (if (stringp formula)
        ;; There is a :formula directive. Add it if not already there
        (if tblfm
	    (unless (string-match (regexp-quote formula) tblfm)
	      (setq tblfm (format "%s::%s" tblfm formula)))
	  (setq tblfm (format "#+TBLFM: %s" formula))))

    (when tblfm
      ;; There are formulas. They need to be evaluated.
      (end-of-line)
      (insert "\n" tblfm)
      (forward-line -1)
      (orgtbl-join--recalculate-fast)

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

;; bazilo]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Org Table Join package really begins here

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
	     (let* ((key (orgtbl-join--cell-to-string (nth refcol row)))
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
              (gethash (orgtbl-join--cell-to-string (nth mascol masrow)) refhash)))
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
                (gethash (orgtbl-join--cell-to-string (nth refcol refrow)) refhash)))
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

(defun orgtbl-join--plist-get-remove (params prop)
  "Like `plist-get', but also remove PROP from PARAMS."
  (let ((v (plist-get params prop)))
    (if v
        (setcar (memq prop params) nil))
    v))

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

;; [bazilo synchronize orgtbl-αggregate & orgtbl-joιn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wizard

;; This variable contains the history of user entered answers,
;; so that they can be entered again or edited.
(defvar orgtbl-join-history-cols ())

(defun orgtbl-join--parse-header-arguments (type)
  "If (point) is on a #+begin: line, parse it, and return an a-list.
TYPE is \"join\", or possibly any type of block.
If the line the (point) is on do not match TYPE, return nil."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position)))
        (case-fold-search t))
    (and
     (string-match
      (rx bos (* blank) "#+begin:" (* blank) (group (+ word)))
      line)
     (equal (match-string 1 line) type)
     (cdr (org-babel-parse-header-arguments line t)))))

(defun orgtbl-join--dismiss-help ()
  "Hide the wizard help window."
  (let ((help-window (get-buffer-window "*orgtbl-join-help*")))
    (if help-window
        (delete-window help-window))))

(defun orgtbl-join--display-help (explain &rest args)
  "Display help for each field the wizard queries.
EXPLAIN is a text in Org Mode to display. It is process
through `format' with replacements in ARGS."
  (let ((docs
         '(
           :isorgid "* Input table locator
The input table may be pointed to by:
- a file and a name,
- an Org Mode identifier"
           :orgid "* Org ID
It is an identifier hidden in a =properties= drawer.
Org Mode globally keeps track of all Ids and knows how to access them.
It is supposed that the ID location is followed by a table or
a Babel block suitable for aggregation."
           :name "* The input table may be:
- a regular Org table,
- a Babel block whose output will be the input table.
Org table & Babel block names are available at completion (type ~TAB~).
Leave empty for a CSV or JSON formatted table."
           :params "* Parameters for Babel code block (optional)
** A Babel code block may require specific parameters
Give them here if needed, surrounded by parenthesis. Example:
  ~(size=4,reverse=nil)~
** CSV or JSON formatted tables.
Examples:
  ~(csv header)~ ~(json)~
** Regular Org Mode table
Leave empty."
           :slice "* Slicing (optional)
Slicing is an Org Mode feature allowing to cut the input table.
It applies to any input: Org table, Babel output, CSV, JSON.
Leave empty for no slicing.
** Examples:
- ~mytable[0:5]~     retains only the first 6 rows of the input table
- ~mytable[*,0:1]~   retains only the first 2 columns
- ~mytable[0:5,0:1]~ retains 5 rows and 2 columns"
           :cond "* Filter rows (optional)
Lisp function, lambda, or Babel block to filter out rows.
** Available input columns
  %s
** Example
  ~(>= (string-to-number quty) 3)~
  only rows with cell ~quty~ higher or equal to ~3~ are retained.
  ~(not (equal tag \"dispose\"))~
  rows with cell ~tag~ equal to ~dispose~ are filtered out."
           :post "* Post-process (optional)
The output table may be post-processed prior to printing it
in the current buffer.
The processor may be a Lisp function, a lambda, or a Babel block.
** Example:
  ~(lambda (table) (append table '(hline (banana 42))))~
  two rows are appended at the end of the output table:
  ~hline~ which means horizontal line,
  and a row with two cells."
;; bazilo]
           :masfile "* In which file is the master table?
The master table may be in another file.
The master table is the one we want to enrich with material from other tables.
Leave answer empty to mean that the table is in the current buffer."
           :reffile "* In which file is the reference table?
A reference table is a storage of knowledge where orgtbl-join will pick
selected rows. The flow of information is from the reference tables to the
master table to be enriched.
Leave answer empty to mean that the table is in the current buffer."
           :cols "* Columns re-arrangement 'optional)
The natural ordering of columns puts the master columns first,
then each of the reference columns.
This order may be changed by specifying the list of output columns.
Columns may also be ignored by this way.
Candidates are:
  %s"
           :mascol "* Which master column?
One of the columns in the master table will be used to search
for a selection of matching rows in the reference table.
Candidates are:
  %s"
           :refcol "* Which reference column?
One of the columns in the reference table will be matched
in order to collect some rows and add them to the master table.
Candidates are:
  %s"
           :full "* Which table should be kept entirely?
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
  the resulting enriched table may therefore be shorter than both tables."
           :another "* Another reference table?
Up to now, the reference tables used in the joining are:
%s"
;; [bazilo synchronize orgtbl-αggregate & orgtbl-joιn
           ))
        (main-window (selected-window))
        (help-window (get-buffer-window "*orgtbl-join-help*")))
    (if help-window
        (select-window help-window)
      (setq main-window (split-window nil 16 'above))
      (switch-to-buffer "*orgtbl-join-help*")
      (setq help-window (selected-window)))
    (org-mode)
    (erase-buffer)
    (insert (apply #'format (plist-get docs explain) args))
    (goto-char (point-min))
    (select-window main-window)))

(defun orgtbl-join--wizard-query-table (table typeoftable)
  "Query the 4 fields composing a generalized table: file:name:params:slice.
It may be only 3 fields in case of orgid:params:slice or
file.csv:(csv):slice.
If TABLE is not nil, it is decomposed into file:name:params:slice, and each
of those 4 fields serve as default answer when prompting.
Alternately, file:name may be orgid, an ID which knows its file location.
TYPEOFTABLE is a qualifier: t for master, nil for reference."
  (let (file name orgid params slice isorgid)
    (if table
        (let ((struct (orgtbl-join--parse-locator table)))
          (setq file   (aref struct 0))
          (setq name   (aref struct 1))
          (setq orgid  (aref struct 2))
          (setq params (aref struct 3))
          (setq slice  (aref struct 4))))

    (setq
     isorgid
     (cond
      (orgid t)
      (name nil)
      (t
       (orgtbl-join--display-help :isorgid)
       (let ((use-short-answers t))
         (yes-or-no-p "Is the input pointed to by an Org Mode ID? ")))))

    (if isorgid
        (progn
          (orgtbl-join--display-help :orgid)
          (unless org-id-locations (org-id-locations-load))
          (setq orgid
                (completing-read
                 "Org ID: "
                 (hash-table-keys org-id-locations)
                 nil
                 nil ;; user is free to input anything
                 orgid)))

      (orgtbl-join--display-help (if typeoftable :masfile :reffile))
      (let ((insert-default-directory nil))
        (setq file
              (orgtbl-join--nil-if-empty
               (read-file-name "File (RET for current buffer): "
                               nil
                               nil
                               nil
                               file))))

      (orgtbl-join--display-help :name)
      (setq name
            (completing-read
             "Table or Babel: "
             (orgtbl-join--list-local-tables file)
             nil
             nil ;; user is free to input anything
             name)))

    (and
     file
     (not params)
     (cond
      ((string-match-p (rx ".csv"  eos) file)
       (setq params "(csv)"))
      ((string-match-p (rx ".json" eos) file)
       (setq params "(json)"))))

    (orgtbl-join--display-help :params)
    (setq params
          (read-string
           "Babel parameters (optional): "
           params
           'orgtbl-join-history-cols))

    (orgtbl-join--display-help :slice)
    (setq slice
          (read-string
           "Input slicing (optional): "
           slice
           'orgtbl-join-history-cols))

    (orgtbl-join--assemble-locator file name orgid params slice)))

;; bazilo]

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
  (let ((completions (orgtbl-join--get-header-table table)))
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
              :mascol
	      "Master column: "
	      mastable
              (or
               (orgtbl-join--plist-get-remove params :mas-column)
	       mascol)
              allcolumns
              t))
       (setq refcol
	     (orgtbl-join--join-query-column
              :refcol
	      "Reference column: "
	      (orgtbl-join-table-from-any-ref reftable)
	      (or
               (orgtbl-join--plist-get-remove params :ref-column)
               mascol)
              allcolumns))
       (orgtbl-join--display-help :full)
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
       (orgtbl-join--display-help :another
        (cl-loop
         for pair on newparams
         if (eq (car pair) :ref-table)
         concat (format " ~%s~" (cadr pair))
         do (setq pair (cdr pair))))
       while
       (y-or-n-p "Another reference table? "))

      (pop allcolumns)

      ;; There may be ambiguities with $1 $2 $3 names.
      ;; Do they refer to columns in the master table or in any of
      ;; the references tables?
      ;; There also might be duplicate column names which create ambiguity.
      (let* ((dup
              (cl-loop
               for el on allcolumns
               if (member (car el) (cdr el))
               collect (car el)))
             (dupmsg
              (if dup
                  (format
                   "\nBeware, those columns have duplicate names:\n  %s"
                   (mapconcat
                    (lambda (x) (format " ~%s~" x))
                    dup))
                "")))
        (orgtbl-join--display-help
         :cols
         (format
          "%s%s"
          (mapconcat
           (lambda (x) (format " ~%s~" x))
           allcolumns)
          dupmsg)))
      (let ((cols
             (orgtbl-join--nil-if-empty
              (read-string
               "(Optional) specify output columns: "
               (orgtbl-join--plist-get-remove params :cols)
               'orgtbl-join-history-cols))))
        (if cols
            (setq newparams
                  `(,@newparams
                    :cols
                    ,cols))))
      )

    ;; recover parameters not taken into account by the wizard
    (cl-loop
     for pair on params
     if (car pair)
     do (nconc newparams `(,(car pair) ,(cadr pair)))
     do (setq pair (cdr pair)))
    `(:name "join" ,@newparams)))

;; [bazilo synchronize orgtbl-αggregate & orgtbl-joιn

;;;###autoload
(defun orgtbl-join-insert-dblock-join ()
  "Wizard to interactively insert a dynamic joined block."
  (interactive)
  (let* ((oldline (flatten-list (orgtbl-join--parse-header-arguments "join")))
         (params (orgtbl-join--wizard-create-update nil oldline)))
    (when oldline
      (org-mark-element)
      (delete-region (region-beginning) (1- (region-end))))
    (org-create-dblock params)
    (org-update-dblock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unfold, Fold
;; Experimental
;; Typing TAB on a line like
;; #+begin join params…
;; unfolds the parameters: a new line for each parameter
;; and a dedicated help & completion for each activated by TAB

;;;###autoload
(defun orgtbl-aggregate-dispatch-TAB ()
  "Type TAB on a line like #+begin: join to activate custom functions.
Actually, any line following this pattern will do:
#+xxxxx: yyyyy
Typing TAB will dispatch to function org-TAB-xxxxx-yyyyy if it exists.
If it does not exist, Org Mode will proceed as usual.
If it exists and returns nil, Org Mode will proceed as usual as well.
It it returns non-nil, the TAB processing will stop there."
  (save-excursion
    (if (and
         (not (bolp))
         (progn
           (end-of-line)
           (not (org-fold-core-folded-p)))
         (progn
           (beginning-of-line)
           (re-search-forward
            (rx
             point
             "#+"
             (group (+ (any "a-z0-9_-")))
             ":"
             (* blank)
             (group (+ (any ":a-z0-9_-")))
             (* blank))
            nil t)))
        (let ((symb
               (intern
                (format
                 "org-TAB-%s-%s"
                 (downcase (match-string-no-properties 1))
                 (downcase (match-string-no-properties 2))))))
          (if (symbol-function symb)
              (funcall symb))))))

(defun org-TAB-begin-join ()
  "Dispatch to unfolding or folding code.
If the line following
  #+begin: join
is an unfoled one of the form:
  #+join: …
then proceed to folding, otherwise unfold."
  (if (save-excursion
        (forward-line 1)
        (beginning-of-line)
        (re-search-forward
         (rx point "#+join:")
         nil t))
      (org-TAB-begin-join-fold)
    (org-TAB-begin-join-unfold)))

(defun orgtbl-join-get-all-unfolded ()
  "Prepare an a-list of all unfolded parameters."
  (interactive)
  (save-excursion
    (re-search-backward (rx bol "#+begin:") nil t)
    (let ((alist))
      (while
          (progn
            (forward-line 1)
            (re-search-forward
             (rx point "#+join:" (* blank)
               (group (+ (any ":a-z0-9_-")))
               (* blank)
               (group (* any)))
             nil t))
        (push (cons
               (intern (match-string-no-properties 1))
               (match-string-no-properties 2))
              alist))
      (reverse alist))))

(defun orgtbl-join--TAB-replace-value (getter)
  "Update a #+join: line
from
  #+join: :tag OLD
to
  #+join: :tag NEW
NEW being the result of executing (GETTER OLD)"
  (let* ((start (point))
         (end (pos-eol))
         (new
          (funcall
           getter
           (buffer-substring-no-properties start end))))
    (when new
      (delete-region start end)
      (delete-horizontal-space)
      (insert " " new))))

;; bazilo]

(defun orgtbl-join--insert-remove-pair-from-alist (tag alist)
  "Helper function for folding a pair (TAG . VALUE) in ALIST."
  (let ((value
         (orgtbl-join--nil-if-empty
          (orgtbl-join--alist-get-remove tag alist))))
    (if value
        (insert (format " %s \"%s\"" tag value)))))

(defun org-TAB-begin-join-fold ()
  "Turn all lines of the form #+join: … into a single line.
That is, fold the may lines of the form:
  #+join: param…
into the single line of the form:
  #+begin: join params…
Note that the resulting :table XXX parameter is composed of several
individual parameters."
  (orgtbl-join--dismiss-help)
  (let* ((alist (orgtbl-join-get-all-unfolded)))
    (end-of-line)
    (insert
     " :mas-table \""
     (orgtbl-join--assemble-locator
      (orgtbl-join--alist-get-remove :mas-file   alist)
      (orgtbl-join--alist-get-remove :mas-name   alist)
      (orgtbl-join--alist-get-remove :mas-orgid  alist)
      (orgtbl-join--alist-get-remove :mas-params alist)
      (orgtbl-join--alist-get-remove :mas-slice  alist))
     "\"")
    (orgtbl-join--insert-remove-pair-from-alist :mas-column alist)
    (while (or (alist-get :ref-file  alist)
               (alist-get :ref-name  alist)
               (alist-get :ref-orgid alist))
      (insert
       " :ref-table \""
       (orgtbl-join--assemble-locator
        (orgtbl-join--alist-get-remove :ref-file   alist)
        (orgtbl-join--alist-get-remove :ref-name   alist)
        (orgtbl-join--alist-get-remove :ref-orgid  alist)
        (orgtbl-join--alist-get-remove :ref-params alist)
        (orgtbl-join--alist-get-remove :ref-slice  alist))
       "\"")
      (orgtbl-join--insert-remove-pair-from-alist :ref-column alist)
      (orgtbl-join--insert-remove-pair-from-alist :full       alist))
    (orgtbl-join--insert-remove-pair-from-alist :cols alist)
    (orgtbl-join--insert-remove-pair-from-alist :cond alist)
    (orgtbl-join--insert-remove-pair-from-alist :post alist)
    (cl-loop
     for pair in alist
     if (car pair)
     do (orgtbl-join--insert-remove-pair-from-alist (car pair) alist))
    (forward-line 1)
    (while
        (progn
          (beginning-of-line)
          (re-search-forward (rx point "#+join:") nil t))
      (beginning-of-line)
      (delete-line))
    (forward-line -1)
    t))

(defun org-TAB-begin-join-unfold ()
  "Turn the single line #+begin: join into several lines.
That is, move all parameters in the line
  #+begin: join params…
into several lines, each with a single parameter.
Note that the :table XXX parameter is decomposed into several
individual parameter for an easier reading."
  (let* ((line (orgtbl-join--parse-header-arguments "join"))
         (point (progn (end-of-line) (point)))
         (ref-table))
    (let ((struct (orgtbl-join--parse-locator
                   (orgtbl-join--alist-get-remove :mas-table line))))
      (insert "\n#+join: :mas-file "   (or (aref struct 0) ""))
      (insert "\n#+join: :mas-name "   (or (aref struct 1) ""))
      (insert "\n#+join: :mas-orgid "  (or (aref struct 2) ""))
      (insert "\n#+join: :mas-params " (or (aref struct 3) ""))
      (insert "\n#+join: :mas-slice "  (or (aref struct 4) "")))
    (insert "\n#+join: :mas-column "
            (or (orgtbl-join--alist-get-remove :mas-column line) ""))
    (while (setq ref-table (orgtbl-join--alist-get-remove :ref-table line))
      (let ((struct (orgtbl-join--parse-locator ref-table)))
        (insert "\n#+join: :ref-file "   (or (aref struct 0) ""))
        (insert "\n#+join: :ref-name "   (or (aref struct 1) ""))
        (insert "\n#+join: :ref-orgid "  (or (aref struct 2) ""))
        (insert "\n#+join: :ref-params " (or (aref struct 3) ""))
        (insert "\n#+join: :ref-slice "  (or (aref struct 4) "")))
      (insert "\n#+join: :ref-column "
              (or (orgtbl-join--alist-get-remove :ref-column line) ""))
      (insert "\n#+join: :full "
              (or (orgtbl-join--alist-get-remove :full line) "")))
    (insert "\n#+join: :cols "
            (or (orgtbl-join--alist-get-remove :cols       line) ""))
    (insert "\n#+join: :cond "
            (or (orgtbl-join--alist-get-remove :cond       line) ""))
    (insert "\n#+join: :post "
            (or (orgtbl-join--alist-get-remove :post       line) ""))
    (cl-loop
     for pair in line
     if (car pair)
     do (insert (format "\n#+join: %s %s" (car pair) (cdr pair))))
    (goto-char point)
    (beginning-of-line)
    (forward-word 2)
    (delete-region (point) point)
    t))

(defun orgtbl-join--column-names-from-unfolded ()
  "Return the list of column names.
They are computed by looking at the distant table
(an Org table, a Babel block, a CSV, or a JSON)
and recovering its header if any.
If there is no header, $1 $2 $3... is returned."
  (let ((alist (orgtbl-join-get-all-unfolded)))
    (mapconcat
     (lambda (x) (format " ~%s~" x))
     (append
      (orgtbl-join--get-header-table
       (orgtbl-join--assemble-locator
        (alist-get :mas-file   alist)
        (alist-get :mas-name   alist)
        (alist-get :mas-orgid  alist)
        (alist-get :mas-params alist)
        (alist-get :mas-slice  alist)))
      (orgtbl-join--get-header-table
       (orgtbl-join--assemble-locator
        (alist-get :ref-file   alist)
        (alist-get :ref-name   alist)
        (alist-get :ref-orgid  alist)
        (alist-get :ref-params alist)
        (alist-get :ref-slice  alist)))))))

;; [bazilo synchronize orgtbl-αggregate & orgtbl-joιn

(defun org-TAB-join-:mas-file ()
  "Provide help and completion for the #+join: mas-file XXX parameter."
  (orgtbl-join--display-help :masfile)
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (read-file-name
      "File: "
      (file-name-directory    old)
      nil
      nil
      (file-name-nondirectory old)))))

(defun org-TAB-join-:mas-name ()
  "Provide help and completion for the #+join: mas-name XXX parameter."
  (orgtbl-join--display-help :name)
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (completing-read
      "Table or Babel name: "
      (orgtbl-join--list-local-tables
       (orgtbl-join--nil-if-empty
        (alist-get :mas-file (orgtbl-join-get-all-unfolded))))
      nil
      nil ;; user is free to input anything
      old))))

(defun org-TAB-join-:mas-orgid ()
  "Provide help and completion for the #+join: mas-orgid XXX parameter."
  (orgtbl-join--display-help :orgid)
  (unless org-id-locations (org-id-locations-load))
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (completing-read
      "Org-ID: "
      (hash-table-keys org-id-locations)
      nil
      nil ;; user is free to input anything
      old))))

(defun org-TAB-join-:mas-params ()
  (orgtbl-join--display-help :params))

(defun org-TAB-join-:mas-slice ()
  (orgtbl-join--display-help :slice))

;; bazilo]

;; [bazilo synchronize orgtbl-αggregate & orgtbl-joιn

(defun org-TAB-join-:ref-file ()
  "Provide help and completion for the #+join: ref-file XXX parameter."
  (orgtbl-join--display-help :reffile)
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (read-file-name
      "File: "
      (file-name-directory    old)
      nil
      nil
      (file-name-nondirectory old)))))

(defun org-TAB-join-:ref-name ()
  "Provide help and completion for the #+join: ref-name XXX parameter."
  (orgtbl-join--display-help :name)
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (completing-read
      "Table or Babel name: "
      (orgtbl-join--list-local-tables
       (orgtbl-join--nil-if-empty
        (alist-get :ref-file (orgtbl-join-get-all-unfolded))))
      nil
      nil ;; user is free to input anything
      old))))

(defun org-TAB-join-:ref-orgid ()
  "Provide help and completion for the #+join: ref-orgid XXX parameter."
  (orgtbl-join--display-help :orgid)
  (unless org-id-locations (org-id-locations-load))
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (completing-read
      "Org-ID: "
      (hash-table-keys org-id-locations)
      nil
      nil ;; user is free to input anything
      old))))

(defun org-TAB-join-:ref-params ()
  (orgtbl-join--display-help :params))

(defun org-TAB-join-:ref-slice ()
  (orgtbl-join--display-help :slice))

(defun org-TAB-join-:cols ()
  (orgtbl-join--display-help :cols
   (orgtbl-join--column-names-from-unfolded)))

(defun org-TAB-join-:cond ()
  (orgtbl-join--display-help :cond
   (orgtbl-join--column-names-from-unfolded)))

(defun org-TAB-join-:post ()
  (orgtbl-join--display-help :post))

;; bazilo]

(defun org-TAB-join-:mas-column ()
  "Provide help and completion for the #+join: mas-column XXX parameter."
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (orgtbl-join--join-query-column
      :mascol
      "Master column: "
      (let*
          ((alist (orgtbl-join-get-all-unfolded))
           (table
            (orgtbl-join--assemble-locator
             (alist-get :mas-file    alist)
             (alist-get :mas-name    alist)
             (alist-get :mas-orgid   alist)
             (alist-get :mas-params  alist)
             (alist-get :mas-slice   alist))))
        (orgtbl-join-table-from-any-ref table))
      old
      (list nil)
      nil))))

(defun org-TAB-join-:ref-column ()
  "Provide help and completion for the #+join: ref-column XXX parameter."
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (orgtbl-join--join-query-column
      :refcol
      "Reference column: "
      (let*
          ((alist (orgtbl-join-get-all-unfolded))
           (table
            (orgtbl-join--assemble-locator
             (alist-get :ref-file    alist)
             (alist-get :ref-name    alist)
             (alist-get :ref-orgid   alist)
             (alist-get :ref-params  alist)
             (alist-get :ref-slice   alist))))
        (orgtbl-join-table-from-any-ref table))
      old
      (list nil)
      nil))))

(defun org-TAB-join-:full ()
  "Hitting TAB on #+join: :full X cycles the parameter value.
The cycle is
nothing → mas → mas+ref → ref → none -> nothing."
  (orgtbl-join--display-help :full)
  (orgtbl-join--TAB-replace-value
   (lambda (old)
     (cond
      ((equal old ""       ) "mas"    )
      ((equal old "mas"    ) "mas+ref")
      ((equal old "mas+ref") "ref"    )
      ((equal old "ref"    ) "none"   )
      ((equal old "none"   ) ""       )
      (t "")))))

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

;; [bazilo synchronize orgtbl-αggregate & orgtbl-joιn

;; Insert a dynamic bloc with the C-c C-x x dispatcher
;; and activate TAB on #+begin: join ...
;;;###autoload
(eval-after-load 'org
  '(progn
     ;; org-dynamic-block-define found in Emacs 27.1
     (org-dynamic-block-define "join" #'orgtbl-join-insert-dblock-join)))

;; This hook will only work if orgtbl-join is loaded,
;; thus the eval-after-load 'orgtbl-join
;; We do not want this hook to be added to Org Mode if orgtbl-join
;; is not used, thus the eval-after-load 'orgtbl-join
;;;###autoload
(eval-after-load 'orgtbl-join
  '(add-hook 'org-cycle-tab-first-hook #'orgtbl-aggregate-dispatch-TAB))

;; bazilo]

(provide 'orgtbl-join)
;;; orgtbl-join.el ends here
