;;; lux-mode.el --- Major mode for Lux code -*- lexical-binding: t; -*-

;; Copyright © 2014-2022 Eduardo Julian
;;
;; Authors: Eduardo Julian <eduardoejp@gmail.com>
;; URL: https://github.com/LuxLang/lux/tree/master/lux-mode
;; Keywords: languages lisp lux
;; Version: 0.6.6
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Based on the code for clojure-mode (http://github.com/clojure-emacs/clojure-mode)
;; By Jeffrey Chu <jochu0@gmail.com> et al

;; Provides font-lock, indentation, and navigation for the Lux programming language.

;; Using lux-mode with paredit or smartparens is highly recommended.

;; Here are some example configurations:

;;   ;; require or autoload paredit-mode
;;   (add-hook 'lux-mode-hook #'paredit-mode)

;;   ;; require or autoload smartparens
;;   (add-hook 'lux-mode-hook #'smartparens-strict-mode)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Compatibility
(eval-and-compile
  ;; `setq-local' for Emacs 24.2 and below
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))

(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp)
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (defvar paredit-space-for-delimiter-predicates)
  (defvar paredit-version)
  (defvar paredit-mode))

(require 'cl)
(require 'imenu)

(defgroup lux nil
  "Major mode for editing Lux code."
  :prefix "lux-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/LuxLang/lux/tree/master/lux-mode")
  :link '(emacs-commentary-link :tag "Commentary" "lux-mode"))

(defconst lux-mode-version "0.6.0"
  "The current version of `lux-mode'.")

(defcustom lux-defun-style-default-indent nil
  "When non-nil, use default indenting for functions and macros.
Otherwise check `define-lux-indent' and `put-lux-indent'."
  :type 'boolean
  :group 'lux
  :safe 'booleanp)

(defvar lux-mode-map
  (make-sparse-keymap)
  "Keymap for Lux mode.  Inherits from `lisp-mode-shared-map'.")

;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Flags.html
(defvar lux-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?\s "-" table)
    (modify-syntax-entry ?\t "-" table)
    (modify-syntax-entry ?\r "-" table)
    (modify-syntax-entry ?\n "> " table)
    (modify-syntax-entry ?# "w" table)
    (modify-syntax-entry ?+ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?, "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry ?~ "w" table)
    (modify-syntax-entry ?' "w" table)
    (modify-syntax-entry ?` "w" table)
    (modify-syntax-entry ?! "w" table)
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?% "w" table)
    (modify-syntax-entry ?^ "w" table)
    (modify-syntax-entry ?& "w" table)
    (modify-syntax-entry ?* "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?| "w" table)
    (modify-syntax-entry ?: "w" table)
    (modify-syntax-entry ?/ "w" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?< "w" table)
    (modify-syntax-entry ?> "w" table)
    (modify-syntax-entry ?\; "w" table)
    (modify-syntax-entry ?\\ "w" table)
    table))

(defun lux-mode-display-version ()
  "Display the current `lux-mode-version' in the minibuffer."
  (interactive)
  (message "lux-mode (version %s)" lux-mode-version))

(defun lux-space-for-delimiter-p (endp delim)
  "Prevent paredit from inserting useless spaces.
See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIM."
  (if (derived-mode-p 'lux-mode)
      (save-excursion
        (backward-char)))
  t)

(defun lux-enable-paredit-backslash ()
  "Getting paredit to work with backslashes (\)"
  (advice-add 'paredit-backslash :around
			  (lambda (self &rest inputs)
				(if (derived-mode-p 'lux-mode)
					(insert ?\\)
				  (apply self inputs))))
  (advice-add 'paredit-in-string-escape-p :around
			  (lambda (self &rest inputs)
				(if (derived-mode-p 'lux-mode)
					nil
				  (apply self inputs))))
  (advice-add 'paredit-unescape-string :around
			  (lambda (self &rest inputs)
				(if (derived-mode-p 'lux-mode)
					nil
				  (apply self inputs))))
  ;; This codes originates from Paredit, but I had to modify it so it stops messing with backslashes (\).
  (advice-add 'paredit-forward-delete-in-string :around
			  (lambda (self &rest inputs)
				(if (derived-mode-p 'lux-mode)
					(let ((start+end (paredit-string-start+end-points)))
					  (cond ((not (eq (point) (cdr start+end)))
							 (delete-char +1))
							((eq (1- (point)) (car start+end))
							 (delete-char -1)
							 (delete-char +1))))
				  (apply self inputs))))
  (advice-add 'paredit-forward-delete :around
			  (lambda (self &optional argument)
				(if (derived-mode-p 'lux-mode)
					(progn
					  (interactive "P")
					  (cond ((or (consp argument) (eobp))
							 (delete-char +1))
							
							((integerp argument)
							 (if (< argument 0)
								 (paredit-backward-delete argument)
							   (while (> argument 0)
								 (paredit-forward-delete)
								 (setq argument (- argument 1)))))
							
							((paredit-in-string-p)
							 (paredit-forward-delete-in-string))
							
							((paredit-in-comment-p)
							 (paredit-forward-delete-in-comment))
							
							((let ((syn (char-syntax (char-after))))
							   (or (eq syn ?\( )
								   (eq syn ?\" )))
							 (if (save-excursion
								   (paredit-handle-sexp-errors (progn (forward-sexp) t)
									 nil))
								 (forward-char)
							   (message "Deleting spurious opening delimiter.")
							   (delete-char +1)))
							
							((and (not (paredit-in-char-p (1- (point))))
								  (eq (char-syntax (char-after)) ?\) )
								  (eq (char-before) (matching-paren (char-after))))
							 (delete-char -1) ; Empty list -- delete both
							 (delete-char +1))	;   delimiters.
							
							((eq (char-syntax (char-after)) ?\) )
							 (if (paredit-handle-sexp-errors
									 (save-excursion (forward-char) (backward-sexp) t)
								   nil)
								 (message "End of list!")
							   (progn
								 (message "Deleting spurious closing delimiter.")
								 (delete-char +1))))
							
							;; Just delete a single character, if it's not a closing
							;; delimiter.  (The character literal case is already handled
							;; by now.)
							(t (delete-char +1))))
				  (funcall self argument))))
  )

(defun lux/insert_semicolon ()
  "Just write a ; in the buffer."
  (interactive)
  (insert-char ?\;))

(defun lux-paredit-setup ()
  "Make \"paredit-mode\" play nice with `lux-mode'."
  (when (>= paredit-version 21)
	;; Extend
    (define-key lux-mode-map "{" #'paredit-open-curly)
    (define-key lux-mode-map "}" #'paredit-close-curly)
	(add-to-list 'paredit-space-for-delimiter-predicates
                 #'lux-space-for-delimiter-p)
	(lux-enable-paredit-backslash)
	;; Re-bind
	(define-key lux-mode-map
	  [remap paredit-semicolon] #'lux/insert_semicolon)
	))

(defun indent_lux_line! (&optional indent)
  (let ((pos (- (point-max) (point)))
        (indent (or indent
					(progn
					  (beginning-of-line)
					  (calculate-lisp-indent (lisp-ppss))))))
	;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Skipping-Characters.html
	(skip-chars-forward " \t")
    (if (null indent)
		(goto-char (- (point-max) pos))
      (progn
		(indent-line-to (if (listp indent)
							(car indent)
						  indent))
		(when (> (- (point-max) pos) (point))
		  (goto-char (- (point-max) pos)))))))

(defun lux-mode-variables ()
  "Set up initial buffer-local variables for Lux mode."
  (setq-local imenu-create-index-function
              (lambda ()
                (imenu--generic-function '((nil lux-match-next-def 0)))))
  (setq-local comment-start "... ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local indent-line-function #'indent_lux_line!)
  
  (setq-local lisp-indent-function #'lux-indent-function)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local open-paren-in-column-0-is-defun-start nil))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Messages.html
(defun lux/log! (text)
  (message "%S" text))

(defun lux-comment-dwim (&optional argument)
  (interactive "P")
  (when (use-region-p)
	;; http://doc.endlessparentheses.com/Fun/comment-only-p.html
	(if (comment-only-p (region-beginning) (region-end))
		(condition-case error
			(uncomment-region (region-beginning) (region-end) 3)
		  (error (uncomment-region (1- (region-beginning)) (region-end) 3)))
	  (comment-dwim "... "))))

;;;###autoload
(define-derived-mode lux-mode prog-mode "Lux"
  "Major mode for editing Lux code.

\\{lux-mode-map}"
  (lux-mode-variables)
  (lux-font-lock-setup)
  (add-hook 'paredit-mode-hook #'lux-paredit-setup)
  (define-key lux-mode-map [remap comment-dwim] 'lux-comment-dwim)
  (define-key lux-mode-map [remap paredit-comment-dwim] 'lux-comment-dwim)
  ;; https://stackoverflow.com/questions/25245469/how-to-define-whole-line-comment-syntax-in-emacs
  (setq-local syntax-propertize-function
			  ;; https://www.emacswiki.org/emacs/RegularExpression
			  ;; http://doc.endlessparentheses.com/Fun/syntax-propertize-rules
			  ;; https://www.emacswiki.org/emacs/EmacsSyntaxTable
              (syntax-propertize-rules ("\\(\\.\\{3\\}\\).*\\($\\)" (1 "<") (2 ">")))))

(defun lux-match-next-def ()
  "Scans the buffer backwards for the next \"top-level\" definition.
Called by `imenu--generic-function'."
  (when (re-search-backward "^(def\\sw*" nil t)
    (save-excursion
      (let (found?
            (start (point)))
        (down-list)
        (forward-sexp)
        (while (not found?)
          (forward-sexp)
          (or (if (char-equal ?[ (char-after (point)))
                              (backward-sexp))
                  (if (char-equal ?) (char-after (point)))
                (backward-sexp)))
          (destructuring-bind (def-beg . def-end) (bounds-of-thing-at-point 'sexp)
            (if (char-equal ?^ (char-after def-beg))
                (progn (forward-sexp) (backward-sexp))
              (setq found? t)
              (set-match-data (list def-beg def-end)))))
        (goto-char start)))))

;; https://www.gnu.org/software/findutils/manual/html_node/find_html/emacs-regular-expression-syntax.html
(defun altRE (&rest alternatives)
  (concat "\\(" (mapconcat 'identity alternatives "\\|") "\\)"))

(defun literal (content)
  (concat "\\<" content "\\>"))

(defun +class (characters)
  (concat "[" characters "]"))

(defun -class (characters)
  (concat "[^" characters "]"))

;; https://www.emacswiki.org/emacs/RegularExpression
(defconst lux-font-lock-keywords
  (eval-when-compile
	(let* ((natural "[0-9][0-9,]*")
		   (identifier_h|label "#")
		   (identifier_h|type "[:upper:]")
		   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Special.html
		   (identifier_t "][)(}{.\"[:space:]")
		   (identifier_h (concat identifier_t "0-9"))
		   (sign (altRE "-" "\\+"))
		   (identifier (concat (-class identifier_h) (-class identifier_t) "*"))
		   (integer (concat sign natural))
		   (bitRE (literal (altRE "#0" "#1")))
		   (natRE (literal natural))
		   (int&fracRE (literal (concat integer "\\(\\." natural "\\(\\(e\\|E\\)" integer "\\)?\\)?")))
		   (revRE (literal (concat "\\." natural)))
		   (specialRE (let (;; Control
							(control//flow (altRE "when" "exec" "let" "if" "cond" "loop" "do" "be"))
							(control//pattern-matching (altRE "open"))
							(control//logic (altRE "and" "or"))
							(control//contract (altRE "pre" "post"))
							;; Type
							(type//syntax (altRE "Union" "Or" "Variant"
												 "Tuple" "And" "Record"
												 "Rec"
												 "Primitive" "->"
												 "All" "Ex"
												 "Interface"
												 "type_literal"))
							(type//checking (altRE "is" "as" "let" "as_expected" "type_of" "sharing" "by_example" "hole"))
							(type//primitive (altRE "primitive" "abstraction" "representation" "transmutation"))
							(type//poly (altRE "polytypic"))
							(type//dynamic (altRE "dynamic" "static"))
							(type//capability (altRE "capability"))
							;; Data
							(data//record (altRE "the" "has" "revised"))
							(data//interface (altRE "use" "implementation" "with" "at"))
							(data//implicit (altRE "implicitly" "a/an" "a" "an"))
							(data//collection (altRE "list" "sequence" "tree"))
							;; Code
							(code//quotation (altRE "`" "`'" "'" "," ",\\*" ",'"))
							(code//super-quotation (altRE "``" ",,"))
							(code//template (altRE "with_template"))
							(code//macro (altRE "macro" "template" "syntax"))
							;; Miscellaneous
							(jvm-host (altRE "import" "export" "class" "interface" "object" "synchronized" "class_for"))
							(alternative-format (altRE "char" "bin" "oct" "hex"))
							(documentation (altRE "comment"))
							(function-application (altRE "|>" "<|" "left" "right" "all"))
							(function-definition (altRE "function" "|>>" "<<|"
														"program"))
							(remember (altRE "remember" "to_do" "fix_me"))
							(extension (altRE "analysis" "synthesis" "generation" "declaration"))
							(definition (altRE "\\.require"
											   "def" "inlined" "type"
											   "vocabulary")))
						(let ((control (altRE control//flow
											  control//pattern-matching
											  control//logic
											  control//contract))
							  (type (altRE type//syntax
										   type//checking
										   type//primitive
										   type//poly
										   type//dynamic
										   type//capability))
							  (data (altRE data//record
										   data//interface
										   data//implicit
										   data//collection))
							  (code (altRE code//quotation
										   code//super-quotation
										   code//template
										   code//macro)))
						  (concat
						   "("
						   (altRE
							control
							type
							data
							code
							;; ;;;;;;;;;;;;;;;;;;;;;;
							jvm-host
							alternative-format
							documentation
							function-application
							function-definition
							remember
							extension
							definition
							;; ;;;;;;;;;;;;;;;;;;;;;;
							"with_expansions"
							"undefined" "symbol"
							"for"
							"io"
							"infix"
							"format"
							"regex")
						   "\\>"))))
		   (separator "\\.")
		   (in-prelude separator)
		   (in-current-module (concat separator separator))
		   (in-module (concat identifier separator))
		   ;; (in-local "")
		   (in-local (altRE "^"
							(+class identifier_t)))
		   (global_prefix (altRE in-prelude
								 in-current-module
								 in-module
								 in-local))
		   (typeRE (concat global_prefix (+class identifier_h|type) (-class identifier_t) "*"))
		   (labelRE (concat global_prefix (+class identifier_h|label) (-class identifier_t) "+"))
		   (literalRE (altRE bitRE ;; Bit literals
							 natRE ;; Nat literals
							 int&fracRE ;; Int literals && Frac literals
							 revRE ;; Rev literals
							 )))
	  `(;; Special forms
		(,specialRE 1 font-lock-builtin-face)
		(,literalRE 0 font-lock-constant-face)
		(,typeRE 0 font-lock-type-face)
		(,labelRE 0 font-lock-keyword-face)
		)))
  "Default expressions to highlight in Lux mode.")

(defun lux-font-lock-syntactic-face-function (state)
  "Find and highlight text with a Lux-friendly syntax table.

This function is passed to `font-lock-syntactic-face-function',
which is called with a single parameter, STATE (which is, in
turn, returned by `parse-partial-sexp' at the beginning of the
highlighted region)."
  (if (nth 3 state)
      ;; This might be a string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          font-lock-constant-face))
    font-lock-comment-face))

(defun lux-font-lock-setup ()
  "Configures font-lock for editing Lux code."
  (setq-local font-lock-multiline t)
  (setq font-lock-defaults
        '(lux-font-lock-keywords         ; keywords
          nil nil
          (("+-*/.<>=!?$%_&~^:@#" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function . lux-font-lock-syntactic-face-function))))

(defvar withRE (concat "\\`" "with" (altRE "_" "\\'")))

(defun lux-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lux function with a
non-nil property `lux-indent-function', that specifies how to do
the indentation.

The property value can be

- `defun', meaning indent `defun'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.

This function also returns nil meaning don't specify the indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be an identifier
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let* ((function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
             (open-paren (elt state 1))
             (method nil)
             (function-tail (first
                             (last
                              (split-string (substring-no-properties function) "\\.")))))
        (setq method (get (intern-soft function-tail) 'lux-indent-function))
        (cond ((member (char-after open-paren) '(?\[ ;; ?\{
												 ))
               (goto-char open-paren)
               (1+ (current-column)))
              ((or (eq method 'defun)
				   (and (null method)
                        (> (length function) 2)
						(string-match withRE function-tail)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))
              )))))

(defun put-lux-indent (sym indent)
  "Instruct `lux-indent-function' to indent the body of SYM by INDENT."
  (if (symbolp sym)
	  (put sym 'lux-indent-function indent)
	(put (intern sym) 'lux-indent-function indent)))

(defmacro define-lux-indent (&rest kvs)
  "Call `put-lux-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x) `(put-lux-indent
							 (quote ,(first x)) ,(second x)))
               kvs)))

(define-lux-indent
  ("function" 'defun)
  ("macro" 'defun)
  ("syntax" 'defun)
  ("template" 'defun)
  ("polytypic" 'defun)
  ("program" 'defun)

  ("def" 'defun)
  ("type" 'defun)
  ("inlined" 'defun)
  ("context" 'defun)
  ("primitive" 'defun)
  
  ("analysis" 'defun)
  ("synthesis" 'defun)
  ("generation" 'defun)
  ("declaration" 'defun)

  ("class" 'defun)
  ("interface" 'defun)
  ("import" 'defun)
  ("export" 'defun)

  ("comment" 'defun)
  ("definition" 'defun)

  ("sharing" 'defun)
  ("by_example" 'defun)
  ("capability" 'defun)
  
  (let 'defun)
  (when 'defun)
  (do 'defun)
  (exec 'defun)
  (be 'defun)
  (if 1)
  (cond 0)
  (loop 1)
  (All 'defun)
  (Ex 'defun)
  (Rec 'defun)

  (synchronized 'defun)
  (object 'defun)
  
  (remember 'defun)
  (to_do 'defun)
  (fix_me 'defun)

  (test 'defun)
  (coverage 'defun)
  )

;;;###autoload
(provide 'lux-mode)
