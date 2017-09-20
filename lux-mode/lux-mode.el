;;; lux-mode.el --- Major mode for Lux code -*- lexical-binding: t; -*-

;; Copyright © 2014-2017 Eduardo Julian
;;
;; Authors: Eduardo Julian <eduardoejp@gmail.com>
;; URL: https://github.com/LuxLang/lux/tree/master/lux-mode
;; Keywords: languages lisp lux
;; Version: 0.1.0
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
  :link '(url-link :tag "Github" "https://github.com/lux/lux-mode")
  :link '(emacs-commentary-link :tag "Commentary" "lux-mode"))

(defconst lux-mode-version "0.1.0"
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

(defvar lux-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()2n" table)
    (modify-syntax-entry ?\) ")(3n" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\" "\"\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?# "w 124b" table)
    (modify-syntax-entry ?\n "> b" table)
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
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?+ "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?| "w" table)
    (modify-syntax-entry ?: "w" table)
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?, "w" table)
    (modify-syntax-entry ?/ "w" table)
    (modify-syntax-entry ?? "w" table)
    (modify-syntax-entry ?< "w" table)
    (modify-syntax-entry ?> "w" table)
    (modify-syntax-entry ?\; "w" table)
    ;; (modify-syntax-entry ?\\ "w" table)
    (modify-syntax-entry ?\s "-" table)
    (modify-syntax-entry ?\t "-" table)
    (modify-syntax-entry ?\r "-" table)
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
        (backward-char)
        (if (and (or (char-equal delim ?\()
                     (char-equal delim ?\")
                     (char-equal delim ?{))
                 (not endp))
            (if (char-equal (char-after) ?#)
                (and (not (bobp))
                     (or (char-equal ?w (char-syntax (char-before)))
                         (char-equal ?_ (char-syntax (char-before)))))
              t)
          t))
    t))

(defun lux-paredit-setup ()
  "Make \"paredit-mode\" play nice with `lux-mode'."
  (when (>= paredit-version 21)
    (define-key lux-mode-map "{" #'paredit-open-curly)
    (define-key lux-mode-map "}" #'paredit-close-curly)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'lux-space-for-delimiter-p)))

(defun lux-mode-variables ()
  "Set up initial buffer-local variables for Lux mode."
  (setq-local imenu-create-index-function
              (lambda ()
                (imenu--generic-function '((nil lux-match-next-def 0)))))
  (setq-local comment-start "## ")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'lux-indent-function)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local open-paren-in-column-0-is-defun-start nil))

;;;###autoload
(define-derived-mode lux-mode prog-mode "Lux"
  "Major mode for editing Lux code.

\\{lux-mode-map}"
  (lux-mode-variables)
  (lux-font-lock-setup)
  (add-hook 'paredit-mode-hook #'lux-paredit-setup)
  (define-key lux-mode-map [remap comment-dwim] 'lux-comment-dwim))

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

(defconst lux-font-lock-keywords
  (eval-when-compile
    `(; Special forms
      (,(concat
         "("
         (regexp-opt
          '(";module:"
            "def:" "type:" "macro:" "syntax:" "program:"
            "sig:" "struct:" "context:" "template:"
            "class:" "interface:"
            "poly:" "derived:"
            "actor:" "message:" "on:"
            "exception:"
            "word:"
            "opaque:"
            "unit:" "scale:"
            "function" "case" ":" ":!" ":!!" "undefined" "ident-for"
            "and" "or"
            "char"
            "exec" "let" "with-expansions" "if" "cond" "do" "be" "open" "loop" "recur" "comment" "list" "list&" "io" "vector" "tree"
            "get@" "set@" "update@" "|>" "|>." "<|" "<|." "_$" "$_" "~" "~@" "~'" "::" ":::"
            "|" "&" "->" "All" "Ex" "Rec" "host" "$" "type"
            "^" "^or" "^slots" "^multi" "^~" "^@" "^template" "^open" "^|>" "^stream&" "^regex"
            "bin" "oct" "hex"
            "pre" "post"
            "sig" "struct" "derive"
            "infix"
            "format"
            "`" "`'" "'" "do-template"
            "object" "do-to" "synchronized" "class-for"
            "doc"
            "regex"
            ) t)
         "\\>")
       1 font-lock-builtin-face)
      ; Bool literals
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false") t)
         "\\>")
       0 font-lock-constant-face)
      ; Nat literals
      ("\\<\\+\\(0\\|[1-9][0-9_]*\\)\\>" 0 font-lock-constant-face)
      ; Int|Frac literals
      ("\\<-?\\(0\\|[1-9][0-9_]*\\)\\(\\.[0-9_]+\\)?\\>" 0 font-lock-constant-face)
      ("\\<-?\\(0\\|[1-9][0-9_]*\\)\\(\\.[0-9_]+\\(\\(e\\|E\\)\\(-\\|\\+\\)?[1-9][0-9_]*\\)?\\)?\\>" 0 font-lock-constant-face)
      ; Frac "ratio" literals
      ("\\<-?\\(0\\|[1-9][0-9_]*\\)/[1-9][0-9_]*\\>" 0 font-lock-constant-face)
      ; Deg literals
      ("\\<\\(\\.[0-9_]+\\)\\>" 0 font-lock-constant-face)
      ; Tags
      ("#;[a-zA-Z0-9-\\+_=!@\\$%\\^&\\*<>\.,/\\\\\\|':~\\?]+" 0 font-lock-type-face)
      ("#;;[a-zA-Z0-9-\\+_=!@\\$%\\^&\\*<>\.,/\\\\\\|':~\\?]+" 0 font-lock-type-face)
      ("#[a-zA-Z0-9-\\+_=!@\\$%\\^&\\*<>\.,/\\\\\\|':~\\?]+\\(;[a-zA-Z0-9-\\+_=!@\\$%\\^&\\*<>\.,/\\\\\\|':~\\?]+\\)?" 0 font-lock-type-face)
      ))
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
          (("+-*/.<>=!?$%_&~^:@" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lux-font-lock-syntactic-face-function))))

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
        ;; car of form doesn't seem to be a symbol
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
                              (split-string (substring-no-properties function) ";")))))
        (setq method (get (intern-soft function-tail) 'lux-indent-function))
        (cond ((member (char-after open-paren) '(?\[ ?\{))
               (goto-char open-paren)
               (1+ (current-column)))
              ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 2)
                        (string-match "\\`\\(?:\\S +/\\)?\\(\\w+:\\|\\(\\w*;\\)?with-\\)"
                                      function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))
              )))))

(defun put-lux-indent (sym indent)
  "Instruct `lux-indent-function' to indent the body of SYM by INDENT."
  (put sym 'lux-indent-function indent))

(defmacro define-lux-indent (&rest kvs)
  "Call `put-lux-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x) `(put-lux-indent
                             (quote ,(first x)) ,(second x)))
               kvs)))

(define-lux-indent
  (def 'defun)
  (function 'defun)
  (let 'defun)
  (case 'defun)
  (do 'defun)
  (exec 'defun)
  (be 'defun)
  (if 1)
  (cond 0)
  (loop 1)
  (do-template 'defun)
  (All 'defun)
  (Ex 'defun)
  (Rec 'defun)
  (_lux_def 'defun)
  (_lux_case 'defun)
  (_lux_function 'defun)
  (synchronized 'defun)
  (object 'defun)
  (do-to 'defun)
  (import 'defun)
  (testing 'defun)
  (comment 'defun)
  (^template 'defun)
  )

;;;###autoload


(provide 'lux-mode)
