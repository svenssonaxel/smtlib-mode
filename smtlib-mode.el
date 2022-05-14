;;; smtlib-mode.el --- Major mode to edit and run SMTLIB v2 files

;;; Commentary:

;; Author: Christoph Sticksel (christoph@sticksel.info), Alain Mebsout

;; This mode is based on lisp-mode and provides highlighting of SMTLIB
;; v2 commands, as well as the command smtlib-mode/run-solver (bound to C-c C-c)
;; to run an SMT solver on the buffer or the region.

;; The command to run the SMT solver is by default "cvc5 --lang smt2",
;; modify and add the following line to your .emacs to change.

;;   (setq smtlib-mode/solver-cmd "cvc5 --lang smt2")

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; References to page numbers are for the specification of smtlib 2.6, see
;; https://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2021-05-12.pdf

;;; Code:

;;;###autoload
(defgroup smtlib-mode nil
  "SMTLIB mode."
  :prefix "smtlib-mode/"
  :group 'languages)

;;;###autoload
(defcustom smtlib-mode/solver-cmd "cvc5 --lang smt2"
  "Command to run SMT solver."
  :type 'string :group 'smtlib-mode)
(defcustom smtlib-mode/output-buffer-name "*SMT Solver Output*"
  "Name of a buffer into which SMT solver output is inserted."
  :type 'string :group 'smtlib-mode)

;; Define a sparse local keymap with default key bindings
(defvar smtlib-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'smtlib-mode/run-solver) map)
  "Keymap for smtlib-mode.")

;; Create syntax table, add characters as word components
(defvar smtlib-mode/syntax-table
  (let ((st (make-syntax-table lisp-mode-syntax-table)))
    ;; See p. 23
    (dolist (ch '(?~ ?! ?@ ?$ ?% ?^ ?& ?* ?_ ?- ?+ ?= ?< ?> ?. ?? ?/))
      (modify-syntax-entry ch "w" st))
    ;; The hash character is made to word component in order to be able to
    ;; highlight hexadecimal and binary constants.
    (modify-syntax-entry ?# "w" st)
    st))

;; Faces
(defvar smtlib-mode/operators-face 'font-lock-function-name-face
  "Face used by smtlib-mode to highlight operators.")
(defvar smtlib-mode/combinators-face 'font-lock-builtin-face
  "Face used by smtlib-mode to highlight combinators.")
(defvar smtlib-mode/cmds-face 'font-lock-keyword-face
  "Face used by smtlib-mode to highlight commands.")

;; Create the list for font-lock
(defvar
  smtlib-mode/font-lock
  (let* ((smtlib-keywords ;; Basic reserved words, p. 23
          '(
            "BINARY"
            "DECIMAL"
            "HEXADECIMAL"
            "NUMERAL"
            "STRING"
            "_"
            "!"
            "as"
            "let"
            "exists"
            "forall"
            "match"
            "par"
            ))
         (smtlib-cmds ;; Commands, p. 45
          '(
            "assert"
            "check-sat"
            "check-sat-assuming"
            "declare-const"
            "declare-datatype"
            "declare-datatypes"
            "declare-fun"
            "declare-sort"
            "define-fun"
            "define-fun-rec"
            "define-funs-rec"
            "define-sort"
            "echo"
            "exit"
            "get-assertions"
            "get-assignment"
            "get-info"
            "get-model"
            "get-option"
            "get-proof"
            "get-unsat-assumptions"
            "get-unsat-core"
            "get-value"
            "pop"
            "push"
            "reset"
            "reset-assertions"
            "set-info"
            "set-logic"
            "set-option"
            ))
         (smtlib-constants
          '(
            "true"
            "false"
            ))
         (smtlib-types
          '(
            "Int"
            "Bool"
            "Real"
            "Array"
            "String"
            ))
         (smtlib-combinators
          '(
            "or"
            "and"
            "xor"
            "=>"
            "not"
            "ite"
            "forall"
            "exists"
            "let"
            "!"
            ))
         (smtlib-operators
          '(
            ;; core (excluding those in `smtlib-combinators')
            "="
            "distinct"
            ;; int
            "+"
            "<"
            "<="
            ">"
            ">="
            "-"
            "*"
            "/"
            "div"
            "mod"
            "abs"
            ;; array
            "select"
            "store"
            ;; string
            "str.++"
            "str.len"
            "str.<"
            ;; regex
            "str.to_re"
            "str.in_re"
            "str.to.re"
            "str.in.re"
            "re.none"
            "re.all"
            "re.allchar"
            "re.++"
            "re.union"
            "re.inter"
            "re.*"
            ;; addtional string functions
            "str.<="
            "str.at"
            "str.substr"
            "str.prefixof"
            "str.suffixof"
            "str.contains"
            "str.indexof"
            "str.replace"
            "str.replace_all"
            "str.replace_re"
            "str.repalce_re_all"
            "re.comp"
            "re.diff"
            "re.+"
            "re.opt"
            "re.range"
            "re.^"
            "re.loop"
            ;; string <-> int
            "str.is_digit"
            "str.to_code"
            "str.from_code"
            "str.to_int"
            "str.from_int"
            ))
         ;; Create optimized regular expressions for commands, match only whole words
         (smtlib-keywords-regexp (regexp-opt smtlib-keywords 'words))
         (smtlib-constants-regexp (regexp-opt smtlib-constants 'words))
         (smtlib-types-regexp (regexp-opt smtlib-types 'words))
         (smtlib-combinators-regexp (regexp-opt smtlib-combinators 'words))
         (smtlib-cmds-regexp (regexp-opt smtlib-cmds 'words))
         (smtlib-operators-regexp (regexp-opt smtlib-operators 'symbols)))
    `(
      (,smtlib-keywords-regexp . font-lock-keyword-face)
      (,smtlib-constants-regexp . font-lock-constant-face)
      (,smtlib-types-regexp . font-lock-type-face)
      (,smtlib-operators-regexp . smtlib-mode/operators-face)
      (,smtlib-combinators-regexp . smtlib-mode/combinators-face)
      (,smtlib-cmds-regexp . smtlib-mode/cmds-face)
      ("\\b\\([0-9]*\\.?[0-9]+\\)\\b" . font-lock-constant-face)
      ("\\b#x\\([0-9a-fA-F]+\\)\\b" . font-lock-constant-face)
      ("\\b#b\\([0-1]+\\)\\b" . font-lock-constant-face)
      (":\\(\\sw+\\)" . font-lock-doc-face)
      ;; recognize logical constants decls/defs
      ("\\(?:declare-fun\\|declare-const\\|define-fun\\|define-fun-rec\\)\\(?:\\s-\\|\n\\)+\\(\\sw+\\)\\(?:\\s-\\|\n\\)+(\\(?:\\s-\\|\n\\)*)"
       (1 font-lock-variable-name-face))
      ;; recognize functions
      ("\\(?:declare-fun\\|define-fun\\|define-fun-rec\\)\\(?:\\s-\\|\n\\)+\\(\\sw+\\)"
       (1 font-lock-function-name-face))
      )))

;; Define the mode
;;;###autoload
(define-derived-mode smtlib-mode lisp-mode
  "SMTLIB"
  "Major mode for editing SMTLIB files."
  ;; Syntax table
  (set-syntax-table smtlib-mode/syntax-table)
  ;; Syntax highlighting
  (setq font-lock-defaults '((smtlib-mode/font-lock))))

(defun smtlib-mode/run-solver ()
  "Run the SMT solver on the buffer."
  (interactive)
  (let ((buffer smtlib-mode/output-buffer-name))
    (shell-command-on-region
     (if (region-active-p) (region-beginning) (point-min))
     (if (region-active-p) (region-end) (point-max))
     (read-shell-command "Run SMT solver: " smtlib-mode/solver-cmd)
     buffer)
    (with-current-buffer buffer (smtlib-mode))))

(add-to-list 'auto-mode-alist '("\\.smtlib$" . smtlib-mode))
(add-to-list 'auto-mode-alist '("\\.smt$" . smtlib-mode))
(add-to-list 'auto-mode-alist '("\\.smt2$" . smtlib-mode))

;; Need this as last line
(provide 'smtlib-mode)
;;; smtlib-mode.el ends here
