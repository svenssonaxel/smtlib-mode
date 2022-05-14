smtlib-mode
===========

An Emacs major mode to edit and run SMTLIB v2 files

This mode is based on lisp-mode and provides highlighting of SMTLIB v2 commands, as well as the command run-solver (bound to `C-c C-c`) to run an SMT solver on the buffer or the region.

Add the following to your `.emacs` file:
```
(autoload 'smtlib-mode "smtlib-mode" "Major mode for SMTLIB" t)
```

The command to run the SMT solver is by default "cvc4 --lang smt2", modify and add the following line to your .emacs to change.

```
(setq smtlib-mode/solver-cmd "cvc4 --lang smt2")
```
