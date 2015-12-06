# fxrd-mode
Emacs major mode for working fixed-field-width files (NACHA, TSO6, RM39, etc.)

Copyright (C) 2015 Marc Sherry

## Summary
This package implements fxrd mode, a major mode for editing files with fixed
field widths. These files are commonly used in the financial industry, such
as in ACH transactions. This package provides:

nacha-mode: a mode for editing NACHA files
tso6-mode: a mode for editing TSO6 files

## Usage
In each of these modes, the following commands are available:

- `M-<right>` `(next-field)` and `M-<left>` `(previous-field)` move to the next
 and previous fields, respectively.

This mode also provides validation of fields -- valid fields are highlighted
with `fxrd-current-field-face`, while invalid fields are highlighted with
`fxrd-invalid-field-face` (both customizable)

## Installation

Put this file where Emacs can find it (on your `load-path` somewhere), and
add the following to your .emacs/init.el file:

```lisp
(require 'fxrd-autoloads)
```
