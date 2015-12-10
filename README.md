# fxrd-mode
Emacs major modes for working with fixed-field-width files (NACHA, TSO6, RM37,
etc.)

Copyright (C) 2015 Marc Sherry

## Summary
This package implements `fxrd-mode`, a major mode for editing files with fixed
field widths. These files are commonly used in the financial industry, such
as in ACH transactions. This package provides:

- `nacha-mode`: a mode for editing NACHA (ACH transaction) files
- `rm37-mode`: a mode for editing RM37 (Mastercard rebate transaction) files
- `tso6-mode`: a mode for editing TSO6 (Mastercard rebate confirmation) files

## Usage
In each of these modes, the following commands are available:

- `M-<right>` `(next-field)` and `M-<left>` `(previous-field)` move to the next
 and previous fields, respectively.

This mode also provides validation of fields -- valid fields are highlighted
with `fxrd-current-field-face`, while invalid fields are highlighted with
`fxrd-invalid-field-face` (both customizable). The current field name is
also displayed in the modeline.

### Examples

Current field highlighted:
![Current field highlighted](http://i.imgur.com/NTZb6Fl.png)

Current field invalid:
![Current field invalid](http://i.imgur.com/EStvzML.png)

All invalid fields highlighted:
![ALl invalid fields highlighted](http://i.imgur.com/ig9BnNZ.png)

## Installation

Installation via MELPA is easiest.
