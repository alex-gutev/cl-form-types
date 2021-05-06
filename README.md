# CL-FORM-TYPES

This library provides functions for determine the value types of
Common Lisp forms, based on type information stored in the
environment.

Currently the types of the following forms can be determined:

* Literal values
* Symbols naming variables for which there are `TYPE` declarations
* Symbols naming constants
* Lists where the `CAR` is a function for which there is an `FTYPE`
  declaration
* `THE` forms

Macros and symbol-macros are fully expanded and the following special
forms are supported:

* `EVAL-WHEN`
* `FLET`
* `FUNCTION`
* `GO`
* `IF`
* `LABELS`
* `LET`
* `LET*`
* `LOAD-TIME-VALUE`
* `LOCALLY`
* `MACROLET`
* `MULTIPLE-VALUE-PROG1`
* `PROGN`
* `PROGV`
* `QUOTE`
* `RETURN-FROM`
* `SETQ`
* `SYMBOL-MACROLET`
* `TAGBODY`
* `THE`
* `THROW`
* `UNWIND-PROTECT`

This library depends on
[cl-environments](https://alex-gutev.github.io/cl-environments/) in
order to extract information from the environment using the CLTL2
API. Check the library's documentation for details on how to use so
that the type information is available across all implementations, and
details on its limitations.

## Package CL-FORM-TYPES

### FORM-TYPE

Function `FORM-TYPE FORM ENV &KEY CONSTANT-EQL-TYPES EXPAND-COMPILER-MACROS`

Determine the type of a form in a given environment.

* `FORM` - The form of which to determine the type
* `ENV` - The environment in which the form is found
* `CONSTANT-EQL-TYPES` - If true an `EQL` type specifier is returned
  for all forms which evaluate to constant values. Otherwise (the
  default) an `EQL` type specifier is returned only for those forms
  which evaluate to a constant comparable with `EQL`.
* `EXPAND-COMPILER-MACROS` - If true compiler-macros are expanded in
  FORM and its subforms before determining form types.

Returns the type specifier of the value to which `FORM` evaluates. If
`FORM` evaluates to multiple values a `(VALUES ...)` type is
returned. If the type of `FORM` could not be determined `T` is
returned.

### NTH-FORM-TYPE

Function `NTH-FORM-TYPE FORM ENV &OPTIONAL (N 0) CONSTANT-EQL-TYPES EXPAND-COMPILER-MACROS`

Determine the type of the nth return value of a form.

* `FORM` - The form of which to determine the type
* `ENV` - The environment in which the form is found
* `N` - Index of the value, type of which, to return
* `CONSTANT-EQL-TYPES` - If true an `EQL` type specifier is returned
  for all forms which evaluate to constant values. Otherwise (the
  default) an `EQL` type specifier is returned only for those forms
  which evaluate to a constant comparable with `EQL`.
* `EXPAND-COMPILER-MACROS` - If true compiler-macros are expanded in
  FORM and its subforms before determining form types.

Returns the type specifier of the `N`th value returned by `FORM`. If
`FORM` only returns a single value or returns less values than `N`,
`NIL` is returned.

### NTH-VALUE-TYPE

Function `NTH-VALUE-TYPE TYPE &OPTIONAL (N 0)`

Extract the type of the nth return value from a `VALUES` type
specifier.

* `TYPE` - A type specifier. If not a `VALUES` type specifier it is
  treated as a `VALUES` type specifier with of a single return value.
* `N` - Index of the value of which to retrieve the type.

Returns the nth value type or NIL if there is no information about the
nth return value, that is the actual number of values in the type
specifier is less than `N`.

### FORM-TYPES

Function `FORM-TYPES FORMS ENV &KEY CONSTANT-EQL-TYPES EXPAND-COMPILER-MACROS`

Determine the type of each form in a list.

* `FORMS` - List of forms of which to determine the types
* `ENV` - Environment in which the forms are found
* `CONSTANT-EQL-TYPES` - If true an `EQL` type specifier is returned
  for all forms which evaluate to constant values. Otherwise (the
  default) an `EQL` type specifier is returned only for those forms
  which evaluate to a constant comparable with `EQL`.
* `EXPAND-COMPILER-MACROS` - If true compiler-macros are expanded in
  FORM and its subforms before determining form types.

Returns a list where each element is the type specifier of the
corresponding form in `FORMS`.

### MALFORMED-FORM-ERROR

Condition `MALFORMED-FORM-ERROR`

Condition signalled when a malformed form is passed to one of the
`FORM-TYPE` functions.

Slots:

* `FORM` - The malformed form.

### RETURN-DEFAULT-TYPE

Function `RETURN-DEFAULT-TYPE &OPTIONAL (TYPE T)`

Invoke the `RETURN-DEFAULT-TYPE` restart.

This restart, for a `MALFORMED-FORM-ERROR` condition, returns the type
`TYPE` as the form type for the malformed form.
