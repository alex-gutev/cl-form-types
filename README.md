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

Function `FORM-TYPE FORM ENV`

Determine the type of a form in a given environment.

* `FORM` - The form of which to determine the type
* `ENV` - The environment in which the form is found

Returns the type specifier of the value to which `FORM` evaluates. If
`FORM` evaluates to multiple values a `(VALUES ...)` type is
returned. If the type of `FORM` could not be determined `T` is
returned.

### NTH-FORM-TYPE

Function `NTH-FORM-TYPE FORM ENV &OPTIONAL (N 0)`

Determine the type of the nth return value of a form.

* `FORM` - The form of which to determine the type
* `ENV` - The environment in which the form is found
* `N` - Index of the value, type of which, to return

Returns the type specifier of the `N`th value returned by `FORM`. If
`FORM` only returns a single value or returns less values than `N`,
`NIL` is returned.

### FORM-TYPES

Function `FORM-TYPES FORMS ENV`

Determine the type of each form in a list.

* `FORMS` - List of forms of which to determine the types
* `ENV` - Environment in which the forms are found

Returns a list where each element is the type specifier of the
corresponding form in `FORMS`.


