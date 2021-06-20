# CL-FORM-TYPES

This library provides functions for determining the value types of
Common Lisp forms, based on type information contained in the
environment.

In order for this library to work the values types of variables and
return types of functions have to be declared.

Macros and symbol-macros are fully expanded and all special
forms, except `CATCH`, are supported.

This library depends on
[cl-environments](https://alex-gutev.github.io/cl-environments/) in
order to extract information from the environment using the CLTL2
API. Check the library's documentation for details on how to ensure
that information contained in the environment can be retrieved, across
all implementations.

## Package CL-FORM-TYPES

### FORM-TYPE

Function `FORM-TYPE FORM ENV &KEY CONSTANT-EQL-TYPES EXPAND-COMPILER-MACROS`

Determine the type of a form in a given environment.

* `FORM` - The form of which to determine the type.
* `ENV` - The environment in which the form is found.
* `CONSTANT-EQL-TYPES` - If true an `EQL` type specifier is returned
  for all forms which evaluate to constant values. Otherwise (the
  default) an `EQL` type specifier is returned only for those forms
  which evaluate to a constant comparable with `EQL`.
* `EXPAND-COMPILER-MACROS` - If true compiler-macros are expanded in
  `FORM` and its subforms before determining the form type.

Returns the type specifier of the value to which `FORM` evaluates. If
`FORM` evaluates to multiple values a `(VALUES ...)` type is
returned. If the type of `FORM` could not be determined `T` is
returned.

### NTH-FORM-TYPE

Function `NTH-FORM-TYPE FORM ENV &OPTIONAL (N 0) CONSTANT-EQL-TYPES EXPAND-COMPILER-MACROS`

Determine the type of the nth return value of a form.

* `FORM` - The form of which to determine the type.
* `ENV` - The environment in which the form is found.
* `N` - Index of the value, type of which, to return
* `CONSTANT-EQL-TYPES` - If true an `EQL` type specifier is returned
  for all forms which evaluate to constant values. Otherwise (the
  default) an `EQL` type specifier is returned only for those forms
  which evaluate to a constant comparable with `EQL`.
* `EXPAND-COMPILER-MACROS` - If true compiler-macros are expanded in
  `FORM` and its subforms before determining the form type.

Returns the type specifier of the `N`th value returned by `FORM`. If
`FORM` returns less values than `N`, `NIL` is returned.

### NTH-VALUE-TYPE

Function `NTH-VALUE-TYPE TYPE &OPTIONAL (N 0)`

Extract the type of the nth return value from a `VALUES` type
specifier.

* `TYPE` - A type specifier. If not a `VALUES` type specifier it is
  treated as a `VALUES` type specifier of a single value.
* `N` - Index of the value of which to retrieve the type.

Returns the nth value type or `NIL` if `N` is greater than the number
values in the type specifier.

### FORM-TYPES

Function `FORM-TYPES FORMS ENV &KEY CONSTANT-EQL-TYPES EXPAND-COMPILER-MACROS`

Determine the type of each form in a list.

* `FORMS` - List of forms of which to determine the types.
* `ENV` - Environment in which the forms are found.
* `CONSTANT-EQL-TYPES` - If true an `EQL` type specifier is returned
  for all forms which evaluate to constant values. Otherwise (the
  default) an `EQL` type specifier is returned only for those forms
  which evaluate to a constant comparable with `EQL`.
* `EXPAND-COMPILER-MACROS` - If true compiler-macros are expanded in
  `FORMS` and their subforms before determining the form types.

Returns a list where each element is the type specifier of the
corresponding form in `FORMS`.

### CUSTOM-FORM-TYPE

Generic Function `CUSTOM-FORM-TYPE OPERATOR ARGUMENTS ENV`

Generic function for determining the type of non-standard special
forms and function calls.

This function is not intended to be called directly but is intended to
be extended with methods to add custom type deduction logic for
function calls and non-standard special forms.

* `OPERATOR` - Expression operator
* `ARGUMENTS` - List containing the expression's argument forms
* `ENV` - Environment in which the form is found.

**NOTE:** The environment argument is not necessarily a native
environment object, but may be an augmented environment object.
Therefore it should not be passed directly to built in functions which
expect an environment object but rather the equivalent functions from
the `CL-ENVIRONMENTS-CL` package should be used, see
<https://alex-gutev.github.io/cl-environments/#wrapper_functions>.

Returns the value type of the form `(OPERATOR . ARGUMENTS)`, or `T` if
it's type could not be determined.

### \*HANDLE-SB-LVARS\*

Variable: `*HANDLE-SB-LVARS*`

Flag for whether SBCL's `SB-C::LVAR` structures should be recognized.

If true and an `SB-C::LVAR` structure is encountered as a constant,
the type returned is the _derived type_ of the `LVAR`.

If NIL `SB-C::LVAR`'s are treated as literal constant objects, and the
type returned is either an `EQL` type specifier or `LVAR` depending on
the value of the `CONSTANT-EQL-TYPES` argument to `FORM-TYPE` /
`NTH-FORM-TYPE`.

**NOTE:** The value of this variable only has an effect on SBCL.

### MALFORMED-FORM-ERROR

Condition `MALFORMED-FORM-ERROR (PROGRAM-ERROR)`

Condition signalled when a malformed form is passed to one of the
`FORM-TYPE` functions.

**NOTE:** Inherits from the `PROGRAM-ERROR` condition which may also
be signalled. Thus if you want to handle all conditions which may be
signalled by `FORM-TYPE`, handle the `PROGRAM-ERROR` condition.

Slots:

* `FORM` - The malformed form.

## UNKNOWN-SPECIAL-OPERATOR

Condition `UNKNOWN-SPECIAL-OPERATOR (PROGRAM-ERROR)`

Condition signalled when an unknown special operator is encountered.

**NOTE:** This indicates the use of a non-standard,
implementation-specific, special operator. This condition will never
be raised for a standard CL operator.

Slots:

* `OPERATOR` - The special operator.
* `OPERANDS` - The operands to the operator.


### RETURN-DEFAULT-TYPE

Function `RETURN-DEFAULT-TYPE &OPTIONAL (TYPE T)`

Invoke the `RETURN-DEFAULT-TYPE` restart.

This restart, for a `MALFORMED-FORM-ERROR` condition, returns the type
`TYPE` as the form type for the malformed form.

## Code Walker

The package `CL-FORM-TYPES.WALKER` exposes the (mostly) portable
code-walker used by the `FORM-TYPE` functions.

### WALK-FORM

Function: `WALK-FORM FN FORM ENV &KEY (RESULT-TYPE 'LIST)`

Apply a function on a form and each of its subforms, and return the
resulting form.

* `FN`

  Function of two arguments to apply on each subform of `FORM`.

  The function is passed to arguments: the form and the environment in
  which it occurs.

  It should return the following values:

  1. The new form. The subforms of this form are walked and it is
     subtituted in place of the old form, in the result returned by
     `WALK-FORM`.

  2. A Boolean flag. If true the subforms of the form returned in (1)
     are not walked further, and the form is substituted as it is in
     the result. Otherwise the subforms are walked.

* `FORM`

  The form to walk. `FN` is first applied on `FORM` itself,
  then on its subforms.

* `ENV`

   The environment in which `FORM` is found.

* `RESULT-TYPE`

   A symbol indicating the type of result that should be returned from
   `WALK-FORM`:

   * `LIST`

      The new form, built out of replacing each subform with the
      result returned by `FN`, is returned. This is the default.

   * `NIL`

      No new form is constructed, meaning the return value of `FN` is
      used only to determine which forms to walk next.

Returns the new transformed form, if `RESULT-TYPE` is `LIST`.

## Type Deduction Logic

This section contains notes on how types are deduced for various
form types.

### LAMBDA Expressions

This refers to lambda expressions occurring within `FUNCTION` forms
and as the operator (the `CAR`) of an expression.

#### In FUNCTION Form

When the argument to a `FUNCTION` form is a `LAMBDA` expression, a
list-form `FUNCTION` type is returned with argument and return value
types.

The number of arguments is deduced from the expression's lambda-list,
with the types deduced from type declarations immediately within the
expression body. If the type cannot be determined for an argument `*`
is placed in its position within the function type specifier.

The return value type is deduced from the return value type of the
last form in the expression body. If this cannot be determined, `T` is
placed as the return value type in the function type specifier.

**Examples:**

```lisp

(form-type
  '#'(lambda (x &optional y &key z)
       (pprint y)
       (pprint z)
       x)
   nil)

;; Returns

(function (* &optional * &key (:z *)) t)
```

A more detailed type is returned when the types of the argument
variables are declared:

```lisp
(form-type
  '#'(lambda (x &optional y &key z)
       (declare (type number x)
                (type symbol y)
                (type string z))

       (pprint y)
       (pprint z)
       x)
   NIL)

;; Returns

(function (number &optional symbol &key (:z string)) number)
```


#### As Form Operator

When a lambda expression appears as the operator in a form, the type
of the lambda expression is deduced and the return type of the
`FUNCTION` type specifier, is returned as the type of the form.


## BLOCK forms

The type of a `BLOCK` form is deduced not only by the type of the last
form in its body but also by analysing the forms within the body of
the `BLOCK` form and combining the types of the result forms in each
`RETURN-FROM` form, that has a block name that matches the name of the
`BLOCK` form.

`RETURN-FROM` forms which appear within a lexically defined function,
by `FLET` or `LABELS`, are only included in the analysis if that
lexically defined function is called or referenced by a `FUNCTION`
form.

**Examples:**

```lisp
(form-type
 '(block blk
    (do-something 1)
    (do-something 2)
    (the number (+ x y)))

  NIL)

;; Returns

number
```

With a `RETURN-FROM` form:

```lisp
(form-type
 '(block blk
    (when (evenp x)
       (return-from blk 'even))

    (the number (+ x y)))

  NIL)

;; Returns

(or number (eql even))
```

With a `RETURN-FROM` inside a lexically defined function which is not
called:

```lisp
(form-type
 '(block blk
    (flet ((test (x)
             (when (evenp x)
               (return-from blk 'even))
             x))

     (the number (+ x y))))

  NIL)

;; Returns

number
```

**NOTE:** The type of the `RETURN-FROM` form inside the `TEST` function is
not included since the function is never called.

With a `RETURN-FROM` inside a lexically defined function which is
referenced:

```lisp
(form-type
 '(block blk
    (flet ((test (x)
             (when (evenp x)
               (return-from blk 'even))
             x))

     (the list (mapcar #'test numbers))))

  NIL)

;; Returns

(or list (eql even))
```

**NOTE:** The type of the `RETURN-FROM` form inside the `TEST`
function is included despite the function never being called since the
function is referenced with `(FUNCTION TEST)` (`#'TEST`), thus it
could potentially be called.

## CATCH forms

No type deduction logic is performed on `CATCH` forms as of yet, due
to the difficulty of the problem posed by dynamic tag names. Thus,
`FORM-TYPE` simply returns `T` when given a `CATCH` form.
