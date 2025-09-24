# fortran-approx

## Description
This repository provides **approx.F90**, a simple Fortran module.
It enables approximate comparisons analogous to the standard relational operators `.eq.`, `.ne.`, `.lt.`, `.le.`, `.gt.`, and `.ge.`.
The default tolerance is 1.0e-10, and it can be changed either by modifying the module or by passing an argument to the function.

In addition, the following custom operators are provided: `.aeq.`, `.sne.`, `.slt.`, `.ale.`, `.sgt.`, and `.age.`
These operators perform comparisons that honor the module's tolerance.
They use the same tolerance value, which can be changed only by modifying the module.

## Available functions and operators
### Functions
- `approx_eq(x, y, tol)`
- `strict_ne(x, y, tol)`
- `strict_lt(x, y, tol)`
- `approx_le(x, y, tol)`
- `strict_gt(x, y, tol)`
- `approx_ge(x, y, tol)`

The argument `tol` can be ignored.

### Operators
- `.approx.`
- `.aeq.` (alias of `.approx.`)
- `.sne.`
- `.slt.`
- `.ale.`
- `.sgt.`
- `.age.`

## Roadmap
- [ ] Provide variants without optional arguments to reduce runtime overhead
- [ ] Publish as a static/shared library
- [ ] Set the default tolerance to a combined absolute + relative form (abs_tol + rel_tol style)
