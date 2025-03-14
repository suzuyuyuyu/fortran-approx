# fortran-approx

> [!NOTE]
> Although approx.f90 has already been implemented, it has not been tested yet.

## Description
This repository distributes **approx.f90**.
It is a simple Fortran program that enables approximate comparisons, such as `.eq.`, `.ne.`, `.lt.`, `.le.`, `.gt.`, and `.ge.`.
The tolerance is set to 1.0e-7 by default, but it can be changed by the user—either by modifying the module or by passing an argument to the function.

In addition, custom operators such as `.aeq.`, `.ane.`, `.alt.`, `.ale.`, `.agt.`, and `.age.` are defined.
These operators perform comparisons that take the specified tolerance into account.
The same tolerance value is used for these operators, and it can only be changed by modifying the module.

## Implemented functions/operators
### Functions
- `approx_eq(x, y, tol)`
- `approx_ne(x, y, tol)`
- `approx_lt(x, y, tol)`
- `approx_le(x, y, tol)`
- `approx_gt(x, y, tol)`
- `approx_ge(x, y, tol)`

### Operators
- `.approx.`
- `.aeq.` (the same as `.approx.`)
- `.ane.`
- `.alt.`
- `.ale.`
- `.agt.`
- `.age.`

## Roadmap
- [ ] ...?
