# fortran-approx

## Description

A Fortran module providing approximate comparison functions for floating-point numbers.
Analogues of the standard relational operators (`.eq.`, `.ne.`, `.lt.`, `.le.`, `.gt.`, `.ge.`) are provided in two tolerance modes:

| Mode | Formula | When to use |
|---|---|---|
| **Absolute** | `|a - b| <= tol` | Values of similar magnitude near 1.0 |
| **Hybrid** | `|a - b| <= atol + rtol * max(|a|, |b|)` | Values across different scales |

All functions are `pure elemental` and work on both scalars and arrays.
Supported types: `real32` (sp), `real64` (dp), `real128` (qp).

Default tolerances (absolute and relative): `1.0e-10` for all types.

---

## API

### Absolute tolerance

| Function | Operator | Description |
|---|---|---|
| `aeq(a, b [,tol])` | `.aeq.` / `.approx.` | Approximate equal: `|a-b| <= tol` |
| `sne(a, b [,tol])` | `.sne.` | Strict not equal: `|a-b| > tol` |
| `slt(a, b [,tol])` | `.slt.` | Strict less than: `a < b - tol` |
| `ale(a, b [,tol])` | `.ale.` | Approx less or equal: `a <= b + tol` |
| `sgt(a, b [,tol])` | `.sgt.` | Strict greater than: `a > b + tol` |
| `age(a, b [,tol])` | `.age.` | Approx greater or equal: `a >= b - tol` |

When `tol` is omitted, `default%adptol = 1.0e-10` (or sp/qp equivalent) is used.

### Hybrid tolerance

| Function | Operator | Description |
|---|---|---|
| `haeq(a, b [,atol, rtol])` | `.haeq.` | Approximate equal |
| `hsne(a, b [,atol, rtol])` | `.hsne.` | Strict not equal |
| `hslt(a, b [,atol, rtol])` | `.hslt.` | Strict less than |
| `hale(a, b [,atol, rtol])` | `.hale.` | Approx less or equal |
| `hsgt(a, b [,atol, rtol])` | `.hsgt.` | Strict greater than |
| `hage(a, b [,atol, rtol])` | `.hage.` | Approx greater or equal |

When `atol`/`rtol` are omitted, `default%adptol` and `default%rdptol` are used.
Operators use the default hybrid tolerance.

---

## Usage

```fortran
use approx
use iso_fortran_env, only: dp=>real64

real(dp) :: a, b, arr(3)

! absolute tolerance (default 1e-10)
if (aeq(a, b)) ...
if (a .aeq. b) ...
if (aeq(a, b, 1.0e-8_dp)) ...   ! explicit tol

! hybrid tolerance (default atol=1e-10, rtol=1e-10)
if (haeq(a, b)) ...
if (a .haeq. b) ...
if (haeq(a, b, 1.0e-6_dp, 1.0e-9_dp)) ...   ! explicit atol, rtol

! elemental: works on arrays
logical :: mask(3)
mask = aeq(arr, 1.0_dp)
mask = haeq(arr, 1.0_dp, 1.0e-8_dp, 1.0e-10_dp)
```

---

## Build

Requires CMake >= 3.21 and a Fortran compiler (gfortran or ifx).

### Using presets (recommended)

```sh
# GNU Fortran
cmake --preset gnu
cmake --build --preset gnu
ctest --preset gnu
cmake --install build/gnu

# Intel Fortran (ifx)
cmake --preset intel-llvm
cmake --build --preset intel-llvm
ctest --preset intel-llvm
cmake --install build/intel-llvm
```

### Manual

```sh
cmake -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=./install -S . -B build
cmake --build build
ctest --test-dir build
cmake --install build
```

### Install layout

```
install/
├── include/
│   └── approx.mod
└── lib64/
    ├── libapprox.a
    └── cmake/approx/
        ├── approxConfig.cmake
        ├── approxConfigVersion.cmake
        └── approxTargets.cmake
```

### Using as a CMake dependency

```cmake
find_package(approx 1.0 REQUIRED)
target_link_libraries(myapp PRIVATE approx::approx)
```
