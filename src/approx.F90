!------------------------------------------------------------------------------
! TOHOKU Univ.
!------------------------------------------------------------------------------
!
! MODULE: approx
!
!> @author
!> Yuta SUZUKI (GitHub:@suzuyuyuyu)
!
! DESCRIPTION:
!> This module provides scalar and array approximate comparison functions for real(kind=sp), real(8), and real(kind=qp) types.
!> Default tolerance is set to 1.0d-10, where you can change it by passing an optional argument or rewrite the default values here directly.
!
! REVISION HISTORY:
! 2025/02/25 - Initial Version
! 2025/09/02 - Updated: Extract functions into a library
!------------------------------------------------------------------------------
module approx
  use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128, short=>int16, int=>int32, longlong=>int64
  implicit none
  private
  type tolerance_parameter
    real(kind=sp) :: asptol = 1.0e-10_sp, rsptol = 1.0e-10_sp
    real(kind=dp) :: adptol = 1.0e-10_dp, rdptol = 1.0e-10_dp
    real(kind=qp) :: aqptol = 1.0e-10_qp, rqptol = 1.0e-10_qp
  end type
  type(tolerance_parameter), parameter :: default = tolerance_parameter()

  interface aeq
    module procedure approx_eq_sp, approx_eq_dp, approx_eq_qp
    module procedure approx_eq_sp_tol, approx_eq_dp_tol, approx_eq_qp_tol
  end interface aeq
  interface sne
    module procedure strict_ne_sp, strict_ne_dp, strict_ne_qp
    module procedure strict_ne_sp_tol, strict_ne_dp_tol, strict_ne_qp_tol
  end interface sne
  interface slt
    module procedure strict_lt_sp, strict_lt_dp, strict_lt_qp
    module procedure strict_lt_sp_tol, strict_lt_dp_tol, strict_lt_qp_tol
  end interface slt
  interface ale
    module procedure approx_le_sp, approx_le_dp, approx_le_qp
    module procedure approx_le_sp_tol, approx_le_dp_tol, approx_le_qp_tol
  end interface ale
  interface sgt
    module procedure strict_gt_sp, strict_gt_dp, strict_gt_qp
    module procedure strict_gt_sp_tol, strict_gt_dp_tol, strict_gt_qp_tol
  end interface sgt
  interface age
    module procedure approx_ge_sp, approx_ge_dp, approx_ge_qp
    module procedure approx_ge_sp_tol, approx_ge_dp_tol, approx_ge_qp_tol
  end interface age

  public :: aeq, sne, slt, ale, sgt, age

  interface operator(.approx.)
    module procedure approx_eq_sp, approx_eq_dp, approx_eq_qp
  end interface operator(.approx.)
  interface operator(.aeq.)
    module procedure approx_eq_sp, approx_eq_dp, approx_eq_qp
  end interface operator(.aeq.)
  interface operator(.sne.)
    module procedure strict_ne_sp, strict_ne_dp, strict_ne_qp
  end interface operator(.sne.)
  interface operator(.slt.)
    module procedure strict_lt_sp, strict_lt_dp, strict_lt_qp
  end interface operator(.slt.)
  interface operator(.ale.)
    module procedure approx_le_sp, approx_le_dp, approx_le_qp
  end interface operator(.ale.)
  interface operator(.sgt.)
    module procedure strict_gt_sp, strict_gt_dp, strict_gt_qp
  end interface operator(.sgt.)
  interface operator(.age.)
    module procedure approx_ge_sp, approx_ge_dp, approx_ge_qp
  end interface operator(.age.)

  public :: operator(.approx.)
  public :: operator(.aeq.), operator(.sne.), operator(.slt.), operator(.ale.), operator(.sgt.), operator(.age.)

contains

  pure elemental function approx_eq_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default%asptol
  end function approx_eq_sp
  !
  pure elemental function approx_eq_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default%adptol
  end function approx_eq_dp
  !
  pure elemental function approx_eq_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default%aqptol
  end function approx_eq_qp
  !
  pure elemental function approx_eq_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .le. tol
  end function approx_eq_sp_tol
  !
  pure elemental function approx_eq_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .le. tol
  end function approx_eq_dp_tol
  !
  pure elemental function approx_eq_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .le. tol
  end function approx_eq_qp_tol
  ! ------------------------------------------
  pure elemental function strict_ne_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default%asptol
  end function strict_ne_sp
  !
  pure elemental function strict_ne_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default%adptol
  end function strict_ne_dp
  !
  pure elemental function strict_ne_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default%aqptol
  end function strict_ne_qp
  !
  pure elemental function strict_ne_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .gt. tol
  end function strict_ne_sp_tol
  !
  pure elemental function strict_ne_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .gt. tol
  end function strict_ne_dp_tol
  !
  pure elemental function strict_ne_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .gt. tol
  end function strict_ne_qp_tol
  ! ------------------------------------------
  pure elemental function strict_lt_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default%asptol
  end function strict_lt_sp
  !
  pure elemental function strict_lt_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default%adptol
  end function strict_lt_dp
  !
  pure elemental function strict_lt_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default%aqptol
  end function strict_lt_qp
  !
  pure elemental function strict_lt_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = a .lt. b - tol
  end function strict_lt_sp_tol
  !
  pure elemental function strict_lt_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = a .lt. b - tol
  end function strict_lt_dp_tol
  !
  pure elemental function strict_lt_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = a .lt. b - tol
  end function strict_lt_qp_tol
  ! ------------------------------------------
  pure elemental function approx_le_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default%asptol
  end function approx_le_sp
  !
  pure elemental function approx_le_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default%adptol
  end function approx_le_dp
  !
  pure elemental function approx_le_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default%aqptol
  end function approx_le_qp
  !
  pure elemental function approx_le_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = a .le. b + tol
  end function approx_le_sp_tol
  !
  pure elemental function approx_le_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = a .le. b + tol
  end function approx_le_dp_tol
  !
  pure elemental function approx_le_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = a .le. b + tol
  end function approx_le_qp_tol
  ! ------------------------------------------
  pure elemental function strict_gt_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default%asptol
  end function strict_gt_sp
  !
  pure elemental function strict_gt_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default%adptol
  end function strict_gt_dp
  !
  pure elemental function strict_gt_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default%aqptol
  end function strict_gt_qp
  !
  pure elemental function strict_gt_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = a .gt. b + tol
  end function strict_gt_sp_tol
  !
  pure elemental function strict_gt_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = a .gt. b + tol
  end function strict_gt_dp_tol
  !
  pure elemental function strict_gt_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = a .gt. b + tol
  end function strict_gt_qp_tol
  ! ------------------------------------------
  pure elemental function approx_ge_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default%asptol
  end function approx_ge_sp
  !
  pure elemental function approx_ge_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default%adptol
  end function approx_ge_dp
  !
  pure elemental function approx_ge_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default%aqptol
  end function approx_ge_qp
  !
  pure elemental function approx_ge_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = a .ge. b - tol
  end function approx_ge_sp_tol
  !
  pure elemental function approx_ge_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = a .ge. b - tol
  end function approx_ge_dp_tol
  !
  pure elemental function approx_ge_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = a .ge. b - tol
  end function approx_ge_qp_tol
end module approx
