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
!> Default tolerance is set to 1.0d-7, where you can change it by passing an optional argument.
!
! REVISION HISTORY:
! 2025/02/25 - Initial Version
!------------------------------------------------------------------------------
module approx
  use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128, short=>int16, int=>int32, longlong=>int64
  implicit none
  private
  type tolerance_parameter
    real(kind=sp) :: stol = 1.0e-10_sp
    real(kind=dp) :: dtol = 1.0e-10_dp
    real(kind=qp) :: qtol = 1.0e-10_qp
  end type
  type(tolerance_parameter), parameter :: default = tolerance_parameter()

  interface aeq
    module procedure approx_eq_sp, approx_eq_dp, approx_eq_qp
    module procedure approx_eq_sp_tol, approx_eq_dp_tol, approx_eq_qp_tol
  end interface aeq
  interface ane
    module procedure approx_ne_sp, approx_ne_dp, approx_ne_qp
    module procedure approx_ne_sp_tol, approx_ne_dp_tol, approx_ne_qp_tol
  end interface ane
  interface alt
    module procedure approx_lt_sp, approx_lt_dp, approx_lt_qp
    module procedure approx_lt_sp_tol, approx_lt_dp_tol, approx_lt_qp_tol
  end interface alt
  interface ale
    module procedure approx_le_sp, approx_le_dp, approx_le_qp
    module procedure approx_le_sp_tol, approx_le_dp_tol, approx_le_qp_tol
  end interface ale
  interface agt
    module procedure approx_gt_sp, approx_gt_dp, approx_gt_qp
    module procedure approx_gt_sp_tol, approx_gt_dp_tol, approx_gt_qp_tol
  end interface agt
  interface age
    module procedure approx_ge_sp, approx_ge_dp, approx_ge_qp
    module procedure approx_ge_sp_tol, approx_ge_dp_tol, approx_ge_qp_tol
  end interface age

  public :: aeq, ane, alt, ale, agt, age

  interface operator(.approx.)
    module procedure approx_eq_sp_op, approx_eq_dp_op, approx_eq_qp_op
  end interface operator(.approx.)
  interface operator(.aeq.)
    module procedure approx_eq_sp_op, approx_eq_dp_op, approx_eq_qp_op
  end interface operator(.aeq.)
  interface operator(.ane.)
    module procedure approx_ne_sp_op, approx_ne_dp_op, approx_ne_qp_op
  end interface operator(.ane.)
  interface operator(.alt.)
    module procedure approx_lt_sp_op, approx_lt_dp_op, approx_lt_qp_op
  end interface operator(.alt.)
  interface operator(.ale.)
    module procedure approx_le_sp_op, approx_le_dp_op, approx_le_qp_op
  end interface operator(.ale.)
  interface operator(.agt.)
    module procedure approx_gt_sp_op, approx_gt_dp_op, approx_gt_qp_op
  end interface operator(.agt.)
  interface operator(.age.)
    module procedure approx_ge_sp_op, approx_ge_dp_op, approx_ge_qp_op
  end interface operator(.age.)

  public :: operator(.approx.)
  public :: operator(.aeq.), operator(.ane.), operator(.alt.), operator(.ale.), operator(.agt.), operator(.age.)

contains

  pure elemental function approx_eq_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default%stol
  end function approx_eq_sp
  !
  pure elemental function approx_eq_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default%dtol
  end function approx_eq_dp
  !
  pure elemental function approx_eq_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default%qtol
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
  pure elemental function approx_ne_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default%stol
  end function approx_ne_sp
  !
  pure elemental function approx_ne_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default%dtol
  end function approx_ne_dp
  !
  pure elemental function approx_ne_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default%qtol
  end function approx_ne_qp
  !
  pure elemental function approx_ne_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .gt. tol
  end function approx_ne_sp_tol
  !
  pure elemental function approx_ne_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .gt. tol
  end function approx_ne_dp_tol
  !
  pure elemental function approx_ne_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = abs(a - b) .gt. tol
  end function approx_ne_qp_tol
  ! ------------------------------------------
  pure elemental function approx_lt_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default%stol
  end function approx_lt_sp
  !
  pure elemental function approx_lt_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default%dtol
  end function approx_lt_dp
  !
  pure elemental function approx_lt_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default%qtol
  end function approx_lt_qp
  !
  pure elemental function approx_lt_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = a .lt. b - tol
  end function approx_lt_sp_tol
  !
  pure elemental function approx_lt_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = a .lt. b - tol
  end function approx_lt_dp_tol
  !
  pure elemental function approx_lt_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = a .lt. b - tol
  end function approx_lt_qp_tol
  ! ------------------------------------------
  pure elemental function approx_le_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default%stol
  end function approx_le_sp
  !
  pure elemental function approx_le_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default%dtol
  end function approx_le_dp
  !
  pure elemental function approx_le_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default%qtol
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
  pure elemental function approx_gt_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default%stol
  end function approx_gt_sp
  !
  pure elemental function approx_gt_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default%dtol
  end function approx_gt_dp
  !
  pure elemental function approx_gt_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default%qtol
  end function approx_gt_qp
  !
  pure elemental function approx_gt_sp_tol(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), intent(in) :: tol
    logical :: res
    res = a .gt. b + tol
  end function approx_gt_sp_tol
  !
  pure elemental function approx_gt_dp_tol(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), intent(in) :: tol
    logical :: res
    res = a .gt. b + tol
  end function approx_gt_dp_tol
  !
  pure elemental function approx_gt_qp_tol(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), intent(in) :: tol
    logical :: res
    res = a .gt. b + tol
  end function approx_gt_qp_tol
  ! ------------------------------------------
  pure elemental function approx_ge_sp(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default%stol
  end function approx_ge_sp
  !
  pure elemental function approx_ge_dp(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default%dtol
  end function approx_ge_dp
  !
  pure elemental function approx_ge_qp(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default%qtol
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
  ! _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  ! operator(.approx.)
  pure elemental function approx_eq_sp_op(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = aeq(a, b)
  end function approx_eq_sp_op
  !
  pure elemental function approx_eq_dp_op(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = aeq(a, b)
  end function approx_eq_dp_op
  !
  pure elemental function approx_eq_qp_op(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = aeq(a, b)
  end function approx_eq_qp_op
  ! ------------------------------------------
  pure elemental function approx_ne_sp_op(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = ane(a, b)
  end function approx_ne_sp_op
  !
  pure elemental function approx_ne_dp_op(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = ane(a, b)
  end function approx_ne_dp_op
  !
  pure elemental function approx_ne_qp_op(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = ane(a, b)
  end function approx_ne_qp_op
  ! ------------------------------------------
  pure elemental function approx_lt_sp_op(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = alt(a, b)
  end function approx_lt_sp_op
  !
  pure elemental function approx_lt_dp_op(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = alt(a, b)
  end function approx_lt_dp_op
  !
  pure elemental function approx_lt_qp_op(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = alt(a, b)
  end function approx_lt_qp_op
  ! ------------------------------------------
  pure elemental function approx_le_sp_op(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = ale(a, b)
  end function approx_le_sp_op
  !
  pure elemental function approx_le_dp_op(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = ale(a, b)
  end function approx_le_dp_op
  !
  pure elemental function approx_le_qp_op(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = ale(a, b)
  end function approx_le_qp_op
  ! ------------------------------------------
  pure elemental function approx_gt_sp_op(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = agt(a, b)
  end function approx_gt_sp_op
  !
  pure elemental function approx_gt_dp_op(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = agt(a, b)
  end function approx_gt_dp_op
  !
  pure elemental function approx_gt_qp_op(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = agt(a, b)
  end function approx_gt_qp_op
  ! ------------------------------------------
  pure elemental function approx_ge_sp_op(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = age(a, b)
  end function approx_ge_sp_op
  !
  pure elemental function approx_ge_dp_op(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = age(a, b)
  end function approx_ge_dp_op
  !
  pure elemental function approx_ge_qp_op(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = age(a, b)
  end function approx_ge_qp_op
end module approx
