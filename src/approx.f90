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
  real(kind=sp), parameter :: default_stol = 1.0e-10_sp
  real(kind=dp), parameter :: default_dtol = 1.0e-10_dp
  real(kind=qp), parameter :: default_qtol = 1.0e-10_qp
  interface approx_eq
    module procedure approx_eq_ssc, approx_eq_sarr
    module procedure approx_eq_dsc, approx_eq_darr
    module procedure approx_eq_qsc, approx_eq_qarr
  end interface approx_eq
  interface approx_ne
    module procedure approx_ne_ssc, approx_ne_sarr
    module procedure approx_ne_dsc, approx_ne_darr
    module procedure approx_ne_qsc, approx_ne_qarr
  end interface approx_ne
  interface approx_lt
    module procedure approx_lt_ssc, approx_lt_sarr
    module procedure approx_lt_dsc, approx_lt_darr
    module procedure approx_lt_qsc, approx_lt_qarr
  end interface approx_lt
  interface approx_le
    module procedure approx_le_ssc, approx_le_sarr
    module procedure approx_le_dsc, approx_le_darr
    module procedure approx_le_qsc, approx_le_qarr
  end interface approx_le
  interface approx_gt
    module procedure approx_gt_ssc, approx_gt_sarr
    module procedure approx_gt_dsc, approx_gt_darr
    module procedure approx_gt_qsc, approx_gt_qarr
  end interface approx_gt
  interface approx_ge
    module procedure approx_ge_ssc, approx_ge_sarr
    module procedure approx_ge_dsc, approx_ge_darr
    module procedure approx_ge_qsc, approx_ge_qarr
  end interface approx_ge

  interface operator(.approx.)
    module procedure approxop_eq_ssc, approxop_eq_sarr
    module procedure approxop_eq_dsc, approxop_eq_darr
    module procedure approxop_eq_qsc, approxop_eq_qarr
  end interface operator(.approx.)
  interface operator(.aeq.)
    module procedure approxop_eq_ssc, approxop_eq_sarr
    module procedure approxop_eq_dsc, approxop_eq_darr
    module procedure approxop_eq_qsc, approxop_eq_qarr
  end interface operator(.aeq.)
  interface operator(.ane.)
    module procedure approxop_ne_ssc, approxop_ne_sarr
    module procedure approxop_ne_dsc, approxop_ne_darr
    module procedure approxop_ne_qsc, approxop_ne_qarr
  end interface operator(.ane.)
  interface operator(.alt.)
    module procedure approxop_lt_ssc, approxop_lt_sarr
    module procedure approxop_lt_dsc, approxop_lt_darr
    module procedure approxop_lt_qsc, approxop_lt_qarr
  end interface operator(.alt.)
  interface operator(.ale.)
    module procedure approxop_le_ssc, approxop_le_sarr
    module procedure approxop_le_dsc, approxop_le_darr
    module procedure approxop_le_qsc, approxop_le_qarr
  end interface operator(.ale.)
  interface operator(.agt.)
    module procedure approxop_gt_ssc, approxop_gt_sarr
    module procedure approxop_gt_dsc, approxop_gt_darr
    module procedure approxop_gt_qsc, approxop_gt_qarr
  end interface operator(.agt.)
  interface operator(.age.)
    module procedure approxop_ge_ssc, approxop_ge_sarr
    module procedure approxop_ge_dsc, approxop_ge_darr
    module procedure approxop_ge_qsc, approxop_ge_qarr
  end interface operator(.age.)
contains
  function approx_eq_ssc(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = abs(a - b) .le. tol
    else
      res = abs(a - b) .le. default_stol
    end if
  end function approx_eq_ssc
  !
  function approx_eq_sarr(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    real(kind=sp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = abs(a - b) .le. tol
    else
      res(:) = abs(a - b) .le. default_stol
    end if
  end function approx_eq_sarr
  !
  function approx_eq_dsc(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = abs(a - b) .le. tol
    else
      res = abs(a - b) .le. default_dtol
    end if
  end function approx_eq_dsc
  !
  function approx_eq_darr(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    real(kind=dp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = abs(a - b) .le. tol
    else
      res(:) = abs(a - b) .le. default_dtol
    end if
  end function approx_eq_darr
  !
  function approx_eq_qsc(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = abs(a - b) .le. tol
    else
      res = abs(a - b) .le. default_qtol
    end if
  end function approx_eq_qsc
  !
  function approx_eq_qarr(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    real(kind=qp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = abs(a - b) .le. tol
    else
      res(:) = abs(a - b) .le. default_qtol
    end if
  end function approx_eq_qarr
  ! ------------------------------------------
  function approx_ne_ssc(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = abs(a - b) .gt. tol
    else
      res = abs(a - b) .gt. default_stol
    end if
  end function approx_ne_ssc
  !
  function approx_ne_sarr(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    real(kind=sp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = abs(a - b) .gt. tol
    else
      res(:) = abs(a - b) .gt. default_stol
    end if
  end function approx_ne_sarr
  !
  function approx_ne_dsc(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = abs(a - b) .gt. tol
    else
      res = abs(a - b) .gt. default_dtol
    end if
  end function approx_ne_dsc
  !
  function approx_ne_darr(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    real(kind=dp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = abs(a - b) .gt. tol
    else
      res(:) = abs(a - b) .gt. default_dtol
    end if
  end function approx_ne_darr
  !
  function approx_ne_qsc(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = abs(a - b) .gt. tol
    else
      res = abs(a - b) .gt. default_qtol
    end if
  end function approx_ne_qsc
  !
  function approx_ne_qarr(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    real(kind=qp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = abs(a - b) .gt. tol
    else
      res(:) = abs(a - b) .gt. default_qtol
    end if
  end function approx_ne_qarr
  ! ------------------------------------------
  function approx_lt_ssc(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .lt. b - tol
    else
      res = a .lt. b - default_stol
    end if
  end function approx_lt_ssc
  !
  function approx_lt_sarr(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    real(kind=sp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .lt. b - tol
    else
      res(:) = a .lt. b - default_stol
    end if
  end function approx_lt_sarr
  !
  function approx_lt_dsc(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .lt. b - tol
    else
      res = a .lt. b - default_dtol
    end if
  end function approx_lt_dsc
  !
  function approx_lt_darr(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    real(kind=dp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .lt. b - tol
    else
      res(:) = a .lt. b - default_dtol
    end if
  end function approx_lt_darr
  !
  function approx_lt_qsc(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .lt. b - tol
    else
      res = a .lt. b - default_qtol
    end if
  end function approx_lt_qsc
  !
  function approx_lt_qarr(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    real(kind=qp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .lt. b - tol
    else
      res(:) = a .lt. b - default_qtol
    end if
  end function approx_lt_qarr
  ! ------------------------------------------
  function approx_le_ssc(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .le. b + tol
    else
      res = a .le. b + default_stol
    end if
  end function approx_le_ssc
  !
  function approx_le_sarr(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    real(kind=sp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .le. b + tol
    else
      res(:) = a .le. b + default_stol
    end if
  end function approx_le_sarr
  !
  function approx_le_dsc(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .le. b + tol
    else
      res = a .le. b + default_dtol
    end if
  end function approx_le_dsc
  !
  function approx_le_darr(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    real(kind=dp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .le. b + tol
    else
      res(:) = a .le. b + default_dtol
    end if
  end function approx_le_darr
  !
  function approx_le_qsc(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .le. b + tol
    else
      res = a .le. b + default_qtol
    end if
  end function approx_le_qsc
  !
  function approx_le_qarr(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    real(kind=qp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .le. b + tol
    else
      res(:) = a .le. b + default_qtol
    end if
  end function approx_le_qarr
  ! ------------------------------------------
  function approx_gt_ssc(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .gt. b + tol
    else
      res = a .gt. b + default_stol
    end if
  end function approx_gt_ssc
  !
  function approx_gt_sarr(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    real(kind=sp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .gt. b + tol
    else
      res(:) = a .gt. b + default_stol
    end if
  end function approx_gt_sarr
  !
  function approx_gt_dsc(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .gt. b + tol
    else
      res = a .gt. b + default_dtol
    end if
  end function approx_gt_dsc
  !
  function approx_gt_darr(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    real(kind=dp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .gt. b + tol
    else
      res(:) = a .gt. b + default_dtol
    end if
  end function approx_gt_darr
  !
  function approx_gt_qsc(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .gt. b + tol
    else
      res = a .gt. b + default_qtol
    end if
  end function approx_gt_qsc
  !
  function approx_gt_qarr(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    real(kind=qp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .gt. b + tol
    else
      res(:) = a .gt. b + default_qtol
    end if
  end function approx_gt_qarr
  ! ------------------------------------------
  function approx_ge_ssc(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a, b
    real(kind=sp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .ge. b - tol
    else
      res = a .ge. b - default_stol
    end if
  end function approx_ge_ssc
  !
  function approx_ge_sarr(a, b, tol) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    real(kind=sp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .ge. b - tol
    else
      res(:) = a .ge. b - default_stol
    end if
  end function approx_ge_sarr
  !
  function approx_ge_dsc(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a, b
    real(kind=dp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .ge. b - tol
    else
      res = a .ge. b - default_dtol
    end if
  end function approx_ge_dsc
  !
  function approx_ge_darr(a, b, tol) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    real(kind=dp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .ge. b - tol
    else
      res(:) = a .ge. b - default_dtol
    end if
  end function approx_ge_darr
  !
  function approx_ge_qsc(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a, b
    real(kind=qp), optional, intent(in) :: tol
    logical :: res
    if (present(tol)) then
      res = a .ge. b - tol
    else
      res = a .ge. b - default_qtol
    end if
  end function approx_ge_qsc
  !
  function approx_ge_qarr(a, b, tol) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    real(kind=qp), optional, intent(in) :: tol
    logical :: res(size(a))
    if (present(tol)) then
      res(:) = a .ge. b - tol
    else
      res(:) = a .ge. b - default_qtol
    end if
  end function approx_ge_qarr
  !
  ! _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  ! operator(.approx.)
  function approxop_eq_ssc(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default_stol
  end function approxop_eq_ssc
  !
  function approxop_eq_sarr(a, b) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = abs(a - b) .le. default_stol
  end function approxop_eq_sarr
  !
  function approxop_eq_dsc(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default_dtol
  end function approxop_eq_dsc
  !
  function approxop_eq_darr(a, b) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = abs(a - b) .le. default_dtol
  end function approxop_eq_darr
  !
  function approxop_eq_qsc(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .le. default_qtol
  end function approxop_eq_qsc
  !
  function approxop_eq_qarr(a, b) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = abs(a - b) .le. default_qtol
  end function approxop_eq_qarr
  ! ------------------------------------------
  function approxop_ne_ssc(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default_stol
  end function approxop_ne_ssc
  !
  function approxop_ne_sarr(a, b) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = abs(a - b) .gt. default_stol
  end function approxop_ne_sarr
  !
  function approxop_ne_dsc(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default_dtol
  end function approxop_ne_dsc
  !
  function approxop_ne_darr(a, b) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = abs(a - b) .gt. default_dtol
  end function approxop_ne_darr
  !
  function approxop_ne_qsc(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = abs(a - b) .gt. default_qtol
  end function approxop_ne_qsc
  !
  function approxop_ne_qarr(a, b) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = abs(a - b) .gt. default_qtol
  end function approxop_ne_qarr
  ! ------------------------------------------
  function approxop_lt_ssc(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default_stol
  end function approxop_lt_ssc
  !
  function approxop_lt_sarr(a, b) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .lt. b - default_stol
  end function approxop_lt_sarr
  !
  function approxop_lt_dsc(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default_dtol
  end function approxop_lt_dsc
  !
  function approxop_lt_darr(a, b) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .lt. b - default_dtol
  end function approxop_lt_darr
  !
  function approxop_lt_qsc(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .lt. b - default_qtol
  end function approxop_lt_qsc
  !
  function approxop_lt_qarr(a, b) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .lt. b - default_qtol
  end function approxop_lt_qarr
  ! ------------------------------------------
  function approxop_le_ssc(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default_stol
  end function approxop_le_ssc
  !
  function approxop_le_sarr(a, b) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .le. b + default_stol
  end function approxop_le_sarr
  !
  function approxop_le_dsc(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default_dtol
  end function approxop_le_dsc
  !
  function approxop_le_darr(a, b) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .le. b + default_dtol
  end function approxop_le_darr
  !
  function approxop_le_qsc(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .le. b + default_qtol
  end function approxop_le_qsc
  !
  function approxop_le_qarr(a, b) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .le. b + default_qtol
  end function approxop_le_qarr
  ! ------------------------------------------
  function approxop_gt_ssc(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default_stol
  end function approxop_gt_ssc
  !
  function approxop_gt_sarr(a, b) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .gt. b + default_stol
  end function approxop_gt_sarr
  !
  function approxop_gt_dsc(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default_dtol
  end function approxop_gt_dsc
  !
  function approxop_gt_darr(a, b) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .gt. b + default_dtol
  end function approxop_gt_darr
  !
  function approxop_gt_qsc(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .gt. b + default_qtol
  end function approxop_gt_qsc
  !
  function approxop_gt_qarr(a, b) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .gt. b + default_qtol
  end function approxop_gt_qarr
  ! ------------------------------------------
  function approxop_ge_ssc(a, b) result(res)
    real(kind=sp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default_stol
  end function approxop_ge_ssc
  !
  function approxop_ge_sarr(a, b) result(res)
    real(kind=sp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .ge. b - default_stol
  end function approxop_ge_sarr
  !
  function approxop_ge_dsc(a, b) result(res)
    real(kind=dp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default_dtol
  end function approxop_ge_dsc
  !
  function approxop_ge_darr(a, b) result(res)
    real(kind=dp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .ge. b - default_dtol
  end function approxop_ge_darr
  !
  function approxop_ge_qsc(a, b) result(res)
    real(kind=qp), intent(in) :: a, b
    logical :: res
    res = a .ge. b - default_qtol
  end function approxop_ge_qsc
  !
  function approxop_ge_qarr(a, b) result(res)
    real(kind=qp), intent(in) :: a(:), b(:)
    logical :: res(size(a))
    res(:) = a .ge. b - default_qtol
  end function approxop_ge_qarr
end module approx
