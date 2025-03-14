!------------------------------------------------------------------------------
! TOHOKU Univ., Civil Engineering
!------------------------------------------------------------------------------
!
! MODULE: approx
!
!> @author
!> Yuta SUZUKI (GitHub:@suzuyuyuyu)
!
! DESCRIPTION:
!> This module provides scalar and array approximate comparison functions for real(4), real(8), and real(16) types.
!> Default tolerance is set to 1.0d-7, where you can change it by passing an optional argument.
!
! REVISION HISTORY:
! 2025/02/25 - Initial Version
!------------------------------------------------------------------------------
module approx
  implicit none
  real(4), parameter :: default_stol = 1.0e-7
  double precision, parameter :: default_dtol = 1.0d-7
  real(16), parameter :: default_qtol = 1.0q-7!&
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
  logical function approx_eq_ssc(a, b, tol)
    real(4), intent(in) :: a, b
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_eq_ssc = abs(a - b) .le. tol
    else
      approx_eq_ssc = abs(a - b) .le. default_stol
    end if
  end function approx_eq_ssc
  !
  logical function approx_eq_sarr(a, b, tol)
    real(4), intent(in) :: a(:), b(:)
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_eq_sarr = all(abs(a - b) .le. tol)
    else
      approx_eq_sarr = all(abs(a - b) .le. default_stol)
    end if
  end function approx_eq_sarr
  !
  logical function approx_eq_dsc(a, b, tol)
    double precision, intent(in) :: a, b
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_eq_dsc = abs(a - b) .le. tol
    else
      approx_eq_dsc = abs(a - b) .le. default_dtol
    end if
  end function approx_eq_dsc
  !
  logical function approx_eq_darr(a, b, tol)
    double precision, intent(in) :: a(:), b(:)
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_eq_darr = all(abs(a - b) .le. tol)
    else
      approx_eq_darr = all(abs(a - b) .le. default_dtol)
    end if
  end function approx_eq_darr
  !
  logical function approx_eq_qsc(a, b, tol)
    real(16), intent(in) :: a, b
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_eq_qsc = abs(a - b) .le. tol
    else
      approx_eq_qsc = abs(a - b) .le. default_qtol
    end if
  end function approx_eq_qsc
  !
  logical function approx_eq_qarr(a, b, tol)
    real(16), intent(in) :: a(:), b(:)
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_eq_qarr = all(abs(a - b) .le. tol)
    else
      approx_eq_qarr = all(abs(a - b) .le. default_qtol)
    end if
  end function approx_eq_qarr
  ! ------------------------------------------
  logical function approx_ne_ssc(a, b, tol)
    real(4), intent(in) :: a, b
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_ne_ssc = abs(a - b) .gt. tol
    else
      approx_ne_ssc = abs(a - b) .gt. default_stol
    end if
  end function approx_ne_ssc
  !
  logical function approx_ne_sarr(a, b, tol)
    real(4), intent(in) :: a(:), b(:)
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_ne_sarr = any(abs(a - b) .gt. tol)
    else
      approx_ne_sarr = any(abs(a - b) .gt. default_stol)
    end if
  end function approx_ne_sarr
  !
  logical function approx_ne_dsc(a, b, tol)
    double precision, intent(in) :: a, b
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_ne_dsc = abs(a - b) .gt. tol
    else
      approx_ne_dsc = abs(a - b) .gt. default_dtol
    end if
  end function approx_ne_dsc
  !
  logical function approx_ne_darr(a, b, tol)
    double precision, intent(in) :: a(:), b(:)
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_ne_darr = any(abs(a - b) .gt. tol)
    else
      approx_ne_darr = any(abs(a - b) .gt. default_dtol)
    end if
  end function approx_ne_darr
  !
  logical function approx_ne_qsc(a, b, tol)
    real(16), intent(in) :: a, b
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_ne_qsc = abs(a - b) .gt. tol
    else
      approx_ne_qsc = abs(a - b) .gt. default_qtol
    end if
  end function approx_ne_qsc
  !
  logical function approx_ne_qarr(a, b, tol)
    real(16), intent(in) :: a(:), b(:)
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_ne_qarr = any(abs(a - b) .gt. tol)
    else
      approx_ne_qarr = any(abs(a - b) .gt. default_qtol)
    end if
  end function approx_ne_qarr
  ! ------------------------------------------
  logical function approx_lt_ssc(a, b, tol)
    real(4), intent(in) :: a, b
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_lt_ssc = a .lt. b - tol
    else
      approx_lt_ssc = a .lt. b - default_stol
    end if
  end function approx_lt_ssc
  !
  logical function approx_lt_sarr(a, b, tol)
    real(4), intent(in) :: a(:), b(:)
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_lt_sarr = all(a .lt. b - tol)
    else
      approx_lt_sarr = all(a .lt. b - default_stol)
    end if
  end function approx_lt_sarr
  !
  logical function approx_lt_dsc(a, b, tol)
    double precision, intent(in) :: a, b
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_lt_dsc = a .lt. b - tol
    else
      approx_lt_dsc = a .lt. b - default_dtol
    end if
  end function approx_lt_dsc
  !
  logical function approx_lt_darr(a, b, tol)
    double precision, intent(in) :: a(:), b(:)
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_lt_darr = all(a .lt. b - tol)
    else
      approx_lt_darr = all(a .lt. b - default_dtol)
    end if
  end function approx_lt_darr
  !
  logical function approx_lt_qsc(a, b, tol)
    real(16), intent(in) :: a, b
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_lt_qsc = a .lt. b - tol
    else
      approx_lt_qsc = a .lt. b - default_qtol
    end if
  end function approx_lt_qsc
  !
  logical function approx_lt_qarr(a, b, tol)
    real(16), intent(in) :: a(:), b(:)
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_lt_qarr = all(a .lt. b - tol)
    else
      approx_lt_qarr = all(a .lt. b - default_qtol)
    end if
  end function approx_lt_qarr
  ! ------------------------------------------
  logical function approx_le_ssc(a, b, tol)
    real(4), intent(in) :: a, b
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_le_ssc = a .le. b + tol
    else
      approx_le_ssc = a .le. b + default_stol
    end if
  end function approx_le_ssc
  !
  logical function approx_le_sarr(a, b, tol)
    real(4), intent(in) :: a(:), b(:)
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_le_sarr = all(a .le. b + tol)
    else
      approx_le_sarr = all(a .le. b + default_stol)
    end if
  end function approx_le_sarr
  !
  logical function approx_le_dsc(a, b, tol)
    double precision, intent(in) :: a, b
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_le_dsc = a .le. b + tol
    else
      approx_le_dsc = a .le. b + default_dtol
    end if
  end function approx_le_dsc
  !
  logical function approx_le_darr(a, b, tol)
    double precision, intent(in) :: a(:), b(:)
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_le_darr = all(a .le. b + tol)
    else
      approx_le_darr = all(a .le. b + default_dtol)
    end if
  end function approx_le_darr
  !
  logical function approx_le_qsc(a, b, tol)
    real(16), intent(in) :: a, b
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_le_qsc = a .le. b + tol
    else
      approx_le_qsc = a .le. b + default_qtol
    end if
  end function approx_le_qsc
  !
  logical function approx_le_qarr(a, b, tol)
    real(16), intent(in) :: a(:), b(:)
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_le_qarr = all(a .le. b + tol)
    else
      approx_le_qarr = all(a .le. b + default_qtol)
    end if
  end function approx_le_qarr
  ! ------------------------------------------
  logical function approx_gt_ssc(a, b, tol)
    real(4), intent(in) :: a, b
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_gt_ssc = a .gt. b + tol
    else
      approx_gt_ssc = a .gt. b + default_stol
    end if
  end function approx_gt_ssc
  !
  logical function approx_gt_sarr(a, b, tol)
    real(4), intent(in) :: a(:), b(:)
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_gt_sarr = all(a .gt. b + tol)
    else
      approx_gt_sarr = all(a .gt. b + default_stol)
    end if
  end function approx_gt_sarr
  !
  logical function approx_gt_dsc(a, b, tol)
    double precision, intent(in) :: a, b
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_gt_dsc = a .gt. b + tol
    else
      approx_gt_dsc = a .gt. b + default_dtol
    end if
  end function approx_gt_dsc
  !
  logical function approx_gt_darr(a, b, tol)
    double precision, intent(in) :: a(:), b(:)
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_gt_darr = all(a .gt. b + tol)
    else
      approx_gt_darr = all(a .gt. b + default_dtol)
    end if
  end function approx_gt_darr
  !
  logical function approx_gt_qsc(a, b, tol)
    real(16), intent(in) :: a, b
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_gt_qsc = a .gt. b + tol
    else
      approx_gt_qsc = a .gt. b + default_qtol
    end if
  end function approx_gt_qsc
  !
  logical function approx_gt_qarr(a, b, tol)
    real(16), intent(in) :: a(:), b(:)
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_gt_qarr = all(a .gt. b + tol)
    else
      approx_gt_qarr = all(a .gt. b + default_qtol)
    end if
  end function approx_gt_qarr
  ! ------------------------------------------
  logical function approx_ge_ssc(a, b, tol)
    real(4), intent(in) :: a, b
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_ge_ssc = a .ge. b - tol
    else
      approx_ge_ssc = a .ge. b - default_stol
    end if
  end function approx_ge_ssc
  !
  logical function approx_ge_sarr(a, b, tol)
    real(4), intent(in) :: a(:), b(:)
    real(4), optional, intent(in) :: tol
    if (present(tol)) then
      approx_ge_sarr = all(a .ge. b - tol)
    else
      approx_ge_sarr = all(a .ge. b - default_stol)
    end if
  end function approx_ge_sarr
  !
  logical function approx_ge_dsc(a, b, tol)
    double precision, intent(in) :: a, b
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_ge_dsc = a .ge. b - tol
    else
      approx_ge_dsc = a .ge. b - default_dtol
    end if
  end function approx_ge_dsc
  !
  logical function approx_ge_darr(a, b, tol)
    double precision, intent(in) :: a(:), b(:)
    double precision, optional, intent(in) :: tol
    if (present(tol)) then
      approx_ge_darr = all(a .ge. b - tol)
    else
      approx_ge_darr = all(a .ge. b - default_dtol)
    end if
  end function approx_ge_darr
  !
  logical function approx_ge_qsc(a, b, tol)
    real(16), intent(in) :: a, b
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_ge_qsc = a .ge. b - tol
    else
      approx_ge_qsc = a .ge. b - default_qtol
    end if
  end function approx_ge_qsc
  !
  logical function approx_ge_qarr(a, b, tol)
    real(16), intent(in) :: a(:), b(:)
    real(16), optional, intent(in) :: tol
    if (present(tol)) then
      approx_ge_qarr = all(a .ge. b - tol)
    else
      approx_ge_qarr = all(a .ge. b - default_qtol)
    end if
  end function approx_ge_qarr
  !
  ! _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  ! operator(.approx.)
  logical function approxop_eq_ssc(a, b)
    real(4), intent(in) :: a, b
    approxop_eq_ssc = abs(a - b) .le. default_stol
  end function approxop_eq_ssc
  !
  logical function approxop_eq_sarr(a, b)
    real(4), intent(in) :: a(:), b(:)
    approxop_eq_sarr = all(abs(a - b) .le. default_stol)
  end function approxop_eq_sarr
  !
  logical function approxop_eq_dsc(a, b)
    double precision, intent(in) :: a, b
    approxop_eq_dsc = abs(a - b) .le. default_dtol
  end function approxop_eq_dsc
  !
  logical function approxop_eq_darr(a, b)
    double precision, intent(in) :: a(:), b(:)
    approxop_eq_darr = all(abs(a - b) .le. default_dtol)
  end function approxop_eq_darr
  !
  logical function approxop_eq_qsc(a, b)
    real(16), intent(in) :: a, b
    approxop_eq_qsc = abs(a - b) .le. default_qtol
  end function approxop_eq_qsc
  !
  logical function approxop_eq_qarr(a, b)
    real(16), intent(in) :: a(:), b(:)
    approxop_eq_qarr = all(abs(a - b) .le. default_qtol)
  end function approxop_eq_qarr
  ! ------------------------------------------
  logical function approxop_ne_ssc(a, b)
    real(4), intent(in) :: a, b
    approxop_ne_ssc = abs(a - b) .gt. default_stol
  end function approxop_ne_ssc
  !
  logical function approxop_ne_sarr(a, b)
    real(4), intent(in) :: a(:), b(:)
    approxop_ne_sarr = any(abs(a - b) .gt. default_stol)
  end function approxop_ne_sarr
  !
  logical function approxop_ne_dsc(a, b)
    double precision, intent(in) :: a, b
    approxop_ne_dsc = abs(a - b) .gt. default_dtol
  end function approxop_ne_dsc
  !
  logical function approxop_ne_darr(a, b)
    double precision, intent(in) :: a(:), b(:)
    approxop_ne_darr = any(abs(a - b) .gt. default_dtol)
  end function approxop_ne_darr
  !
  logical function approxop_ne_qsc(a, b)
    real(16), intent(in) :: a, b
    approxop_ne_qsc = abs(a - b) .gt. default_qtol
  end function approxop_ne_qsc
  !
  logical function approxop_ne_qarr(a, b)
    real(16), intent(in) :: a(:), b(:)
    approxop_ne_qarr = any(abs(a - b) .gt. default_qtol)
  end function approxop_ne_qarr
  ! ------------------------------------------
  logical function approxop_lt_ssc(a, b)
    real(4), intent(in) :: a, b
    approxop_lt_ssc = a .lt. b - default_stol
  end function approxop_lt_ssc
  !
  logical function approxop_lt_sarr(a, b)
    real(4), intent(in) :: a(:), b(:)
    approxop_lt_sarr = all(a .lt. b - default_stol)
  end function approxop_lt_sarr
  !
  logical function approxop_lt_dsc(a, b)
    double precision, intent(in) :: a, b
    approxop_lt_dsc = a .lt. b - default_dtol
  end function approxop_lt_dsc
  !
  logical function approxop_lt_darr(a, b)
    double precision, intent(in) :: a(:), b(:)
    approxop_lt_darr = all(a .lt. b - default_dtol)
  end function approxop_lt_darr
  !
  logical function approxop_lt_qsc(a, b)
    real(16), intent(in) :: a, b
    approxop_lt_qsc = a .lt. b - default_qtol
  end function approxop_lt_qsc
  !
  logical function approxop_lt_qarr(a, b)
    real(16), intent(in) :: a(:), b(:)
    approxop_lt_qarr = all(a .lt. b - default_qtol)
  end function approxop_lt_qarr
  ! ------------------------------------------
  logical function approxop_le_ssc(a, b)
    real(4), intent(in) :: a, b
    approxop_le_ssc = a .le. b + default_stol
  end function approxop_le_ssc
  !
  logical function approxop_le_sarr(a, b)
    real(4), intent(in) :: a(:), b(:)
    approxop_le_sarr = all(a .le. b + default_stol)
  end function approxop_le_sarr
  !
  logical function approxop_le_dsc(a, b)
    double precision, intent(in) :: a, b
    approxop_le_dsc = a .le. b + default_dtol
  end function approxop_le_dsc
  !
  logical function approxop_le_darr(a, b)
    double precision, intent(in) :: a(:), b(:)
    approxop_le_darr = all(a .le. b + default_dtol)
  end function approxop_le_darr
  !
  logical function approxop_le_qsc(a, b)
    real(16), intent(in) :: a, b
    approxop_le_qsc = a .le. b + default_qtol
  end function approxop_le_qsc
  !
  logical function approxop_le_qarr(a, b)
    real(16), intent(in) :: a(:), b(:)
    approxop_le_qarr = all(a .le. b + default_qtol)
  end function approxop_le_qarr
  ! ------------------------------------------
  logical function approxop_gt_ssc(a, b)
    real(4), intent(in) :: a, b
    approxop_gt_ssc = a .gt. b + default_stol
  end function approxop_gt_ssc
  !
  logical function approxop_gt_sarr(a, b)
    real(4), intent(in) :: a(:), b(:)
    approxop_gt_sarr = all(a .gt. b + default_stol)
  end function approxop_gt_sarr
  !
  logical function approxop_gt_dsc(a, b)
    double precision, intent(in) :: a, b
    approxop_gt_dsc = a .gt. b + default_dtol
  end function approxop_gt_dsc
  !
  logical function approxop_gt_darr(a, b)
    double precision, intent(in) :: a(:), b(:)
    approxop_gt_darr = all(a .gt. b + default_dtol)
  end function approxop_gt_darr
  !
  logical function approxop_gt_qsc(a, b)
    real(16), intent(in) :: a, b
    approxop_gt_qsc = a .gt. b + default_qtol
  end function approxop_gt_qsc
  !
  logical function approxop_gt_qarr(a, b)
    real(16), intent(in) :: a(:), b(:)
    approxop_gt_qarr = all(a .gt. b + default_qtol)
  end function approxop_gt_qarr
  ! ------------------------------------------
  logical function approxop_ge_ssc(a, b)
    real(4), intent(in) :: a, b
    approxop_ge_ssc = a .ge. b - default_stol
  end function approxop_ge_ssc
  !
  logical function approxop_ge_sarr(a, b)
    real(4), intent(in) :: a(:), b(:)
    approxop_ge_sarr = all(a .ge. b - default_stol)
  end function approxop_ge_sarr
  !
  logical function approxop_ge_dsc(a, b)
    double precision, intent(in) :: a, b
    approxop_ge_dsc = a .ge. b - default_dtol
  end function approxop_ge_dsc
  !
  logical function approxop_ge_darr(a, b)
    double precision, intent(in) :: a(:), b(:)
    approxop_ge_darr = all(a .ge. b - default_dtol)
  end function approxop_ge_darr
  !
  logical function approxop_ge_qsc(a, b)
    real(16), intent(in) :: a, b
    approxop_ge_qsc = a .ge. b - default_qtol
  end function approxop_ge_qsc
  !
  logical function approxop_ge_qarr(a, b)
    real(16), intent(in) :: a(:), b(:)
    approxop_ge_qarr = all(a .ge. b - default_qtol)
  end function approxop_ge_qarr
end module approx
