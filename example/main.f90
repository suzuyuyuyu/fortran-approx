program test
  use approx
  use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
  implicit none
  real(sp) :: sx, sy
  real(dp) :: dx(3), dy(3)
  real(qp) :: qx, qy

  sx = 1.0_sp
  sy = 1.0_sp
  dx = 1.0_dp
  dy = 1.0_dp + 1.0e-11_dp
  dy(2) = 2.0_dp
  qx = 1.0_qp
  qy = 1.0_qp


  ! print '("1.0_sp .ale. 2.0_sp: ",)'
  print *, aeq(dx, dy)
  print *, aeq(dx, dy, 1.0e-8_dp)
  print *, dx .aeq. dy

end program test
