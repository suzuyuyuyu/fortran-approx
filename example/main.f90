program test
  use approx
  use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
  implicit none
  real(dp) :: dx(3), dy(3)
  real(dp) :: large_a, large_b

  ! --- absolute tolerance (existing) ---
  dx = 1.0_dp
  dy = [1.0_dp + 1.0e-11_dp, 2.0_dp, 1.0_dp + 1.0e-11_dp]
  print '(a)', "=== absolute tolerance (atol=1e-10) ==="
  print '(a,3l2)', "aeq(dx,dy)       :", aeq(dx, dy)
  print '(a,3l2)', "aeq(dx,dy, 1e-8) :", aeq(dx, dy, 1.0e-8_dp)
  print '(a,3l2)', "dx .aeq. dy      :", dx .aeq. dy

  ! --- hybrid tolerance ---
  large_a = 1.0e8_dp
  large_b = 1.0e8_dp + 1.0_dp
  print '(a)', ""
  print '(a)', "=== hybrid tolerance ==="
  print '(a,f12.1,a,f12.1)', "a = ", large_a, "  b = ", large_b
  print '(a,l2)', "aeq  (tol=1e-6)               :", aeq(large_a, large_b, 1.0e-6_dp)
  print '(a,l2)', "haeq default                  :", haeq(large_a, large_b)
  print '(a,l2)', "haeq(atol=1e-6, rtol=1e-7)    :", haeq(large_a, large_b, 1.0e-6_dp, 1.0e-7_dp)
  print '(a,l2)', "large_a .haeq. large_b        :", large_a .haeq. large_b

  print '(a)', ""
  print '(a)', "=== near-zero (a=1e-15, b=2e-15) ==="
  print '(a,l2)', "haeq(atol=1e-14, rtol=1e-9)   :", &
    haeq(1.0e-15_dp, 2.0e-15_dp, 1.0e-14_dp, 1.0e-9_dp)

end program test
