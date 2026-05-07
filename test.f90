program p
  use, intrinsic :: iso_fortran_env, only : real64, real32
  implicit none

  integer, parameter :: n = 10000000
  integer, parameter :: r = real32
  real(r), allocatable :: x(:), y(:), z(:)
  real(r) :: checksum
  integer :: i

  allocate(x(n), y(n), z(n))

  do i = 1, n
    x(i) = 0.000001_r * real(i, r)
    y(i) = 0.000002_r * real(i, r) + 0.25_r
  end do

  call vector_math_kernel(n, x, y, z)

  checksum = sum(z)
  print '(a,es24.16)', 'checksum = ', checksum

contains

  subroutine vector_math_kernel(n, x, y, z)
    integer, intent(in) :: n
    real(r), intent(in) :: x(n), y(n)
    real(r), intent(out) :: z(n)
    integer :: i

    do i = 1, n
      z(i) = scale(1.0_r, int(x(i)))
    end do
    do i = 1, n
      z(i) = sin(z(i))
    end do
  end subroutine vector_math_kernel
end program
