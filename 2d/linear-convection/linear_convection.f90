module functions_module
contains
  subroutine fill_matrix_values(matrix, n, h)
    real *8, allocatable, intent(inout):: matrix(:, :)
    real *8, intent(in):: h
    integer, intent(in):: n

    integer:: i, j

    do i=0, n
      do j=0, n
        if ((i*h <= 0.5) .and. (j*h <= 1)) then
          matrix(i, j) = 2.0d0
        else
          matrix(i, j) = 1.0d0
        end if
      end do
    end do

    do i=0, n
      matrix(i, 0) = 1.0d0
      matrix(0, i) = 1.0d0
    end do

    return
  end subroutine fill_matrix_values

  subroutine update_time_step(coeff, ans, n, a)
    real *8, allocatable, intent(inout):: ans(:, :)
    real *8, allocatable, intent(in):: coeff(:, :)
    real *8, intent(in):: a
    integer, intent(in):: n

    integer:: i, j

    do i=1, n-1
      do j=1, n-1
        ans(i, j) = a*coeff(i-1, j) + coeff(i, j) * (1.0d0 - 2.0d0*a) + a*coeff(i, j-1)
      end do
    end do

    do i=0, n
      ans(i, 0) = 1.0d0
      ans(0, i) = 1.0d0
    end do

    return
  end subroutine update_time_step

  subroutine copy_matrix(or, cp, n)
    real *8, allocatable, intent(inout):: cp(:, :)
    real *8, allocatable, intent(in):: or(:, :)
    integer, intent(in):: n

    integer:: i, j

    do i=0, n
      do j=0, n
        cp(i, j) = or(i, j)
      end do
    end do

    return
  end subroutine copy_matrix

  subroutine save_numeric(nu, n)
    real *8, allocatable, intent(in):: nu(:,:)
    integer, intent(in):: n

    integer:: i, j

    open(unit=2, file="numeric.dat", status="unknown", access="append")

    do i=0, n
      do j=0, n
        write (2,*) real(i), ',', real(j), ',', nu(i, j)
      end do
    end do
    close(2)

    return
  end subroutine save_numeric
end module functions_module

program linear_convection
  use functions_module

  print *, "Executing the numeric method."

  integer:: n, i, time_steps_nb, total_time
  real *8:: dh, a, c, dt
  real *8, allocatable:: matrix(:, :), ans(:, :)

  n = 81
  total_time = 1
  time_steps_nb = 500
  c = 1.0d0
  dt = real(total_time) / real(time_steps_nb)
  dh = 2.0d0 / (n -1)
  a = (c * dt) / dh

  allocate(matrix(0:n, 0:n), ans(0:n, 0:n))

  call fill_matrix_values(matrix, n, dh)

  do i = 1, time_steps_nb
    call copy_matrix(matrix, ans, n)
    call save_numeric(ans, n)
    call update_time_step(ans, matrix, n, a)
  end do

  deallocate(matrix, ans)
end program linear_convection
