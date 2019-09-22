module mathConstants ! {{{
    use precisions
    use exceptation
    use IOhelper
    implicit none
    interface times
        module procedure r_times, c_times
    end interface times
    real(kind=DP), parameter :: pi = 4e0_DP*atan(1e0_DP)
    complex(kind=DP), parameter :: zi = cmplx(0e0_DP,1e0_DP)
    real(kind=DP), parameter :: units(3, 3) = reshape( (/ 1e0_DP,0e0_DP,0e0_DP, 0e0_DP,1e0_DP,0e0_DP, 0e0_DP,0e0_DP,1e0_DP /), shape(units))
    complex(kind=DP), parameter :: sigma_0(2, 2) =    reshape( (/ 1e0_DP,  0e0_DP, 0e0_DP,  1e0_DP /), shape(sigma_0))
    complex(kind=DP), parameter :: sigma_x(2, 2) =    reshape( (/ 0e0_DP,  1e0_DP, 1e0_DP,  0e0_DP /), shape(sigma_x))
    complex(kind=DP), parameter :: sigma_y(2, 2) = zi*reshape( (/ 0e0_DP, -1e0_DP, 1e0_DP,  0e0_DP /), shape(sigma_y))
    complex(kind=DP), parameter :: sigma_z(2, 2) =    reshape( (/ 1e0_DP,  0e0_DP, 0e0_DP, -1e0_DP /), shape(sigma_z))
    complex(kind=DP), parameter :: sigma(2, 2, 0:3) = (/ sigma_0, sigma_x, sigma_y, sigma_z /)
    public:: pi, zi, sigma
    contains

! vector operations: 'times', 'cdot', 'length' {{{
    subroutine r_times(vector1, vector2, op)    ! {{{
        implicit none
        real(kind=DP), intent(in)  :: vector1(3), vector2(3)
        real(kind=DP), intent(out) :: op(3)
            op = 0e0_DP
            op(1) = vector1(2) * vector2(3) - vector1(3) * vector2(2)
            op(2) = vector1(3) * vector2(1) - vector1(1) * vector2(3)
            op(3) = vector1(1) * vector2(2) - vector1(2) * vector2(1)
        return
    end subroutine r_times  ! }}}

    subroutine c_times(vector1, vector2, op)    ! {{{
        implicit none
        complex(kind=DP), intent(in)  :: vector1(3), vector2(3)
        complex(kind=DP), intent(out) :: op(3)
            op = 0e0_DP
            op(1) = vector1(2) * vector2(3) - vector1(3) * vector2(2)
            op(2) = vector1(3) * vector2(1) - vector1(1) * vector2(3)
            op(3) = vector1(1) * vector2(2) - vector1(2) * vector2(1)
        return
    end subroutine c_times  ! }}}

    function cdot(vector1, vector2) result(res)    ! {{{
        implicit none
        real(kind=DP), intent(in) :: vector1(:), vector2(:)
        integer(kind=IT) :: i
        real(kind=DP) :: res
            res = 0e0_DP
            if ( size(vector1) == size(vector2) ) then
                do i = lbound(vector1,1), ubound(vector1,1)
                    res = res + vector1(i) * vector2(i)
                end do
            else
                print *, "size does not match."
            end if
        return
    end function cdot  ! }}}

    function length(R1, R2) result(res) ! {{{
        implicit none
        real(kind=DP), intent(in) :: R1(2), R2(2)
        real(kind=DP) :: res
            res = sqrt( (R1(1)-R2(1))**2 + (R1(2)-R2(2))**2 )
        return
    end function length  ! }}}
! }}}

! matrix operations: 'prod', 'trace' {{{
    subroutine prod(matrix_in1, matrix_in2, matrix_out) ! {{{
        implicit none
        complex(kind=DP), intent(in) :: matrix_in1(:,:), matrix_in2(:,:)
        complex(kind=DP), intent(out):: matrix_out(:,:)
        integer(kind=IT) :: i, j, k
        integer(kind=IT) :: i_max, j_max, k_max, k_max1, k_max2
            i_max = ubound(matrix_in1,1)
            j_max = ubound(matrix_in2,2)
            k_max1 = ubound(matrix_in1,2)
            k_max2 = ubound(matrix_in2,1)
            if ( k_max1  == k_max2 ) then
                k_max = k_max1
            else
!                print lt//'ERROR @ matrix product; '//iform1//' \= '//iform1//rt, k_max1, k_max2
                error = 1
                return
            end if
            do i = 1, i_max
                do j = 1, j_max
                    matrix_out(i, j) = 0e0_DP
                    do k = 1, k_max
                        matrix_out(i, j) = matrix_out(i, j) + matrix_in1(i, k) * matrix_in2(k, j)
                    end do
                end do
            end do
        return
    end subroutine prod! }}}

    function trace(matrix) result(res) ! {{{
        implicit none
        complex(kind=DP), intent(in) :: matrix(:,:)
        complex(kind=DP) :: res
        integer(kind=IT) :: i1, i2, i
            i1 = ubound(matrix,1)
            i2 = ubound(matrix,2)

            res = 0e0_DP
            if ( i1 == i2 ) then
                do i = 1, i1
                    res = res + matrix(i,i)
                end do
            end if
        return
    end function trace ! }}}
! }}}

    function Lorentzian(x, eps) ! {{{
        implicit none
        real(kind=DP) :: x, Lorentzian, eps
            Lorentzian = eps / (x**2 + eps**2)
        return
    end function Lorentzian ! }}}

! condition of an efficient convergence for bisec routine ! {{{
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! HOW TO USE
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! hoge hoge...
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! EX)
!   real(kind=DP), dimension(1:n) :: s_in, s_out, diff
!   real(kind=DP) :: error_new, error_old
!       do i = 1, 100
!
!           <some calculations for s_in & s_out>
!
!           diff(1:n) = s_out(1:n) - s_in(1:n)
!           error_new = sqrt( sum(diff**2) ) / sqrt( sum(s_out**2) )
!           call set_ratio(ratio, error_new,error_old, 5d-3)
!
!           s_in(1:n) = ratio * s_out(1:n) + (1e0_DP - ratio) * s_in(1:n)
!       end do
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    subroutine set_ratio(ratio_, error_new_, error_old_, ratio_eps_) ! {{{
        implicit none
        real(kind=DP), intent(inout) :: ratio_
        real(kind=DP), intent(inout) :: error_old_
        real(kind=DP), intent(in)    :: error_new_
        real(kind=DP), intent(in)    :: ratio_eps_
            if ( error_old_ /= 0e0_DP) then
                if (error_new_ > error_old_*2e0_DP) then
                    ratio_ = ratio_*1e-1_DP
                else if (error_new_ > error_old_) then
                    ratio_ = ratio_ / 2e0_DP
                else
                    ratio_ = ratio_ * 1.15e0_DP
                endif
                if (ratio_ < ratio_eps_) ratio_ = 2e0_DP!10.0e0_DP
            end if
            error_old_ = error_new_
        return
    end subroutine set_ratio ! }}}
! }}}

    function theta(x,y) ! {{{
        implicit none
        real(kind=DP) :: x, y, theta
            theta = ( 1e0_DP + (x-y)/abs(x-y) ) / 2e0_DP
        return
    end function theta ! }}}

end module mathConstants  ! }}}
