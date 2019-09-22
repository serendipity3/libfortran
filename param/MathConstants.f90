module mathConstants ! {{{
    use precisions
!    use exceptation
    use ioHelper
    implicit none
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
