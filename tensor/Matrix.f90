module modMatrix ! {{{
    use precisions
    type :: matrix ! {{{
        integer(kind=IT) :: dim
        complex(kind=DP), allocatable :: value(:,:)
        contains
        procedure, pass :: init => init_matrix
        procedure, pass :: assign_matrix
        procedure, pass :: add_matrix
!        procedure, pass :: multiply_c_mat2x2
!        procedure, pass :: multiply_r_mat2x2
        generic :: assignment(=) => assign_matrix
        generic :: operator(+) => add_matrix
!        generic :: operator(*) => multiply_r_mat2x2, multiply_c_mat2x2
        final :: finalize_matrix
    end type matrix ! }}}
    contains
        subroutine init_matrix(this, n) ! {{{
            class(matrix), intent(inout) :: this
            integer(kind=IT), intent(in) :: n
                this%dim = n
                allocate(this%value(1:n,1:n))
            return
        end subroutine init_matrix ! }}}

        subroutine finalize_matrix(this) ! {{{
            implicit none
            type(matrix), intent(inout) :: this
!                deallocate(this%value)
            return
        end subroutine finalize_matrix ! }}}

        subroutine assign_matrix(this, m_) ! {{{
            implicit none
            class(matrix), intent(inout) :: this
            class(matrix), intent(in) :: m_
                this%value(:,:) = m_%value(:,:)
            return
        end subroutine assign_matrix ! }}}

        function add_matrix(this, m_) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(matrix), intent(in) :: m_
            class(matrix), allocatable:: res
                allocate(res)
                call res%init(this%dim)
                res%value(:,:) = this%value(:,:) + m_%value(:,:)
            return
        end function add_matrix ! }}}
!
!        function multiply_r_mat2x2(this, r_) result(res) ! {{{
!            implicit none
!            class(mat2x2), intent(in) :: this
!            real(kind=DP), intent(in) :: r_
!            class(mat2x2), pointer :: res
!                allocate(res)
!                res%coef(0:3)       = r_ * this%coef(0:3)
!                res%comp(1:2,1:2)   = r_ * this%comp(1:2,1:2)
!            return
!        end function multiply_r_mat2x2 ! }}}
!
!        function multiply_c_mat2x2(this, c_) result(res) ! {{{
!            implicit none
!            class(mat2x2), intent(in) :: this
!            complex(kind=DP), intent(in) :: c_
!            class(mat2x2), pointer :: res
!                allocate(res)
!                res%coef(0:3)       = c_ * this%coef(0:3)
!                res%comp(1:2,1:2)   = c_ * this%comp(1:2,1:2)
!            return
!        end function multiply_c_mat2x2 ! }}}
end module modMatrix ! }}}
