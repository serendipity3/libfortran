module modMatrix ! {{{
!  this module uses
!   LAPACK: zgetrf, zgetri
!
    use precisions
    use formats
    type :: matrix ! {{{
        integer(kind=IT) :: dim
        complex(kind=DP), allocatable :: value(:,:)
        contains
        procedure, pass :: init => init_matrix
        procedure, pass :: inverse => inverse_matrix
        procedure, pass :: trace => trace_matrix
        procedure, pass :: conjg => conjg_matrix
        procedure, pass :: toArray => convert_toArray_matrix
        procedure, pass :: fromArray => convert_fromArray_matrix
        procedure, pass :: diagonalize_N => diagonalize_N_matrix
        procedure, pass :: diagonalize_V => diagonalize_V_matrix

        procedure, pass :: assign_matrix
        procedure, pass :: add_matrix
        procedure, pass(this) :: add2_matrix
        procedure, pass :: minus_matrix
        procedure, pass(this) :: minus2_matrix
        procedure, pass :: multiply_matrix
        procedure, pass(this) :: multiply2_matrix
        procedure, pass :: divide_matrix
        procedure, pass(this) :: divide2_matrix
        generic :: assignment(=) => assign_matrix
        generic :: operator(+) => add_matrix, add2_matrix
        generic :: operator(-) => minus_matrix, minus2_matrix
        generic :: operator(*) => multiply_matrix, multiply2_matrix
        generic :: operator(/) => divide_matrix, divide2_matrix
        procedure, pass :: print => print_matrix
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
                deallocate(this%value)
            return
        end subroutine finalize_matrix ! }}}

        subroutine assign_matrix(this, m_) ! {{{
            implicit none
            class(matrix), intent(inout) :: this
            class(*), intent(in) :: m_
                select type (m_)
                    type is (real(kind=DP))
                        this%value(:,:) = m_
                    type is (complex(kind=DP))
                        this%value(:,:) = m_
                    type is (matrix)
                        this%value(:,:) = m_%value(:,:)
                end select
            return
        end subroutine assign_matrix ! }}}

        function inverse_matrix(this) result (res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(matrix), allocatable :: res
            integer(4) :: info
            integer(4) :: lda
            complex(kind=DP) :: work(64*this%dim)
            complex(kind=DP) :: A(this%dim,this%dim)
            integer(4) :: ipiv(this%dim)
                lda = this%dim
                allocate(res)
                call res%init(this%dim)
                A(:,:) = this%value(:,:)
                call zgetrf(this%dim, this%dim, A, lda, ipiv, info)
                if (info == 0) then
                    call zgetri(this%dim, A, this%dim, ipiv, work, lda, info)
                    res%value(:,:) = A(:,:)
                else
                    call error_header("info is not zero", .true., .true.)
                end if
            return
        end function inverse_matrix ! }}}

        function trace_matrix(this) result (res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            complex(kind=DP), allocatable :: res
            integer(kind=IT) :: i
                allocate(res)
                do i = 1, this%dim
                    res = res + this%value(i,i)
                end do
            return
        end function trace_matrix ! }}}

        function conjg_matrix(this) result (res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(matrix), allocatable :: res
            integer(kind=IT) :: i, j
                allocate(res)
                call res%init(this%dim)
                do i = 1, this%dim
                    do j = 1, this%dim
                        res%value(j,i) = conjg(this%value(i,j))
                    end do
                end do
            return
        end function conjg_matrix ! }}}

        subroutine convert_toArray_matrix(this, res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            complex(kind=DP), intent(out) :: res(1:this%dim*this%dim)
                res(:) = reshape(this%value(:,:), shape(res))
            return
        end subroutine convert_toArray_matrix ! }}}

        subroutine convert_fromArray_matrix(this, res) ! {{{
            implicit none
            class(matrix), intent(inout) :: this
            complex(kind=DP), intent(in) :: res(1:this%dim*this%dim)
                this%value(:,:) = reshape(res(:), shape(this%value))
            return
        end subroutine convert_fromArray_matrix ! }}}

        subroutine diagonalize_N_matrix(this, EigenValue_) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            real(kind=DP), intent(out) :: EigenValue_(1:this%dim)
            complex(kind=DP) :: work(4*this%dim-1)
            integer(kind=IT) :: rwork(3*this%dim-2)

            integer(kind=IT) :: n, lwork
            integer(4) :: info
                n = ubound(this%value,1)
                lwork = 4*n - 1

                call zheev('N', 'L', n, this%value, n, EigenValue_, work, lwork, rwork, info)
                if (info .ne. 0) then
                    write(*,*) "error! in diagonalization info = ", info
                end if

            return
        end subroutine diagonalize_N_matrix ! }}}

        subroutine diagonalize_V_matrix(this, EigenValue_, EigenVector_) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            real(kind=DP), intent(out) :: EigenValue_(1:this%dim)
            class(matrix), intent(out) :: EigenVector_
            complex(kind=DP) :: work(4*this%dim-1)
            integer(kind=IT) :: rwork(3*this%dim-2)

            integer(kind=IT) :: n, lwork
            integer(4) :: info
                n = ubound(this%value,1)
                lwork = 4*n - 1

                call EigenVector_%init(this%dim)
                EigenVector_ = this
                call zheev('V', 'L', n, EigenVector_%value, n, EigenValue_, work, lwork, rwork, info)

                if (info .ne. 0) then
                    write(*,*) "error! in diagonalization info = ", info
                end if

            return
        end subroutine diagonalize_V_matrix ! }}}

        function add_matrix(this, p_) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(*), intent(in) :: p_
            class(matrix), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type (p_)
                    type is (real(kind=DP))
                        res%value(:,:) = this%value(:,:)
                        do i = 1, this%dim
                            res%value(i,i) = res%value(i,i) + p_
                        end do
                    type is (complex(kind=DP))
                        res%value(:,:) = this%value(:,:)
                        do i = 1, this%dim
                            res%value(i,i) = res%value(i,i) + p_
                        end do
                    type is (matrix)
                        res%value(:,:) = this%value(:,:) + p_%value(:,:)
                end select
            return
        end function add_matrix ! }}}

        function add2_matrix(p_, this) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(*), intent(in) :: p_
            class(matrix), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type (p_)
                    type is (real(kind=DP))
                        res%value(:,:) = this%value(:,:)
                        do i = 1, this%dim
                            res%value(i,i) = res%value(i,i) + p_
                        end do
                    type is (complex(kind=DP))
                        res%value(:,:) = this%value(:,:)
                        do i = 1, this%dim
                            res%value(i,i) = res%value(i,i) + p_
                        end do
                    type is (matrix)
                        res%value(:,:) = this%value(:,:) + p_%value(:,:)
                end select
            return
        end function add2_matrix ! }}}

        function minus_matrix(this, p_) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(*), intent(in) :: p_
            class(matrix), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type (p_)
                    type is (real(kind=DP))
                        res%value(:,:) = this%value(:,:)
                        do i = 1, this%dim
                            res%value(i,i) = res%value(i,i) - p_
                        end do
                    type is (complex(kind=DP))
                        res%value(:,:) = this%value(:,:)
                        do i = 1, this%dim
                            res%value(i,i) = res%value(i,i) - p_
                        end do
                    type is (matrix)
                        res%value(:,:) = this%value(:,:) - p_%value(:,:)
                end select
            return
        end function minus_matrix ! }}}

        function minus2_matrix(p_, this) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(*), intent(in) :: p_
            class(matrix), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type (p_)
                    type is (real(kind=DP))
                        res%value(:,:) = this%value(:,:)
                        do i = 1, this%dim
                            res%value(i,i) = p_ - res%value(i,i)
                        end do
                    type is (complex(kind=DP))
                        res%value(:,:) = this%value(:,:)
                        do i = 1, this%dim
                            res%value(i,i) = p_ - res%value(i,i)
                        end do
                    type is (matrix)
                        res%value(:,:) = p_%value(:,:) - this%value(:,:)
                end select
            return
        end function minus2_matrix ! }}}

        function multiply_matrix(this, p_) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(matrix), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:,:) = this%value(:,:) * p_
                    type is (complex(kind=DP))
                        res%value(:,:) = this%value(:,:) * p_
                    type is (matrix)
                        res%value(:,:) = matmul(this%value(:,:), p_%value(:,:))
                end select
            return
        end function multiply_matrix ! }}}

        function multiply2_matrix(p_, this) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(matrix), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:,:) = p_ * this%value(:,:)
                    type is (complex(kind=DP))
                        res%value(:,:) = p_ * this%value(:,:)
                    type is (matrix)
                        res%value(:,:) = matmul(p_%value(:,:), this%value(:,:))
                end select
            return
        end function multiply2_matrix ! }}}

        function divide_matrix(this, p_) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(matrix), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:,:) = this%value(:,:) / p_
                    type is (complex(kind=DP))
                        res%value(:,:) = this%value(:,:) / p_
                    type is (matrix)
                        res = p_%inverse()
                        res%value(:,:) = this%value(:,:) * res%value(:,:)
                end select
            return
        end function divide_matrix ! }}}

        function divide2_matrix(p_, this) result(res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(matrix), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                res = this%inverse()
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:,:) = p_ * res%value(:,:)
                    type is (complex(kind=DP))
                        res%value(:,:) = p_ * res%value(:,:)
                    type is (matrix)
                        res%value(:,:) = p_%value(:,:) * res%value(:,:)
                end select
            return
        end function divide2_matrix ! }}}

        subroutine print_matrix(this, unit_num_) ! {{{
            implicit none
            class(matrix), intent(inout) :: this
            integer(kind=IT), intent(in) :: unit_num_(1:2)
            integer(kind=IT) :: j
            character(:), allocatable :: style

                style = lt//iform1
                do j = 1, this%dim
                    style = style//space//Deform1
                end do
                style = style//rt

                do j = 1, this%dim
                    write(unit_num_(1), style) j,  real(this%value(:, j))
                    write(unit_num_(2), style) j, aimag(this%value(:, j))
                end do
            return
        end subroutine print_matrix ! }}}
end module modMatrix ! }}}
