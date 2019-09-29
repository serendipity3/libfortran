module modMatrix ! {{{
!  this module uses
!   LAPACK: zgetrf, zgetri
!
    use precisions
    use formats
    type :: matrix ! {{{
        integer(kind=IT) :: dim
        real(kind=DP), allocatable :: value(:,:)
        contains
        procedure, pass :: init => init_matrix
        procedure, pass :: inverse => inverse_matrix
        procedure, pass :: transverse => transverse_matrix
        procedure, pass :: trace => trace_matrix
        procedure, pass :: toArray => convert_toArray_matrix
        procedure, pass :: fromArray => convert_fromArray_matrix

        procedure, pass :: write_formatted_matrix

        procedure, pass :: assign_matrix
        procedure, pass :: assign_array_matrix
        procedure, pass :: add_matrix
        procedure, pass(this) :: add2_matrix
        procedure, pass :: minus_matrix
        procedure, pass(this) :: minus2_matrix
        procedure, pass :: multiply_matrix
        procedure, pass(this) :: multiply2_matrix
        procedure, pass :: divide_matrix
        procedure, pass(this) :: divide2_matrix
        generic :: assignment(=) => assign_matrix, assign_array_matrix
        generic :: operator(+) => add_matrix, add2_matrix
        generic :: operator(-) => minus_matrix, minus2_matrix
        generic :: operator(*) => multiply_matrix, multiply2_matrix
        generic :: operator(/) => divide_matrix, divide2_matrix

        generic :: write(formatted) => write_formatted_matrix
        final :: finalize_matrix
    end type matrix ! }}}
    type :: matrixComplex ! {{{
        integer(kind=IT) :: dim
        complex(kind=DP), allocatable :: value(:,:)
        contains
        procedure, pass :: init => init_matrixComplex
        procedure, pass :: inverse => inverse_matrixComplex
        procedure, pass :: trace => trace_matrixComplex
        procedure, pass :: conjg => conjg_matrixComplex
        procedure, pass :: toArray => convert_toArray_matrixComplex
        procedure, pass :: fromArray => convert_fromArray_matrixComplex
        procedure, pass :: diagonalize_N => diagonalize_N_matrixComplex
        procedure, pass :: diagonalize_V => diagonalize_V_matrixComplex

        procedure, pass :: write_formatted_matrixComplex

        procedure, pass :: assign_matrixComplex
        procedure, pass :: assign_array_matrixComplex
        procedure, pass :: add_matrixComplex
        procedure, pass(this) :: add2_matrixComplex
        procedure, pass :: minus_matrixComplex
        procedure, pass(this) :: minus2_matrixComplex
        procedure, pass :: multiply_matrixComplex
        procedure, pass(this) :: multiply2_matrixComplex
        procedure, pass :: divide_matrixComplex
        procedure, pass(this) :: divide2_matrixComplex
        generic :: assignment(=) => assign_matrixComplex, assign_array_matrixComplex
        generic :: operator(+) => add_matrixComplex, add2_matrixComplex
        generic :: operator(-) => minus_matrixComplex, minus2_matrixComplex
        generic :: operator(*) => multiply_matrixComplex, multiply2_matrixComplex
        generic :: operator(/) => divide_matrixComplex, divide2_matrixComplex
        generic :: write(formatted) => write_formatted_matrixComplex
        final :: finalize_matrixComplex
    end type matrixComplex ! }}}
    contains
! procedures for matrix {{{
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

        subroutine assign_array_matrix(this, m_) ! {{{
            implicit none
            class(matrix), intent(inout) :: this
            class(*), intent(in) :: m_(:)
                select type (m_)
                    type is (real(kind=DP))
                        this%value(:,:) = reshape(m_, shape(this%value))
                end select
            return
        end subroutine assign_array_matrix ! }}}

        function inverse_matrix(this) result (res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(matrix), allocatable :: res
            integer(4) :: info
            integer(4) :: lda
            real(kind=DP) :: work(64*this%dim)
            real(kind=DP) :: A(this%dim,this%dim)
            integer(4) :: ipiv(this%dim)
                lda = this%dim
                allocate(res)
                call res%init(this%dim)
                A(:,:) = this%value(:,:)
                call dgetrf(this%dim, this%dim, A, lda, ipiv, info)
                if (info == 0) then
                    call dgetri(this%dim, A, this%dim, ipiv, work, lda, info)
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

        function transverse_matrix(this) result (res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            class(matrix), allocatable :: res
            integer(kind=IT) :: i, j
                allocate(res)
                call res%init(this%dim)
                do i = 1, this%dim
                    do j = 1, this%dim
                        res%value(j,i) = this%value(i,j)
                    end do
                end do
            return
        end function transverse_matrix ! }}}

        subroutine convert_toArray_matrix(this, res) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            real(kind=DP), intent(out) :: res(1:this%dim*this%dim)
                res(:) = reshape(this%value(:,:), shape(res))
            return
        end subroutine convert_toArray_matrix ! }}}

        subroutine convert_fromArray_matrix(this, res) ! {{{
            implicit none
            class(matrix), intent(inout) :: this
            real(kind=DP), intent(in) :: res(1:this%dim*this%dim)
                this%value(:,:) = reshape(res(:), shape(this%value))
            return
        end subroutine convert_fromArray_matrix ! }}}

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
                    type is (matrix)
                        res%value(:,:) = p_%value(:,:) * res%value(:,:)
                end select
            return
        end function divide2_matrix ! }}}

        subroutine write_formatted_matrix(this, Unit, IOType, argList, IOStatus, IOMessage) ! {{{
            implicit none
            class(matrix), intent(in) :: this
            integer, intent(in) :: Unit
            character(*), intent(in) :: IOType
            integer, intent(in) :: argList(:)
            integer, intent(out) :: IOStatus
            character(*), intent(inout) :: IOMessage

            character(2) :: width_tol, width_dec
            character(6) :: spec
            integer(kind=IT) :: j
            character(:), allocatable :: style

            if(IOType == "LISTDIRECTED" .or. size(argList) < 2)then
                do j = 1, this%dim
                    write(unit=Unit, fmt = *, iostat = IOStatus, iomsg = IOMessage) this%value(:,j)
                end do
                IOStatus = 0
                return
            else
                write(width_tol,'(I2)') argList(1)
                write(width_dec,'(I2)') argList(2)
                spec = 'F'//width_tol//'.'//width_dec
                style = lt
                do j = 1, this%dim
                    style = style//space//spec
                end do
                style = style//rt

                do j = 1, this%dim
                    write(unit = Unit, fmt = style, iostat = IOStatus, iomsg = IOMessage) this%value(:,j)
                end do
                IOStatus = 0
            end if
            return
        end subroutine write_formatted_matrix ! }}}
! }}}

! procedures for matrixComplex {{{
        subroutine init_matrixComplex(this, n) ! {{{
            class(matrixComplex), intent(inout) :: this
            integer(kind=IT), intent(in) :: n
                this%dim = n
                allocate(this%value(1:n,1:n))
            return
        end subroutine init_matrixComplex ! }}}

        subroutine finalize_matrixComplex(this) ! {{{
            implicit none
            type(matrixComplex), intent(inout) :: this
                deallocate(this%value)
            return
        end subroutine finalize_matrixComplex ! }}}

        subroutine assign_matrixComplex(this, m_) ! {{{
            implicit none
            class(matrixComplex), intent(inout) :: this
            class(*), intent(in) :: m_
                select type (m_)
                    type is (real(kind=DP))
                        this%value(:,:) = m_
                    type is (complex(kind=DP))
                        this%value(:,:) = m_
                    type is (matrixComplex)
                        this%value(:,:) = m_%value(:,:)
                end select
            return
        end subroutine assign_matrixComplex ! }}}

        subroutine assign_array_matrixComplex(this, m_) ! {{{
            implicit none
            class(matrixComplex), intent(inout) :: this
            class(*), intent(in) :: m_(:)
                select type (m_)
                    type is (real(kind=DP))
                        this%value(:,:) = reshape(m_, shape(this%value))
                    type is (complex(kind=DP))
                        this%value(:,:) = reshape(m_, shape(this%value))
                end select
            return
        end subroutine assign_array_matrixComplex ! }}}

        function inverse_matrixComplex(this) result (res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            class(matrixComplex), allocatable :: res
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
        end function inverse_matrixComplex ! }}}

        function trace_matrixComplex(this) result (res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            complex(kind=DP), allocatable :: res
            integer(kind=IT) :: i
                allocate(res)
                do i = 1, this%dim
                    res = res + this%value(i,i)
                end do
            return
        end function trace_matrixComplex ! }}}

        function conjg_matrixComplex(this) result (res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            class(matrixComplex), allocatable :: res
            integer(kind=IT) :: i, j
                allocate(res)
                call res%init(this%dim)
                do i = 1, this%dim
                    do j = 1, this%dim
                        res%value(j,i) = conjg(this%value(i,j))
                    end do
                end do
            return
        end function conjg_matrixComplex ! }}}

        subroutine convert_toArray_matrixComplex(this, res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            complex(kind=DP), intent(out) :: res(1:this%dim*this%dim)
                res(:) = reshape(this%value(:,:), shape(res))
            return
        end subroutine convert_toArray_matrixComplex ! }}}

        subroutine convert_fromArray_matrixComplex(this, res) ! {{{
            implicit none
            class(matrixComplex), intent(inout) :: this
            complex(kind=DP), intent(in) :: res(1:this%dim*this%dim)
                this%value(:,:) = reshape(res(:), shape(this%value))
            return
        end subroutine convert_fromArray_matrixComplex ! }}}

        subroutine diagonalize_N_matrixComplex(this, EigenValue_) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
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
        end subroutine diagonalize_N_matrixComplex ! }}}

        subroutine diagonalize_V_matrixComplex(this, EigenValue_, EigenVector_) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            real(kind=DP), intent(out) :: EigenValue_(1:this%dim)
            class(matrixComplex), intent(out) :: EigenVector_
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
        end subroutine diagonalize_V_matrixComplex ! }}}

        function add_matrixComplex(this, p_) result(res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            class(*), intent(in) :: p_
            class(matrixComplex), allocatable:: res
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
                    type is (matrixComplex)
                        res%value(:,:) = this%value(:,:) + p_%value(:,:)
                end select
            return
        end function add_matrixComplex ! }}}

        function add2_matrixComplex(p_, this) result(res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            class(*), intent(in) :: p_
            class(matrixComplex), allocatable:: res
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
                    type is (matrixComplex)
                        res%value(:,:) = this%value(:,:) + p_%value(:,:)
                end select
            return
        end function add2_matrixComplex ! }}}

        function minus_matrixComplex(this, p_) result(res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            class(*), intent(in) :: p_
            class(matrixComplex), allocatable:: res
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
                    type is (matrixComplex)
                        res%value(:,:) = this%value(:,:) - p_%value(:,:)
                end select
            return
        end function minus_matrixComplex ! }}}

        function minus2_matrixComplex(p_, this) result(res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            class(*), intent(in) :: p_
            class(matrixComplex), allocatable:: res
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
                    type is (matrixComplex)
                        res%value(:,:) = p_%value(:,:) - this%value(:,:)
                end select
            return
        end function minus2_matrixComplex ! }}}

        function multiply_matrixComplex(this, p_) result(res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(matrixComplex), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:,:) = this%value(:,:) * p_
                    type is (complex(kind=DP))
                        res%value(:,:) = this%value(:,:) * p_
                    type is (matrixComplex)
                        res%value(:,:) = matmul(this%value(:,:), p_%value(:,:))
                end select
            return
        end function multiply_matrixComplex ! }}}

        function multiply2_matrixComplex(p_, this) result(res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(matrixComplex), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:,:) = p_ * this%value(:,:)
                    type is (complex(kind=DP))
                        res%value(:,:) = p_ * this%value(:,:)
                    type is (matrixComplex)
                        res%value(:,:) = matmul(p_%value(:,:), this%value(:,:))
                end select
            return
        end function multiply2_matrixComplex ! }}}

        function divide_matrixComplex(this, p_) result(res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(matrixComplex), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:,:) = this%value(:,:) / p_
                    type is (complex(kind=DP))
                        res%value(:,:) = this%value(:,:) / p_
                    type is (matrixComplex)
                        res = p_%inverse()
                        res%value(:,:) = this%value(:,:) * res%value(:,:)
                end select
            return
        end function divide_matrixComplex ! }}}

        function divide2_matrixComplex(p_, this) result(res) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(matrixComplex), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                res = this%inverse()
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:,:) = p_ * res%value(:,:)
                    type is (complex(kind=DP))
                        res%value(:,:) = p_ * res%value(:,:)
                    type is (matrixComplex)
                        res%value(:,:) = p_%value(:,:) * res%value(:,:)
                end select
            return
        end function divide2_matrixComplex ! }}}

        subroutine write_formatted_matrixComplex(this, Unit, IOType, argList, IOStatus, IOMessage) ! {{{
            implicit none
            class(matrixComplex), intent(in) :: this
            integer, intent(in) :: Unit
            character(*), intent(in) :: IOType
            integer, intent(in) :: argList(:)
            integer, intent(out) :: IOStatus
            character(*), intent(inout) :: IOMessage

            character(2) :: width_tol, width_dec
            character(6) :: spec
            integer(kind=IT) :: j
            character(:), allocatable :: style

            if(IOType == "LISTDIRECTED" .or. size(argList) < 2)then
                style = lt
                do j = 1, this%dim
                    style = style//space//Dzform1
                end do
                style = style//rt
                do j = 1, this%dim
                    write(unit=Unit, fmt = style, iostat = IOStatus, iomsg = IOMessage) real(this%value(:,j)), aimag(this%value(:,j))
                end do
                IOStatus = 0
                return
            else
                write(width_tol,'(I2)') argList(1)
                write(width_dec,'(I2)') argList(2)
                spec = 'F'//width_tol//'.'//width_dec
                style = lt
                do j = 1, this%dim
                    style = style//space//spec//space//spec
                end do
                style = style//rt

                do j = 1, this%dim
                    write(unit = Unit, fmt = style, iostat = IOStatus, iomsg = IOMessage) real(this%value(:,j)), aimag(this%value(:,j))
                end do
                IOStatus = 0
            end if
            return
        end subroutine write_formatted_matrixComplex ! }}}
! }}}
end module modMatrix ! }}}
