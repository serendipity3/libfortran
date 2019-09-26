module modVector ! {{{
    use precisions
    use formats
    type :: vector ! {{{
        integer(kind=IT) :: dim
        real(kind=DP), allocatable :: value(:)
        contains
        procedure, pass :: init => init_vector
        procedure, pass :: length_vector
        procedure, pass :: length_vector_vector
        generic :: length => length_vector, length_vector_vector

        procedure, pass :: assign_vector
        procedure, pass :: assign_vector_vector
        procedure, pass :: add_vector
        procedure, pass :: minus_vector
        procedure, pass :: multiply_vector
        procedure, pass(this) :: multiply2_vector
        procedure, pass :: divide_vector

        procedure, pass :: dot_product_vector
        procedure, pass :: cross_product_vector

        procedure, pass :: write_formatted_vector

        generic :: assignment(=) => assign_vector, assign_vector_vector
        generic :: operator(+) => add_vector
        generic :: operator(-) => minus_vector
        generic :: operator(*) => multiply_vector, multiply2_vector
        generic :: operator(/) => divide_vector

        generic :: operator(.dot.) => dot_product_vector
        generic :: operator(.x.) => cross_product_vector

        generic :: write(formatted) => write_formatted_vector
        final :: finalize_vector
    end type vector ! }}}
    type :: vectorComplex ! {{{
        integer(kind=IT) :: dim
        complex(kind=DP), allocatable :: value(:)
        contains
        procedure, pass :: init => init_vectorComplex
        procedure, pass :: conjg => conjg_vectorComplex
        procedure, pass :: length_vectorComplex
        procedure, pass :: length_vector_vectorComplex
        generic :: length => length_vectorComplex, length_vector_vectorComplex

        procedure, pass :: assign_vectorComplex
        procedure, pass :: assign_vector_vectorComplex
        procedure, pass :: add_vectorComplex
        procedure, pass(this) :: add2_vectorComplex
        procedure, pass :: minus_vectorComplex
        procedure, pass(this) :: minus2_vectorComplex
        procedure, pass :: multiply_vectorComplex
        procedure, pass(this) :: multiply2_vectorComplex
        procedure, pass :: divide_vectorComplex

        procedure, pass :: dot_product_vectorComplex
        procedure, pass :: cross_product_vectorComplex

        procedure, pass :: write_formatted_vectorComplex

        generic :: assignment(=) => assign_vectorComplex, assign_vector_vectorComplex
        generic :: operator(+) => add_vectorComplex, add2_vectorComplex
        generic :: operator(-) => minus_vectorComplex, minus2_vectorComplex
        generic :: operator(*) => multiply_vectorComplex, multiply2_vectorComplex
        generic :: operator(/) => divide_vectorComplex

        generic :: operator(.dot.) => dot_product_vectorComplex
        generic :: operator(.x.) => cross_product_vectorComplex

        generic :: write(formatted) => write_formatted_vectorComplex
        final :: finalize_vectorComplex
    end type vectorComplex ! }}}
    contains
! procedures for vector {{{
        subroutine init_vector(this, n) ! {{{
            class(vector), intent(inout) :: this
            integer(kind=IT), intent(in) :: n
                this%dim = n
                allocate(this%value(1:n))
            return
        end subroutine init_vector ! }}}

        subroutine finalize_vector(this) ! {{{
            implicit none
            type(vector), intent(inout) :: this
                deallocate(this%value)
            return
        end subroutine finalize_vector ! }}}

        function length_vector(this) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            real(kind=DP) :: res
            integer(kind=IT) :: i
                res = 0e0_DP
                do i = 1, this%dim
                    res = res + this%value(i)**2
                end do
            return
        end function length_vector ! }}}

        function length_vector_vector(this, v_) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            class(vector), intent(in) :: v_
            real(kind=DP) :: res
            integer(kind=IT) :: i
                res = 0e0_DP
                do i = 1, this%dim
                    res = res + (this%value(i) - v_%value(i))**2
                end do
            return
        end function length_vector_vector ! }}}

        subroutine assign_vector(this, v_) ! {{{
            implicit none
            class(vector), intent(inout) :: this
            class(*), intent(in) :: v_
                select type (v_)
                    type is (real(kind=DP))
                        this%value(:) = v_
                    type is (vector)
                        this%value(:) = v_%value(:)
                end select
            return
        end subroutine assign_vector ! }}}

        subroutine assign_vector_vector(this, v_) ! {{{
            implicit none
            class(vector), intent(inout) :: this
            class(*), intent(in) :: v_(:)
                select type (v_)
                    type is (real(kind=DP))
                        this%value(:) = v_(:)
                end select
            return
        end subroutine assign_vector_vector ! }}}

        function add_vector(this, p_) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            class(vector), intent(in) :: p_
            class(vector), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                if (this%dim == p_%dim) then
                    res%value(:) = this%value(:) + p_%value(:)
                else
                    print *, "Error, dimension of the vectors does not match."
                end if
            return
        end function add_vector ! }}}

        function minus_vector(this, p_) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            class(vector), intent(in) :: p_
            class(vector), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                if (this%dim == p_%dim) then
                    res%value(:) = this%value(:) - p_%value(:)
                else
                    print *, "Error, dimension of the vectors does not match."
                end if
            return
        end function minus_vector ! }}}

        function multiply_vector(this, p_) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(vector), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:) = this%value(:) * p_
                end select
            return
        end function multiply_vector ! }}}

        function multiply2_vector(p_, this) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(vector), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:) = p_ * this%value(:)
                end select
            return
        end function multiply2_vector ! }}}

        function divide_vector(this, p_) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(vector), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:) = this%value(:) / p_
                end select
            return
        end function divide_vector ! }}}

        function dot_product_vector(this, p_) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            class(vector), intent(in) :: p_
            real(kind=DP) :: res
                if (this%dim == p_%dim) then
                    res = dot_product(this%value(:), p_%value(:))
                else
                    print *, "Error, dimension of the vectors does not match."
                end if
        end function dot_product_vector ! }}}

        function cross_product_vector(this, p_) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            class(vector), intent(in) :: p_
            class(vector), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                if ((this%dim == p_%dim) .and. (this%dim == 3_IT)) then
                    res%value(1) = this%value(2) * p_%value(3) - this%value(3) * p_%value(2)
                    res%value(2) = this%value(3) * p_%value(1) - this%value(1) * p_%value(3)
                    res%value(3) = this%value(1) * p_%value(2) - this%value(2) * p_%value(1)
                else
                    print *, "Error, dimension of the vectors does not match."
                end if
        end function cross_product_vector ! }}}

        subroutine write_formatted_vector(this, Unit, IOType, argList, IOStatus, IOMessage) ! {{{
            implicit none
            class(vector), intent(in) :: this
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
                write(unit=Unit, fmt = *, iostat = IOStatus, iomsg = IOMessage) this%value(:)
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

                write(unit = Unit, fmt = style, iostat = IOStatus, iomsg = IOMessage) this%value(:)
                IOStatus = 0
            end if
            return
        end subroutine write_formatted_vector ! }}}
! }}}

! procedures for vectorComplex {{{
        subroutine init_vectorComplex(this, n) ! {{{
            class(vectorComplex), intent(inout) :: this
            integer(kind=IT), intent(in) :: n
                this%dim = n
                allocate(this%value(1:n))
            return
        end subroutine init_vectorComplex ! }}}

        subroutine finalize_vectorComplex(this) ! {{{
            implicit none
            type(vectorComplex), intent(inout) :: this
                deallocate(this%value)
            return
        end subroutine finalize_vectorComplex ! }}}

        function conjg_vectorComplex(this) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            class(vectorComplex), allocatable:: res
                allocate(res)
                call res%init(this%dim)
                res%value(:) = conjg(this%value(:))
            return
        end function conjg_vectorComplex ! }}}

        function length_vectorComplex(this) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            real(kind=DP) :: res
            integer(kind=IT) :: i
                res = 0e0_DP
                do i = 1, this%dim
                    res = res + this%value(i)**2
                end do
            return
        end function length_vectorComplex ! }}}

        function length_vector_vectorComplex(this, v_) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            class(*), intent(in) :: v_
            real(kind=DP) :: res
            integer(kind=IT) :: i
                res = 0e0_DP
                select type(v_)
                    type is (vector)
                        if (this%dim == v_%dim) then
                            do i = 1, this%dim
                                res = res + (this%value(i) - v_%value(i))**2
                            end do
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                    type is (vectorComplex)
                        if (this%dim == v_%dim) then
                            do i = 1, this%dim
                                res = res + (this%value(i) - v_%value(i))**2
                            end do
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                end select
            return
        end function length_vector_vectorComplex ! }}}

        subroutine assign_vectorComplex(this, v_) ! {{{
            implicit none
            class(vectorComplex), intent(inout) :: this
            class(*), intent(in) :: v_
                select type (v_)
                    type is (real(kind=DP))
                        this%value(:) = v_
                    type is (complex(kind=DP))
                        this%value(:) = v_
                    type is (vector)
                        this%value(:) = v_%value(:)
                    type is (vectorComplex)
                        this%value(:) = v_%value(:)
                end select
            return
        end subroutine assign_vectorComplex ! }}}

        subroutine assign_vector_vectorComplex(this, v_) ! {{{
            implicit none
            class(vectorComplex), intent(inout) :: this
            class(*), intent(in) :: v_(:)
                if (this%dim == size(v_)) then
                    select type (v_)
                        type is (real(kind=DP))
                            this%value(:) = v_(:)
                        type is (complex(kind=DP))
                            this%value(:) = v_(:)
                    end select
                else
                    print *, "Error, dimension of the vectors does not match."
                end if
            return
        end subroutine assign_vector_vectorComplex ! }}}

        function add_vectorComplex(this, p_) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            class(*), intent(in) :: p_
            class(vectorComplex), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (vector)
                        if (this%dim == p_%dim) then
                            res%value(:) = this%value(:) + p_%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                    type is (vectorComplex)
                        if (this%dim == p_%dim) then
                            res%value(:) = this%value(:) + p_%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                end select
            return
        end function add_vectorComplex ! }}}

        function add2_vectorComplex(p_, this) result(res) ! {{{
            implicit none
            class(*), intent(in) :: p_
            class(vectorComplex), intent(in) :: this
            class(vectorComplex), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (vector)
                        if (this%dim == p_%dim) then
                            res%value(:) = p_%value(:) + this%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                    type is (vectorComplex)
                        if (this%dim == p_%dim) then
                            res%value(:) = p_%value(:) + this%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                end select
            return
        end function add2_vectorComplex ! }}}

        function minus_vectorComplex(this, p_) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            class(*), intent(in) :: p_
            class(vectorComplex), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (vector)
                        if (this%dim == p_%dim) then
                            res%value(:) = this%value(:) - p_%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                    type is (vectorComplex)
                        if (this%dim == p_%dim) then
                            res%value(:) = this%value(:) - p_%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                end select
            return
        end function minus_vectorComplex ! }}}

        function minus2_vectorComplex(p_, this) result(res) ! {{{
            implicit none
            class(*), intent(in) :: p_
            class(vectorComplex), intent(in) :: this
            class(vectorComplex), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (vector)
                        if (this%dim == p_%dim) then
                            res%value(:) = p_%value(:) - this%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                    type is (vectorComplex)
                        if (this%dim == p_%dim) then
                            res%value(:) = p_%value(:) - this%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                end select
            return
        end function minus2_vectorComplex ! }}}

        function multiply_vectorComplex(this, p_) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(vectorComplex), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:) = this%value(:) * p_
                    type is (complex(kind=DP))
                        res%value(:) = this%value(:) * p_
                end select
            return
        end function multiply_vectorComplex ! }}}

        function multiply2_vectorComplex(p_, this) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(vectorComplex), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:) = p_ * this%value(:)
                    type is (complex(kind=DP))
                        res%value(:) = p_ * this%value(:)
                end select
            return
        end function multiply2_vectorComplex ! }}}

        function divide_vectorComplex(this, p_) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            CLASS(*), intent(in) :: p_
            class(vectorComplex), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                select type(p_)
                    type is (real(kind=DP))
                        res%value(:) = this%value(:) / p_
                    type is (complex(kind=DP))
                        res%value(:) = this%value(:) / p_
                end select
            return
        end function divide_vectorComplex ! }}}

        function dot_product_vectorComplex(this, p_) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            class(vectorComplex), intent(in) :: p_
            real(kind=DP) :: res
                if (this%dim == p_%dim) then
                    res = dot_product(this%value(:), p_%value(:))
                else
                    print *, "Error, dimension of the vectors does not match."
                end if
        end function dot_product_vectorComplex ! }}}

        function cross_product_vectorComplex(this, p_) result(res) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            class(vectorComplex), intent(in) :: p_
            class(vectorComplex), allocatable :: res
                allocate(res)
                call res%init(this%dim)
                if ((this%dim == p_%dim) .and. (this%dim == 3_IT)) then
                    res%value(1) = this%value(2) * p_%value(3) - this%value(3) * p_%value(2)
                    res%value(2) = this%value(3) * p_%value(1) - this%value(1) * p_%value(3)
                    res%value(3) = this%value(1) * p_%value(2) - this%value(2) * p_%value(1)
                else
                    print *, "Error, dimension of the vectors does not match."
                end if
        end function cross_product_vectorComplex ! }}}

        subroutine write_formatted_vectorComplex(this,unit,iotype,vlist,iostat,iomsg) ! {{{
            implicit none
            class(vectorComplex), intent(in) :: this
            integer,intent(in) :: unit
            character(*),intent(in) :: iotype
            integer,intent(in) :: vlist(:)
            integer,intent(out) :: iostat
            character(*),intent(inout) :: iomsg

            integer(kind=IT) :: j
            character(:), allocatable :: style

                style = lt
                do j = 1, this%dim
                    style = style//space//Dzform1
                end do
                style = style//rt

                write(unit, style), this%value(:)

                iostat = 0
            return
        end subroutine write_formatted_vectorComplex ! }}}
! }}}
end module modVector ! }}}
