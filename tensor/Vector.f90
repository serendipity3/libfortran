module modVector ! {{{
    use precisions
    use formats
    type :: vector ! {{{
        integer(kind=IT) :: dim
        real(kind=DP), allocatable :: value(:)
        contains
        procedure, pass :: init => init_vector
!        procedure, pass :: conjg => conjg_vector
        procedure, pass :: length_vector
        procedure, pass :: length_vector_vector
        generic :: length => length_vector, length_vector_vector

        procedure, pass :: assign_vector
        procedure, pass :: assign_vector_vector
        procedure, pass :: add_vector
        procedure, pass :: minus_vector
        procedure, pass(this) :: minus2_vector
        procedure, pass :: multiply_vector
        procedure, pass(this) :: multiply2_vector
        procedure, pass :: divide_vector

        procedure, pass :: dot_product_vector
        procedure, pass :: cross_product_vector

        generic :: assignment(=) => assign_vector, assign_vector_vector
        generic :: operator(+) => add_vector
        generic :: operator(-) => minus_vector, minus2_vector
        generic :: operator(*) => multiply_vector, multiply2_vector
        generic :: operator(/) => divide_vector

        generic :: operator(.dot.) => dot_product_vector
        generic :: operator(.x.) => cross_product_vector
        procedure, pass :: print => print_vector
        final :: finalize_vector
    end type vector ! }}}
    contains
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
            class(*), intent(in) :: p_
            class(vector), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type (p_)
                    type is (vector)
                        if (this%dim == p_%dim) then
                            res%value(:) = this%value(:) - p_%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                end select
            return
        end function minus_vector ! }}}

        function minus2_vector(p_, this) result(res) ! {{{
            implicit none
            class(vector), intent(in) :: this
            class(*), intent(in) :: p_
            class(vector), allocatable:: res
            integer(kind=IT) :: i
                allocate(res)
                call res%init(this%dim)
                select type (p_)
                    type is (vector)
                        if (this%dim == p_%dim) then
                            res%value(:) = p_%value(:) - this%value(:)
                        else
                            print *, "Error, dimension of the vectors does not match."
                        end if
                end select
            return
        end function minus2_vector ! }}}

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

        subroutine print_vector(this, unit_num_) ! {{{
            implicit none
            class(vector), intent(inout) :: this
            integer(kind=IT), intent(in), optional :: unit_num_
            integer(kind=IT) :: j
            character(:), allocatable :: style

                style = lt
                do j = 1, this%dim
                    style = style//space//Deform1
                end do
                style = style//rt

                if (present(unit_num_)) then
                    write(unit_num_, style) this%value(:)
                else
                    write(*, style) this%value(:)
                end if
            return
        end subroutine print_vector ! }}}
end module modVector ! }}}
