module modSection !{{{
    use precisions
    use formats
    implicit none
    type, abstract, public :: abstractSection !{{{
        real(kind=DP), private :: frst
        real(kind=DP), private :: last
        integer(kind=IT), public :: mesh
        real(kind=IT), public :: curr = 0e0_DP
        real(kind=DP), public :: step = 0e0_DP

        integer(kind=IT), private :: order = 0
        contains
            procedure(set_abstractSection ), deferred :: set
            procedure(show_abstractSection), deferred :: show
    end type abstractSection

    abstract interface
        subroutine set_abstractSection(this, i_current)
            use precisions
            import :: abstractSection
            class(abstractSection), intent(inout) :: this
            integer(kind=IT), intent(in) :: i_current
        end subroutine set_abstractSection

        subroutine show_abstractSection(this, i_current)
            use precisions
            import :: abstractSection
            class(abstractSection), intent(inout) :: this
            integer(kind=IT), intent(in), optional :: i_current
        end subroutine show_abstractSection
    end interface !}}}

    type, extends(abstractSection) :: Section !{{{
        real(kind=DP) :: power = 0e0_DP
        contains
            procedure, pass :: set  => set_Section
            procedure, pass :: show => show_Section
    end type Section

    interface Section
        module procedure init_Section
    end interface Section !}}}

    type, extends(Section) :: Sec1D !{{{
        type(Section), pointer :: x => null()

        type(Sec1D), pointer :: another_Sec1D => null()
        contains
            final :: finalize_Sec1D
    end type Sec1D

    interface Sec1D
        module procedure init_Sec1D_origin_length
    end interface Sec1D !}}}

    type :: Sec2D !{{{
        type(Section) :: Section(1:2)
        type(Section), pointer :: x => null()
        type(Section), pointer :: y => null()

        type(Sec2D), pointer :: another_Sec2D => null()
        contains
            final :: finalize_Sec2D
    end type Sec2D

    interface Sec2D
        module procedure init_Sec2D_symmetric_xyz, init_Sec2D_center_volume_xyz, init_Sec2D_origin_volume_xyz
    end interface Sec2D !}}}

    type :: Sec3D !{{{
        type(Section) :: Section(1:3)
        type(Section), pointer :: x => null()
        type(Section), pointer :: y => null()
        type(Section), pointer :: z => null()

        type(Sec3D), pointer :: another_Sec3D => null()
        contains
            final :: finalize_Sec3D
    end type Sec3D

    interface Sec3D
        module procedure init_Sec3D_symmetric_xyz, init_Sec3D_center_volume_xyz, init_Sec3D_origin_volume_xyz
    end interface Sec3D !}}}
    contains
         function init_Section(frst, last, mesh,  power) result (this) !{{{
            class(Section), pointer :: this
            real(kind=DP), intent(in) :: frst
            real(kind=DP), intent(in) :: last
            integer(kind=IT), intent(in) :: mesh
            real(kind=DP), intent(in), optional :: power
                allocate(this)
                this%frst = frst
                this%last = last
                this%mesh = mesh
                this%step = (last - frst) / real(mesh+0e0_DP)
                this%order= int(log10(real(this%mesh+0e0_DP))) + 1
                if (present(power)) this%power = power
            return
        end function init_Section !}}}

        subroutine set_Section(this, i_current)!{{{
            class(Section), intent(inout) :: this
            integer(kind=IT), intent(in) :: i_current
                if (this%power == 0e0_DP) then
                    this%curr = this%frst + this%step * real(i_current+0e0_DP)
                else
                end if
            return
        end subroutine set_Section!}}}

        subroutine show_Section(this, i_current)!{{{
            class(Section), intent(inout) :: this
            integer(kind=IT), intent(in), optional :: i_current
            character(5) :: order
                if ( present(i_current) ) then
                    write(order,lt//'"i", i0'//rt) this%order
                    print *, HR
                    print lt//'"|| i_curr = ", '//order//',", curr = "'//peform1//rt, i_current, this%curr
                else
                    call header('SHOW configuration')
                    print lt//'" | mesh  = "'//Diform1//rt,  this%mesh
                    print lt//'" | first = "'//Dpeform1//rt, this%frst
                    print lt//'" | last  = "'//Dpeform1//rt, this%last
                    print lt//'" | step  = "'//Dpeform1//rt, this%step
                    print *, HR
                end if
            return
        end subroutine show_Section!}}}

        function init_Sec1D_origin_length(origin, length, mesh, power) result (this) !{{{
            class(Sec1D), pointer :: this
            real(kind=DP), intent(in) :: origin
            real(kind=DP), intent(in) :: length
            integer(kind=IT), intent(in) :: mesh
            real(kind=DP), intent(in), optional :: power
            real(kind=DP) :: p = 0e0_DP
            real(kind=DP) :: frst, last
                allocate(this)
                if (present(power)) p = power
                frst = origin
                last = origin + length
                this%Section = Section(frst, last, mesh, p)
                this%x => this%Section
            return
        end function init_Sec1D_origin_length !}}}

        function init_Sec2D_symmetric_xyz(frst, last, mesh, power) result (this) !{{{
            integer(kind=IT), parameter :: n = 2
            class(Sec2D), pointer :: this
            real(kind=DP), intent(in) :: frst
            real(kind=DP), intent(in) :: last
            integer(kind=IT), intent(in) :: mesh
            real(kind=DP), intent(in), optional :: power
            integer(kind=IT) :: i
            real(kind=DP) :: p = 0e0_DP
                allocate(this)
                if (present(power)) p = power
                do i = 1, n
                    this%Section(i) = Section(frst, last, mesh, p)
                end do
                this%x => this%Section(1)
                this%y => this%Section(2)
        end function init_Sec2D_symmetric_xyz !}}}

        function init_Sec2D_center_volume_xyz(center, length, mesh) result (this) !{{{
            integer(kind=IT), parameter :: n = 2
            class(Sec2D), pointer :: this
            real(kind=DP), intent(in) :: center(1:n)
            real(kind=DP), intent(in) :: length
            integer(kind=IT), intent(in) :: mesh
            integer(kind=IT) :: i
            real(kind=DP) :: p = 0e0_DP
            real(kind=DP) :: frst(1:n), last(1:n)
                allocate(this)
                do i = 1, n
                    frst(i) = center(i) - length
                    last(i) = center(i) + length
                    this%Section(i) = Section(frst(i), last(i), mesh, p)
                end do
                this%x => this%Section(1)
                this%y => this%Section(2)
        end function init_Sec2D_center_volume_xyz !}}}

        function init_Sec2D_origin_volume_xyz(origin, length, mesh, power) result (this) !{{{
            integer(kind=IT), parameter :: n = 2
            class(Sec2D), pointer :: this
            real(kind=DP), intent(in) :: origin(1:n)
            real(kind=DP), intent(in) :: length(1:n)
            integer(kind=IT), intent(in) :: mesh
            real(kind=DP), intent(in), optional :: power
            integer(kind=IT) :: i
            real(kind=DP) :: p = 0e0_DP
            real(kind=DP) :: frst(1:n), last(1:n)
                allocate(this)
                if (present(power)) p = power
                do i = 1, n
                    frst(i) = origin(i)
                    last(i) = origin(i) + length(i)
                    this%Section(i) = Section(frst(i), last(i), mesh, p)
                end do
                this%x => this%Section(1)
                this%y => this%Section(2)
        end function init_Sec2D_origin_volume_xyz !}}}

        function init_Sec3D_symmetric_xyz(frst, last, mesh, power) result (this) !{{{
            integer(kind=IT), parameter :: n = 3
            class(Sec3D), pointer :: this
            real(kind=DP), intent(in) :: frst
            real(kind=DP), intent(in) :: last
            integer(kind=IT), intent(in) :: mesh
            real(kind=DP), intent(in), optional :: power
            integer(kind=IT) :: i
            real(kind=DP) :: p = 0e0_DP
                allocate(this)
                if (present(power)) p = power
                do i = 1, n
                    this%Section(i) = Section(frst, last, mesh, p)
                end do
                this%x => this%Section(1)
                this%y => this%Section(2)
                this%z => this%Section(3)
        end function init_Sec3D_symmetric_xyz !}}}

        function init_Sec3D_center_volume_xyz(center, length, mesh) result (this) !{{{
            integer(kind=IT), parameter :: n = 3
            class(Sec3D), pointer :: this
            real(kind=DP), intent(in) :: center(1:n)
            real(kind=DP), intent(in) :: length
            integer(kind=IT), intent(in) :: mesh
            integer(kind=IT) :: i
            real(kind=DP) :: p = 0e0_DP
            real(kind=DP) :: frst(1:n), last(1:n)
                allocate(this)
                do i = 1, n
                    frst(i) = center(i) - length
                    last(i) = center(i) + length
                    this%Section(i) = Section(frst(i), last(i), mesh, p)
                end do
                this%x => this%Section(1)
                this%y => this%Section(2)
                this%z => this%Section(3)
        end function init_Sec3D_center_volume_xyz !}}}

        function init_Sec3D_origin_volume_xyz(origin, length, mesh, power) result (this) !{{{
            integer(kind=IT), parameter :: n = 3
            class(Sec3D), pointer :: this
            real(kind=DP), intent(in) :: origin(1:n)
            real(kind=DP), intent(in) :: length(1:n)
            integer(kind=IT), intent(in) :: mesh
            real(kind=DP), intent(in), optional :: power
            integer(kind=IT) :: i
            real(kind=DP) :: p = 0e0_DP
            real(kind=DP) :: frst(1:n), last(1:n)
                allocate(this)
                if (present(power)) p = power
                do i = 1, n
                    frst(i) = origin(i)
                    last(i) = origin(i) + length(i)
                    this%Section(i) = Section(frst(i), last(i), mesh, p)
                end do
                this%x => this%Section(1)
                this%y => this%Section(2)
                this%z => this%Section(3)
        end function init_Sec3D_origin_volume_xyz !}}}

        subroutine finalize_Sec1D(this) !{{{
            type(Sec1D), intent(inout) :: this
                nullify(this%x)
                if(associated(this%another_Sec1D)) deallocate(this%another_Sec1D)
            return
        end subroutine finalize_Sec1D !}}}

        subroutine finalize_Sec2D(this) !{{{
            type(Sec2D), intent(inout) :: this
                nullify(this%x)
                nullify(this%y)
                if(associated(this%another_Sec2D)) deallocate(this%another_Sec2D)
            return
        end subroutine finalize_Sec2D !}}}

        subroutine finalize_Sec3D(this) !{{{
            type(Sec3D), intent(inout) :: this
                nullify(this%x)
                nullify(this%y)
                nullify(this%z)
                if(associated(this%another_Sec3D)) deallocate(this%another_Sec3D)
            return
        end subroutine finalize_Sec3D !}}}

end module modSection !}}}
