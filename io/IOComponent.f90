module ioComponent  ! {{{
    !!
    !! This module uses 'system call'
    !! call system( mkdir -p )
    !!
    use precisions
!    use exceptation
    use formats
    type :: IOunit ! {{{
        integer(kind=IT) :: unit
        character(:), allocatable :: dir
        character(:), allocatable :: name
        character(:), allocatable :: params
        character(:), allocatable :: file
        character(7) :: status
        type(IOunit), pointer :: next => null()
        contains
        procedure, pass :: init     => init_IOunit
        procedure, pass :: init_dir => init_with_dir_IOunit
        procedure, pass :: init_prm => init_with_parameter_IOunit
        procedure, pass :: open     => open_IOunit
        procedure, pass :: close    => close_IOunit
    end type IOunit! }}}
    type :: IOlist ! {{{
        integer(kind=IT) :: size = 0
        type(IOunit), pointer :: item => null()
        type(IOunit), pointer :: last => null()
        contains
        procedure, pass :: init     => init_IOlist
        procedure, pass :: init_dir => init_with_dir_IOlist
        procedure, pass :: init_prm => init_with_parameter_IOlist
        procedure, pass :: add      => add_IOunit_IOlist
        procedure, pass :: add_dir  => add_IOunit_with_dir_IOlist
        procedure, pass :: add_prm  => add_IOunit_with_parameter_IOlist
        procedure, pass :: get_num  => get_num_IOlist
        procedure, pass :: show     => show_IOlist
        procedure, pass :: rm       => rm_IOunit_IOlist
        procedure, pass :: close    => close_IOlist
        final :: finalize_IOlist
    end type IOlist ! }}}
    contains ! {{{ ! subroutine IOunit {{{
        subroutine init_IOunit(this, unit_, name_, file_, status_) ! {{{
            implicit none
            class(IOunit), intent(inout) :: this
            integer(kind=IT), intent(in) :: unit_
            character(*), intent(in) :: name_, file_, status_
                this%unit   = unit_
                this%name   = trim(adjustl(name_))
                this%file   = trim(adjustl(file_))
                this%status = trim(adjustl(status_))
                call this%open()
            return
        end subroutine init_IOunit ! }}}

        subroutine init_with_dir_IOunit(this, unit_, name_, file_, status_, dir_) ! {{{
            implicit none
            class(IOunit), intent(inout) :: this
            integer(kind=IT), intent(in) :: unit_
            character(*), intent(in) :: name_, file_, status_, dir_
                this%unit   = unit_
                this%name   = trim(adjustl(name_))
                this%dir    = trim(adjustl(dir_))
                this%file   = trim(adjustl(file_))
                this%status = trim(adjustl(status_))
                call this%open()
            return
        end subroutine init_with_dir_IOunit ! }}}

        subroutine init_with_parameter_IOunit(this, unit_, name_, file_, status_, params_) ! {{{
            implicit none
            class(IOunit), intent(inout) :: this
            integer(kind=IT), intent(in) :: unit_
            character(*), intent(in) :: name_, file_, status_, params_
                this%unit   = unit_
                this%name   = trim(adjustl(name_))
                this%params = trim(adjustl(params_))
                this%file   = trim(adjustl(file_))
                this%status = trim(adjustl(status_))
                call this%open()
            return
        end subroutine init_with_parameter_IOunit ! }}}

        subroutine open_IOunit(this)   ! {{{
            implicit none
            class(IOunit), intent(inout) :: this
                open(this%unit, file=trim(this%file), status=trim(this%status))
            return
        end subroutine open_IOunit ! }}}

        subroutine close_IOunit(this)   ! {{{
            implicit none
            class(IOunit), intent(inout) :: this
                close(this%unit)
            return
        end subroutine close_IOunit ! }}}
! }}}
! subroutine IOlist {{{

        subroutine init_IOlist(this, outputfiles_) ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            character(*), intent(in) :: outputfiles_(:,:)
            integer(kind=IT) :: i
            integer(kind=IT) :: unit_num, comp_num
            character(7) :: stat
                allocate(this%item)
                do i = lbound(outputfiles_,2), ubound(outputfiles_,2)
                    read(outputfiles_(1,i), *) comp_num
                    stat = trim(outputfiles_(2,i))
                    call this%add(outputfiles_(3,i), stat, comp_num)
                end do
                call this%show()
            return
        end subroutine init_IOlist ! }}}

        subroutine init_with_dir_IOlist(this, outputfiles_, dir_) ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            character(*), intent(in) :: outputfiles_(:,:)
            character(*), intent(in) :: dir_
            integer(kind=IT) :: i
            integer(kind=IT) :: unit_num, comp_num
            character(7) :: stat
            character(:), allocatable :: command
            character(:), allocatable :: sh_mkdir
                sh_mkdir = "mkdir -p "//dir_
                call system(sh_mkdir)

                allocate(this%item)
                do i = lbound(outputfiles_,2), ubound(outputfiles_,2)
                    read(outputfiles_(1,i), *) comp_num
                    stat = trim(outputfiles_(2,i))
                    call this%add_dir(outputfiles_(3,i), stat, dir_, comp_num)
                end do
                call this%show()
            return
        end subroutine init_with_dir_IOlist ! }}}

        subroutine init_with_parameter_IOlist(this, outputfiles_, param_) ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            character(*), intent(in) :: outputfiles_(:,:)
            character(*), intent(in) :: param_
            integer(kind=IT) :: i
            integer(kind=IT) :: unit_num, comp_num
            character(7) :: stat
            character(:), allocatable :: command

                allocate(this%item)
                do i = lbound(outputfiles_,2), ubound(outputfiles_,2)
                    read(outputfiles_(1,i), *) comp_num
                    stat = trim(outputfiles_(2,i))
                    call this%add_prm(outputfiles_(3,i), stat, param_, comp_num)
                end do
                call this%show()
            return
        end subroutine init_with_parameter_IOlist ! }}}

        subroutine add_IOunit_IOlist(this, name_, status_, component_num_)   ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            character(*), intent(in) :: name_, status_
            integer(kind=IT), intent(in), optional :: component_num_
            integer(kind=IT) :: branch(1:3), i_b1, i_b2, i_b3
            integer(kind=IT) :: i
            integer(kind=IT) :: add_num
            integer(kind=IT) :: unit_number
            type(IOunit), pointer :: new_item
            character(4) :: label
            character(:), allocatable :: file_name
            character(:), allocatable :: work
                if (this%size == 0) then
                    this%last => this%item
                    unit_number = 100
                else
                    allocate(new_item)
                    unit_number = this%last%unit + 1
                    this%last%next => new_item
                    this%last => new_item
                end if
! set "add_num" and "branch" | how many IOunits are added {{{
                if ( present(component_num_ ) .and. ( component_num_ /= 0 ) ) then
                    branch(3) = component_num_/100
                    branch(2) = component_num_/10 - branch(3)*100
                    branch(1) = component_num_ - branch(3)*100 - branch(2)*10
                    add_num = 1
                    do i = 1, 3
                        if ( branch(i) > 0) then
                            add_num = add_num*( branch(i) + 1 )
                        end if
                    end do
                else
                    add_num = 1
                    branch(1:3) = 0
                end if
! }}}
! set label's format {{{
                if ( branch(3) /= 0) then
                    work = lt//'"_", i3.3'//rt
                else
                    if ( branch(2) /= 0) then
                        work = lt//'"_", i2.2'//rt
                    else
                        if( branch(1) /= 0) then
                            work = lt//'"_", i1.1'//rt
                        else
                            label = ""
                        end if
                    end if
                end if
!}}}

                i = 0
                do i_b3 = 0, branch(3)
                do i_b2 = 0, branch(2)
                do i_b1 = 0, branch(1)
                    if( component_num_ /= 0 ) then
                        write(label, work) i_b3*100 + i_b2*10 + i_b1
                        file_name = trim(name_)//trim(label)//".dat"
                    else
                        file_name = trim(name_)//".dat"
                    end if
                    call this%last%init(unit_number+i, trim(name_), trim(file_name), status_)

                    i = i + 1
                    if ( i < add_num ) then
                        allocate(new_item)
                        this%last%next => new_item
                        this%last => new_item
                    end if
                end do
                end do
                end do

                this%size = this%size + add_num
            return
        end subroutine add_IOunit_IOlist ! }}}

        subroutine add_IOunit_with_dir_IOlist(this, name_, status_, dir_, component_num_)   ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            character(*), intent(in) :: name_, status_, dir_
            integer(kind=IT), intent(in), optional :: component_num_
            integer(kind=IT) :: branch(1:3), i_b1, i_b2, i_b3
            integer(kind=IT) :: i
            integer(kind=IT) :: add_num
            integer(kind=IT) :: unit_number
            type(IOunit), pointer :: new_item
            character(4) :: label
            character(:), allocatable :: file_name
            character(:), allocatable :: work
                if (this%size == 0) then
                    this%last => this%item
                    unit_number = 100
                else
                    allocate(new_item)
                    unit_number = this%last%unit + 1
                    this%last%next => new_item
                    this%last => new_item
                end if
! set "add_num" and "branch" | how many IOunits are added {{{
                if ( present(component_num_ ) .and. ( component_num_ /= 0 ) ) then
                    branch(3) = component_num_/100
                    branch(2) = component_num_/10 - branch(3)*100
                    branch(1) = component_num_ - branch(3)*100 - branch(2)*10
                    add_num = 1
                    do i = 1, 3
                        if ( branch(i) > 0) then
                            add_num = add_num*( branch(i) + 1 )
                        end if
                    end do
                else
                    add_num = 1
                    branch(1:3) = 0
                end if
! }}}
! set label's format {{{
                if ( branch(3) /= 0) then
                    work = lt//'"_", i3.3'//rt
                else
                    if ( branch(2) /= 0) then
                        work = lt//'"_", i2.2'//rt
                    else
                        if( branch(1) /= 0) then
                            work = lt//'"_", i1.1'//rt
                        else
                            label = ""
                        end if
                    end if
                end if
!}}}

                i = 0
                do i_b3 = 0, branch(3)
                do i_b2 = 0, branch(2)
                do i_b1 = 0, branch(1)
                    if( component_num_ /= 0 ) then
                        write(label, work) i_b3*100 + i_b2*10 + i_b1
                        file_name = dir_//"/"//trim(name_)//trim(label)//".dat"
                    else
                        file_name = dir_//"/"//trim(name_)//".dat"
                    end if
                    call this%last%init_dir(unit_number+i, trim(name_), trim(file_name), status_, dir_)

                    i = i + 1
                    if ( i < add_num ) then
                        allocate(new_item)
                        this%last%next => new_item
                        this%last => new_item
                    end if
                end do
                end do
                end do

                this%size = this%size + add_num
            return
        end subroutine add_IOunit_with_dir_IOlist ! }}}

        subroutine add_IOunit_with_parameter_IOlist(this, name_, status_, param_, component_num_)   ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            character(*), intent(in) :: name_, status_, param_
            integer(kind=IT), intent(in), optional :: component_num_
            integer(kind=IT) :: branch(1:3), i_b1, i_b2, i_b3
            integer(kind=IT) :: i
            integer(kind=IT) :: add_num
            integer(kind=IT) :: unit_number
            type(IOunit), pointer :: new_item
            character(4) :: label
            character(:), allocatable :: file_name
            character(:), allocatable :: work
                if (this%size == 0) then
                    allocate(this%item)
                    this%last => this%item
                    unit_number = 100
                else
                    allocate(new_item)
                    unit_number = this%last%unit + 1
                    this%last%next => new_item
                    this%last => new_item
                end if
! set "add_num" and "branch" | how many IOunits are added {{{
                if ( present(component_num_ ) .and. ( component_num_ /= 0 ) ) then
                    branch(3) = component_num_/100
                    branch(2) = component_num_/10 - branch(3)*100
                    branch(1) = component_num_ - branch(3)*100 - branch(2)*10
                    add_num = 1
                    do i = 1, 3
                        if ( branch(i) > 0) then
                            add_num = add_num*( branch(i) + 1 )
                        end if
                    end do
                else
                    add_num = 1
                    branch(1:3) = 0
                end if
! }}}
! set label's format {{{
                if ( branch(3) /= 0) then
                    work = lt//'"_", i3.3'//rt
                else
                    if ( branch(2) /= 0) then
                        work = lt//'"_", i2.2'//rt
                    else
                        if( branch(1) /= 0) then
                            work = lt//'"_", i1.1'//rt
                        else
                            label = ""
                        end if
                    end if
                end if
!}}}

                i = 0
                do i_b3 = 0, branch(3)
                do i_b2 = 0, branch(2)
                do i_b1 = 0, branch(1)
                    if( component_num_ /= 0 ) then
                        write(label, work) i_b3*100 + i_b2*10 + i_b1
                        file_name = trim(name_)//trim(label)//"_"//param_//".dat"
                    else
                        file_name = trim(name_)//"_"//param_//".dat"
                    end if
                    call this%last%init_prm(unit_number+i, trim(name_), trim(file_name), status_, param_)

                    i = i + 1
                    if ( i < add_num ) then
                        allocate(new_item)
                        this%last%next => new_item
                        this%last => new_item
                    end if
                end do
                end do
                end do

                this%size = this%size + add_num
            return
        end subroutine add_IOunit_with_parameter_IOlist ! }}}

        subroutine rm_IOunit_IOlist(this, unit_num_) ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            integer(kind=IT), intent(in) :: unit_num_
            integer(kind=IT) :: i
            type(IOunit), pointer :: pointer_item, pointer_prev
                pointer_item => this%item
                pointer_prev => null()
                do while ( associated(pointer_item) )
                    if ( pointer_item%unit == unit_num_) then
                        if ( associated(pointer_prev) ) then
                            pointer_prev%next => pointer_item%next
                        else
                            this%item => this%item%next
                        end if
                        call pointer_item%close()
                        pointer_item%next => null()
                        deallocate(pointer_item)
                    end if
                    pointer_prev => pointer_item
                    pointer_item => pointer_item%next
                end do
            return
        end subroutine rm_IOunit_IOlist ! }}}

        subroutine close_IOlist(this)    ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            type(IOunit), pointer :: pointer_item
                pointer_item => this%item
                do while ( associated(pointer_item) )
                    call this%item%close()
                    pointer_item => pointer_item%next
                end do
            return
        end subroutine close_IOlist  ! }}}

        subroutine show_IOlist(this) ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            type(IOunit), pointer :: pointer_item
            character(:), allocatable :: work
                call header('SHOW IO lists')
                work = lt//'"| unit #: "'//'i4.4'//'",   status: "'//'a7'//'",   file: "'//'a'//rt
!
                pointer_item => this%item
                do while ( associated(pointer_item) )
                    print work, pointer_item%unit, pointer_item%status, pointer_item%file
                    pointer_item => pointer_item%next
                end do
                print *, HR
            return
        end subroutine show_IOlist ! }}}

        function get_num_IOlist(this, name_) result(unit_num_) ! {{{
            implicit none
            class(IOlist), intent(inout) :: this
            character(*), intent(in) :: name_
            integer(kind=IT), allocatable :: unit_num_(:)
            integer(kind=IT) :: i, j
            integer(kind=IT) :: workspace(1:this%size)
            type(IOunit), pointer :: pointer_item
                j = 0
                pointer_item => this%item
                do while ( associated(pointer_item) )
                    if ( pointer_item%name == trim(name_) ) then
                        j =  j + 1
                        workspace(j) = pointer_item%unit
                    end if
                    pointer_item => pointer_item%next
                end do
                allocate(unit_num_(1:j))
                unit_num_(1:j) = workspace(1:j)
            return
        end function get_num_IOlist ! }}}

        subroutine finalize_IOlist(this) ! {{{
            implicit none
            type(IOlist), intent(inout) :: this
                call this%close()
! Debug level 1 {{{
#if debug>=1
                print *, "finalizing IOlist..."

#endif
! }}}
            return
        end subroutine finalize_IOlist ! }}}
! }}}
! }}}
end module ioComponent ! }}}
