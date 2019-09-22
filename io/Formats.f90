module formats ! {{{
    use precisions
    implicit none
!
    character(*), parameter :: space  = ", 2x, "
! FORMAT character sets for output to file {{{
    character(*), parameter :: lt = "("
    character(*), parameter :: rt = ")"

    character(*), parameter :: form1  = "f35.15"
    character(*), parameter :: form2  = form1//space//form1
    character(*), parameter :: form3  = form2//space//form1
    character(*), parameter :: form4  = form3//space//form1
    character(*), parameter :: form5  = form4//space//form1
    character(*), parameter :: form6  = form5//space//form1
    character(*), parameter :: form7  = form6//space//form1
    character(*), parameter :: form8  = form7//space//form1
    character(*), parameter :: form9  = form8//space//form1
!
    character(*), parameter :: eform1 = "e22.15e2"
    character(*), parameter :: eform2 = eform1//space//eform1
    character(*), parameter :: eform3 = eform2//space//eform1
    character(*), parameter :: eform4 = eform3//space//eform1
    character(*), parameter :: eform5 = eform4//space//eform1
    character(*), parameter :: eform6 = eform5//space//eform1
    character(*), parameter :: eform7 = eform6//space//eform1
    character(*), parameter :: eform8 = eform7//space//eform1
    character(*), parameter :: eform9 = eform8//space//eform1
!
    character(*), parameter :: peform1 = "pe22.15e2"
    character(*), parameter :: peform2 = peform1//space//peform1
    character(*), parameter :: peform3 = peform2//space//peform1
    character(*), parameter :: peform4 = peform3//space//peform1
    character(*), parameter :: peform5 = peform4//space//peform1
    character(*), parameter :: peform6 = peform5//space//peform1
    character(*), parameter :: peform7 = peform6//space//peform1
    character(*), parameter :: peform8 = peform7//space//peform1
    character(*), parameter :: peform9 = peform8//space//peform1
!
    character(*), parameter :: zform1 = form1//space//form1
    character(*), parameter :: zform2 = zform1//space//zform1
    character(*), parameter :: zform3 = zform2//space//zform1
    character(*), parameter :: zform4 = zform3//space//zform1
    character(*), parameter :: zform5 = zform4//space//zform1
    character(*), parameter :: zform6 = zform5//space//zform1
    character(*), parameter :: zform7 = zform6//space//zform1
    character(*), parameter :: zform8 = zform7//space//zform1
    character(*), parameter :: zform9 = zform8//space//zform1
!
    character(*), parameter :: iform1 = "i10.9"
    character(*), parameter :: iform2 = iform1//space//iform1
    character(*), parameter :: iform3 = iform2//space//iform1
    character(*), parameter :: iform4 = iform3//space//iform1
    character(*), parameter :: iform5 = iform4//space//iform1
    character(*), parameter :: iform6 = iform5//space//iform1
    character(*), parameter :: iform7 = iform6//space//iform1
    character(*), parameter :: iform8 = iform7//space//iform1
    character(*), parameter :: iform9 = iform8//space//iform1
! }}}
! FORMAT character sets for print to display {{{
    character(*), parameter :: Dlt = '(" |"'//space
    character(*), parameter :: Drt = ')'

    character(*), parameter :: Dform1  = "f0.15"
    character(*), parameter :: Dform2  = Dform1//space//Dform1
    character(*), parameter :: Dform3  = Dform2//space//Dform1
    character(*), parameter :: Dform4  = Dform3//space//Dform1
    character(*), parameter :: Dform5  = Dform4//space//Dform1
    character(*), parameter :: Dform6  = Dform5//space//Dform1
    character(*), parameter :: Dform7  = Dform6//space//Dform1
    character(*), parameter :: Dform8  = Dform7//space//Dform1
    character(*), parameter :: Dform9  = Dform8//space//Dform1
!
    character(*), parameter :: Deform1 = "e22.15e2"
    character(*), parameter :: Deform2 = Deform1//space//Deform1
    character(*), parameter :: Deform3 = Deform2//space//Deform1
    character(*), parameter :: Deform4 = Deform3//space//Deform1
    character(*), parameter :: Deform5 = Deform4//space//Deform1
    character(*), parameter :: Deform6 = Deform5//space//Deform1
    character(*), parameter :: Deform7 = Deform6//space//Deform1
    character(*), parameter :: Deform8 = Deform7//space//Deform1
    character(*), parameter :: Deform9 = Deform8//space//Deform1
!
    character(*), parameter :: Dpeform1 = "pe22.15e2"
    character(*), parameter :: Dpeform2 = Dpeform1//space//Dpeform1
    character(*), parameter :: Dpeform3 = Dpeform2//space//Dpeform1
    character(*), parameter :: Dpeform4 = Dpeform3//space//Dpeform1
    character(*), parameter :: Dpeform5 = Dpeform4//space//Dpeform1
    character(*), parameter :: Dpeform6 = Dpeform5//space//Dpeform1
    character(*), parameter :: Dpeform7 = Dpeform6//space//Dpeform1
    character(*), parameter :: Dpeform8 = Dpeform7//space//Dpeform1
    character(*), parameter :: Dpeform9 = Dpeform8//space//Dpeform1
!
    character(*), parameter :: Dzform1 = Dform1//space//Dform1
    character(*), parameter :: Dzform2 = Dzform1//space//Dzform1
    character(*), parameter :: Dzform3 = Dzform2//space//Dzform1
    character(*), parameter :: Dzform4 = Dzform3//space//Dzform1
    character(*), parameter :: Dzform5 = Dzform4//space//Dzform1
    character(*), parameter :: Dzform6 = Dzform5//space//Dzform1
    character(*), parameter :: Dzform7 = Dzform6//space//Dzform1
    character(*), parameter :: Dzform8 = Dzform7//space//Dzform1
    character(*), parameter :: Dzform9 = Dzform8//space//Dzform1
!
    character(*), parameter :: Diform1 = "i0.9"
    character(*), parameter :: Diform2 = Diform1//space//Diform1
    character(*), parameter :: Diform3 = Diform2//space//Diform1
    character(*), parameter :: Diform4 = Diform3//space//Diform1
    character(*), parameter :: Diform5 = Diform4//space//Diform1
    character(*), parameter :: Diform6 = Diform5//space//Diform1
    character(*), parameter :: Diform7 = Diform6//space//Diform1
    character(*), parameter :: Diform8 = Diform7//space//Diform1
    character(*), parameter :: Diform9 = Diform8//space//Diform1
! }}}
!
    character(*), parameter :: HR = ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '
    character(*), parameter :: NB = ', \' ! no break
    character(:), allocatable :: wformat
    character(24), allocatable :: tmp_format(:)
!
    contains
        subroutine header(str, top_, bottom_) ! {{{
            character(*), intent(in) :: str
            logical, intent(in), optional :: top_, bottom_
            integer(kind=IT) :: len_str
            character(75) :: raw
            character(79) :: deco
            logical :: top_HR, bot_HR
                top_HR = .true.
                bot_HR = .true.
                if ( present(top_) ) top_HR = top_
                if ( present(bottom_) ) bot_HR = bottom_
                len_str = len_trim(str)
                if (len_str < 75) then
                    raw = trim(str)
                else
                    raw = str(1:72)//'...'
                end if
                deco = '| '//raw//' |'
!
                if ( top_HR ) print *, HR
                print *, deco
                if ( bot_HR ) print *, HR
            return
        end subroutine header ! }}}

        subroutine error_header(str, top_, bottom_) ! {{{
            character(*), intent(in) :: str
            logical, intent(in), optional :: top_, bottom_
            integer(kind=IT) :: len_str
            character(67) :: raw
            character(79) :: deco
            logical :: top_HR, bot_HR
                top_HR = .true.
                bot_HR = .true.
                if ( present(top_) ) top_HR = top_
                if ( present(bottom_) ) bot_HR = bottom_
                len_str = len_trim(str)
                if (len_str < 75) then
                    raw = trim(str)
                else
                    raw = str(1:72)//'...'
                end if
                deco = '| *** '//raw//' *** |'
!
                if ( top_HR ) then
                    print *, HR
                    print *, "| - + - + -                        E R R O R                        - + - + - |"
                end if
                print *, deco
                if ( top_HR ) then
                    print *, "|"
                    print *, HR
                end if
            return
        end subroutine error_header ! }}}

        subroutine set_wformat_from_tmp_format(format_in, format_out) ! {{{
            implicit none
            character(24), intent(in) :: format_in(:)
            character(:), allocatable, intent(inout) :: format_out
            integer(kind=IT) :: j
                do j = 1, size(format_in)
                    format_out = format_out//format_in(j)
                end do
            return
        end subroutine set_wformat_from_tmp_format ! }}}

        subroutine join_params(list_, param_, jointed_) ! {{{
            implicit none
            character(*), intent(in) :: list_(:)
            real(kind=DP), intent(in) :: param_(:)
            character(:), allocatable :: jointed_
            integer(kind=IT) :: i

            character(10) :: param
                if (size(list_) == size(param_)) then
                    jointed_ = ""
                    do i = lbound(list_,1), ubound(list_,1)
                        write(param, lt//'1p e9.2e2'//rt) param_(i)
                        jointed_ = jointed_//trim(adjustl(list_(i)))//trim(adjustl(param))

                        if (i /= ubound(list_,1)) jointed_ = jointed_//"_"
                    end do
                else
                    print *, "Error!, size of list_(:) and param_(:) do not match."
                end if
            return
        end subroutine join_params ! }}}
end module formats ! }}}
