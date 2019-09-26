program main
    use precisions
    use formats
    use ioHelper
    use modVector
    use modMatrix
    use mathConstants

    type(vector) :: a, b, c

    type(matrix) :: p, q, r

        call a%init(3)
        call b%init(3)
        call c%init(3)

        a = (/ 1e0_DP, 0e0_DP, 0e0_DP /)
        b = 1e-1_DP
        c = a + b

        print *, a
        print *, a .dot. b
        print *, c%length()
        print *, c%length(b)

        a = units(1:3,1)
        b = units(1:3,2)
        c = a .x. b

        print *, c

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        call p%init(3)
        call q%init(3)
        call r%init(3)

        p = (/ 1e0_DP, 2e0_DP, -1e0_DP,  3e0_DP, -2e0_DP, 1e0_DP,  1e0_DP, 2e0_DP, 2e0_DP /)
        q = 1e0_DP
        call p%print()

        call p%diagonalize_N(a%value)
        call p%diagonalize_V(a%value, r)
        call a%print()

        call r%print()

        r = p + q
    stop
end program main
