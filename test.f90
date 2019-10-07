program main
    use precisions
    use formats
    use ioHelper
    use modVector
    use modMatrix
    use mathConstants

    type(vector) :: a, b, c
    type(vectorComplex) :: s, t, u

    type(matrixComplex) :: p, q, r

        call a%init(3)
        call b%init(3)
        call c%init(3)

        a = (/ 1e0_DP, 0e0_DP, 0e0_DP /)
        b = 1e-1_DP
        c = a + b

        print '(DT(7,3))', a
        print *, a .dot. b
        print *, c%length()
        print *, c%length(b)

        a = units(1:3,1)
        b = units(1:3,2)
        c = a .x. b

        print *, c

        print *, "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"

        call s%init(3)
        call t%init(3)
        call u%init(3)

        s = (/ z1, z0, zi /)
        t = 2e0_DP * zi
        print *, s
        print *, s + c
        print *, s + t
        print *, s%conjg()
 
        print *, "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"

        call p%init(3)
        call q%init(3)
        call r%init(3)

        p = (/ 1e0_DP, 2e0_DP, -1e0_DP,  3e0_DP, -2e0_DP, 1e0_DP,  1e0_DP, 2e0_DP, 2e0_DP /)
        p = (/ (/1e0_DP, 2e0_DP, -1e0_DP/), (/3e0_DP, -2e0_DP, 1e0_DP/), (/1e0_DP, 2e0_DP, 2e0_DP/) /)
        q = 1e0_DP
        print '(DT(7,3))', p

        call p%diagonalize_N(a%value)
        call p%diagonalize_V(a%value, r)
        print *, a

        print *, r

        r = p + q
    stop
end program main
