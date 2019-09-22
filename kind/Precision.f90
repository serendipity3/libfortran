module precisions ! {{{
    integer, parameter :: DP = selected_real_kind(15, 300)
    integer, parameter :: IT = selected_int_kind(10)
    real(kind=DP), parameter :: EPS = 1e-1_DP**(PRECISION(1e0_DP))
end module precisions ! }}}
