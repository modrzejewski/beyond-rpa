module cisd_block_22_mod
      use cisd_block_22_mod_part1
      use cisd_block_22_mod_part2
      use cisd_block_22_mod_part3
      use cisd_block_22_mod_part4
      use cisd_block_22_mod_part5
      implicit none
      !
      ! File generated automatically on 2014-11-10 21:36:57 UTC.
      !
contains
      subroutine cisd_block_22(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, &
            n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0, eorb)
            real(F64), dimension(:,:), intent(inout) :: hci
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: bra0, ket0
            integer, intent(in)                                 :: n0a, n1a, n0b, n1b, n0c, n1c, &
                  n0d, n1d
            integer, intent(in)                                 :: n0i, n1i, n0j, n1j, n0k, n1k, &
                  n0l, n1l
            real(F64), dimension(:), intent(in)                 :: eorb
            !
            ! Generating full CC Jacobian
            !
            call cisd_block_22_part1(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, &
                  n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0)
            call cisd_block_22_part2(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, &
                  n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0)
            call cisd_block_22_part3(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, &
                  n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0, eorb)
            call cisd_block_22_part4(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, &
                  n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0, eorb)
            call cisd_block_22_part5(hci, nocc0, nocc1, nvirt0, nvirt1, n0a, n1a, n0b, n1b, n0c, &
                  n1c, n0d, n1d, n0i, n1i, n0j, n1j, n0k, n1k, n0l, n1l, bra0, ket0, eorb)
      end subroutine cisd_block_22
end module cisd_block_22_mod
