module drv_wm_intermediates_init

      use arithmetic
      use scf
      use density_matrix_gr_exc
      use density_matrix_ground
!      use density_exc_exc_functions
!      use density_exc_exc_functions_pt0123
!      use density_exc_exc_functions_pt4
!      use density_exc_exc_functions_triplet_pt012
!      use density_exc_exc_functions_triplet_pt3
!      use density_exc_exc_functions_triplet_pt4

 !     use density_exc_exc_functions_triplet
 !     use density_exc_exc_functions_cc3
  !    use density_exc_exc_functions_cc3_triplet_pt0123
   !   use density_exc_exc_functions_cc3_triplet_pt4
    !  use density_exc_exc_functions_cc3_pt0123
     ! use density_exc_exc_functions_cc3_pt4

      !use density_exc_exc_functions_so_pt0123_sym
      !use density_exc_exc_functions_so_pt0123_asym
      !use density_exc_exc_functions_cc3_so_pt0123
      use ccsd
      use eom_vectors
      use symmetry

      use ss_ccsd_pt012
      use ss_ccsd_pt3
      use ss_ccsd_pt4

      use ss_cc3_pt012
      use ss_cc3_pt3a
      use ss_cc3_pt3b
      use ss_cc3_pt4

      use tt_ccsd_pt012a
      use tt_ccsd_pt012b
      use tt_ccsd_pt3a
      use tt_ccsd_pt3b
      use tt_ccsd_pt4a
      use tt_ccsd_pt4b
      use tt_ccsd_pt4c
      use tt_ccsd_pt4d

      use tt_cc3_pt012a
      use tt_cc3_pt012b
      use tt_cc3_pt3a
      use tt_cc3_pt3b
      use tt_cc3_pt3c
      use tt_cc3_pt4

      use so_ccsd_pt012
      use so_ccsd_pt3
      use so_ccsd_pt4a
      use so_ccsd_pt4b

     use so_cc3_pt012
      use so_cc3_pt3a
      use so_cc3_pt3b
      use so_cc3_pt4




      implicit none

contains

      subroutine exc_exc_init(method, mocoeff, nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt, nactive, &
            t1, t2, s1, s2, rveck1, rveck2, pt)

            integer, intent(in) :: method
            real(F64), dimension(:,:), intent(in) :: mocoeff
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1
            integer, intent(in) :: nocc, nvirt, nactive
            real(F64), dimension(:,:), intent(in) :: t1, s1
            real(F64), dimension(:,:,:,:), intent(in) :: t2, s2
            real(F64), dimension(:), intent(in) :: rveck1, rveck2
            integer, intent(in) :: pt

            call eom_vectors_init(nocc0, nocc1, nvirt0, nvirt1, nocc, nvirt)

            select case(pt)
            case(0)
                  if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_init_pt0(nocc, nactive)
                        call wm_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_intermediates_cc3_init_pt0(nocc, nactive)
                              call wm_intermediates_cc3_pt0(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_init_pt0(nocc, nactive)
                        call wm_triplet_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        
                        !call wm_triplet_intermediates_ccsd_init(nocc, nactive)
                        !call wm_triplet_intermediates_ccsd(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                  else if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_init_pt0(nocc, nactive)
                        call wm_so_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)

                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_init_pt0(nocc, nactive)
                              call wm_so_intermediates_cc3_pt0(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        end if
                 else if (cc_multip == cc_mixed_left) then
                        call wm_so_left_intermediates_ccsd_init_pt0(nocc, nactive)
                        call wm_so_left_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                  end if

            case(1)
                 if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_init_pt1(nocc, nactive)
                        call wm_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_intermediates_cc3_init_pt1(nocc, nactive)
                                    call wm_intermediates_cc3_pt1(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_init_pt1(nocc, nactive)
                        call wm_triplet_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_triplet_intermediates_cc3_init_pt1(nocc, nactive)
                              call wm_triplet_intermediates_cc3_pt1(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_init_pt1(nocc, nactive)
                        call wm_so_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)                        
                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_init_pt1(nocc, nactive)
                              call wm_so_intermediates_cc3_pt1(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        end if
                  else if (cc_multip == cc_mixed_left) then
                        call wm_so_left_intermediates_ccsd_init_pt1(nocc, nactive)
                        call wm_so_left_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                  end if

            case(2)
                 if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_init_pt2(nocc, nactive)
                        call wm_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_intermediates_cc3_init_pt2(nocc, nactive)
                                    call wm_intermediates_cc3_pt2(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_init_pt2(nocc, nactive)
                        call wm_triplet_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_triplet_intermediates_cc3_init_pt2(nocc, nactive)
                              call wm_triplet_intermediates_cc3_pt2(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_init_pt2(nocc, nactive)
                        call wm_so_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)

                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_init_pt2(nocc, nactive)
                              call wm_so_intermediates_cc3_pt2(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        end if
                  else if (cc_multip == cc_mixed_left) then
                        call wm_so_left_intermediates_ccsd_init_pt2(nocc, nactive)
                        call wm_so_left_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                  end if
            case(3)
                  if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_init_pt3(nocc, nactive)
                        call wm_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_intermediates_cc3_init_pt3(nocc, nactive)
                              call wm_intermediates_cc3_pt3(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_init_pt3(nocc, nactive)
                        call wm_triplet_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_triplet_intermediates_cc3_init_pt3(nocc, nactive)
                              call wm_triplet_intermediates_cc3_pt3(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_init_pt3(nocc, nactive)
                        call wm_so_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        
                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_init_pt3(nocc, nactive)
                              call wm_so_intermediates_cc3_pt3(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        end if
                  else if (cc_multip == cc_mixed_left) then
                        call wm_so_left_intermediates_ccsd_init_pt3(nocc, nactive)
                        call wm_so_left_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                  end if

            case(4)
                  if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_init_pt4(nocc, nactive)
                        call wm_intermediates_ccsd_pt4(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_intermediates_cc3_init_pt4(nocc, nactive)
                              call wm_intermediates_cc3_pt4(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_init_pt4(nocc, nactive)
                        call wm_triplet_intermediates_ccsd_pt4(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        if (method .eq. THEORY_CC3) then
                              call wm_triplet_intermediates_cc3_init_pt4(nocc, nactive)
                              call wm_triplet_intermediates_cc3_pt4(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                              print*, 'DONE'
                        end if
                  else if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_init_pt4(nocc, nactive)
                        call wm_so_intermediates_ccsd_pt4(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)

                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_init_pt4(nocc, nactive)
                              call wm_so_intermediates_cc3_pt4(t2, t1, s2, s1, nocc, nactive, rveck1, rveck2)
                        end if
                  end if
            end select


      end subroutine exc_exc_init

      subroutine exc_exc_free(method, pt)

            integer, intent(in) :: method
            integer, intent(in) :: pt


            select case(pt)
            case(0)
                  if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_free_pt0()
                        if (method .eq. THEORY_CC3) then
                              call wm_intermediates_cc3_free_pt0()
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_free_pt0()
                        !call wm_triplet_intermediates_ccsd_free()
                  else if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_free_pt0()
                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_free_pt0()
                        end if
                  end if
            case(1)
                  if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_free_pt1()
                        if (method .eq. THEORY_CC3) then
                                    call wm_intermediates_cc3_free_pt1()
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_free_pt1()
                        if (method .eq. THEORY_CC3) then
                              call wm_triplet_intermediates_cc3_free_pt1()
                        end if
                  else  if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_free_pt1()
                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_free_pt1()
                        end if

                  end if
            case(2)
                  if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_free_pt2()
                        if (method .eq. THEORY_CC3) then
                                    call wm_intermediates_cc3_free_pt2()
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_free_pt2()
                        if (method .eq. THEORY_CC3) then
                              call wm_triplet_intermediates_cc3_free_pt2()
                        end if
                  else  if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_free_pt2()
                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_free_pt2()
                        end if

                  end if
            case(3)
                  if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_free_pt3()
                        if (method .eq. THEORY_CC3) then
                                    call wm_intermediates_cc3_free_pt3()
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_free_pt3()
                        if (method .eq. THEORY_CC3) then
                              call wm_triplet_intermediates_cc3_free_pt3()
                        end if
                  else if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_free_pt3()
                        if (method .eq. THEORY_CC3) then
                              call wm_so_intermediates_cc3_free_pt3()
                        end if
                  end if
            case(4)
                  if (cc_multip == cc_singlet) then
                        call wm_intermediates_ccsd_free_pt4()
                        if (method .eq. THEORY_CC3) then
                                    call wm_intermediates_cc3_free_pt4()
                        end if
                  else if (cc_multip == cc_triplet) then
                        call wm_triplet_intermediates_ccsd_free_pt4()
                        if (method .eq. THEORY_CC3) then
                              call wm_triplet_intermediates_cc3_free_pt4()
                        end if
                  else if (cc_multip == cc_mixed) then
                        call wm_so_intermediates_ccsd_free_pt4()
                        if (method .eq. THEORY_CC3) then
                                    call wm_so_intermediates_cc3_free_pt4()
                        end if
                  end if

            end select

      end subroutine exc_exc_free

end module drv_wm_intermediates_init
