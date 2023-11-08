module ccjac_block_diag
      use eom_ccsd_22_trans
      use eom_ccsd_11_trans
      use davidson_main
      use math_constants
      use arithmetic
      use cmpidx
      use cc_gparams 
 implicit none
!
! File generated automatically on 2013-07-13 00:46:55 UTC.
!
contains

      subroutine ccjac_11_diag(guess, t2, nocc0, nocc1, nvirt0, nvirt1, &
            n0a, n1a, n0i, n1i, bra0, ket0, sdim)
            real(F64), dimension(:), intent(inout) :: guess
            real(F64), dimension(:, :, :, :), intent(in) :: t2
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: n0a, n1a
            integer, intent(in)                                 :: n0i, n1i
            integer, intent(in)                                 :: bra0, ket0
            integer, intent(inout) :: sdim
            real(F64) :: jac_ibra_iket
            integer :: a
            integer :: i
            integer :: ai
            integer :: nocc, nvirt
            integer :: npair, nactive
            integer :: ibra
            integer :: braoffset, ketoffset
            !
            ! Offset of the jacobian blocks                                                                                                                          
            !
            braoffset = bra0 - 1
            ketoffset = ket0 - 1
            !
            ! Number of occupied and virtual orbitals                                                               
            ! present in calculations                     
            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nactive = nocc + nvirt

            !                                                                                                                                
            ! Elementary loop diag_3                                                                                                        
            ! --------------------                                                                                                             
            ! Free virtual indices: a                                                                                                           
            ! Free occupied indices: i                                                                                                        
            ! Equalities: b == a, j == i                                                                                                           
            ! No equalities independent of the above can hold.                                                                               
            !                                                                                                                         
            a_aiai: do a = n0a, n1a
                  i_aiai: do i = n0i, n1i
                        ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                        ibra = braoffset + ai
                        jac_ibra_iket = eom_ccsd_11_trans_aiai(t2, nocc, nactive, a, i)
                        guess(ibra) = jac_ibra_iket
                        sdim = sdim + 1
                  end do i_aiai
            end do a_aiai

      end subroutine ccjac_11_diag

      subroutine ccjac_22_diag(guess, t2, nocc0, nocc1, nvirt0, &
            nvirt1, n0a, n1a, n0b, n1b, n0i, n1i, n0j, n1j, &
            bra0, ket0, ddim) 
            real(F64), dimension(:), intent(inout) :: guess
            real(F64), dimension(:, :, :, :), intent(in) :: t2
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: n0a, n1a, n0b, n1b
            integer, intent(in)                                 :: n0i, n1i, n0j, n1j
            integer, intent(in)                                 :: bra0, ket0
            integer, intent(inout) :: ddim
            !
            ! Local variables
            !
            real(F64) :: jac_ibra_iket
            integer :: a, b, c, d
            integer :: i, j, k
            integer :: ai, bj
            integer :: a0, a1, i0, i1
            integer :: n0ab, n0abc, n0abcd, n0abd, n0ac
            integer :: n0acd, n0bcd, n0bd, n0cd, n0ij
            integer :: n0ijkl, n0ijl, n0ik, n0ikl, n0jkl
            integer :: n0jl, n0kl
            integer :: n1ab, n1abc, n1abcd, n1abd, n1ac
            integer :: n1acd, n1bcd, n1bd, n1cd, n1ij
            integer :: n1ijkl, n1ijl, n1ik, n1ikl, n1jkl
            integer :: n1jl, n1kl
            integer :: nocc, nvirt
            integer :: npair, nactive
            integer :: ibra, iket
            integer :: braoffset, ketoffset
            !
            ! Offset of the jacobian blocks
            !
            braoffset = bra0 - 1
            ketoffset = ket0 - 1

            !
            ! Number of occupied and virtual orbitals
            ! present in calculations
            !
            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1
            npair = nocc * nvirt
            nactive = nocc + nvirt
            n0ab = max(n0a, n0b)
            n0ij = max(n0i, n0j)
            n1ab = min(n1a, n1b)
            n1ij = min(n1i, n1j)



            !                                                                                                                                        
            ! Elementary loop diag_10                                                                                                     
            ! --------------------                                                                                                                                  
            ! Free virtual indices: a, b                                                                                                                            
            ! Free occupied indices: i, j                                                                                                   
            ! Equalities: c == a, d == b, k == i, l == j                                                                                      
            ! No equalities independent of the above can hold.                                                                              
            !                                                                                                                         
            b_aibjaibj: do b = n0b, n1b
                  j_aibjaibj: do j = n0j, n1j
                        bj = (b - nvirt0) * nocc + (j - nocc0) + 1
                        a0 = max(b + 1, n0a)
                        a_aibjaibj: do a = a0, n1a
                              if (a == b) cycle a_aibjaibj
                              i_aibjaibj: do i = n0i, n1i
                                    if (i == j) cycle i_aibjaibj
                                    ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                    ibra = braoffset + &
                                          ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                                    jac_ibra_iket = eom_ccsd_22_trans_aibjaibj(t2, nocc, nactive, a, i, b, j)
                                    guess(ibra) = jac_ibra_iket
                                    ddim = ddim + 1
                              end do i_aibjaibj
                        end do a_aibjaibj
                  end do j_aibjaibj
            end do b_aibjaibj

            !
            ! Elementary loop diag_4
            ! --------------------
            ! Free virtual indices: a
            ! Free occupied indices: i, j
            ! Equalities: b == a, c == a, d == a, k == i, l == j
            ! No equalities independent of the above can hold.
            !
            j_aiajaiaj: do j = n0j, n1j
                  a_aiajaiaj: do a = n0ab, n1ab
                        bj = (a - nvirt0) * nocc + (j - nocc0) + 1
                        i0 = max(j + 1, n0i)
                        i_aiajaiaj: do i = i0, n1i
                              if (i == j) cycle i_aiajaiaj
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              jac_ibra_iket = eom_ccsd_22_trans_aiajaiaj(t2, nocc, nactive, a, i, j)
                              guess(ibra) = jac_ibra_iket
                              ddim = ddim + 1
                        end do i_aiajaiaj
                  end do a_aiajaiaj
            end do j_aiajaiaj
            !
            ! Elementary loop diag_9
            ! --------------------
            ! Free virtual indices: a, b
            ! Free occupied indices: i
            ! Equalities: c == a, d == b, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            b_aibiaibi: do b = n0b, n1b
                  a0 = max(b + 1, n0a)
                  a_aibiaibi: do a = a0, n1a
                        if (a == b) cycle a_aibiaibi
                        i_aibiaibi: do i = n0ij, n1ij
                              ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                              bj = (b - nvirt0) * nocc + (i - nocc0) + 1
                              ibra = braoffset + &
                                    ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                              jac_ibra_iket = eom_ccsd_22_trans_aibiaibi(t2, nocc, nactive, a, i, b)
                              guess(ibra) = jac_ibra_iket
                              ddim = ddim + 1
                        end do i_aibiaibi
                  end do a_aibiaibi
            end do b_aibiaibi
            !
            ! Elementary loop diag_12
            ! --------------------
            ! Free virtual indices: a
            ! Free occupied indices: i
            ! Equalities: b == a, c == a, d == a, j == i, k == i, l == i
            ! No equalities independent of the above can hold.
            !
            a_aiaiaiai: do a = n0ab, n1ab
                  i_aiaiaiai: do i = n0ij, n1ij
                        ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                        bj = (a - nvirt0) * nocc + (i - nocc0) + 1
                        ibra = braoffset + &
                              ((2 * npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1
                        jac_ibra_iket = eom_ccsd_22_trans_aiaiaiai(t2, nocc, nactive, a, i)
                        guess(ibra) = jac_ibra_iket
                        ddim = ddim + 1
                  end do i_aiaiaiai
            end do a_aiaiaiai
      end subroutine ccjac_22_diag
end module ccjac_block_diag
