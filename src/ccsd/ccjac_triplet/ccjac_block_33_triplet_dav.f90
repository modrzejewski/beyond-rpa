module ccjac_block_33_triplet_dav

      use davidson_main
      use symmetry
      use cc_gparams
      implicit none

      !
      ! File generated automatically on 2012-03-01 03:52:56 UTC.
      !
contains

      subroutine ccjac_33_triplet_dav(sigup_diag, eorb, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0, &
            irrep0, irrep1, irrep_idx)

            procedure(dav_sigma_update_diag) :: sigup_diag
            double precision, dimension(:), intent(in)          :: eorb
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: bra0, ket0
            !
            ! Local variables
            !
            double precision :: jac_ibra_iket
            integer :: a, b, c
            integer :: i, j, k, l
            integer :: ai, bj, ck
            integer :: nocc, nvirt
            integer :: npair, nactive
            integer :: ibra
            integer :: braoffset, ketoffset
            integer, dimension(6) :: input_irreps
            integer :: output_irrep
            integer :: m0a, m1a, m0b, m1b
            integer :: m0c, m1c
            integer :: m0i, m1i, m0j, m1j
            integer :: m0k, m1k
            integer :: idimt
            integer :: b0, j0
            integer, dimension(:,:), allocatable :: itriples
            integer, intent(in)                  :: irrep_idx
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
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

            ! print*, 'zaczynam blok 33', nocc, nvirt
            ! do a = nvirt0, nvirt1
            !       do i = nocc0, nocc1
            !             do b = nvirt0, nvirt1
            !                   do j = nocc0, nocc1
            !                         do c = nvirt0, b-1
            !                               do k = nocc0, j-1
            !                                     ai = (a - nvirt0) * nocc + (i - nocc0) + 1
            !                                     ibra = braoffset + &
            !                                           (ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
            !                                           (nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
            !                                           ((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
            !                                     jac_ibra_iket = eorb(a) + eorb(b) + eorb(c)&
            !                                           - eorb(i) - eorb(j) - eorb(k)
            !                                     call sigup_diag(jac_ibra_iket, ibra) 
! 
            !                               end do
            !                         end do
            !                   end do
            !             end do
            !       end do
            ! end do



            ! call irrep_triples_triplet(irrep_idx, irrep0, irrep1, nocc0, &
            !       nocc, nvirt0, itriples, POINT_GROUP, idimt, .true.)
            ! allocate(itriples(6, idimt))
            ! call irrep_triples_triplet(irrep_idx, irrep0, irrep1, nocc0, &
            !       nocc, nvirt0, itriples, POINT_GROUP, idimt, .false.)

            call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                 itriples, POINT_GROUP, idimt, .true.)
            allocate(itriples(6, idimt))
            call irrep_triples_triplet(irrep_idx, irrep0, irrep1, &
                  itriples, POINT_GROUP, idimt, .false.)

            

            do l = 1, idimt
                  call loop_boundaries_sp(itriples(1:2, l), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a)
                  call loop_boundaries_sp(itriples(3:4, l), irrep0, irrep1, &
                        m0j, m1j, m0b, m1b)
                  call loop_boundaries_sp(itriples(5:6, l), irrep0, irrep1, &
                        m0k, m1k, m0c, m1c)

                  do c = m0c, m1c
                        do k = m0k, m1k
                              b0 = max(m0b, c + 1)
                              do b = b0, m1b
                                    j0 = max(m0j, k + 1)
                                    do j = j0, m1j
                                          do a = m0a, m1a
                                                do i = m0i, m1i
                                                      ai = (a - nvirt0) * nocc + (i - nocc0) + 1
                                                      ibra = braoffset + &
                                                            (ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
                                                            (nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
                                                            ((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
                                                      jac_ibra_iket = eorb(a) + eorb(b) + eorb(c)&
                                                            - eorb(i) - eorb(j) - eorb(k) 
                                                      call sigup_diag(jac_ibra_iket, ibra) 
  ! jac_temp(ibra, ibra) = jac_ibra_iket
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do





       !      !
      !       ! Elementary loop 1
      !       ! --------------------
      !       ! Free virtual indices: a, b, c, d, e
      !       ! Free occupied indices: i, j, k, l, m
      !       ! Equalities: none
      !       ! No equalities independent of the above can hold.
      !       !

      !       do ai = 1, npair
      !                   do b = nvirt0, nvirt1
      !                         do j = nocc0, nocc1
      !       do c = nvirt0, b-1
      !             do k = nocc0, j-1

      !                               bj = (b - nvirt0) * nocc + (j - nocc0) + 1
      !                               ck = (c - nvirt0) * nocc + (k - nocc0) + 1

      !                                     a = (ai - 1) / nocc + nocc+1
      !                                     i = ai - nocc * (a - (nocc + 1))
      !                                     input_irreps(1) = a
      !                                     input_irreps(2) = i
      !                                     input_irreps(3) = b
      !                                     input_irreps(4) = j
      !                                     input_irreps(5) = c
      !                                     input_irreps(6) = k
      !                                     print*, morep(a), morep(i), morep(b), morep(j), morep(c), morep(k)
      !                                     call direct_product(input_irreps, POINT_GROUP, output_irrep)
      !                                     ibra = braoffset + &
      !                                           (ai-1) * npair * (nvirt - 1)*(nocc - 1)/4 + &
      !                                           (nocc * (nocc - 1) * (b - nvirt0) * (b - 1 - nvirt0)) / 4 + &
      !                                           ((b - nvirt0) * (j - 1) * (j - 2)) / 2 + (c - nvirt0) * (j - 1) + k
      !                                     jac_ibra_iket = eorb(a) + eorb(b) + eorb(c)&
      !                                           - eorb(i) - eorb(j) - eorb(k)
      !                                     print*, output_irrep
      !                                     print*, '33', print_rep(output_irrep)
      !                                     call sigup_diag(jac_ibra_iket, ibra) 
! 

      !                               enddo
      !                         end do
      !                   end do
      !             end do
      !       end do
      end subroutine ccjac_33_triplet_dav
                  
end module ccjac_block_33_triplet_dav
