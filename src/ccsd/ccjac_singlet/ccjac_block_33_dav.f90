 module ccjac_block_33_dav

      use davidson_main

      use cc_gparams 
use symmetry
implicit none
      !
      ! File generated automatically on 2012-03-01 03:52:56 UTC.
      !
contains

      subroutine ccjac_33_dav(sigup_diag, eorb, nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, irrep_idx, bra0, ket0)

            procedure(dav_sigma_update_diag) :: sigup_diag
            double precision, dimension(:), intent(in)          :: eorb
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, dimension(:,:), intent(in)                             :: irrep0
            integer, dimension(:,:), intent(in)                             :: irrep1
            integer, intent(in)                                             :: irrep_idx
            
            integer, intent(in)                                 :: bra0, ket0
            !
            ! Local variables
            !
            double precision :: jac_ibra_iket
            integer :: a, b, c
            integer :: i, j, k
            integer :: ai, bj, ck
            integer :: nocc, nvirt
            integer :: npair, nactive
            integer :: ibra, iket
            integer :: braoffset, ketoffset
            integer :: qbj, qbj2
            integer :: qck, qck2
            integer :: q00
            integer, dimension(6) :: input_irreps
            integer :: output_irrep


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
            qbj  = 3 + 6 * npair
            qbj2 = -3
            qck  = 2 + 3 * npair * (2 + npair)
            qck2 = -3 * (1 + npair)
            q00  = -3 * npair * (3 + npair)

            !
            ! Elementary loop 2704
            ! --------------------
            ! Free virtual indices: a, b, c, d, e
            ! Free occupied indices: i, j, k, l, m
            ! Equalities: none
            ! No equalities independent of the above can hold.
            !


            do ck = 1, npair
                  c = (ck - 1) / nocc + nocc+1
                  k = ck - nocc * (c - (nocc + 1))
                  do bj = ck, npair
                        b = (bj - 1) / nocc + nocc+1
                        j = bj - nocc * (b - (nocc + 1))
                        do ai = bj, npair
                              a = (ai - 1) / nocc + nocc+1
                              i = ai - nocc * (a - (nocc + 1))
                              

                              call test_irrep(irrep0, irrep1, a, i, b, j, c, k, input_irreps)
                              ! print*, input_irreps
                              ! stop

                              call direct_product(input_irreps, POINT_GROUP, output_irrep)

                              if (output_irrep == 6) then
                              
                              if ((i > j .and. j > k) .or. &                ! Cases 1, 3, 4
                                    (i == j .and. a == b) .or. &            ! Cases 5, 6
                                    (j == k .and. b == c) .or. &            ! Cases 7, 8
                                    (a > b .and. b > c .and. i == j) .or. & ! Case 2
                                    (a == b .and. b == c) .or. &            ! Three identical virtual
                                    (i == j .and. j == k)) then             ! Three identical occupied
                                    cycle
                              else
                                    ibra = braoffset + mu3(ai, bj, ck)
                                    !ibra = braoffset + mu3_mem(a, i, b, j, c, k)
                                    iket = ibra
                                    jac_ibra_iket = eorb(a) + eorb(b) + eorb(c)&
                                         - eorb(i) - eorb(j) - eorb(k)
!                                    print*, '33', braoffset, ibra, jac_ibra_iket
                                    write(*, '(9I5, 1I10, 1F20.15)') a, i, b, j, c, k, ai, bj, ck, ibra, jac_ibra_iket
!                                    ! write(*, '(6I4)') input_irreps
                                    ! print*, '--------------------------------------------------------------------'
                                    call sigup_diag(jac_ibra_iket, ibra) 

                              end if
                        end if
                        end do
                  end do
            end do
stop
      contains
            !
            ! Locally visible functions
            !
            function mu3(ai, bj, ck)
                  !
                  ! Compute compound three-electron index
                  ! (assumed that ai >= bj >= ck)
                  !
                  integer :: mu3
                  integer, intent(in) :: ai, bj, ck
                  integer :: mu31, mu32
                  !
                  ! Compound index is compouted relative
                  ! to the first element of matrix block.
                  ! Braoffset/ketoffset should be added
                  ! to get the absolute position
                  !
                  

                  mu31 = (qbj + qbj2 * bj) * bj 
                  mu32 = (qck + (qck2 + ck) * ck) * ck
                  mu3  = ai + (mu31 + mu32 + q00) / 6
            end function mu3


            subroutine test_irrep(irrep0, irrep1, a, l, b, j, c, k, input_irreps)
                  integer, dimension(:,:), intent(in) :: irrep0, irrep1
                  integer, intent(in) :: a, l, b, j, c, k 
                  integer, dimension(:), intent(out) :: input_irreps
                  integer :: i

                  ! print*, a, l, b, j, c, k
                  ! write(*,'(8I4)') irrep0(1, 1:8)
                  ! write(*,'(8I4)') irrep1(1, 1:8)
                  ! write(*,'(8I4)') irrep0(2, 1:8)
                  ! write(*,'(8I4)') irrep1(2, 1:8)

                  do i = 1, 8
                        if (a .ge.irrep0( 2,i) .and. a .le. irrep1(2, i) )then
                              input_irreps(1) = i
                        end if
                        if (l .ge.irrep0(1, i) .and. l .le. irrep1(1, i) )then
                              input_irreps(2) = i
                        end if
                        
                        if (b .ge.irrep0(2, i) .and. b .le. irrep1(2, i) )then
                              input_irreps(3) = i
                        end if
                        
                        if (j .ge.irrep0(1, i) .and. j .le. irrep1(1, i) )then
                              input_irreps(4) = i
                        end if
                        
                        if (c .ge.irrep0(2, i) .and. c .le. irrep1(2, i) )then
                              input_irreps(5) = i
                        end if
                        
                        if (k .ge.irrep0(1, i) .and. k .le. irrep1(1, i) )then
                              input_irreps(6) = i
                        end if
                  end do
!stop
            end subroutine test_irrep
! function mu3_mem(a, i, b, j, c, k)
! integer :: mu3_mem
! integer, intent(in) :: a, i, b, j, c, k
! mu3_mem = offset + (i-n0i)+mi + (a-n0a)*ma + (j-n0j)*mj + (b-n0b)*mb + (k-n0k)*mk  &
! + (c-n0c)*mc
! end function mu3_mem

      end subroutine ccjac_33_dav
end module ccjac_block_33_dav
