module ccjac_block_33

      use davidson_main

      use cc_gparams 
      use symmetry
 implicit none
      !
      ! File generated automaticall on 2012-03-01 03:52:56 UTC.
      !
contains

      subroutine ccjac_33(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
            double precision, dimension(:,:), intent(out)       :: jac
            double precision, dimension(:), intent(in)          :: eorb
            double precision, dimension(:, :), intent(in)       :: t1
            double precision, dimension(:, :, :, :), intent(in) :: t2
            integer, intent(in)                                 :: nocc0, nocc1
            integer, intent(in)                                 :: nvirt0, nvirt1
            integer, intent(in)                                 :: bra0, ket0
            !
            ! Local variables
            !
            integer :: a, b, c, d, e
            integer :: i, j, k, l, m
            integer :: ai, bj, ck, dl, em
            integer :: a0, a1, b0, i0, i1, j0
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


            do ck = 1, npair
                  c = (ck - 1) / nocc + nocc+1
                  k = ck - nocc * (c - (nocc + 1))
                  do bj = ck, npair
                        b = (bj - 1) / nocc + nocc+1
                        j = bj - nocc * (b - (nocc + 1))
                        do ai = bj, npair
                              a = (ai - 1) / nocc + nocc+1
                              i = ai - nocc * (a - (nocc + 1))
                              input_irreps(1) = a
                              input_irreps(2) = i
                              input_irreps(3) = b
                              input_irreps(4) = j
                              input_irreps(5) = c
                              input_irreps(6) = k
                              call direct_product(input_irreps, POINT_GROUP, output_irrep)
                              if (output_irrep == 6) then

                              if ((i > j .and. j > k) .or. &                ! Cases 1, 3, 4
                                    (i == j .and. a == b) .or. &            ! Cases 5, 6
                                    (j == k .and. b == c) .or. &            ! Cases 7, 8
                                    (a > b .and. b > c .and. i == j) .or. & ! Case 2
                                    (a == b .and. b == c) .or.&             ! Three identical virtual
                                    (i == j .and. j == k)) then             ! Three identical occupied
                                    cycle
                              else
                                    ibra = braoffset + mu3(ai, bj, ck)
                                    iket = ibra
                                    jac(ibra, iket) = eorb(a) + eorb(b) + eorb(c)&
                                          - eorb(i) - eorb(j) - eorb(k)
                              end if
                        end if
                        end do
                  end do
            end do

      contains
            !
            ! Lo! cally visible functions
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
      end subroutine ccjac_33
end module ccjac_block_33
