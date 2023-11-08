module hfexch
      use arithmetic
      use math_constants
      use sort
      use ints
      use gparam

      implicit none

      double precision, private :: threshold

contains

      subroutine hfexch_setthresh(t, kscal)
            double precision, intent(in) :: t
            double precision, intent(in) :: kscal

            if (abs(kscal) > ZERO) then
                  threshold = t / abs(kscal)
            else
                  threshold = huge(ONE)
            end if
      end subroutine hfexch_setthresh


      pure subroutine preorder(n, work, iwork, c, rhomax, erimax, k3, k4)
            integer, intent(out) :: n
            double precision, dimension(:), intent(out) :: work
            integer, intent(out), dimension(:) :: iwork
            integer, intent(in) :: c
            double precision, dimension(:, :), intent(in) :: rhomax
            double precision, dimension(:, :), intent(in) :: erimax
            integer, intent(in) :: k3, k4

            integer :: b0, d
            !
            ! Pre-order D indices according to
            ! |RHO(C, D)| * SQRT(|(B*D|B*D)|), where
            ! B* is the maximizer of (BD|BD), C
            ! fixed
            !
            b0 = sh0(k3)
            n = 1
            do d = sh0(k4), sh0(k4 + 1) - 1
                  work(n) = rhomax(d, c) * erimax(b0, d)
                  iwork(n) = d
                  n = n + 1
            end do
            n = n - 1
            !
            ! Sort array in increasing order.
            ! DSORT subroutine will return
            ! if N .LE. 1
            !
            call dsort(work, iwork, n)
      end subroutine preorder


      pure subroutine search(list, n, thresh, idx)
            ! ------------------------------------------------------------
            ! Perform binary search in a list of numbers sorted in
            ! decreasing order. The output, IDX value, is the index
            ! of the smallest value belonging to LIST which is equal
            ! of greater than THRESH. IDX equal to N is returned if
            ! all numbers belonging to LIST are equal or greater than
            ! THRESH. If multiple numbers are exactly equal to THRESH,
            ! then the IDX output is set equal to the largest possible
            ! index satisfying LIST(IDX) >= THRESH.
            !
            ! ENSURE THAT A ZERO-SIZE ARRAY (N==0) IS NEVER PASSED TO
            ! THIS SUBROUTINE! This condition is never checked here
            ! for performance reasons.
            !
            ! (Monospaced font should be used to read the following
            ! text.)
            ! THRESH = 0.4
            ! LIST = [0.9, 0.8, 0.5, 0.4, 0.1, 0.0]
            !        IDX=4 ------------>| |<-- BELOW THRESHOLD
            ! THRESH = 0.45
            ! LIST = [0.9, 0.8, 0.5, 0.4, 0.1, 0.0]
            !        IDX=3 ------->| |<---- BELOW THRESHOLD
            ! THRESH = 0.1
            ! LIST = [0.2, 0.1, 0.1, 0.0]
            ! IDX=3 -------------->| |<----- BELOW THRESHOLD
            ! ------------------------------------------------------------
            ! LIST 
            !        List of elements
            ! N
            !        Number of elements in LIST
            ! THRESH
            !        Threshold value
            ! IDX  
            !        Output, LIST(IDX) >= THRESH,
            !        LIST(IDX + 1) < THRESH OR
            !        IDX + 1 > N.
            !        IDX == 1 if LIST(1) < THRESH
            !        (i.e., all elements of LIST are
            !        smaller than THRESH)
            !
            double precision, dimension(:), intent(in) :: list
            integer, intent(in) :: n
            double precision, intent(in) :: thresh
            integer, intent(out) :: idx

            integer :: j, k

            if (list(n) .ge. thresh) then
                  idx = n
            else
                  idx = 1
                  j = n
                  bisection: do
                        k = (idx + j) / 2
                        if (thresh .le. list(k)) then
                              idx = k
                        else
                              j = k
                        end if
                        if (idx + 1 .ge. j) then
                              exit bisection
                        end if
                  end do bisection
            end if
      end subroutine search


      pure subroutine onx(rhomax, erimax, ierimax, k1, k2, k3, k4, &
            m1, m2, m3, m4, n1, n2, n3, n4, quartflag, flag, onxwork, ionxwork)
            ! -----------------------------------------------------------------
            ! Generate list of shell-quartets contributing to HF exchange
            ! matrix. Shells are centered at K1, K2, K3, K4 atoms, i.e.
            ! (K1K2|K3K4) batch is considered. If contribution from given ERI
            ! is above acceptance threshold, appropriate flag is set in
            ! QUARTFLAG array.
            !
            ! Integral screening is performed strictly according to Schwarz
            ! inequality:
            ! |RHO(C, D) * (AC|BD)| <= |RHO(C,D)| * SQRT(|(AC|AC)|) 
            !                          * SQRT(|(BD|BD)|)
            ! Presence of density matrix element in the inequality makes this
            ! algorithm efficient in direct SCF approach. 
            ! -----------------------------------------------------------------
            ! 1. Weber, V., Challacombe, M., Parallel algorithm for
            !    the computation of the Hartree-Fock exchange matrix: Gas phase
            !    and periodic parallel ONX, J. Chem. Phys, 125, 104110(2006).
            ! 2. Ochsenfeld, Ch., White, Ch., and Head-Gordon, M., Linear and 
            !    sublinear scaling formation of Hartree-Fock-type exchange
            !    matrices, J. Chem. Phys. 109, 1663(1998).
            ! 3. Schwegler, E. Challacompe, M., and Head-Gordon, M., Linear
            !    scaling computation of the Fock matrix. II. Rigorous bounds on
            !    exchange integrals and incremental Fock build, J. Chem. Phys.,
            !    106, 9708(1997).
            ! 
            !
            double precision, dimension(:, :), intent(in)    :: rhomax
            double precision, dimension(:, :), intent(in)    :: erimax
            integer, dimension(:, :), intent(in)             :: ierimax
            integer, intent(in)                              :: k1, k2, k3, k4
            integer, intent(in)                              :: m1, m2, m3, m4
            integer, intent(in)                              :: n1, n2, n3, n4
            integer, dimension(:), intent(inout)             :: quartflag
            integer, intent(in)                              :: flag
            double precision, dimension(:), intent(out)      :: onxwork
            integer, dimension(:), intent(out)               :: ionxwork

            double precision :: rhocd
            integer :: aa, bb, cc, dd
            integer :: k, n
            integer :: i, j
            integer :: a0, b0, c0
            integer :: a1, b1, c1
            integer :: na, nb
            logical :: contrib
            double precision :: bra, ket
            double precision :: thr
            integer :: idx
            !
            ! Compute contributions 
            ! to lower triangle of the Fock matrix
            !
            if (k1 .lt. k3) return
            !
            ! Considered contributions to the exchange matrix
            ! are of the form:
            ! RHO(C, D) * (AC|BD),
            ! A in K1, B in K3, C in K2, D in K4
            !     
            a0 = sh0(k1)
            na = sh0(k1 + 1) - a0
            b0 = sh0(k3)
            nb = sh0(k3 + 1) - b0
            c0 = sh0(k2)
            c1 = sh0(k2 + 1) - 1

            cloop: do cc = c0, c1
                  !
                  ! Calculate quantities of the form |RHO(C,D)| * SQRT(|(B*D|B*D|)
                  ! and sort in ascending order (C index fixed, B* is maximizer,
                  ! D index is variable) 
                  !
                  call preorder(n, onxwork, ionxwork, cc, rhomax, erimax, k3, k4)
                  !
                  ! ERIMAX(A0, CC) contains SQRT(|(A*C|A*C)|), A* is the maximizer in the subset of
                  ! shells centered at K1. Jump to next iteration if largest possible contibution is
                  ! below threshold.
                  !
                  if (erimax(a0, cc) * onxwork(n) .lt. threshold) then
                        cycle cloop
                  end if
                  dloop: do k = n, 1, -1
                        dd = ionxwork(k)
                        rhocd = rhomax(cc, dd)
                        ket = rhocd * erimax(b0, dd)
                        !
                        ! Exit loop over D shell if
                        ! |RHO(C,D)| * SQRT(|(A*C|A*C)|) * SQRT(|(B*D|B*D)|) < THRESHOLD,
                        ! A*, B* are maximizers in the subsets of integrals.
                        ! If the given inequality is not satisfied, use binary search to find
                        ! A value for which inequality is satisfied or use full range of A
                        ! indices.
                        !
                        if (ket * erimax(a0, cc) .lt. threshold) then
                              exit dloop
                        else
                              call search(erimax(a0:, cc), na, threshold / ket, a1)
                              a1 = a1 + a0 - 1
                        end if
                        bra = rhocd * erimax(a0, cc)
                        !
                        ! Find the maximum index B for which SQRT(|(BD|BD)|) 
                        ! satisfies Schwarz inequality (A is fixed at A* value).
                        ! It is guaranteed that there exists such an index I that
                        ! erimax(B0+I-1, dd) >= THRESHOLD / BRA.
                        !
                        call search(erimax(b0:, dd), nb, threshold / bra, b1)
                        b1 = b1 + b0 - 1
                        thr = threshold / rhocd
                        iloop: do i = a0, a1
                              aa = ierimax(i, cc)
                              bra = erimax(i, cc)
                              contrib = .false.
                              jloop: do j = b0, b1
                                    bb = ierimax(j, dd)
                                    !
                                    ! Only lower half of the exchange matrix
                                    ! is referenced
                                    !
                                    if (bb .gt. aa) cycle jloop
                                    ket = erimax(j, dd)
                                    if (bra * ket .ge. thr) then
                                          !
                                          ! For a given (ab|cd) shell quartet,
                                          ! IDX is invariant of permutation if 
                                          ! correct M1, M2, M3, M4, N1, N2, N3,
                                          ! N4 values were provided. Set bit
                                          ! denoting permutation label given by
                                          ! FLAG value.
                                          !
                                          idx = (aa - m1) * n1 + (cc - m2) * n2 + &
                                                (bb - m3) * n3 + (dd - m4) * n4 + 1
                                          quartflag(idx) = ior(quartflag(idx), flag)
                                          contrib = .true.
                                    else
                                          exit jloop
                                    end if
                              end do jloop
                              ! if (.not. contrib) then
                              !       exit iloop
                              ! end if
                        end do iloop
                  end do dloop
            end do cloop
      end subroutine onx
end module hfexch
