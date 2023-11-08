module mp2
      use math_constants
      use display
      use gparam
      use basis
      use ints
      use images
      use linalg

      implicit none

contains

      function ijtriang(k0, k1)
            !
            ! Number of (i, j) pairs satisfying the
            ! following conditions:
            ! 1. k0 <= i, j <= k1
            ! 2. i >= j
            !
            integer             :: ijtriang
            integer, intent(in) :: k0, k1
            integer :: d

            if (k0 .le. k1) then
                  d = k1 - k0 + 1
                  ijtriang = (d * (d + 1)) / 2
            else
                  ijtriang = 0
            end if
      end function ijtriang

      
      function numpairs(i0, i1, j0, j1)
            !
            ! Number of (i, j) pairs satisfying the
            ! following conditions:
            ! 1. i0 <= i <= i1
            ! 2. j0 <= j <= j1
            ! 3. i >= j
            !
            integer             :: numpairs
            integer, intent(in) :: i0, i1
            integer, intent(in) :: j0, j1
            
            numpairs = ijtriang(j0,i1) - ijtriang(j1 + 1, i1) &
                   - ijtriang(j0, i0 - 1)
      end function numpairs
          
      
      subroutine mp2corr(eorb, mocoeff, nocc, nvirt, nfreeze, emp2)
            ! -------------------------------------------------------
            ! MP2 subroutine utilizing twofold redundant integral
            ! direct (PQ|RS)->(IA|JB) transformation. Results
            ! compared against NWChem 6.0 and Dalton 2.0.
            ! -------------------------------------------------------
            ! 1. Wong, A., Harrison, R., Rendell, A., Parallel
            !    direct four-index transformations, 
            !    Theoretical Chemistry Accounts: Theory, Computation,
            !    and Modelling, 93, 317(1996)
            !
            double precision, dimension(:), intent(in)    :: eorb
            double precision, dimension(:, :), intent(in) :: mocoeff
            integer, intent(in)                           :: nocc
            integer, intent(in)                           :: nvirt
            integer, intent(in)                           :: nfreeze
            double precision, intent(out)                 :: emp2
                  
            double precision, dimension(:, :, :), allocatable :: kpqij
            double precision, dimension(:, :, :), allocatable :: wrpqj
            double precision, dimension(:, :), allocatable :: work
            double precision, dimension(:, :), allocatable :: kabij
            double precision :: prefac
            double precision :: iajb, ibja
            double precision :: ea, eb, ei, ej
            double precision :: eij, sumij, sumab
            integer :: i, j, a, b
            integer :: ii, jj, aa, bb
            integer :: i0, i1
            integer :: j0
            integer :: irange, jrange
            integer :: ijidx, ijdim
            integer :: nbatch
            integer :: k, npass
            double precision :: allocsize, max_storage

            i0 = 1 + nfreeze
            j0 = 1 + nfreeze
            irange = 0
            jrange = nocc - j0 + 1
            !
            ! Calculate range of occupied I indices
            ! transformed during single intgral pass
            !
            max_storage = dble(MP2_MWORD)
            kloop: do k = i0, nocc
                  allocsize = (dble(norb**2) / 10.d+6) &
                         * dble(numpairs(k, nocc, j0, nocc))
                  if (allocsize .le. max_storage) then
                        irange = nocc - k + 1
                        exit kloop
                  end if
            end do kloop           
            !
            ! Compute number of integral passes needed
            ! to transform full set of (IA|JB) integrals
            !
            if (irange .gt. 0) then
                  npass = (nocc - nfreeze) / irange
                  if (modulo(nocc - nfreeze, irange) .gt. 0) npass = npass + 1
            end if
            
            call toprule()
            call msg("INTEGRAL DIRECT MP2 MODULE")
            call midrule()
            if (irange .eq. 0) then
                  call msg("MP2: NOT ENOUGH MEMORY DECLARED.", MSG_ERROR)
                  stop
            else
                  call msg("(PQ|RS) -> (IA|JB) INTEGRAL TRANSFORMATION")
                  call msg("TWOFOLD REDUNDANT INTEGRAL DIRECT ALGORITHM")
                  call imsg("NUMBER OF INTEGRAL PASSES: ", npass)
            end if
            
            nbatch = nfunc(maxval(atoml(1:natom)))
            ijdim = numpairs(nocc - irange + 1, nocc, j0, nocc)
                       
            allocate(kpqij(norb, norb, ijdim))
            allocate(wrpqj(norb, nbatch * nbatch, jrange))
            allocate(work(norb, nvirt))
            allocate(kabij(nvirt, nvirt))

            emp2 = zero            
            do k = 1, npass
                  !
                  ! Restritions on occupied index
                  ! (partitioning master loop)
                  !
                  i0 = 1 + nfreeze + (k - 1) * irange
                  i1 = min(i0 + irange - 1, nocc)
                  !
                  ! (PQ|RS) -> (IP|JQ) (for a given range of I, J)
                  !
                  call transf_piqj(kpqij, wrpqj, i0, i1, j0, mocoeff)

                  ijidx = 0            
                  do i = i0, i1
                        jloop: do j = j0, i
                              ijidx = ijidx + 1
                              if (j .eq. i) then
                                    prefac = one
                              else
                                    prefac = two
                              end if

                              ii = i - i0 + 1
                              jj = j - j0 + 1

                              ei = eorb(i)
                              ej = eorb(j)
                              sumij = ei + ej
                              !
                              ! (IP|JQ) -> (IA|JB) (for a given pair I, J)
                              ! ------------------------------------------                         
                              ! 1. WORK <- KPQIJ * CVIRT                                                   
                              ! 2. KABIJ <- CVIRT^T WORK                                                      
                              !                                                                      
                              call gemmwrap("N", "N", norb, nvirt, norb, ONE, kpqij(:, :, ijidx), &
                                   mocoeff(:, nocc+1:), ZERO, work)
                              call gemmwrap("T", "N", nvirt, nvirt, norb, ONE, mocoeff(:, nocc+1:), &
                                   work, ZERO, kabij)
                              !
                              ! KABIJ(A, B) = (IB|JA)
                              ! KABIJ(B, A) = (IA|JB)
                              !
                              eij = zero
                              do b = nocc + 1, nocc + nvirt
                                    do a = nocc + 1, nocc + nvirt
                                          aa = a - nocc
                                          bb = b - nocc

                                          iajb = kabij(bb, aa)
                                          ibja = kabij(aa, bb)

                                          ea = eorb(a)
                                          eb = eorb(b)
                                          sumab = ea + eb

                                          eij = eij - iajb * (two * iajb - ibja) &
                                                / (sumab - sumij)
                                    end do
                              end do
                              emp2 = emp2 + prefac * eij
                        end do jloop
                  end do
            end do
            
            deallocate(kpqij)
            deallocate(wrpqj)
            deallocate(work)
            deallocate(kabij)
      end subroutine mp2corr
      
      
      subroutine transf_piqj(kpqij, wrpqj, i0, i1, j0, mocoeff)
            double precision, dimension(:, :, :), intent(out) :: kpqij
            double precision, dimension(:, :, :), intent(out) :: wrpqj
            integer, intent(in)                               :: i0, i1
            integer, intent(in)                               :: j0
            double precision, dimension(:, :), intent(in)     :: mocoeff
      
            integer :: center_p, center_q
            integer :: shell_p, shell_q
            integer :: v, i, j, jj, r, ijidx
            integer :: p, p0, p1
            integer :: q, q0, q1
            
            kpqij = zero
            
            do center_p = 1, natom
                  do shell_p = sh0(center_p), sh0(center_p + 1) - 1
                        p0 = shpos(shell_p)
                        p1 = shpos(shell_p + 1) - 1
                        do center_q = 1, center_p
                              shellq: do shell_q = sh0(center_q), sh0(center_q + 1) - 1
                                    q0 = shpos(shell_q)
                                    q1 = shpos(shell_q + 1) - 1
                                    if (shell_q .gt. shell_p) exit shellq
                                    !
                                    ! (PQ|RS) -> (PQ|JR)
                                    ! (for a given range of J, full range of R)
                                    !
                                    call transf_pqrj(wrpqj, center_p, shell_p, &
                                          center_q, shell_q, j0, i1, mocoeff)
                                    
                                    if (shell_q .lt. shell_p) then
                                          ijidx = 0
                                          do i = i0, i1
                                                do j = j0, i           
                                                      ijidx = ijidx + 1
                                                      jj = j - j0 + 1
                                                      v = 1
                                                      do p = p0, p1
                                                            do q = q0, q1       
                                                                  do r = 1, norb
                                                                        kpqij(r, q, ijidx) = kpqij(r, q, ijidx) + &
                                                                              mocoeff(p, i) * wrpqj(r, v, jj)
                                                                        kpqij(r, p, ijidx) = kpqij(r, p, ijidx) + &
                                                                              mocoeff(q, i) * wrpqj(r, v, jj)
                                                                  end do
                                                                  v = v + 1
                                                            end do  
                                                      end do
                                                end do
                                          end do
                                    else
                                          ijidx = 0
                                          do i = i0, i1
                                                do j = j0, i      
                                                      ijidx = ijidx + 1
                                                      jj = j - j0 + 1
                                                      v = 1
                                                      do p = p0, p1
                                                            do q = q0, q1       
                                                                  do r = 1, norb
                                                                        kpqij(r, q, ijidx) = kpqij(r, q, ijidx) + &
                                                                              mocoeff(p, i) * wrpqj(r, v, jj)
                                                                  end do
                                                                  v = v + 1 
                                                            end do     
                                                      end do
                                                end do
                                          end do
                                    end if
                              end do shellq
                        end do
                  end do
            end do
      end subroutine transf_piqj
      
      
      subroutine transf_pqrj(wrpqj, center_p, shell_p, center_q, shell_q, j0, j1, mocoeff)
            double precision, dimension(:, :, :), intent(out) :: wrpqj
            integer, intent(in)                               :: center_p
            integer, intent(in)                               :: shell_p
            integer, intent(in)                               :: center_q
            integer, intent(in)                               :: shell_q
            integer, intent(in)                               :: j0, j1
            double precision, dimension(:, :), intent(in)     :: mocoeff
            
            integer :: v, j, jj
            integer :: ipq, npq
            integer :: r, r0, r1
            integer :: s, s0, s1
            integer :: pp, qq, rr, ss
            integer :: center_r, center_s
            integer :: shell_r, shell_s
            double precision, dimension(max_nfunc**4) :: gpqrs
            
            wrpqj = zero
            pp = sh(shell_p)
            qq = sh(shell_q)
            npq = nfunc(shtype(pp)) * nfunc(shtype(qq))
            
            do center_r = 1, natom
                  do shell_r = sh0(center_r), sh0(center_r + 1) - 1
                        r0 = shpos(shell_r)
                        r1 = shpos(shell_r + 1) - 1
                        rr = sh(shell_r)
                        do center_s = 1, center_r
                              shells: do shell_s = sh0(center_s), sh0(center_s + 1) - 1
                                    s0 = shpos(shell_s)
                                    s1 = shpos(shell_s + 1) - 1
                                    ss = sh(shell_s)
                                    if (shell_s .gt. shell_r) then
                                          exit shells
                                    else if (shell_s .lt. shell_r) then
                                          !
                                          ! Off-diagonal ket: shell R > shell S.
                                          ! Use permutational symmetry
                                          !
                                          call ints2e(pp, center_p, qq, &
                                                center_q, rr, center_r, ss, center_s, gpqrs)
                                          !
                                          ! AO->OCC transformation. Ket pair permutation
                                          ! is used
                                          !
                                          do j = j0, j1
                                                jj = j - j0 + 1
                                                v = 1
                                                do ipq = 1, npq
                                                      do r = r0, r1
                                                            do s = s0, s1
                                                                  !
                                                                  ! GPQRS(V) == (PQ|RS)
                                                                  !
                                                                  wrpqj(r, ipq, jj) = wrpqj(r, ipq, jj) &
                                                                        + mocoeff(s, j) * gpqrs(v)
                                                                  wrpqj(s, ipq, jj) = wrpqj(s, ipq, jj) &
                                                                        + mocoeff(r, j) * gpqrs(v)
                                                                  v = v + 1
                                                            end do
                                                      end do     
                                                end do
                                          end do
                                    else
                                          !
                                          ! Diagonal ket: shell R == shell S
                                          !
                                          call ints2e(pp, center_p, qq, &
                                                center_q, rr, center_r, ss, center_s, gpqrs)
                                          !
                                          ! AO->OCC transformation. Ket pair permutation
                                          ! is used
                                          !
                                          do j = j0, j1
                                                jj = j - j0 + 1
                                                v = 1
                                                do ipq = 1, npq
                                                      do r = r0, r1
                                                            do s = s0, s1
                                                                  wrpqj(r, ipq, jj) = wrpqj(r, ipq, jj) &
                                                                        + mocoeff(s, j) * gpqrs(v)
                                                                  v = v + 1
                                                            end do
                                                      end do     
                                                end do
                                          end do
                                    end if
                              end do shells
                        end do
                  end do
            end do            
      end subroutine transf_pqrj
end module mp2
