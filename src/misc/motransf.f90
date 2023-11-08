! ----------------------------------------------------------------
!  FOUR-INDEX TRANSFORMATION OF ELECTRON-REPULSION INTEGRALS
! ----------------------------------------------------------------
!
module motransf
      use gparam
      use math_constants
      use threads
      use ints
      use linalg
      use display
      use ccsd_transformed_integrals

      implicit none
      save

      integer, parameter :: MOTRANSF_RS_INDICES = 2**1
      integer, parameter :: MOTRANSF_QS_INDICES = 2**2

      integer, private :: MAX_NBATCH = -1
      integer, dimension(:), allocatable :: kk3
      integer, dimension(:), allocatable :: kk4

contains

      subroutine motransf_init()
            integer :: natompair, nbatch
            !
            ! Maximum batch of atom pairs as a function of the number of threads.
            ! Tune this parameter if large number of threads is used.
            !
            MAX_NBATCH = OMP_NTHREAD * 100

            natompair = (NATOM * (NATOM + 1)) / 2
            nbatch = min(natompair, MAX_NBATCH)

            allocate(kk3(nbatch))
            allocate(kk4(nbatch))
      end subroutine motransf_init


      subroutine debug_motransf(mocoeff, nocc, nvirt)
            double precision, dimension(:, :), intent(in) :: mocoeff
            integer, intent(in) :: nocc
            integer, intent(in) :: nvirt
            
            integer :: b0, b1, j0, j1, i0, i1
            double precision, dimension(:, :), allocatable :: faibj
            double precision, dimension(:, :), allocatable :: fabij
            double precision, dimension(:, :), allocatable :: wpqrs
            double precision, dimension(:, :), allocatable :: wairs
            double precision, dimension(:, :), allocatable :: wabrs
            double precision, dimension(:), allocatable    :: pwabrs
            double precision, dimension(:, :), allocatable :: fpqrs
            double precision, dimension(:, :), allocatable :: fpqk_ref
            double precision, dimension(:, :), allocatable :: fpqk_this
            double precision, dimension(:, :, :), allocatable :: xvec
            double precision, dimension(:, :, :), allocatable :: fprk_ref
            double precision, dimension(:, :, :), allocatable :: fprk_this
            integer, parameter :: nxvec = 5
            integer :: maxnrs
            integer :: nocc0, nocc1
            integer :: nvirt0, nvirt1
            double precision, dimension(MAX_NFUNC**4) :: gabcd

            integer :: a, i, b, j
            integer :: aa, bb, cc, dd
            integer :: p, q, r, s, p0, p1, q0, q1, r0, r1, s0, s1
            integer :: v, nerrors
            double precision :: gref, gcurrent
            integer, dimension(:), allocatable :: iwork
            integer :: n
            integer :: k
            integer :: ipq

            call motransf_init()

            b0 = 1
            b1 = nvirt

            i0 = 1
            i1 = nocc

            j0 = 1
            j1 = nocc
         !   maxnrs = p_ge_q2pq(NORB, NORB, NORB)
         !   maxnrs = nfunc(maxval(ATOML(1:NATOM)))
            maxnrs = fpqrs_minnrs() !maxnrs * maxnrs !(maxnrs * (maxnrs-1))/2+maxnrs
            
            allocate(faibj(nvirt*nocc, (b1-b0+1)*(j1-j0+1)))
            allocate(fabij(p_ge_q2pq(nvirt,nvirt,nvirt),p_ge_q2pq(nocc,nocc,nocc)))
            allocate(fpqrs(p_ge_q2pq(NORB,NORB,NORB), maxnrs))
            allocate(wpqrs(NORB, NORB))
            allocate(wairs(nvirt, nocc))
            allocate(wabrs(nvirt, nvirt))
            allocate(pwabrs(p_ge_q2pq(nvirt, nvirt, nvirt)))
            allocate(iwork((NSHELL*(NSHELL-1))/2+NSHELL))

            call motransf_aibj(faibj, mocoeff, nocc, nvirt, fpqrs, &
                  maxnrs, wpqrs, wairs, b0, b1, j0, j1, iwork)

            nocc0 = 1
            nocc1 = nocc
            nvirt0 = nocc + 1
            nvirt1 = nocc + nvirt

            call transformed_integrals_init(nocc, nocc+nvirt)
            call transformed_integrals(mocoeff, nocc, nvirt, nocc+nvirt, nocc0, nocc1, nvirt0, nvirt1)
            print *, "> TEST TRANSFORMACJI (PQ|RS) -> (AI|BJ) (ABS THRESH 1.D-13)"
            nerrors = 0
            n = 0
            do j = 1, nocc
                  do b = 1, nvirt
                        do i = 1, nocc
                              do a = 1, nvirt
                                    gref = vovo(nocc+a, i, nocc+b, j)
                                    gcurrent =  faibj(a_i2ai(a, i, nvirt), a_i2ai(b, j, nvirt))
                                    if (abs(gref-gcurrent) > 1.d-13) then
                                          print *, "REF",gref
                                          print *, "CUR",gcurrent
                                          print *, "--"
                                          nerrors = nerrors+1
                                    end if
                                    n = n + 1
                              end do
                        end do
                  end do
            end do
            print *, "SPRAWDZONO CALEK: ", N
            if (nerrors == 0) then
                  print *, "BRAK BLEDOW"
            ELSE
                  PRINT *, "LICZBA BLEDOW: ", nerrors
            end if


            call motransf_abij(fabij, mocoeff, nocc, nvirt, fpqrs, &
                  maxnrs, wpqrs, wabrs, pwabrs, i0, i1, j0, j1, iwork)
            print *, "> TEST TRANSFORMACJI (PQ|RS) -> (AB|IJ) (ABS THRESH 1.D-13)"
            nerrors = 0
            n = 0
            do j = 1, nocc
                  do i = j, nocc
                           do b = 1, nvirt
                              do a = b, nvirt
                                    gref = vvoo(nocc+a, nocc+b, i, j)
                                    gcurrent =  fabij(p_ge_q2pq(a, b, nvirt), p_ge_q2pq(i, j, nocc))
                                    if (abs(gref-gcurrent) > 1.d-13) then
                                          print *, "REF",gref
                                          print *, "CUR",gcurrent
                                          print *, "(AB|IJ)=", a, b, i, j
                                          print *, "--"
                                          nerrors = nerrors+1
                                    end if
                                    n = n + 1
                              end do
                        end do
                  end do
            end do
            print *, "SPRAWDZONO CALEK: ", N
            if (nerrors == 0) then
                  print *, "BRAK BLEDOW"
            ELSE
                  PRINT *, "LICZBA BLEDOW: ", nerrors
            end if


            deallocate(fpqrs)


            print *, "> TEST KONTRAKCJI MACIERZY (AI|BJ) * X(BJ,K) (THRESH 1.d-13)"
            nerrors = 0
            n = 0
            allocate(fpqk_ref(p_ge_q2pq(NORB,NORB,NORB),nxvec))
            allocate(fpqk_this(p_ge_q2pq(NORB,NORB,NORB),nxvec))
            allocate(xvec(NORB,NORB,nxvec))
            
            call random_number(xvec)
            ! do k = 1, nxvec
            !       do p = 1, NORB
            !             xvec(p, p, k) = ZERO
            !       end do
            ! end do
           ! xvec = ZERO
           ! do k = 1, nxvec
           !       do p = 1, NORB
           !             call random_number(xvec(p, p, k))
           !       end do
           ! end do

            call simple_fpqk(fpqk_ref, xvec, nxvec)
            call fpqkbatch(fpqk_this, xvec, nxvec, 1, NSHELL, 1, NSHELL)
            do k = 1, nxvec
                  do q = 1, NORB
                        do p = 1, NORB
                              ipq = p_ge_q2pq(p, q, NORB)
                              gref = fpqk_ref(ipq, k)
                              gcurrent = fpqk_this(ipq, k)
                              if (abs(gref-gcurrent) > 1.d-13) then
                                    print *, "-----"
                                    print *, "GREF=",gref
                                    print *, "GCUR=",gcurrent
                                    nerrors = nerrors + 1
                              end if
                              n = n + 1
                        end do
                  end do
            end do
            print *, "SPRAWDZONO CALEK:", N
            if (nerrors == 0) then
                  print *, "BRAK BLEDOW"
            else
                  PRINT *, "LICZBA BLEDOW: ", nerrors
            end if

            print *, "> TEST KONTRAKCJI MACIERZY (PQ|RS) * X(Q,S) (THRESH 1.d-13)"
            call random_number(xvec)
            nerrors = 0
            n = 0
            allocate(fprk_ref(NORB, NORB, nxvec))
            allocate(fprk_this(NORB, NORB, nxvec))
            call simple_fprk(fprk_ref, xvec, nxvec)
            call motransfdrv(fprk_this, fpqk_this, xvec, nxvec, MOTRANSF_QS_INDICES)
            do k = 1, nxvec
                  do q = 1, NORB
                        do p = 1, NORB
                              gref = fprk_ref(p, q, k)
                              gcurrent = fprk_this(p, q, k)
                              if (abs(gref-gcurrent) > 1.d-13) then
                                    print *, "-----"
                                    print *, "(",p,",",q,"|",k,")"
                                    print *, "GREF=",gref
                                    print *, "GCUR=",gcurrent
                                    nerrors = nerrors + 1
                              end if
                              n = n + 1
                        end do
                  end do
            end do
            print *, "SPRAWDZONO CALEK:", N
            if (nerrors == 0) then
                  print *, "BRAK BLEDOW"
            else
                  PRINT *, "LICZBA BLEDOW: ", nerrors
            end if



            ! print *, "> TEST MACIERZY CALEK W BAZIE AO: FPQRS (REL THRESH 1.d-12)"
            ! allocate(fpqrs(p_ge_q2pq(NORB,NORB,NORB), p_ge_q2pq(NORB,NORB,NORB)))
            ! nerrors = 0
            ! fpqrs = ZERO
            ! call fpqrsbatch(fpqrs, 1, NSHELL, 1, NSHELL, iwork)
            ! do aa = 1, NSHELL
            !       do bb = 1, aa
            !             do cc = 1, NSHELL
            !                   do dd = 1, cc
            !                         call ints2e(SH(aa),SHATOM(aa),SH(bb),SHATOM(bb),SH(cc),SHATOM(cc),SH(dd),SHATOM(dd),gabcd)
            !                         p0 = SHPOS(aa)
            !                         p1 = SHPOS(aa+1)-1
            !                         q0 = SHPOS(bb)
            !                         q1 = SHPOS(bb+1)-1
            !                         r0 = SHPOS(cc)
            !                         r1 = SHPOS(cc+1)-1
            !                         s0 = SHPOS(dd)
            !                         s1 = SHPOS(dd+1)-1

            !                         v = 1
            !                         do p = p0, p1
            !                               do q = q0, q1
            !                                     do r = r0, r1
            !                                           do s = s0, s1
            !                                                 if (p >= q .and. r >= s) then
            !                                                       gref = gabcd(v)
            !                                                       gcurrent = &
            !                                                             fpqrs(p_ge_q2pq(p, q, NORB), &
            !                                                             p_ge_q2pq(r, s, NORB))
            !                                                       if (abs(gref-gcurrent) > 1.d-12 * abs(gref)) then
            !                                                             ! print *, "--------------"
            !                                                             ! print *, "(AA,BB,CC,DD)", AA, BB, CC, DD
            !                                                             ! print *, "REF", gref
            !                                                             ! print *, "F  ", gcurrent
            !                                                             nerrors = nerrors + 1
            !                                                       end if
            !                                                 end if
            !                                                 v = v + 1
            !                                           end do
            !                                     end do
            !                               end do
            !                         end do
            !                   end do
            !             end do
            !       end do
            ! end do
            ! if (nerrors == 0) then
            !       print *, "BRAK BLEDOW"
            ! ELSE
            !       PRINT *, "LICZBA BLEDOW: ", nerrors
            ! end if

            deallocate(faibj)
!            deallocate(fpqrs)
            deallocate(wpqrs)
            deallocate(wairs)
      end subroutine debug_motransf


      subroutine simple_fpqk(fpqk, xvec, nxvec)
            double precision, dimension(:, :), intent(out) :: fpqk
            double precision, dimension(:, :, :), intent(in)  :: xvec
            integer, intent(in)                               :: nxvec

            double precision, dimension(MAX_NFUNC**4) :: gabcd
            double precision, dimension(:, :, :, :), allocatable :: pqrs
            integer :: aa, bb, cc, dd, a, b, c, d, k1, k2, k3, k4
            integer :: p, q, r, s, p0, p1, q0, q1, r0, r1, s0, s1
            integer :: v, k
            integer :: ipq


            allocate(pqrs(NORB, NORB, NORB, NORB))
            
            do dd = 1, NSHELL
                  d = SH(dd)
                  k4 = SHATOM(dd)
                  do cc = 1, NSHELL
                        c = SH(cc)
                        k3 = SHATOM(cc)
                        do bb = 1, NSHELL
                              k2 = SHATOM(bb)
                              b = SH(bb)
                              do aa = 1, NSHELL
                                    k1 = SHATOM(aa)
                                    a = SH(aa)

                                    s0 = SHPOS(dd)
                                    s1 = SHPOS(dd+1) - 1

                                    r0 = SHPOS(cc)
                                    r1 = SHPOS(cc+1) - 1

                                    q0 = SHPOS(bb)
                                    q1 = SHPOS(bb+1) - 1

                                    p0 = SHPOS(aa)
                                    p1 = SHPOS(aa+1) - 1

                                    call ints2e(d, k4, c, k3, b, k2, a, k1, gabcd)
                                    
                                    v = 1
                                    do s = s0, s1
                                          do r = r0, r1
                                                do q = q0, q1
                                                      do p = p0, p1
                                                            pqrs(p, q, r, s) = gabcd(v)
                                                            v = v + 1
                                                      end do
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            
            fpqk = ZERO

            do k = 1, nxvec
                  do q = 1, NORB
                        do p = q, NORB
                              ipq = p_ge_q2pq(p, q, NORB)
                              do s = 1, NORB
                                    do r = 1, NORB
                                          fpqk(ipq, k) = fpqk(ipq, k) + pqrs(p, q, r, s) * xvec(r, s, k)
                                    end do
                              end do
                        end do
                  end do
            end do

            deallocate(pqrs)
      end subroutine simple_fpqk



      subroutine simple_fprk(fprk, xvec, nxvec)
            double precision, dimension(:, :, :), intent(out) :: fprk
            double precision, dimension(:, :, :), intent(in)  :: xvec
            integer, intent(in)                               :: nxvec

            double precision, dimension(MAX_NFUNC**4) :: gabcd
            double precision, dimension(:, :, :, :), allocatable :: pqrs
            integer :: aa, bb, cc, dd, a, b, c, d, k1, k2, k3, k4
            integer :: p, q, r, s, p0, p1, q0, q1, r0, r1, s0, s1
            integer :: v, k
            integer :: ipq


            allocate(pqrs(NORB, NORB, NORB, NORB))
            
            do dd = 1, NSHELL
                  d = SH(dd)
                  k4 = SHATOM(dd)
                  do cc = 1, NSHELL
                        c = SH(cc)
                        k3 = SHATOM(cc)
                        do bb = 1, NSHELL
                              k2 = SHATOM(bb)
                              b = SH(bb)
                              do aa = 1, NSHELL
                                    k1 = SHATOM(aa)
                                    a = SH(aa)

                                    s0 = SHPOS(dd)
                                    s1 = SHPOS(dd+1) - 1

                                    r0 = SHPOS(cc)
                                    r1 = SHPOS(cc+1) - 1

                                    q0 = SHPOS(bb)
                                    q1 = SHPOS(bb+1) - 1

                                    p0 = SHPOS(aa)
                                    p1 = SHPOS(aa+1) - 1

                                    call ints2e(d, k4, c, k3, b, k2, a, k1, gabcd)
                                    
                                    v = 1
                                    do s = s0, s1
                                          do r = r0, r1
                                                do q = q0, q1
                                                      do p = p0, p1
                                                            pqrs(p, q, r, s) = gabcd(v)
                                                            v = v + 1
                                                      end do
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do

            fprk = ZERO

            do k = 1, nxvec
                  do q = 1, NORB
                        do p = 1, NORB
                              do s = 1, NORB
                                    do r = 1, NORB
                                          fprk(p, r, k) = fprk(p, r, k) + pqrs(p, q, r, s) * xvec(q, s, k)
                                    end do
                              end do
                        end do
                  end do
            end do

            deallocate(pqrs)
      end subroutine simple_fprk



      subroutine motransf_aibj(faibj, mocoeff, nocc, nvirt, fpqrs, &
            maxnrs, wpqrs, wairs, b0, b1, j0, j1, iwork)
            ! ------------------------------------------------------------------------
            ! Generate two-electron integrals in AO basis and perform four-index
            ! contraction to the MO basis:
            ! (PQ|RS) -> (AI|BJ),
            ! where A, B are virtual indices and I, J are occupied orbital indices.
            ! Bra indices are unconstrained, whereas ket indices of the transformed
            ! integrals belong to the requested subset of all orbital pairs:
            ! B \in B0, B0+1, ..., B1;
            ! J \in J0, J0+1, ..., J1.
            ! -------------------------------------------------------------------------
            ! FAIBJ   - Output, matrix of MO-transformed integrals.
            !           (AI|BJ)=FAIBJ(A_I2AI(A,I),A_I2AI(B,J))
            ! MOCOEFF - Input, matrix of MO coefficients in AO basis,
            !           [ ... NOCC occupied vecs ... | ... NVIRT virtual vecs ... ]
            ! NOCC    - Input, number of occupied vectors
            ! NVIRT   - Input, number of virtual vectors
            ! FPQRS   - Scratch matrix, temporary storage for a batch of AO integrals.
            !           This matrix is required to be of dimension
            !           FPQRS(P_GE_Q2PQ(NORB,NORB,NORB) x MAXNRS,
            !           where MAXNRS is user-defined number satisfying
            !           MAXNRS >= FPQRS_MINNRS().
            ! MAXNRS  - Input, maximum number of kets (RS compound indices) 
            ! WPQRS   - Scratch matrix, matrix of dimension NORB x NORB
            ! WAIRS   - Scratch matrix, matrix of dimension NVIRT x NOCC
            ! B0, B1  - Input, the range of the virtual index
            ! J0, J1  - Input, the range of the occupied index
            ! IWORK   - Scratch matrix, matrix of dimension
            !           P_GE_Q2PQ(NSHELL,NSHELL,NSHELL)
            !
            double precision, dimension(:, :), intent(out)    :: faibj
            double precision, dimension(:, :), intent(in)     :: mocoeff
            integer, intent(in)                               :: nocc
            integer, intent(in)                               :: nvirt
            double precision, dimension(:, :), intent(out)    :: fpqrs
            integer, intent(in)                               :: maxnrs
            double precision, dimension(:, :), intent(out)    :: wpqrs
            double precision, dimension(:, :), intent(out)    :: wairs
            integer, intent(in)                               :: b0
            integer, intent(in)                               :: b1
            integer, intent(in)                               :: j0
            integer, intent(in)                               :: j1
            integer, dimension(:), intent(out)                :: iwork
            
            integer :: cc, dd, c, d
            integer :: cc0, dd0, cc1, dd1
            integer :: nfuncc, nfuncd
            integer :: n, ncd
            integer :: iccdd0, iccdd1, iccdd
            integer :: irs0
            integer :: minnrs
            !
            ! Calclate minimum required MAXNRS value
            !
            minnrs = fpqrs_minnrs()
            if (maxnrs < minnrs) then
                  call msg("MOTRANSF ERROR: FPQRS ARRAY DECLARED NOT LARGE ENOUGH.", &
                        priority=MSG_ERROR)
                  stop
            end if
            
            faibj = ZERO
            !
            ! Number of AO ket pairs stored in a single batch
            ! of integrals
            !
            n = 0
            iccdd0 = 1
            iccdd1 = 1
            cc0 = 1
            cc1 = 1
            dd0 = 1
            dd1 = 1
            irs0 = 1
            iwork(1) = 1
            
            do dd = 1, NSHELL
                  d = SH(dd)
                  nfuncd = nfunc(SHTYPE(d))
                  do cc = dd, NSHELL
                        c = SH(cc)
                        nfuncc = nfunc(SHTYPE(c))
                        !
                        ! Calculate how many AO ket orbital indices
                        ! will be contributed by the current ket shell
                        ! pair. Note that only those (PQ|RS) integrals 
                        ! will be stored, for which R>=S, where R and S
                        ! are AO orbital indices. This inequality holds,
                        ! in particular, when ket shell pair is diagonal.
                        !
                        if (cc .ne. dd) then
                              ncd = nfuncc * nfuncd
                        else
                              ncd = (nfuncc * (nfuncc - 1)) / 2 + nfuncc
                        end if
                        iccdd = p_ge_q2pq(cc, dd, NSHELL)

                        if (n + ncd <= maxnrs) then
                              iccdd1 = iccdd
                              cc1 = cc
                              dd1 = dd
                              iwork(iccdd-iccdd0+1) = n + 1
                              n = n + ncd
                        else
                              !
                              ! Generate a batch of two-electron integrals
                              ! in AO basis. A batch is defined as a set of all
                              ! two-electron integrals, (PQ|RS), for which P>=Q,
                              ! R>=S, and where the compound ket index is in a
                              ! specified range. The range of ket indices depends
                              ! on the allocated memory dedicated for FPQRS matrix.
                              ! The bra-ket permutational symmetry, (PQ|RS)=(RS|PQ),
                              ! is not utilized for the memory *storage* of two-electron
                              ! integrals in FPQRS matrix. Nevertheless, this symmetry is
                              ! fully recognized by the subroutine that *computes*
                              ! the two-electron integrals.
                              !
                              call fpqrsbatch(fpqrs, cc0, cc1, dd0, dd1, iwork)
                              call partial_aibj(faibj, fpqrs, mocoeff, cc0, cc1, &
                                    dd0, dd1, b0, b1, j0, j1, wpqrs, wairs, nocc, nvirt)
                              !
                              ! Set up variables for the next batch of AO integrals
                              !
                              iccdd0 = iccdd
                              iccdd1 = iccdd
                              cc0 = cc
                              dd0 = dd
                              irs0 = p_ge_q2pq(SHPOS(cc), SHPOS(dd), NORB)
                              cc1 = cc
                              dd1 = dd
                              iwork(1) = 1
                              n = ncd
                        end if
                  end do
            end do

            if (n > 0) then
                  call fpqrsbatch(fpqrs, cc0, cc1, dd0, dd1, iwork)
                  call partial_aibj(faibj, fpqrs, mocoeff, cc0, cc1, &
                        dd0, dd1, b0, b1, j0, j1, wpqrs, wairs, nocc, nvirt)
            end if
      end subroutine motransf_aibj


      subroutine motransf_abij(fabij, mocoeff, nocc, nvirt, fpqrs, &
            maxnrs, wpqrs, wabrs, pwabrs, i0, i1, j0, j1, iwork)
            ! ------------------------------------------------------------------------
            ! Generate two-electron integrals in AO basis and perform four-index
            ! contraction to the MO basis:
            ! (PQ|RS) -> (AB|IJ),
            ! where A, B are virtual indices and I, J are occupied orbital indices.
            ! Bra indices are unconstrained, whereas ket indices of the transformed
            ! integrals belong to the requested subset of all orbital pairs:
            ! I \in I0, I0+1, ..., I1;
            ! J \in J0, J0+1, ..., J1.
            ! -------------------------------------------------------------------------
            ! FABIJ   - Output, matrix of MO-transformed integrals.
            !           (AB|IJ)=FABIJ(P_GE_Q2PQ(A,B,NVIRT),P_GE_Q2PQ(I,J,NOCC))
            ! MOCOEFF - Input, matrix of MO coefficients in AO basis,
            !           [ ... NOCC occupied vecs ... | ... NVIRT virtual vecs ... ]
            ! NOCC    - Input, number of occupied vectors
            ! NVIRT   - Input, number of virtual vectors
            ! FPQRS   - Scratch matrix, temporary storage for a batch of AO integrals.
            !           This matrix is required to be of dimension
            !           FPQRS(P_GE_Q2PQ(NORB,NORB,NORB) x MAXNRS,
            !           where MAXNRS is user-defined number satisfying
            !           MAXNRS >= FPQRS_MINNRS().
            ! MAXNRS  - Input, maximum number of kets (RS compound indices) 
            ! WPQRS   - Scratch matrix, matrix of dimension NORB x NORB
            ! WABRS   - Scratch matrix, matrix of dimension NVIRT x NVIRT
            ! PWABRS  - Scratch matrix, matrix of dimension 
            !           P_GE_Q2PQ(NVIRT,NVIRT,NVIRT)
            ! I0, I1  - Input, the range of the first occupied index
            ! J0, J1  - Input, the range of the second occupied index
            ! IWORK   - Scratch matrix, matrix of dimension
            !           P_GE_Q2PQ(NSHELL,NSHELL,NSHELL)
            !
            double precision, dimension(:, :), intent(out)    :: fabij
            double precision, dimension(:, :), intent(in)     :: mocoeff
            integer, intent(in)                               :: nocc
            integer, intent(in)                               :: nvirt
            double precision, dimension(:, :), intent(out)    :: fpqrs
            integer, intent(in)                               :: maxnrs
            double precision, dimension(:, :), intent(out)    :: wpqrs
            double precision, dimension(:, :), intent(out)    :: wabrs
            double precision, dimension(:), intent(out)       :: pwabrs
            integer, intent(in)                               :: i0
            integer, intent(in)                               :: i1
            integer, intent(in)                               :: j0
            integer, intent(in)                               :: j1
            integer, dimension(:), intent(out)                :: iwork
            
            integer :: cc, dd, c, d
            integer :: cc0, dd0, cc1, dd1
            integer :: nfuncc, nfuncd
            integer :: n, ncd
            integer :: iccdd0, iccdd1, iccdd
            integer :: irs0
            integer :: minnrs
            !
            ! Calclate minimum required MAXNRS value
            !
            minnrs = fpqrs_minnrs()
            if (maxnrs < minnrs) then
                  call msg("MOTRANSF ERROR: FPQRS ARRAY DECLARED NOT LARGE ENOUGH.", &
                        priority=MSG_ERROR)
                  stop
            end if
            
            fabij = ZERO
            !
            ! Number of AO ket pairs stored in a single batch
            ! of integrals
            !
            n = 0
            iccdd0 = 1
            iccdd1 = 1
            cc0 = 1
            cc1 = 1
            dd0 = 1
            dd1 = 1
            irs0 = 1
            iwork(1) = 1
            
            do dd = 1, NSHELL
                  d = SH(dd)
                  nfuncd = nfunc(SHTYPE(d))
                  do cc = dd, NSHELL
                        c = SH(cc)
                        nfuncc = nfunc(SHTYPE(c))
                        !
                        ! Calculate how many AO ket orbital indices
                        ! will be contributed by the current ket shell
                        ! pair. Note that only those (PQ|RS) integrals 
                        ! will be stored, for which R>=S, where R and S
                        ! are AO orbital indices. This inequality holds,
                        ! in particular, when ket shell pair is diagonal.
                        !
                        if (cc .ne. dd) then
                              ncd = nfuncc * nfuncd
                        else
                              ncd = (nfuncc * (nfuncc - 1)) / 2 + nfuncc
                        end if
                        iccdd = p_ge_q2pq(cc, dd, NSHELL)

                        if (n + ncd <= maxnrs) then
                              iccdd1 = iccdd
                              cc1 = cc
                              dd1 = dd
                              iwork(iccdd-iccdd0+1) = n + 1
                              n = n + ncd
                        else
                              !
                              ! Generate a batch of two-electron integrals
                              ! in AO basis. A batch is defined as a set of all
                              ! two-electron integrals, (PQ|RS), for which P>=Q,
                              ! R>=S, and where the compound ket index is in a
                              ! specified range. The range of ket indices depends
                              ! on the allocated memory dedicated for FPQRS matrix.
                              ! The bra-ket permutational symmetry, (PQ|RS)=(RS|PQ),
                              ! is not utilized for the memory *storage* of two-electron
                              ! integrals in FPQRS matrix. Nevertheless, this symmetry is
                              ! fully recognized by the subroutine that *computes*
                              ! the two-electron integrals.
                              !
                              call fpqrsbatch(fpqrs, cc0, cc1, dd0, dd1, iwork)
                              call partial_abij(fabij, fpqrs, mocoeff, cc0, cc1, &
                                    dd0, dd1, i0, i1, j0, j1, wpqrs, wabrs, pwabrs, &
                                    nocc, nvirt)
                              !
                              ! Set up variables for the next batch of AO integrals
                              !
                              iccdd0 = iccdd
                              iccdd1 = iccdd
                              cc0 = cc
                              dd0 = dd
                              irs0 = p_ge_q2pq(SHPOS(cc), SHPOS(dd), NORB)
                              cc1 = cc
                              dd1 = dd
                              iwork(1) = 1
                              n = ncd
                        end if
                  end do
            end do

            if (n > 0) then
                  call fpqrsbatch(fpqrs, cc0, cc1, dd0, dd1, iwork)
                  call partial_abij(fabij, fpqrs, mocoeff, cc0, cc1, &
                        dd0, dd1, i0, i1, j0, j1, wpqrs, wabrs, pwabrs, nocc, nvirt)
            end if
      end subroutine motransf_abij


      subroutine partial_aibj(faibj, fpqrs, mocoeff, cc0, cc1, dd0, dd1, b0, b1, j0, j1, &
            wpqrs, wairs, nocc, nvirt)
            !
            ! Partial incremental AO->MO transform
            !
            double precision, dimension(:, :), intent(inout)    :: faibj
            double precision, dimension(:, :), intent(in)       :: fpqrs
            double precision, dimension(:, :), intent(in)       :: mocoeff
            integer, intent(in)                                 :: cc0
            integer, intent(in)                                 :: cc1
            integer, intent(in)                                 :: dd0
            integer, intent(in)                                 :: dd1
            integer, intent(in)                                 :: b0
            integer, intent(in)                                 :: b1
            integer, intent(in)                                 :: j0
            integer, intent(in)                                 :: j1
            double precision, dimension(:, :), intent(out)      :: wpqrs
            double precision, dimension(:, :), intent(out)      :: wairs
            integer, intent(in)                                 :: nocc
            integer, intent(in)                                 :: nvirt
            
            integer :: r, s
            integer :: rstart
            integer :: ccstart, ccstop
            integer :: cc, dd
            integer :: irs

            irs = 1
            do dd = dd0, dd1
                  if (dd == dd0) then
                        ccstart = cc0
                  else
                        ccstart = dd
                  end if
                  if (dd == dd1) then
                        ccstop = cc1
                  else
                        ccstop = NSHELL
                  end if

                  do cc = ccstart, ccstop
                        do s = SHPOS(dd), SHPOS(dd+1)-1
                              rstart = max(s, SHPOS(cc))
                              do r = rstart, SHPOS(cc+1)-1
                                    call smunpack(fpqrs(:, irs), wpqrs)
                                    irs = irs + 1
                                    !
                                    ! Transform bra indices: FPQRS -> WAIRS (RS fixed)
                                    !
                                    call atsybc(wairs, mocoeff(:, nocc+1:), wpqrs, mocoeff, nvirt, nocc)
                                    !
                                    ! Transform ket indices: WAIRS -> FAIBJ
                                    !
                                    call airs2aibj(faibj, wairs, nocc, nvirt, &
                                          mocoeff, r, s, b0, b1, j0, j1)
                              end do
                        end do
                  end do
            end do
      end subroutine partial_aibj


      subroutine partial_abij(fabij, fpqrs, mocoeff, cc0, cc1, dd0, dd1, i0, i1, j0, j1, &
            wpqrs, wabrs, pwabrs, nocc, nvirt)
            !
            ! Partial incremental AO->MO transform
            !
            double precision, dimension(:, :), intent(inout)    :: fabij
            double precision, dimension(:, :), intent(in)       :: fpqrs
            double precision, dimension(:, :), intent(in)       :: mocoeff
            integer, intent(in)                                 :: cc0
            integer, intent(in)                                 :: cc1
            integer, intent(in)                                 :: dd0
            integer, intent(in)                                 :: dd1
            integer, intent(in)                                 :: i0
            integer, intent(in)                                 :: i1
            integer, intent(in)                                 :: j0
            integer, intent(in)                                 :: j1
            double precision, dimension(:, :), intent(out)      :: wpqrs
            double precision, dimension(:, :), intent(out)      :: wabrs
            double precision, dimension(:), intent(out)         :: pwabrs
            integer, intent(in)                                 :: nocc
            integer, intent(in)                                 :: nvirt
            
            integer :: r, s
            integer :: rstart
            integer :: ccstart, ccstop
            integer :: cc, dd
            integer :: irs

            irs = 1
            do dd = dd0, dd1
                  if (dd == dd0) then
                        ccstart = cc0
                  else
                        ccstart = dd
                  end if
                  if (dd == dd1) then
                        ccstop = cc1
                  else
                        ccstop = NSHELL
                  end if

                  do cc = ccstart, ccstop
                        do s = SHPOS(dd), SHPOS(dd+1)-1
                              rstart = max(s, SHPOS(cc))
                              do r = rstart, SHPOS(cc+1)-1
                                    call smunpack(fpqrs(:, irs), wpqrs)
                                    irs = irs + 1
                                    !
                                    ! Transform bra indices: FPQRS -> WABRS (RS fixed)
                                    ! (only lower trianglar part of WABRS is being
                                    ! referenced)
                                    !
                                    call atsyba(wabrs, mocoeff(:, nocc+1:), wpqrs, nvirt)
                                    !
                                    ! Translate symmetric matrix into packed linear format
                                    !
                                    call smpack(wabrs, pwabrs, n=nvirt)
                                    !
                                    ! Transform ket indices: WABRS -> FABIJ
                                    !
                                    call abrs2abij(fabij, pwabrs, nocc, nvirt, &
                                          mocoeff, r, s, i0, i1, j0, j1)
                              end do
                        end do
                  end do
            end do
      end subroutine partial_abij


      subroutine airs2aibj(faibj, wairs, nocc, nvirt, mocoeff, r, s, b0, b1, j0, j1)
            !
            ! (AI|RS) -> (AI|BJ)
            !
            double precision, dimension(:, :), intent(inout)    :: faibj
            double precision, dimension(*), intent(in)          :: wairs
            integer, intent(in)                                 :: nocc
            integer, intent(in)                                 :: nvirt
            double precision, dimension(:, :), intent(in)       :: mocoeff
            integer, intent(in)                                 :: r
            integer, intent(in)                                 :: s
            integer, intent(in)                                 :: b0
            integer, intent(in)                                 :: b1
            integer, intent(in)                                 :: j0
            integer, intent(in)                                 :: j1

            integer :: b, j
            double precision :: crb, csb
            double precision :: crj, csj
            double precision :: cbj1, cbj2, cbj
            integer :: bstart, bstop
            double precision :: scal
            integer :: ibj0, ibj
            integer :: n

            external :: daxpy
            !
            ! Number of bra indices
            !
            n = nocc * nvirt
            scal = ONE
            if (r == s) scal = FRAC12
            ibj0 = a_i2ai(b0, j0, nvirt)
            do j = j0, j1
                  crj = mocoeff(r, j) * scal
                  csj = mocoeff(s, j) * scal
                  if (j == j0) then
                        bstart = b0
                  else
                        bstart = 1
                  end if
                  if (j == j1) then
                        bstop = b1
                  else
                        bstop = nvirt
                  end if
                  do b = bstart, bstop
                        crb = mocoeff(r, nocc+b)
                        csb = mocoeff(s, nocc+b)
                        cbj1 = crb * csj
                        cbj2 = csb * crj
                        cbj = cbj1 + cbj2
                        ibj = a_i2ai(b, j, nvirt) - ibj0 + 1
                        !
                        ! FAIBJ(:, IBJ) <- CBJ * WAIRS + FAIBJ(:, IBJ)
                        !
                        call daxpy(n, cbj, wairs, 1, faibj(:, ibj), 1)
                  end do
            end do
      end subroutine airs2aibj

      
      subroutine abrs2abij(fabij, wabrs, nocc, nvirt, mocoeff, r, s, i0, i1, j0, j1)
            !
            ! (AB|RS) -> (AB|IJ)
            !
            double precision, dimension(:, :), intent(inout)    :: fabij
            double precision, dimension(:), intent(in)          :: wabrs
            integer, intent(in)                                 :: nocc
            integer, intent(in)                                 :: nvirt
            double precision, dimension(:, :), intent(in)       :: mocoeff
            integer, intent(in)                                 :: r
            integer, intent(in)                                 :: s
            integer, intent(in)                                 :: i0
            integer, intent(in)                                 :: i1
            integer, intent(in)                                 :: j0
            integer, intent(in)                                 :: j1

            integer :: i, j
            double precision :: cri, csi
            double precision :: crj, csj
            double precision :: cij1, cij2, cij
            integer :: istart, istop
            double precision :: scal
            integer :: ij0, ij
            integer :: n

            external :: daxpy
            !
            ! Number of bra indices
            ! (permutational symmetry of bra indices
            ! is utilized)
            !
            n = p_ge_q2pq(nvirt, nvirt, nvirt)
            scal = ONE
            if (r == s) scal = FRAC12
            ij0 = p_ge_q2pq(i0, j0, nocc)
            do j = j0, j1
                  crj = mocoeff(r, j) * scal
                  csj = mocoeff(s, j) * scal
                  if (j == j0) then
                        istart = i0
                  else
                        !
                        ! Permutational symmetry of ket indices
                        ! is utilized
                        !
                        istart = j
                  end if
                  if (j == j1) then
                        istop = i1
                  else
                        istop = nocc
                  end if
                  do i = istart, istop
                        cri = mocoeff(r, i)
                        csi = mocoeff(s, i)
                        cij1 = cri * csj
                        cij2 = csi * crj
                        cij = cij1 + cij2
                        ij = p_ge_q2pq(i, j, nocc) - ij0 + 1
                        !
                        ! FABIJ(:, IJ) <- CIJ * WABRS + FABIJ(:, IJ)
                        !
                        call daxpy(n, cij, wabrs, 1, fabij(:, ij), 1)
                  end do
            end do
      end subroutine abrs2abij


      subroutine fpqrsbatch(fpqrs, cc0, cc1, dd0, dd1, idx)
            ! -----------------------------------------------------------
            ! Compute a matrix of two-electron repulsion integrals,
            ! (PQ|RS), 
            ! satisfying the following conditions:
            ! P >= Q,
            ! R >= S,
            ! ICCDD0 <= P_GE_Q2PQ(CC,DD,NSHELL) <= ICCDD1,
            ! R \in CC,
            ! S \in DD,
            ! where P, Q, R, S denote indices of AO orbitals, and
            ! CC and DD are shells to which R and S belong.
            ! ------------------------------------------------------------
            double precision, dimension(:, :), intent(inout) :: fpqrs
            integer, intent(in)                              :: cc0
            integer, intent(in)                              :: cc1
            integer, intent(in)                              :: dd0
            integer, intent(in)                              :: dd1
            integer, dimension(:), intent(in)                :: idx

            integer :: aa, bb, cc, dd
            integer :: a, b, c, d
            integer :: k1, k2, k3, k4
            integer :: iaabb, iccdd
            integer :: abmember, cdmember
            double precision, dimension(MAX_NFUNC**4) :: gabcd
            integer :: iccdd0, iccdd1, irs0
            integer :: abidx0, cdidx0
            integer :: hab, hcd
            integer :: aastart
            integer :: shposcc11

            shposcc11 = SHPOS(cc1+1)
            iccdd0 = p_ge_q2pq(cc0, dd0, NSHELL)
            iccdd1 = p_ge_q2pq(cc1, dd1, NSHELL)
            irs0 = p_ge_q2pq(SHPOS(cc0), SHPOS(dd0), NORB)
            !
            ! Loop over permutationally unique quartet of shells:
            ! AA >= BB
            ! CC >= DD
            ! AA >= CC
            ! if AA == CC then BB >= DD
            !
            do dd = 1, NSHELL
                  d = SH(dd)
                  k4 = SHATOM(dd)
                  if (dd == dd1) then
                        hcd = NORB - shposcc11 + 1
                  else
                        hcd = 0
                  end if
                  do cc = dd, NSHELL
                        c = SH(cc)
                        k3 = SHATOM(cc)
                        !
                        ! Test if ket pair belongs to the requested set of shell pairs
                        !
                        iccdd = p_ge_q2pq(cc, dd, NSHELL)
                        cdmember = imembership(iccdd, iccdd0, iccdd1)
                        do bb = 1, NSHELL
                              k2 = SHATOM(bb)
                              b = SH(bb)
                              if (bb == dd1) then
                                    hab = NORB - shposcc11 + 1
                              else
                                    hab = 0
                              end if
                              if (bb < dd) then
                                    aastart = cc + 1
                              else 
                                    aastart = max(cc, bb)
                              end if
                              do aa = aastart, NSHELL
                                    k1 = SHATOM(aa)
                                    a = SH(aa)
                                    !
                                    ! Test if bra pair belongs to the requested set of shell pairs
                                    !
                                    iaabb = p_ge_q2pq(aa, bb, NSHELL)
                                    abmember = imembership(iaabb, iccdd0, iccdd1)
                                    if (cdmember >= 0 .and. abmember < 0) then
                                          cdidx0 = idx(iccdd-iccdd0+1)
                                          call ints2e(d, k4, c, k3, b, k2, a, k1, gabcd)
                                          call digestquartet_abcd(fpqrs, gabcd, aa, bb, cc, dd, cdidx0)
                                    else if (cdmember < 0 .and. abmember >= 0) then
                                          abidx0 = idx(iaabb-iccdd0+1)
                                          call ints2e(b, k2, a, k1, d, k4, c, k3, gabcd)
                                          call digestquartet_abcd(fpqrs, gabcd, cc, dd, aa, bb, abidx0)
                                    else if (cdmember >= 0 .and. abmember >= 0) then
                                          cdidx0 = idx(iccdd-iccdd0+1)
                                          abidx0 = idx(iaabb-iccdd0+1)
                                          call ints2e(d, k4, c, k3, b, k2, a, k1, gabcd)
                                          call digestquartet_abcd_cdab(fpqrs, gabcd, aa, bb, cc, dd, &
                                                abidx0, cdidx0)
                                    end if
                              end do
                        end do
                  end do
            end do
      end subroutine fpqrsbatch


      subroutine fpqkbatch(fpqk, xvec, nxvec, cc0, cc1, dd0, dd1)
            ! -----------------------------------------------------------
            ! Compute a matrix of two-electron repulsion integrals,
            ! (PQ|RS) contracted with X_k vectors:
            ! FPQK(PQ,K) <- \sum_{RS} (PQ|RS) X_{RS,K}^{AO}
            ! P >= Q,
            ! where the sum over ket AO indices, RS, is constained to the 
            ! orbitals in the current batch of orbital shells:
            ! R \in CC   or  R \in DD
            ! S \in DD       S \in CC,
            ! (CC0, DD0) <= (CC, DD) <= (CC1, DD1).
            ! ------------------------------------------------------------
            ! FPQK
            ! XVEC
            ! NXVEC
            ! ------------------------------------------------------------
            double precision, dimension(:, :), intent(out)   :: fpqk
            double precision, dimension(:, :, :), intent(in) :: xvec
            integer, intent(in)                              :: nxvec
            integer, intent(in)                              :: cc0
            integer, intent(in)                              :: cc1
            integer, intent(in)                              :: dd0
            integer, intent(in)                              :: dd1

            integer :: aa, bb, cc, dd
            integer :: a, b, c, d
            integer :: k1, k2, k3, k4
            integer :: iaabb, iccdd
            integer :: abmember, cdmember
            double precision, dimension(MAX_NFUNC**4) :: gpqrs
            integer :: iccdd0, iccdd1
            integer :: aastart

            fpqk = ZERO

            iccdd0 = p_ge_q2pq(cc0, dd0, NSHELL)
            iccdd1 = p_ge_q2pq(cc1, dd1, NSHELL)
            !
            ! Loop over permutationally unique quartet of shells:
            ! AA >= BB
            ! CC >= DD
            ! AA >= CC
            ! if AA == CC then BB >= DD
            !
            do dd = 1, NSHELL
                  d = SH(dd)
                  k4 = SHATOM(dd)
                  do cc = dd, NSHELL
                        c = SH(cc)
                        k3 = SHATOM(cc)
                        !
                        ! Test if ket pair belongs to the requested set of shell pairs
                        !
                        iccdd = p_ge_q2pq(cc, dd, NSHELL)
                        cdmember = imembership(iccdd, iccdd0, iccdd1)
                        do bb = 1, NSHELL
                              k2 = SHATOM(bb)
                              b = SH(bb)
                              if (bb < dd) then
                                    aastart = cc + 1
                              else 
                                    aastart = max(cc, bb)
                              end if
                              do aa = aastart, NSHELL
                                    k1 = SHATOM(aa)
                                    a = SH(aa)
                                    !
                                    ! Test if bra pair belongs to the requested set of shell pairs
                                    !
                                    iaabb = p_ge_q2pq(aa, bb, NSHELL)
                                    abmember = imembership(iaabb, iccdd0, iccdd1)
                                    if (cdmember >= 0 .and. abmember >= 0) then
                                          if (iaabb .ne. iccdd) then
                                                !
                                                ! Compute all integrals belonging to the (AB|CD) shell
                                                ! quartet. The fastest changing index is P \in A,
                                                ! the slowest changing index is S \in D.
                                                !
                                                call ints2e(d, k4, c, k3, b, k2, a, k1, gpqrs)
                                                call fpqk_digest_pqrs_rspq(fpqk, xvec, nxvec, gpqrs, &
                                                      aa, bb, cc, dd)
                                          else
                                                call ints2e(d, k4, c, k3, b, k2, a, k1, gpqrs)
                                                call fpqk_digest_pqrs(fpqk, xvec, nxvec, gpqrs, &
                                                      aa, bb, cc, dd)
                                          end if
                                    else if (cdmember >= 0 .and. abmember < 0) then
                                          call ints2e(d, k4, c, k3, b, k2, a, k1, gpqrs)
                                          call fpqk_digest_pqrs(fpqk, xvec, nxvec, gpqrs, &
                                                aa, bb, cc, dd)
                                    else if (cdmember < 0 .and. abmember >= 0) then
                                          call ints2e(b, k2, a, k1, d, k4, c, k3, gpqrs)
                                          call fpqk_digest_pqrs(fpqk, xvec, nxvec, gpqrs, &
                                                aa, bb, cc, dd)
                                    end if
                              end do
                        end do
                  end do
            end do
      end subroutine fpqkbatch


      subroutine fprkbatch(fprk, xvec, nxvec, k1, k2, kk3, kk4, n)
            ! -----------------------------------------------------------
            ! Compute a matrix of two-electron repulsion integrals,
            ! (PQ|RS) contracted with X_k vectors:
            ! FPRK(PR,K) <- \sum_{QS} (PQ|RS) X_{QS,K}^{AO}.
            ! ------------------------------------------------------------
            ! FPRK
            ! XVEC
            ! NXVEC
            ! ------------------------------------------------------------
            double precision, dimension(:, :, :), intent(inout) :: fprk
            double precision, dimension(:, :, :), intent(in)    :: xvec
            integer, intent(in)                                 :: nxvec
            integer, intent(in)                                 :: k1
            integer, intent(in)                                 :: k2
            integer, dimension(:), intent(in)                   :: kk3
            integer, dimension(:), intent(in)                   :: kk4
            integer, intent(in)                                 :: n

            integer :: i
            integer :: k3, k4, aa, bb, cc, dd
            integer :: a, b, c, d
            integer :: bb1, cc1, dd1
            double precision, dimension(MAX_NFUNC**4) :: gpqrs

            do i = 1, n
                  k3 = kk3(i)
                  k4 = kk4(i)
                  do aa = SH0(k1), SH0(k1+1)-1
                        a = SH(aa)
                        bb1 = min(aa, SH0(k2+1)-1)
                        do bb = SH0(k2), bb1
                              b = SH(bb)
                              cc1 = min(aa, SH0(k3+1)-1)
                              do cc = SH0(k3), cc1
                                    c = SH(cc)
                                    if (cc == aa) then
                                          dd1 = min(SH0(k4+1)-1, bb)
                                    else
                                          dd1 = min(SH0(k4+1)-1, cc)
                                    end if
                                    do dd = SH0(k4), dd1
                                          d = SH(dd)
                                          call ints2e(d, k4, c, k3, b, k2, a, k1, gpqrs)
                                          call fprk_digest(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd)
                                    end do
                              end do
                        end do
                  end do
            end do
      end subroutine fprkbatch


      ! subroutine fprkbatch(fprk, xvec, nxvec, k1, k2, kk3, kk4, n)
      !       ! -----------------------------------------------------------
      !       ! Compute a matrix of two-electron repulsion integrals,
      !       ! (PQ|RS) contracted with X_k vectors:
      !       ! FPRK(PR,K) <- \sum_{QS} (PQ|RS) X_{QS,K}^{AO}.
      !       ! ------------------------------------------------------------
      !       ! FPRK
      !       ! XVEC
      !       ! NXVEC
      !       ! ------------------------------------------------------------
      !       double precision, dimension(:, :), intent(out)   :: fprk
      !       double precision, dimension(:, :, :), intent(in) :: xvec
      !       integer, intent(in)                              :: nxvec
      !       integer, intent(in)                              :: k1
      !       integer, intent(in)                              :: k2
      !       integer, intent(in)                              :: kk3
      !       integer, intent(in)                              :: kk4
      !       integer, intent(in)                              :: n

      !       integer :: aa, bb, cc, dd
      !       integer :: a, b, c, d
      !       integer :: k1, k2, k3, k4
      !       integer :: iaabb, iccdd
      !       integer :: abmember, cdmember
      !       double precision, dimension(MAX_NFUNC**4) :: gpqrs
      !       integer :: iccdd0, iccdd1
      !       integer :: aastart

      !       fprk = ZERO

      !       iccdd0 = p_ge_q2pq(cc0, dd0, NSHELL)
      !       iccdd1 = p_ge_q2pq(cc1, dd1, NSHELL)
      !       !
      !       ! Loop over permutationally unique quartet of shells:
      !       ! AA >= BB
      !       ! CC >= DD
      !       ! AA >= CC
      !       ! if AA == CC then BB >= DD
      !       !
      !       do dd = 1, NSHELL
      !             d = SH(dd)
      !             k4 = SHATOM(dd)
      !             do cc = dd, NSHELL
      !                   c = SH(cc)
      !                   k3 = SHATOM(cc)
      !                   !
      !                   ! Test if ket pair belongs to the requested set of shell pairs
      !                   !
      !                   iccdd = p_ge_q2pq(cc, dd, NSHELL)
      !                   cdmember = imembership(iccdd, iccdd0, iccdd1)
      !                   do bb = 1, NSHELL
      !                         k2 = SHATOM(bb)
      !                         b = SH(bb)
      !                         if (bb < dd) then
      !                               aastart = cc + 1
      !                         else 
      !                               aastart = max(cc, bb)
      !                         end if
      !                         do aa = aastart, NSHELL
      !                               k1 = SHATOM(aa)
      !                               a = SH(aa)
      !                               !
      !                               ! Test if bra pair belongs to the requested set of shell pairs
      !                               !
      !                               iaabb = p_ge_q2pq(aa, bb, NSHELL)
      !                               abmember = imembership(iaabb, iccdd0, iccdd1)
      !                               if (cdmember >= 0 .and. abmember >= 0) then
      !                                     if (iaabb .ne. iccdd) then
      !                                           !
      !                                           ! Compute all integrals belonging to the (AB|CD) shell
      !                                           ! quartet. The fastest changing index is P \in A,
      !                                           ! the slowest changing index is S \in D.
      !                                           !
      !                                           call ints2e(d, k4, c, k3, b, k2, a, k1, gpqrs)
      !                                           call fpqk_digest_pqrs_rspq(fpqk, xvec, nxvec, gpqrs, &
      !                                                 aa, bb, cc, dd)
      !                                     else
      !                                           call ints2e(d, k4, c, k3, b, k2, a, k1, gpqrs)
      !                                           call fpqk_digest_pqrs(fpqk, xvec, nxvec, gpqrs, &
      !                                                 aa, bb, cc, dd)
      !                                     end if
      !                               else if (cdmember >= 0 .and. abmember < 0) then
      !                                     call ints2e(d, k4, c, k3, b, k2, a, k1, gpqrs)
      !                                     call fpqk_digest_pqrs(fpqk, xvec, nxvec, gpqrs, &
      !                                           aa, bb, cc, dd)
      !                               else if (cdmember < 0 .and. abmember >= 0) then
      !                                     call ints2e(b, k2, a, k1, d, k4, c, k3, gpqrs)
      !                                     call fpqk_digest_pqrs(fpqk, xvec, nxvec, gpqrs, &
      !                                           aa, bb, cc, dd)
      !                               end if
      !                         end do
      !                   end do
      !             end do
      !       end do
      ! end subroutine fprkbatch


      pure subroutine fpqk_digest_pqrs_rspq(fpqk, xvec, nxvec, gpqrs, aa, bb, cc, dd)
            double precision, dimension(:, :), intent(inout) :: fpqk
            double precision, dimension(:, :, :), intent(in) :: xvec
            integer, intent(in)                              :: nxvec
            double precision, dimension(:), intent(in)       :: gpqrs
            integer, intent(in)                              :: aa
            integer, intent(in)                              :: bb
            integer, intent(in)                              :: cc
            integer, intent(in)                              :: dd
            
            integer :: p, q, r, s
            integer :: s0, s1, r0, r1, q0, q1, p0, p1
            integer :: ns, nr, nq, np
            integer :: ws, wr, wq
            integer :: ipq, irs
            integer :: v, vsr, vqp
            integer :: rs, pq
            integer :: nfunc_pq, nfunc_rs
            integer :: tp, tq, tr, ts
            double precision :: t
            double precision :: xrs, xsr, xpq, xqp, xrs_sr, xpq_qp
            double precision, dimension(MAX_NFUNC**2) :: xbufpq
            double precision, dimension(MAX_NFUNC**2) :: xbufqp
            double precision, dimension(MAX_NFUNC**2) :: xbufrs
            double precision, dimension(MAX_NFUNC**2) :: xbufsr
            integer :: xidxrs
            integer :: xidxsr
            integer :: xidxpq
            integer :: xidxqp
            integer :: k

            s0 = SHPOS(dd)
            s1 = SHPOS(dd+1) - 1
            ns = s1 - s0 + 1
            r0 = SHPOS(cc)
            r1 = SHPOS(cc+1) - 1
            nr = r1 - r0 + 1
            q0 = SHPOS(bb)
            q1 = SHPOS(bb+1) - 1
            nq = q1 - q0 + 1
            p0 = SHPOS(aa)
            p1 = SHPOS(aa+1) - 1
            np = p1 - p0 + 1
            
            nfunc_rs = nr * ns
            nfunc_pq = np * nq

            wq = np
            wr = np * nq
            ws = wr * nr

            if (aa .ne. bb .and. cc .ne. dd) then
                  do k = 1, nxvec
                        call xbuf(xbufpq, xvec(:, :, k), p0, p1, q0, q1)
                        call xbuf(xbufqp, xvec(:, :, k), q0, q1, p0, p1)
                        call xbuf(xbufrs, xvec(:, :, k), r0, r1, s0, s1)
                        call xbuf(xbufsr, xvec(:, :, k), s0, s1, r0, r1)
                        v = 1
                        do rs = 0, nfunc_rs-1
                              !
                              ! R changes faster than S:
                              ! rs = (s - 1) * nr + r - 1
                              !
                              ts = rs / nr
                              tr = rs - ts * nr
                              r = tr + r0
                              s = ts + s0
                              irs = p_ge_q2pq(r, s, NORB)
                              xidxrs = ts * nr + tr + 1
                              xidxsr = tr * ns + ts + 1
                              xrs = xbufrs(xidxrs)
                              xsr = xbufsr(xidxsr)
                              xrs_sr = xrs + xsr
                              do pq = 0, nfunc_pq-1
                                    !
                                    ! P changes faster than Q:
                                    ! pq = (q - 1) * np + p - 1
                                    !
                                    tq = pq / np
                                    tp = pq - tq * np
                                    p = tp + p0
                                    q = tq + q0
                                    ipq = p_ge_q2pq(p, q, NORB)
                                    xidxpq = tq * np + tp + 1
                                    xidxqp = tp * nq + tq + 1
                                    xpq = xbufpq(xidxpq)
                                    xqp = xbufqp(xidxqp)

                                    t = gpqrs(v)
                                    v = v + 1
                                    
                                    fpqk(ipq, k) = fpqk(ipq, k) + t * xrs_sr
                                    fpqk(irs, k) = fpqk(irs, k) + t * (xpq + xqp)
                              end do
                        end do
                  end do
            else
                  !
                  ! Shell quartets with at least one diagonal shell pair
                  ! must be treated in a special way. Here, the inequalities
                  ! p >= q,
                  ! r >= s,
                  ! will not hold if the loops over diagonal shells were
                  ! non-constrained. This would break the code, because
                  ! the subroutine calculationg the compound index assumes
                  ! canonical ordering: p >= q.
                  !
                  if (aa .ne. bb .and. cc .eq. dd) then
                        do k = 1, nxvec
                              call xbuf(xbufpq, xvec(:, :, k), p0, p1, q0, q1)
                              call xbuf(xbufqp, xvec(:, :, k), q0, q1, p0, p1)
                              call xbuf(xbufrs, xvec(:, :, k), r0, r1, s0, s1)
                              call xbuf_scaldiag(xbufrs, nr)
                              do s = s0, s1
                                    ts = s - s0
                                    do r = s, r1
                                          tr = r - r0
                                          irs = p_ge_q2pq(r, s, NORB)
                                          xidxrs = ts * nr + tr + 1
                                          xidxsr = tr * ns + ts + 1
                                          xrs = xbufrs(xidxrs)
                                          xsr = xbufrs(xidxsr)
                                          xrs_sr = xrs + xsr
                                          vsr = tr * wr + ts * ws
                                          do pq = 0, nfunc_pq-1
                                                !
                                                ! P changes faster than Q:
                                                ! pq = (q - 1) * np + p - 1
                                                !
                                                tq = pq / np
                                                tp = pq - tq * np
                                                v = vsr + tq * wq + tp + 1
                                                p = tp + p0
                                                q = tq + q0
                                                ipq = p_ge_q2pq(p, q, NORB)
                                                xidxpq = tq * np + tp + 1
                                                xidxqp = tp * nq + tq + 1
                                                xpq = xbufpq(xidxpq)
                                                xqp = xbufqp(xidxqp)

                                                t = gpqrs(v)

                                                fpqk(ipq, k) = fpqk(ipq, k) + t * xrs_sr
                                                fpqk(irs, k) = fpqk(irs, k) + t * (xpq + xqp)
                                          end do
                                    end do
                              end do
                        end do
                  else if (aa .eq. bb .and. cc .ne. dd) then
                        do k = 1, nxvec
                              call xbuf(xbufpq, xvec(:, :, k), p0, p1, q0, q1)
                              call xbuf(xbufrs, xvec(:, :, k), r0, r1, s0, s1)
                              call xbuf(xbufsr, xvec(:, :, k), s0, s1, r0, r1)
                              call xbuf_scaldiag(xbufpq, np)
                              do q = q0, q1
                                    do p = q, p1
                                          tq = q - q0
                                          tp = p - p0
                                          vqp = tq * wq + tp + 1
                                          ipq = p_ge_q2pq(p, q, NORB)
                                          xidxpq = tq * np + tp + 1
                                          xidxqp = tp * nq + tq + 1
                                          xpq = xbufpq(xidxpq)
                                          xqp = xbufpq(xidxqp)
                                          xpq_qp = xpq + xqp
                                          do rs = 0, nfunc_rs-1
                                                !
                                                ! R changes faster than S:
                                                ! rs = (s - 1) * nr + r - 1
                                                !
                                                ts = rs / nr
                                                tr = rs - ts * nr
                                                v = vqp + ts * ws + tr * wr
                                                r = tr + r0
                                                s = ts + s0
                                                irs = p_ge_q2pq(r, s, NORB)
                                                xidxrs = ts * nr + tr + 1
                                                xidxsr = tr * ns + ts + 1
                                                xrs = xbufrs(xidxrs)
                                                xsr = xbufsr(xidxsr)

                                                t = gpqrs(v)

                                                fpqk(ipq, k) = fpqk(ipq, k) + t * (xrs + xsr)
                                                fpqk(irs, k) = fpqk(irs, k) + t * xpq_qp
                                          end do
                                    end do
                              end do
                        end do
                  else
                        !
                        ! AA == BB .and. CC == DD
                        !
                        do k = 1, nxvec
                              call xbuf(xbufpq, xvec(:, :, k), p0, p1, q0, q1)
                              call xbuf(xbufrs, xvec(:, :, k), r0, r1, s0, s1)
                              call xbuf_scaldiag(xbufpq, np)
                              call xbuf_scaldiag(xbufrs, nr)
                              v = 1
                              do s = s0, s1
                                    ts = s - s0
                                    v = v + (s - r0) * nfunc_pq
                                    do r = s, r1
                                          tr = r - r0
                                          irs = p_ge_q2pq(r, s, NORB)
                                          xidxrs = ts * nr + tr + 1
                                          xidxsr = tr * ns + ts + 1
                                          xrs = xbufrs(xidxrs)
                                          xsr = xbufrs(xidxsr)
                                          xrs_sr = xrs + xsr
                                          do q = q0, q1
                                                tq = q - q0
                                                v = v + q - p0
                                                do p = q, p1
                                                      tp = p - p0
                                                      ipq = p_ge_q2pq(p, q, NORB)
                                                      xidxpq = tq * np + tp + 1
                                                      xidxqp = tp * nq + tq + 1
                                                      xpq = xbufpq(xidxpq)
                                                      xqp = xbufpq(xidxqp)

                                                      t = gpqrs(v)
                                                      v = v + 1

                                                      fpqk(ipq, k) = fpqk(ipq, k) + t * xrs_sr
                                                      fpqk(irs, k) = fpqk(irs, k) + t * (xpq + xqp)
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end if
            end if
      end subroutine fpqk_digest_pqrs_rspq


      pure subroutine fpqk_digest_pqrs(fpqk, xvec, nxvec, gpqrs, aa, bb, cc, dd)
            double precision, dimension(:, :), intent(inout) :: fpqk
            double precision, dimension(:, :, :), intent(in) :: xvec
            integer, intent(in)                              :: nxvec
            double precision, dimension(:), intent(in)       :: gpqrs
            integer, intent(in)                              :: aa
            integer, intent(in)                              :: bb
            integer, intent(in)                              :: cc
            integer, intent(in)                              :: dd
            
            integer :: p, q, r, s
            integer :: s0, s1, r0, r1, q0, q1, p0, p1
            integer :: ns, nr, nq, np
            integer :: ws, wr, wq
            integer :: ipq, irs
            integer :: v, vsr, vqp
            integer :: rs, pq
            integer :: nfunc_pq, nfunc_rs
            integer :: tp, tq, tr, ts
            double precision :: t
            double precision :: xrs, xsr, xrs_sr
            double precision, dimension(MAX_NFUNC**2) :: xbufrs
            double precision, dimension(MAX_NFUNC**2) :: xbufsr
            integer :: xidxrs
            integer :: xidxsr
            integer :: k

            s0 = SHPOS(dd)
            s1 = SHPOS(dd+1) - 1
            ns = s1 - s0 + 1
            r0 = SHPOS(cc)
            r1 = SHPOS(cc+1) - 1
            nr = r1 - r0 + 1
            q0 = SHPOS(bb)
            q1 = SHPOS(bb+1) - 1
            nq = q1 - q0 + 1
            p0 = SHPOS(aa)
            p1 = SHPOS(aa+1) - 1
            np = p1 - p0 + 1
            
            nfunc_rs = nr * ns
            nfunc_pq = np * nq

            wq = np
            wr = np * nq
            ws = wr * nr

            if (aa .ne. bb .and. cc .ne. dd) then
                  do k = 1, nxvec
                        call xbuf(xbufrs, xvec(:, :, k), r0, r1, s0, s1)
                        call xbuf(xbufsr, xvec(:, :, k), s0, s1, r0, r1)
                        v = 1
                        do rs = 0, nfunc_rs-1
                              !
                              ! R changes faster than S:
                              ! rs = (s - 1) * nr + r - 1
                              !
                              ts = rs / nr
                              tr = rs - ts * nr
                              r = tr + r0
                              s = ts + s0
                              irs = p_ge_q2pq(r, s, NORB)
                              xidxrs = ts * nr + tr + 1
                              xidxsr = tr * ns + ts + 1
                              xrs = xbufrs(xidxrs)
                              xsr = xbufsr(xidxsr)
                              xrs_sr = xrs + xsr
                              do pq = 0, nfunc_pq-1
                                    !
                                    ! P changes faster than Q:
                                    ! pq = (q - 1) * np + p - 1
                                    !
                                    tq = pq / np
                                    tp = pq - tq * np
                                    p = tp + p0
                                    q = tq + q0
                                    ipq = p_ge_q2pq(p, q, NORB)

                                    t = gpqrs(v)
                                    v = v + 1
                                    
                                    fpqk(ipq, k) = fpqk(ipq, k) + t * xrs_sr
                              end do
                        end do
                  end do
            else
                  !
                  ! Shell quartets with at least one diagonal shell pair
                  ! must be treated in a special way. Here, the inequalities
                  ! p >= q,
                  ! r >= s,
                  ! will not hold if the loops over diagonal shells were
                  ! non-constrained. This would break the code, because
                  ! the subroutine calculationg the compound index assumes
                  ! canonical ordering: p >= q.
                  !
                  if (aa .ne. bb .and. cc .eq. dd) then
                        do k = 1, nxvec
                              call xbuf(xbufrs, xvec(:, :, k), r0, r1, s0, s1)
                              call xbuf_scaldiag(xbufrs, nr)
                              do s = s0, s1
                                    ts = s - s0
                                    do r = s, r1
                                          tr = r - r0
                                          irs = p_ge_q2pq(r, s, NORB)
                                          xidxrs = ts * nr + tr + 1
                                          xidxsr = tr * ns + ts + 1
                                          xrs = xbufrs(xidxrs)
                                          xsr = xbufrs(xidxsr)
                                          xrs_sr = xrs + xsr
                                          vsr = tr * wr + ts * ws
                                          do pq = 0, nfunc_pq-1
                                                !
                                                ! P changes faster than Q:
                                                ! pq = (q - 1) * np + p - 1
                                                !
                                                tq = pq / np
                                                tp = pq - tq * np
                                                v = vsr + tq * wq + tp + 1
                                                p = tp + p0
                                                q = tq + q0
                                                ipq = p_ge_q2pq(p, q, NORB)

                                                t = gpqrs(v)

                                                fpqk(ipq, k) = fpqk(ipq, k) + t * xrs_sr
                                          end do
                                    end do
                              end do
                        end do
                  else if (aa .eq. bb .and. cc .ne. dd) then
                        do k = 1, nxvec
                              call xbuf(xbufrs, xvec(:, :, k), r0, r1, s0, s1)
                              call xbuf(xbufsr, xvec(:, :, k), s0, s1, r0, r1)
                              do q = q0, q1
                                    do p = q, p1
                                          tq = q - q0
                                          tp = p - p0
                                          vqp = tq * wq + tp + 1
                                          ipq = p_ge_q2pq(p, q, NORB)
                                          do rs = 0, nfunc_rs-1
                                                !
                                                ! R changes faster than S:
                                                ! rs = (s - 1) * nr + r - 1
                                                !
                                                ts = rs / nr
                                                tr = rs - ts * nr
                                                v = vqp + ts * ws + tr * wr
                                                r = tr + r0
                                                s = ts + s0
                                                irs = p_ge_q2pq(r, s, NORB)
                                                xidxrs = ts * nr + tr + 1
                                                xidxsr = tr * ns + ts + 1
                                                xrs = xbufrs(xidxrs)
                                                xsr = xbufsr(xidxsr)

                                                t = gpqrs(v)

                                                fpqk(ipq, k) = fpqk(ipq, k) + t * (xrs + xsr)
                                          end do
                                    end do
                              end do
                        end do
                  else
                        !
                        ! AA == BB .and. CC == DD
                        !
                        do k = 1, nxvec
                              call xbuf(xbufrs, xvec(:, :, k), r0, r1, s0, s1)
                              call xbuf_scaldiag(xbufrs, nr)
                              v = 1
                              do s = s0, s1
                                    ts = s - s0
                                    v = v + (s - r0) * nfunc_pq
                                    do r = s, r1
                                          tr = r - r0
                                          irs = p_ge_q2pq(r, s, NORB)
                                          xidxrs = ts * nr + tr + 1
                                          xidxsr = tr * ns + ts + 1
                                          xrs = xbufrs(xidxrs)
                                          xsr = xbufrs(xidxsr)
                                          xrs_sr = xrs + xsr
                                          do q = q0, q1
                                                tq = q - q0
                                                v = v + q - p0
                                                do p = q, p1
                                                      tp = p - p0
                                                      ipq = p_ge_q2pq(p, q, NORB)

                                                      t = gpqrs(v)
                                                      v = v + 1

                                                      fpqk(ipq, k) = fpqk(ipq, k) + t * xrs_sr
                                                end do
                                          end do
                                    end do
                              end do
                        end do
                  end if
            end if
      end subroutine fpqk_digest_pqrs


      subroutine fprk_digest(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd)
            double precision, dimension(:, :, :), intent(inout) :: fprk
            double precision, dimension(:, :, :), intent(in)    :: xvec
            integer, intent(in)                                 :: nxvec
            double precision, dimension(:), intent(in)          :: gpqrs
            integer, intent(in)                                 :: aa
            integer, intent(in)                                 :: bb
            integer, intent(in)                                 :: cc
            integer, intent(in)                                 :: dd

            integer :: s0, s1, ns, r0, r1, nr
            integer :: p0, p1, np, q0, q1, nq
            integer :: wp, wq, wr, ws
            integer :: iaabb, iccdd

            s0 = SHPOS(dd)
            s1 = SHPOS(dd+1) - 1
            ns = s1 - s0 + 1
            r0 = SHPOS(cc)
            r1 = SHPOS(cc+1) - 1
            nr = r1 - r0 + 1
            q0 = SHPOS(bb)
            q1 = SHPOS(bb+1) - 1
            nq = q1 - q0 + 1
            p0 = SHPOS(aa)
            p1 = SHPOS(aa+1) - 1
            np = p1 - p0 + 1
            
            wp = 1
            wq = np
            wr = np * nq
            ws = wr * nr

            iaabb = p_ge_q2pq(aa, bb, NSHELL)
            iccdd = p_ge_q2pq(cc, dd, NSHELL)
            !
            ! A single ERI can be utilized at most eight times.
            ! These are the possibilities.
            ! ---------------------------------------------------------
            ! CASE   CONTRIBUTION     SHELL QUARTET     WEIGHTS
            ! ---------------------------------------------------------
            ! 1      (PQ|RS) X_{QS}   AA, BB, CC, DD    WP, WQ, WR, WS
            ! 2      (QP|SR) X_{PR}   BB, AA, DD, CC    WQ, WP, WS, WR
            ! 3      (PQ|SR) X_{QR}   AA, BB, DD, CC    WP, WQ, WS, WR
            ! 4      (QP|RS) X_{PS}   BB, AA, CC, DD    WQ, WP, WR, WS
            ! 
            ! 5      (RS|PQ) X_{SQ}   CC, DD, AA, BB    WR, WS, WP, WQ
            ! 6      (SR|QP) X_{RP}   DD, CC, BB, AA    WS, WR, WQ, WP
            ! 7      (SR|PQ) X_{RQ}   DD, CC, AA, BB    WS, WR, WP, WQ
            ! 8      (RS|QP) X_{SP}   CC, DD, BB, AA    WR, WS, WQ, WP
            !
            if (iaabb .ne. iccdd) then
                  if (aa .ne. bb) then
                        if (cc .ne. dd) then
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, bb, aa, dd, cc, wq, wp, ws, wr)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, dd, cc, wp, wq, ws, wr)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, bb, aa, cc, dd, wq, wp, wr, ws)

                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, cc, dd, aa, bb, wr, ws, wp, wq)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, dd, cc, bb, aa, ws, wr, wq, wp)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, dd, cc, aa, bb, ws, wr, wp, wq)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, cc, dd, bb, aa, wr, ws, wq, wp)
                        else
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)            
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, bb, aa, cc, dd, wq, wp, wr, ws)

                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, cc, dd, aa, bb, wr, ws, wp, wq)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, cc, dd, bb, aa, wr, ws, wq, wp)
                        end if
                  else
                        if (cc .ne. dd) then
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)            
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, dd, cc, wp, wq, ws, wr)

                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, cc, dd, aa, bb, wr, ws, wp, wq)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, dd, cc, aa, bb, ws, wr, wp, wq)
                        else
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)            

                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, cc, dd, aa, bb, wr, ws, wp, wq)
                        end if
                  end if
            else
                  if (aa .ne. bb) then
                        if (cc .ne. dd) then
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)            
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, bb, aa, dd, cc, wq, wp, ws, wr)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, dd, cc, wp, wq, ws, wr)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, bb, aa, cc, dd, wq, wp, wr, ws)
                        else
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)            
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, bb, aa, cc, dd, wq, wp, wr, ws)
                        end if
                  else
                        if (cc .ne. dd) then
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, dd, cc, wp, wq, ws, wr)
                        else
                              call fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)
                        end if
                  end if
            end if
      end subroutine fprk_digest


      subroutine fprk_digest_pr_qs(fprk, xvec, nxvec, gpqrs, aa, bb, cc, dd, wp, wq, wr, ws)
            double precision, dimension(:, :, :), intent(inout) :: fprk
            double precision, dimension(:, :, :), intent(in)    :: xvec
            integer, intent(in)                                 :: nxvec
            double precision, dimension(:), intent(in)          :: gpqrs
            integer, intent(in)                                 :: aa
            integer, intent(in)                                 :: bb
            integer, intent(in)                                 :: cc
            integer, intent(in)                                 :: dd
            integer, intent(in)                                 :: wp
            integer, intent(in)                                 :: wq
            integer, intent(in)                                 :: wr
            integer, intent(in)                                 :: ws

            integer :: p, q, r, s
            integer :: s0, s1, r0, r1, q0, q1, p0, p1
            integer :: ns, nr, nq, np
            integer :: v, vqs, vpr
            integer :: qs, pr
            integer :: nfunc_pr, nfunc_qs
            integer :: tp, tq, tr, ts
            double precision :: t
            double precision :: xqs, xpr
            double precision, dimension(MAX_NFUNC**2) :: xbufqs
            double precision, dimension(MAX_NFUNC**2) :: xbufpr
            integer :: xidxqs
            integer :: xidxpr
            integer :: k

            s0 = SHPOS(dd)
            s1 = SHPOS(dd+1) - 1
            ns = s1 - s0 + 1
            r0 = SHPOS(cc)
            r1 = SHPOS(cc+1) - 1
            nr = r1 - r0 + 1
            q0 = SHPOS(bb)
            q1 = SHPOS(bb+1) - 1
            nq = q1 - q0 + 1
            p0 = SHPOS(aa)
            p1 = SHPOS(aa+1) - 1
            np = p1 - p0 + 1

            nfunc_qs = nq * ns
            nfunc_pr = np * nr

            do k = 1, nxvec
                  call xbuf(xbufqs, xvec(:, :, k), q0, q1, s0, s1)
                  call xbuf(xbufpr, xvec(:, :, k), p0, p1, r0, r1)

                  do qs = 0, nfunc_qs-1
                        !
                        ! Q changes faster than S:
                        ! qs = (s - 1) * nq + q - 1
                        !
                        ts = qs / nq
                        tq = qs - ts * nq
                        q = tq + q0
                        s = ts + s0

                        vqs = wq * tq + ws * ts

                        xidxqs = qs + 1
                        xqs = xbufqs(xidxqs)
                        do pr = 0, nfunc_pr-1
                              !
                              ! P changes faster than R:
                              ! pr = (r - 1) * np + p - 1
                              !
                              tr = pr / np
                              tp = pr - tr * np
                              p = tp + p0
                              r = tr + r0

                              vpr = wp * tp + wr * tr
                              v = vqs + vpr + 1

                              xidxpr = pr + 1
                              xpr = xbufpr(xidxpr)

                              t = gpqrs(v)

                              fprk(p, r, k) = fprk(p, r, k) + t * xqs
                              !fprk(q, s, k) = fprk(q, s, k) + t * xpr
                        end do
                  end do
            end do
      end subroutine fprk_digest_pr_qs


      pure subroutine xbuf(xblock, xmatrix, p0, p1, q0, q1)
            double precision, dimension(:), intent(out)   :: xblock
            double precision, dimension(:, :), intent(in) :: xmatrix
            integer, intent(in)                           :: p0
            integer, intent(in)                           :: p1
            integer, intent(in)                           :: q0
            integer, intent(in)                           :: q1

            integer :: p, q
            integer :: v
            
            v = 1
            do q = q0, q1
                  do p = p0, p1
                        xblock(v) = xmatrix(p, q)
                        v = v + 1
                  end do
            end do
      end subroutine xbuf


      pure subroutine xbuf_scaldiag(xblock, np)
            double precision, dimension(:), intent(out) :: xblock
            integer, intent(in)                         :: np

            integer :: v, p

            do p = 1, np
                  v = (p - 1) * np + p
                  xblock(v) = FRAC12 * xblock(v)
            end do
      end subroutine xbuf_scaldiag


      subroutine digestquartet_abcd_cdab(fpqrs, gabcd, aa, bb, cc, dd, abidx0, cdidx0)
            double precision, dimension(:, :), intent(inout) :: fpqrs
            double precision, dimension(:), intent(in)       :: gabcd
            integer, intent(in)                              :: aa
            integer, intent(in)                              :: bb
            integer, intent(in)                              :: cc
            integer, intent(in)                              :: dd
            integer, intent(in)                              :: abidx0
            integer, intent(in)                              :: cdidx0
            
            integer :: p, q, r, s
            integer :: s0, s1, r0, r1, q0, q1, p0, p1
            integer :: ipq, irs, irs_shifted, ipq_shifted
            integer :: v
            integer :: nfunc_pq
            double precision :: t

            s0 = SHPOS(dd)
            s1 = SHPOS(dd+1) - 1
            r0 = SHPOS(cc)
            r1 = SHPOS(cc+1) - 1
            q0 = SHPOS(bb)
            q1 = SHPOS(bb+1) - 1
            p0 = SHPOS(aa)
            p1 = SHPOS(aa+1) - 1
            nfunc_pq = (p1 - p0 + 1) * (q1 - q0 + 1)

            if (aa .ne. bb .and. cc .ne. dd) then
                  v = 1
                  irs_shifted = cdidx0 - 1
                  do s = s0, s1
                        do r = r0, r1
                              irs = p_ge_q2pq(r, s, NORB)
                              irs_shifted = irs_shifted + 1
                              ipq_shifted = abidx0 - 1
                              do q = q0, q1
                                    do p = p0, p1
                                          ipq = p_ge_q2pq(p, q, NORB)
                                          ipq_shifted = ipq_shifted + 1
                                          t = gabcd(v)
                                          fpqrs(ipq, irs_shifted) = t
                                          fpqrs(irs, ipq_shifted) = t
                                          v = v + 1
                                    end do
                              end do
                        end do
                  end do
            else
                  !
                  ! Shell quartets with at least one diagonal shell pair
                  ! must be treated in a special way. Here, the inequalities
                  ! p >= q,
                  ! r >= s,
                  ! will not hold if loops over diagonal shells were non-constrained.
                  ! This would break the code, because the subroutine calculationg
                  ! compound index assumes canonical ordering: p >= q.
                  !
                  if (aa .ne. bb .and. cc .eq. dd) then
                        v = 1
                        irs_shifted = cdidx0 - 1
                        do s = s0, s1
                              v = v + (s - r0) * nfunc_pq
                              do r = s, r1
                                    irs = p_ge_q2pq(r, s, NORB)
                                    irs_shifted = irs_shifted + 1
                                    ipq_shifted = abidx0 - 1
                                    do q = q0, q1
                                          do p = p0, p1
                                                ipq = p_ge_q2pq(p, q, NORB)
                                                ipq_shifted = ipq_shifted + 1
                                                t = gabcd(v)
                                                fpqrs(ipq, irs_shifted) = t
                                                fpqrs(irs, ipq_shifted) = t
                                                v = v + 1
                                          end do
                                    end do
                              end do
                        end do
                  else if (aa .eq. bb .and. cc .ne. dd) then
                        v = 1
                        irs_shifted = cdidx0 - 1
                        do s = s0, s1
                              do r = r0, r1
                                    irs = p_ge_q2pq(r, s, NORB)
                                    irs_shifted = irs_shifted + 1
                                    ipq_shifted = abidx0 - 1
                                    do q = q0, q1
                                          v = v + q - p0
                                          do p = q, p1
                                                ipq = p_ge_q2pq(p, q, NORB)
                                                ipq_shifted = ipq_shifted + 1
                                                t = gabcd(v)
                                                fpqrs(ipq, irs_shifted) = t
                                                fpqrs(irs, ipq_shifted) = t
                                                v = v + 1
                                          end do
                                    end do
                              end do
                        end do
                  else
                        !
                        ! AA == BB .and. CC == DD
                        !
                        v = 1
                        irs_shifted = cdidx0 - 1
                        do s = s0, s1
                              v = v + (s - r0) * nfunc_pq
                              do r = s, r1
                                    irs = p_ge_q2pq(r, s, NORB)
                                    irs_shifted = irs_shifted + 1
                                    ipq_shifted = abidx0 - 1
                                    do q = q0, q1
                                          v = v + q - p0
                                          do p = q, p1
                                                ipq = p_ge_q2pq(p, q, NORB)
                                                ipq_shifted = ipq_shifted + 1
                                                t = gabcd(v)
                                                fpqrs(ipq, irs_shifted) = t
                                                fpqrs(irs, ipq_shifted) = t
                                                v = v + 1
                                          end do
                                    end do
                              end do
                        end do
                  end if
            end if
      end subroutine digestquartet_abcd_cdab


      subroutine digestquartet_abcd(fpqrs, gabcd, aa, bb, cc, dd, cdidx0)
            double precision, dimension(:, :), intent(inout) :: fpqrs
            double precision, dimension(:), intent(in)       :: gabcd
            integer, intent(in)                              :: aa
            integer, intent(in)                              :: bb
            integer, intent(in)                              :: cc
            integer, intent(in)                              :: dd
            integer, intent(in)                              :: cdidx0
            
            integer :: p, q, r, s
            integer :: s0, s1, r0, r1, q0, q1, p0, p1
            integer :: ipq, irs, irs_shifted
            integer :: v
            integer :: nfunc_pq
            double precision :: t

            s0 = SHPOS(dd)
            s1 = SHPOS(dd+1) - 1
            r0 = SHPOS(cc)
            r1 = SHPOS(cc+1) - 1
            q0 = SHPOS(bb)
            q1 = SHPOS(bb+1) - 1
            p0 = SHPOS(aa)
            p1 = SHPOS(aa+1) - 1
            nfunc_pq = (p1 - p0 + 1) * (q1 - q0 + 1)

            if (aa .ne. bb .and. cc .ne. dd) then
                  v = 1
                  irs_shifted = cdidx0 - 1
                  do s = s0, s1
                        do r = r0, r1
                              irs = p_ge_q2pq(r, s, NORB)
                              irs_shifted = irs_shifted + 1
                              do q = q0, q1
                                    do p = p0, p1
                                          ipq = p_ge_q2pq(p, q, NORB)
                                          t = gabcd(v)
                                          fpqrs(ipq, irs_shifted) = t
                                          v = v + 1
                                    end do
                              end do
                        end do
                  end do
            else
                  !
                  ! Shell quartets with at least one diagonal shell pair
                  ! must be treated in a special way. Here, the inequalities
                  ! p >= q,
                  ! r >= s,
                  ! will not hold if loops over diagonal shells were non-constrained.
                  ! This would break the code, because the subroutine calculationg
                  ! compound index assumes canonical ordering: p >= q.
                  !
                  if (aa .ne. bb .and. cc .eq. dd) then
                        v = 1
                        irs_shifted = cdidx0 - 1
                        do s = s0, s1
                              v = v + (s - r0) * nfunc_pq
                              do r = s, r1
                                    irs = p_ge_q2pq(r, s, NORB)
                                    irs_shifted = irs_shifted + 1
                                    do q = q0, q1
                                          do p = p0, p1
                                                ipq = p_ge_q2pq(p, q, NORB)
                                                t = gabcd(v)
                                                fpqrs(ipq, irs_shifted) = t
                                                v = v + 1
                                          end do
                                    end do
                              end do
                        end do
                  else if (aa .eq. bb .and. cc .ne. dd) then
                        v = 1
                        irs_shifted = cdidx0 - 1
                        do s = s0, s1
                              do r = r0, r1
                                    irs = p_ge_q2pq(r, s, NORB)
                                    irs_shifted = irs_shifted + 1
                                    do q = q0, q1
                                          v = v + q - p0
                                          do p = q, p1
                                                ipq = p_ge_q2pq(p, q, NORB)
                                                t = gabcd(v)
                                                fpqrs(ipq, irs_shifted) = t
                                                v = v + 1
                                          end do
                                    end do
                              end do
                        end do
                  else
                        !
                        ! AA == BB .and. CC == DD
                        !
                        v = 1
                        irs_shifted = cdidx0 - 1
                        do s = s0, s1
                              v = v + (s - r0) * nfunc_pq
                              do r = s, r1
                                    irs = p_ge_q2pq(r, s, NORB)
                                    irs_shifted = irs_shifted + 1
                                    do q = q0, q1
                                          v = v + q - p0
                                          do p = q, p1
                                                ipq = p_ge_q2pq(p, q, NORB)
                                                t = gabcd(v)
                                                fpqrs(ipq, irs_shifted) = t
                                                v = v + 1
                                          end do
                                    end do
                              end do
                        end do
                  end if
            end if
     end subroutine digestquartet_abcd


     pure function p_ge_q2pq(p, q, m)
           ! ----------------------------------------------------------------
           ! Compute compound 2-index, (pq), assuming that p is always
           ! greater or equal q. Min. index: 1, max index: M.
           ! Example: P and Q enumerate, respectively, rows and columns
           ! of a symmetric matrix. The compound 2-index enumerates 
           ! consecutive elements of the lower triangle of the 5x5 matrix.
           !
           ! M = 5
           !             Q 
           !    | 1                |
           !    | 2  6             |
           ! P  | 3  7  10         |
           !    | 4  8  11  13     |
           !    | 5  9  12  14  15 |
           !
           integer             :: p_ge_q2pq
           integer, intent(in) :: p
           integer, intent(in) :: q
           integer, intent(in) :: m

           integer :: i1, i2

           i1 = ((2 * m - q + 2) * (q - 1)) / 2
           i2 = p - q + 1
           p_ge_q2pq = i1 + i2
     end function p_ge_q2pq


      pure function a_i2ai(b, j, nvirt)
            !
            ! Compute compound ket index, bj.
            ! It is assumed that virtual index
            ! changes faster.
            !
            integer             :: a_i2ai
            integer, intent(in) :: b
            integer, intent(in) :: j
            integer, intent(in) :: nvirt

            a_i2ai = nvirt * (j - 1) + b
      end function a_i2ai


      ! pure subroutine faibj_b_j(b, j, ibj, nvirt)
      !       integer, intent(out) :: b
      !       integer, intent(out) :: j
      !       integer, intent(in)  :: ibj
      !       integer, intent(in)  :: nvirt

      !       integer :: t

      !       t = (ibj - 1) / nvirt
      !       b = ibj - nvirt * t
      !       j = t + 1
      ! end subroutine faibj_b_j


      pure function imembership(a, a0, a1)
            !
            ! Return non-negative integer if A0 <= A <= A1, and
            ! negative integer otherwise.
            !
            integer             :: imembership
            integer, intent(in) :: a
            integer, intent(in) :: a0
            integer, intent(in) :: a1

            imembership = (a - a0) * (a1 - a)
      end function imembership


      pure function fpqrs_minnrs()
            !
            ! Calculate minimum number of ket AO indices
            ! required to store a batch of AO two-electron
            ! integrals 
            !
            integer :: fpqrs_minnrs
            integer :: dd, d, cc, c
            integer :: nfuncc, nfuncd, ncd
            integer :: minnrs

            minnrs = 1
            do dd = 1, NSHELL
                  d = SH(dd)
                  nfuncd = nfunc(SHTYPE(d))
                  do cc = dd, NSHELL
                        c = SH(cc)
                        nfuncc = nfunc(SHTYPE(c))
                        if (cc .ne. dd) then
                              ncd = nfuncc * nfuncd
                        else
                              ncd = (nfuncc * (nfuncc - 1)) / 2 + nfuncc
                        end if
                        minnrs = max(minnrs, ncd)
                  end do
            end do
            fpqrs_minnrs = minnrs
      end function fpqrs_minnrs


      subroutine motransfdrv(fprk, fpqk, xvec, nxvec, mask)
            double precision, dimension(:, :, :), intent(out) :: fprk
            double precision, dimension(:, :), intent(out)    :: fpqk
            double precision, dimension(:, :, :), intent(in)  :: xvec
            integer, intent(in)                               :: nxvec
            integer, intent(in)                               :: mask

            integer :: k1, k2, k3, k4
            integer :: k4_max, n
            !
            ! Loop over quartets of atoms, (K1 K2|K3 K4),
            ! where K1 >= K2, K3 >= K4, (K1,K2) >= (K3,K4)
            !
            fprk = ZERO
            fpqk = ZERO
            do k1 = 1, NATOM
                  do k2 = 1, k1
                        n = 0
                        do k3 = 1, k1
                              do k4 = 1, k3
                                    n = n + 1
                                    kk3(n) = k3
                                    kk4(n) = k4

                                    if (n .eq. MAX_NBATCH) then
                                          call fprkbatch(fprk, xvec, nxvec, k1, k2, kk3, kk4, n)
                                          n = 0
                                    end if
                              end do
                        end do
                        if (n .gt. 0) then
                              call fprkbatch(fprk, xvec, nxvec, k1, k2, kk3, kk4, n)
                        end if
                  end do
            end do
      end subroutine motransfdrv
end module motransf
