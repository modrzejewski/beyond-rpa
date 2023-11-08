module fock2el
      use arithmetic
      use basis
      use ints
      use gparam
      use threads
      use jengine
      use hfcoul
      use hfexch
      use tiledmatrix
      use auto2e

      implicit none
      save
 
      integer, dimension(:), allocatable, private             :: kk3
      integer, dimension(:), allocatable, private             :: kk4
      integer, dimension(:), allocatable, private             :: qq
      integer, dimension(:), allocatable, private             :: ff
      !
      ! ERIs permutations' labels (bit flags). 
      !
      integer, parameter, private                             :: EXCH_PERM_ABCD = 2**0
      integer, parameter, private                             :: EXCH_PERM_ABDC = 2**1
      integer, parameter, private                             :: EXCH_PERM_BACD = 2**2
      integer, parameter, private                             :: EXCH_PERM_BADC = 2**3
      integer, parameter, private                             :: EXCH_PERM_CDAB = 2**4
      integer, parameter, private                             :: EXCH_PERM_CDBA = 2**5
      integer, parameter, private                             :: EXCH_PERM_DCAB = 2**6
      integer, parameter, private                             :: EXCH_PERM_DCBA = 2**7
      integer, parameter, private                             :: COUL_PERM_ABCD = 2**8
      integer, parameter, private                             :: COUL_PERM_CDAB = 2**9
      
      integer, parameter                                      :: COULSUM = &
            COUL_PERM_ABCD + COUL_PERM_CDAB
      integer, parameter                                      :: EXCHSUM = &
            EXCH_PERM_ABCD + EXCH_PERM_ABDC + EXCH_PERM_BACD + EXCH_PERM_BADC + &
            EXCH_PERM_CDAB + EXCH_PERM_CDBA + EXCH_PERM_DCAB + EXCH_PERM_DCBA
      integer, parameter                                      :: COULEXCHSUM = COULSUM + EXCHSUM

      integer, private                                        :: MAX_NBATCH = -1
      double precision, dimension(:), allocatable, private    :: RHO_TILE, KMATRIX_TILE, JMATRIX_TILE

contains

      subroutine fock2el_init()
            integer :: natompair, nbatch, npool
            !
            ! Maximum batch of atom pairs as a function of the number of threads.
            ! Tune this parameter if large number of threads is used.
            !
            MAX_NBATCH = OMP_NTHREAD * 100
            natompair = (NATOM * (NATOM + 1)) / 2
            nbatch = min(natompair, MAX_NBATCH)
            allocate(kk3(nbatch))
            allocate(kk4(nbatch))
            allocate(RHO_TILE(NORB**2))
            allocate(KMATRIX_TILE(NORB**2))
            allocate(JMATRIX_TILE(NORB**2))
            npool = nbatch * MAX_ATOMNSHELL**4
            allocate(ff(npool))
            allocate(qq(npool))
      end subroutine fock2el_init

      
      subroutine fock2el_free()
            deallocate(kk3)
            deallocate(kk4)
            deallocate(ff)
            deallocate(qq)
            deallocate(RHO_TILE)
            deallocate(KMATRIX_TILE)
            deallocate(JMATRIX_TILE)
      end subroutine fock2el_free


      pure subroutine flagpack(idx, iscalab, iscalcd, flags, i)
            !
            ! Bit ordering (assumed at least 32 bit integer)
            !  >= 20       1         1        10
            ! ... IDX | ISCALAB | ISCALCD | FLAGS |
            !
            integer, intent(in)  :: idx
            integer, intent(in)  :: iscalab, iscalcd
            integer, intent(in)  :: flags
            integer, intent(out) :: i
            
            i = ishft(idx, 12)
            call mvbits(iscalab, 0, 1, i, 11)
            call mvbits(iscalcd, 0, 1, i, 10)
            call mvbits(flags, 0, 10, i, 0)
      end subroutine flagpack
      
      
      pure subroutine flagunpack(i, idx, iscalab, iscalcd, flags)
            integer, intent(in)  :: i
            integer, intent(out) :: idx
            integer, intent(out) :: iscalab, iscalcd
            integer, intent(out) :: flags
            !
            ! Bit ordering (assumed at least 32 bit integer)
            !  >= 20       1         1        10
            ! ... IDX | ISCALAB | ISCALCD | FLAGS |
            !
            idx = ishft(i, -12)
            iscalab = ibits(i, 11, 1)
            iscalcd = ibits(i, 10, 1)
            flags = ibits(i, 0, 10)
      end subroutine flagunpack
      

      pure subroutine digest_rhobd_tile(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, gabcd)
            double precision, dimension(:), intent(out)   :: t
            integer, intent(out)                          :: k0, k1
            double precision, dimension(:), intent(in)    :: rho
            integer, intent(in)                           :: a, na
            integer, intent(in)                           :: b, nb
            integer, intent(in)                           :: c, nc
            integer, intent(in)                           :: d, nd
            double precision, dimension(:), intent(in)    :: gabcd

            integer :: k, r, r0, n
            integer :: idxa, idxb, idxc, idxd
            integer :: u
            
            n = na * nc
            k0 = tilepos(a, c)
            k1 = k0 + n - 1
            r0 = tilepos(d, b) - 1
            u = 1
            t(1:n) = ZERO
            do idxa = 1, na
                  do idxb = 1, nb
                        do idxc = 1, nc
                              do idxd = 1, nd
                                    k = na * (idxc - 1) + idxa
                                    r = r0 + nd * (idxb - 1) + idxd
                                    t(k) = t(k) + rho(r) * gabcd(u)
                                    u = u + 1
                                    !
                                    ! kmatrix(idxa, idxc) = kmatrix(idxa, idxc) &
                                    !       + rho(idxd, idxb) * (AB|CD)
                                    !
                              end do
                        end do
                  end do
            end do
      end subroutine digest_rhobd_tile


      pure subroutine digest_rhobc_tile(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, gabcd)
            double precision, dimension(:), intent(out)   :: t
            integer, intent(out)                          :: k0, k1
            double precision, dimension(:), intent(in)    :: rho
            integer, intent(in)                           :: a, na
            integer, intent(in)                           :: b, nb
            integer, intent(in)                           :: c, nc
            integer, intent(in)                           :: d, nd
            double precision, dimension(:), intent(in)    :: gabcd

            integer :: k, r, r0, n
            integer :: idxa, idxb, idxc, idxd
            integer :: u

            n = na * nd
            k0 = tilepos(a, d)
            k1 = k0 + n - 1
            r0 = tilepos(c, b) - 1
            u = 1
            t(1:n) = ZERO
            do idxa = 1, na
                  do idxb = 1, nb
                        do idxc = 1, nc
                              do idxd = 1, nd
                                    k = na * (idxd - 1) + idxa
                                    r = r0 + nc * (idxb - 1) + idxc
                                    t(k) = t(k) + rho(r) * gabcd(u)
                                    u = u + 1
                                    !
                                    ! kmatrix(idxa, idxd) = kmatrix(idxa, idxd) &
                                    !       + rho(idxc, idxb) * (AB|CD)
                                    !
                              end do
                        end do
                  end do
            end do
      end subroutine digest_rhobc_tile


      pure subroutine digest_rhoad_tile(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, gabcd)
            double precision, dimension(:), intent(out)   :: t
            integer, intent(out)                          :: k0, k1
            double precision, dimension(:), intent(in)    :: rho
            integer, intent(in)                           :: a, na
            integer, intent(in)                           :: b, nb
            integer, intent(in)                           :: c, nc
            integer, intent(in)                           :: d, nd
            double precision, dimension(:), intent(in)    :: gabcd

            integer :: k, r, r0, n
            integer :: idxa, idxb, idxc, idxd
            integer :: u

            n = nb * nc
            k0 = tilepos(b, c)
            k1 = k0 + n - 1
            r0 = tilepos(d, a) - 1
            u = 1
            t(1:n) = ZERO
            do idxa = 1, na
                  do idxb = 1, nb
                        do idxc = 1, nc
                              do idxd = 1, nd
                                    k = nb * (idxc - 1) + idxb
                                    r = r0 + nd * (idxa - 1) + idxd
                                    t(k) = t(k) + rho(r) * gabcd(u)
                                    u = u + 1
                                    !
                                    ! kmatrix(idxb, idxc) = kmatrix(idxb, idxc) &
                                    !       + rho(idxd, idxa) * (AB|CD)
                                    !
                              end do
                        end do
                  end do
            end do
      end subroutine digest_rhoad_tile


      pure subroutine digest_rhoac_tile(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, gabcd)
            double precision, dimension(:), intent(out)   :: t
            integer, intent(out)                          :: k0, k1
            double precision, dimension(:), intent(in)    :: rho
            integer, intent(in)                           :: a, na
            integer, intent(in)                           :: b, nb
            integer, intent(in)                           :: c, nc
            integer, intent(in)                           :: d, nd
            double precision, dimension(:), intent(in)    :: gabcd

            integer :: k, r, r0, n
            integer :: idxa, idxb, idxc, idxd
            integer :: u

            n = nb * nd
            k0 = tilepos(b, d)
            k1 = k0 + n - 1
            r0 = tilepos(c, a) - 1
            u = 1
            t(1:n) = ZERO
            do idxa = 1, na
                  do idxb = 1, nb
                        do idxc = 1, nc
                              do idxd = 1, nd
                                    k = nb * (idxd - 1) + idxb
                                    r = r0 + nc * (idxa - 1) + idxc
                                    t(k) = t(k) + rho(r) * gabcd(u)
                                    u = u + 1
                                    ! kmatrix(idxb, idxd) = kmatrix(idxb, idxd) &
                                    !       + rho(idxc, idxa) * (AB|CD)
                              end do
                        end do
                  end do
            end do
      end subroutine digest_rhoac_tile


      pure subroutine digest_rhodb_tile(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, gabcd)
            double precision, dimension(:), intent(out) :: t
            integer, intent(out)                        :: k0
            integer, intent(out)                        :: k1
            double precision, dimension(:), intent(in)  :: rho
            integer, intent(in)                         :: a, na
            integer, intent(in)                         :: b, nb
            integer, intent(in)                         :: c, nc
            integer, intent(in)                         :: d, nd
            double precision, dimension(:), intent(in)  :: gabcd

            integer :: k, r, r0, n
            integer :: idxa, idxb, idxc, idxd
            integer :: u

            n = nc * na
            k0 = tilepos(c, a)
            k1 = k0 + n - 1
            r0 = tilepos(d, b) - 1
            u = 1
            t(1:n) = ZERO
            do idxa = 1, na
                  do idxb = 1, nb
                        do idxc = 1, nc
                              do idxd = 1, nd
                                    k = nc * (idxa - 1) + idxc
                                    r = r0 + nd * (idxb - 1) + idxd
                                    t(k) = t(k) + rho(r) * gabcd(u)
                                    u = u + 1
                                    !
                                    ! kmatrix(idxc, idxa) = kmatrix(idxc, idxa) &
                                    !       + rho(idxd, idxb) * (AB|CD)
                                    !
                              end do
                        end do
                  end do
            end do
      end subroutine digest_rhodb_tile


      pure subroutine digest_rhoda_tile(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, gabcd)
            double precision, dimension(:), intent(out) :: t
            integer, intent(out)                        :: k0, k1
            double precision, dimension(:), intent(in)  :: rho
            integer, intent(in)                         :: a, na
            integer, intent(in)                         :: b, nb
            integer, intent(in)                         :: c, nc
            integer, intent(in)                         :: d, nd
            double precision, dimension(:), intent(in)  :: gabcd

            integer :: k, r, r0, n
            integer :: idxa, idxb, idxc, idxd
            integer :: u

            n = nc * nb
            k0 = tilepos(c, b)
            k1 = k0 + n - 1
            r0 = tilepos(d, a) - 1
            u = 1
            t(1:n) = ZERO
            do idxa = 1, na
                  do idxb = 1, nb
                        do idxc = 1, nc
                              do idxd = 1, nd
                                    k = nc * (idxb - 1) + idxc
                                    r = r0 + nd * (idxa - 1) + idxd
                                    t(k) = t(k) + rho(r) * gabcd(u)
                                    u = u + 1
                                    !
                                    ! kmatrix(idxc, idxb) = kmatrix(idxc, idxb) &
                                    !       + rho(idxd, idxa) * (AB|CD)
                                    !
                              end do
                        end do
                  end do
            end do
      end subroutine digest_rhoda_tile


      pure subroutine digest_rhocb_tile(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, gabcd)
            double precision, dimension(:), intent(out) :: t
            integer, intent(out)                        :: k0, k1
            double precision, dimension(:), intent(in)  :: rho
            integer, intent(in)                         :: a, na
            integer, intent(in)                         :: b, nb
            integer, intent(in)                         :: c, nc
            integer, intent(in)                         :: d, nd
            double precision, dimension(:), intent(in)  :: gabcd

            integer :: k, r, r0, n
            integer :: idxa, idxb, idxc, idxd
            integer :: u

            n = nd * na
            k0 = tilepos(d, a)
            k1 = k0 + n - 1
            r0 = tilepos(c, b) - 1
            u = 1
            t(1:n) = ZERO
            do idxa = 1, na
                  do idxb = 1, nb
                        do idxc = 1, nc
                              do idxd = 1, nd
                                    k = nd * (idxa - 1) + idxd
                                    r = r0 + nc * (idxb - 1) + idxc
                                    t(k) = t(k) + rho(r) * gabcd(u)
                                    u = u + 1
                                    !
                                    ! kmatrix(idxd, idxa) = kmatrix(idxd, idxa) &
                                    !       + rho(idxc, idxb) * (AB|CD)
                                    !
                              end do
                        end do
                  end do
            end do
      end subroutine digest_rhocb_tile


      pure subroutine digest_rhoca_tile(t, k0, k1, rho, a, na, b, nb, c, nc, d, nd, gabcd)
            double precision, dimension(:), intent(out) :: t
            integer, intent(out)                        :: k0, k1
            double precision, dimension(:), intent(in)  :: rho
            integer, intent(in)                         :: a, na
            integer, intent(in)                         :: b, nb
            integer, intent(in)                         :: c, nc
            integer, intent(in)                         :: d, nd
            double precision, dimension(:), intent(in)  :: gabcd

            integer :: k, r, r0, n
            integer :: idxa, idxb, idxc, idxd
            integer :: u

            n = nd * nb
            k0 = tilepos(d, b)
            k1 = k0 + n - 1
            r0 = tilepos(c, a) - 1
            u = 1
            t(1:n) = ZERO
            do idxa = 1, na
                  do idxb = 1, nb
                        do idxc = 1, nc
                              do idxd = 1, nd
                                    k = nd * (idxb - 1) + idxd
                                    r = r0 + nc * (idxa - 1) + idxc
                                    t(k) = t(k) + rho(r) * gabcd(u)
                                    u = u + 1
                                    !
                                    ! kmatrix(idxd, idxb) = kmatrix(idxd, idxb) &
                                    !       + rho(idxc, idxa) * (AB|CD)
                                    !
                              end do
                        end do
                  end do
            end do
      end subroutine digest_rhoca_tile


      subroutine fockcontrib_tile(kmatrix, jmatrix, flags, aa, bb, cc, dd, k1, k2, k3, k4, &
            kscal, scalab, scalcd, rho, Kappa)
            
            double precision, dimension(:), intent(inout) :: kmatrix
            double precision, dimension(:), intent(inout) :: jmatrix
            integer, intent(in)                           :: flags
            integer, intent(in)                           :: aa, bb, cc, dd
            integer, intent(in)                           :: k1, k2, k3, k4
            double precision, intent(in)                  :: kscal
            double precision, intent(in)                  :: scalab, scalcd
            double precision, dimension(:), intent(in)    :: rho
            real(F64), intent(in)                         :: Kappa
            
            integer :: w
            integer :: na, nb, nc, nd
            integer :: a, b, c, d
            integer :: ab0, cd0
            integer :: exchbits, coulbits
            integer :: nab, ncd, nints
            double precision, dimension(max_nfunc**4) :: gabcd
            double precision, dimension(max_nfunc**2) :: scratch, scratch2
            integer :: s0, s1, p0, p1

            a = sh(aa)
            b = sh(bb)
            c = sh(cc)
            d = sh(dd)
            na = SHPOS(aa+1) - SHPOS(aa)
            nb = SHPOS(bb+1) - SHPOS(bb)
            nc = SHPOS(cc+1) - SHPOS(cc)
            nd = SHPOS(dd+1) - SHPOS(dd)

            exchbits = iand(flags, EXCHSUM)
            coulbits = iand(flags, COULSUM)
            !
            ! Exchange integrals
            !
            if (exchbits .gt. 0) then
                  nints = na * nb * nc * nd
                  !
                  ! NOTE THE INVERTED ORDER OF SHELLS: (BA|DC)
                  !
                  call AUTO2EERI(auto2e_idx(SHTYPE(b), SHTYPE(a), SHTYPE(d), SHTYPE(c)))%ptr(Gabcd, &
                        ATOMR(:, k2), CNTR(:, b), CNTRNORM(:, b), EXPN(:, b), NPRM(b), &
                        ATOMR(:, k1), CNTR(:, a), CNTRNORM(:, a), EXPN(:, a), NPRM(a), &
                        ATOMR(:, k4), CNTR(:, d), CNTRNORM(:, d), EXPN(:, d), NPRM(d), &
                        ATOMR(:, k3), CNTR(:, c), CNTRNORM(:, c), EXPN(:, c), NPRM(c), &
                        Kappa)
                  
                  !                  call ints2e(b, k2, a, k1, d, k4, c, k3, gabcd)

                  
                  do w = 1, nints
                        gabcd(w) = kscal * gabcd(w)
                  end do
                  !
                  ! The EXCH_PERM_ABCD flag indicates that the contribution
                  ! K <- K + RHO(B, D) * (AB|CD)
                  ! is to be computed. In general case, EXCH_PERM_UVXY
                  ! corresponds to multiplying the two electron integral
                  ! by RHO(V, Y).
                  !
                  if (iand(exchbits, EXCH_PERM_ABCD) .gt. 0) then
                        call digest_rhoac_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_ABDC) .gt. 0) then
                        call digest_rhoad_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_BACD) .gt. 0) then
                        call digest_rhobc_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_BADC) .gt. 0) then
                        call digest_rhobd_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_CDAB) .gt. 0) then
                        call digest_rhoca_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_CDBA) .gt. 0) then
                        call digest_rhocb_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_DCAB) .gt. 0) then
                        call digest_rhoda_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_DCBA) .gt. 0) then
                        call digest_rhodb_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if
            end if
            !
            ! Coulomb integrals
            !
            if ((coulbits .gt. 0) .and. (exchbits .gt. 0)) then
                  nab = na * nb
                  ncd = nc * nd
                  ab0 = tilepos(aa, bb)
                  cd0 = tilepos(cc, dd)
                  if (iand(coulbits, COULSUM) .eq. COULSUM) then
                        call jab_tile(scratch, rho, cd0, gabcd, nab, ncd, scalab / kscal)
                        s0 = ab0
                        s1 = ab0 + nab - 1
                        call offload_scratch(jmatrix(s0:s1), scratch, s1-s0+1)
                        call jcd_tile(scratch, rho, ab0, gabcd, nab, ncd, scalcd / kscal)
                        s0 = cd0
                        s1 = cd0 + ncd - 1
                        call offload_scratch(jmatrix(s0:s1), scratch, s1-s0+1)
                  else if (iand(coulbits, COUL_PERM_ABCD) .gt. 0) then                            
                        call jab_tile(scratch, rho, cd0, gabcd, nab, ncd, scalab / kscal)
                        s0 = ab0
                        s1 = ab0 + nab - 1
                        call offload_scratch(jmatrix(s0:s1), scratch, s1-s0+1)
                  else
                        call jcd_tile(scratch, rho, ab0, gabcd, nab, ncd, scalcd / kscal)
                        s0 = cd0
                        s1 = cd0 + ncd - 1
                        call offload_scratch(jmatrix(s0:s1), scratch, s1-s0+1)
                  end if
            else if ((coulbits .gt. 0) .and. (exchbits .eq. 0)) then
                  nab = na * nb
                  ncd = nc * nd
                  ab0 = tilepos(aa, bb)
                  cd0 = tilepos(cc, dd)
                  if (iand(coulbits, COULSUM) .eq. COULSUM) then
                        call jeng_tile(scratch, scratch2, a, k1, b, k2, c, k3, d, k4, &
                              rho(ab0:), rho(cd0:), .true., .true., scalab, scalcd)
                        s0 = ab0
                        s1 = ab0 + nab - 1
                        call offload_scratch(jmatrix(s0:s1), scratch, s1-s0+1)
                        p0 = cd0
                        p1 = cd0 + ncd - 1
                        call offload_scratch(jmatrix(p0:p1), scratch2, p1-p0+1)
                  else if (iand(coulbits, COUL_PERM_ABCD) .gt. 0) then                            
                        call jeng_tile(scratch, scratch2, a, k1, b, k2, c, k3, d, k4, &
                              rho(ab0:), rho(cd0:), .true., .false., scalab, scalcd)
                        s0 = ab0
                        s1 = ab0 + nab - 1
                        call offload_scratch(jmatrix(s0:s1), scratch, s1-s0+1)
                  else
                        call jeng_tile(scratch, scratch2, a, k1, b, k2, c, k3, d, k4, &
                              rho(ab0:), rho(cd0:), .false., .true., scalab, scalcd) 
                        p0 = cd0
                        p1 = cd0 + ncd - 1
                        call offload_scratch(jmatrix(p0:p1), scratch2, p1-p0+1)
                  end if
            end if
      end subroutine fockcontrib_tile


      subroutine exchange_antisymm_tile(kmatrix, flags, aa, bb, cc, dd, &
            k1, k2, k3, k4, kscal, rho, Kappa)
            !
            ! Compute the contributions to the Hartree-Fock exchange matrix in the case of an antisymmetric
            ! density matrix Rho.
            !
            ! The definition of the exchange matrix element is (note the interchanged indices pq):
            !
            ! K(p, q) <- K(p, q) + kscal * Rho(r, s) * (qr|ps)
            ! Rho(r, s) = -Rho(s, r)
            !
            ! The code below employs the antisymmetry of K and computes the matrix element as
            !
            ! K(p, q) <- K(p, q) - kscal * Rho(r, s) * (pr|qs)
            !
            ! FLAGS controls which permutations of (pq|rs) contribute to K.
            !
            double precision, dimension(:), intent(inout) :: kmatrix
            integer, intent(in)                           :: flags
            integer, intent(in)                           :: aa, bb, cc, dd
            integer, intent(in)                           :: k1, k2, k3, k4
            double precision, intent(in)                  :: kscal
            double precision, dimension(:), intent(in)    :: rho
            real(F64), intent(in)                         :: Kappa

            integer :: w
            integer :: na, nb, nc, nd
            integer :: a, b, c, d
            integer :: exchbits
            integer :: nints
            double precision, dimension(max_nfunc**4) :: gabcd
            double precision, dimension(max_nfunc**2) :: scratch
            integer :: s0, s1

            a = sh(aa)
            b = sh(bb)
            c = sh(cc)
            d = sh(dd)
            na = SHPOS(aa+1) - SHPOS(aa)
            nb = SHPOS(bb+1) - SHPOS(bb)
            nc = SHPOS(cc+1) - SHPOS(cc)
            nd = SHPOS(dd+1) - SHPOS(dd)

            exchbits = iand(flags, EXCHSUM)
            !
            ! Exchange integrals
            !
            if (exchbits .gt. 0) then
                  nints = na * nb * nc * nd
                  !
                  ! NOTE THE INVERTED ORDER OF SHELLS: (BA|DC)
                  !
                  call AUTO2EERI(auto2e_idx(SHTYPE(b), SHTYPE(a), SHTYPE(d), SHTYPE(c)))%ptr(Gabcd, &
                        ATOMR(:, k2), CNTR(:, b), CNTRNORM(:, b), EXPN(:, b), NPRM(b), &
                        ATOMR(:, k1), CNTR(:, a), CNTRNORM(:, a), EXPN(:, a), NPRM(a), &
                        ATOMR(:, k4), CNTR(:, d), CNTRNORM(:, d), EXPN(:, d), NPRM(d), &
                        ATOMR(:, k3), CNTR(:, c), CNTRNORM(:, c), EXPN(:, c), NPRM(c), &
                        Kappa)
                  
                  !                  call ints2e(b, k2, a, k1, d, k4, c, k3, gabcd)

                  
                  do w = 1, nints
                        !
                        ! The minus sign is required so that K(p,q) adheres to the definition
                        ! K(p, q) <- K(p, q) + kscal * Rho(r, s) * (qr|ps)
                        !
                        gabcd(w) = -kscal * gabcd(w)
                  end do
                  !
                  ! Compute all contributions to the exchange matrix, which
                  ! originate from the computed batch of integrals (AB|CD).
                  ! Full permutational symmetry is used (8 contribs).
                  ! The flag EXCH_PERM_UVXY corresponds to +-Rho(v,y) (AB|CD).
                  !
                  ! Note that some contributions are added to K while others
                  ! are subtracted from K to account for the antisymmetry of Rho.
                  !
                  if (iand(exchbits, EXCH_PERM_ABCD) .gt. 0) then
                        !
                        ! K(a,c) = K(a,c) - kscal * Rho(d,b) * (ab|cd)
                        !
                        call digest_rhoac_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch_m(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if
                  
                  if (iand(exchbits, EXCH_PERM_ABDC) .gt. 0) then
                        !
                        ! K(a,d) = K(a,d) - kscal * Rho(c,b) * (ab|cd)
                        !
                        call digest_rhoad_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch_m(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if
                  
                  if (iand(exchbits, EXCH_PERM_BACD) .gt. 0) then
                        !
                        ! K(b,c) = K(b,c) - kscal * Rho(d,a) * (ab|cd)
                        !
                        call digest_rhobc_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch_m(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_BADC) .gt. 0) then
                        !
                        ! K(b,d) = K(b,d) - kscal * Rho(c,a) * (ab|cd)
                        !                       
                        call digest_rhobd_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch_m(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_CDAB) .gt. 0) then
                        !
                        ! K(c,a) = K(c,a) + kscal * Rho(d,b) * (ab|cd)
                        !
                        call digest_rhoca_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_CDBA) .gt. 0) then
                        !
                        ! K(c,b) = K(c,b) + kscal * Rho(d,a) * (ab|cd)
                        !
                        call digest_rhocb_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_DCAB) .gt. 0) then
                        !
                        ! K(d,a) = K(d,a) + kscal * Rho(c,b) * (ab|cd)
                        !
                        call digest_rhoda_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if

                  if (iand(exchbits, EXCH_PERM_DCBA) .gt. 0) then
                        !
                        ! K(d,b) = K(d,b) + kscal * Rho(c,a) * (ab|cd)
                        !
                        call digest_rhodb_tile(scratch, s0, s1, rho, &
                              bb, nb, aa, na, dd, nd, cc, nc, gabcd)
                        call offload_scratch(kmatrix(s0:s1), scratch, s1-s0+1)
                  end if
            end if
      end subroutine exchange_antisymm_tile


      subroutine offload_scratch(a, scratch, n)
            double precision, dimension(:), intent(inout) :: a
            double precision, dimension(:), intent(in)    :: scratch
            integer, intent(in)                           :: n
            
            integer :: k

            do k = 1, n
                  !$omp atomic
                  a(k) = a(k) + scratch(k)
            end do
      end subroutine offload_scratch


      subroutine offload_scratch_m(a, scratch, n)
            real(F64), dimension(:), intent(inout) :: a
            real(F64), dimension(:), intent(in)    :: scratch
            integer, intent(in)                    :: n
            
            integer :: k

            do k = 1, n
                  !$omp atomic
                  a(k) = a(k) - scratch(k)
            end do
      end subroutine offload_scratch_m


      subroutine jab_tile(t, rho, cd0, gabcd, nab, ncd, scalab)
            double precision, dimension(:), intent(out) :: t
            double precision, dimension(:), intent(in)  :: rho
            integer, intent(in)                         :: cd0
            double precision, dimension(:), intent(in)  :: gabcd
            integer, intent(in)                         :: nab, ncd
            double precision, intent(in)                :: scalab
            
            double precision :: w
            integer :: r, s, v

            v = 1
            do r = 1, nab
                  w = ZERO
                  do  s = 0, ncd-1
                        w = w + rho(cd0+s) * gabcd(v)
                        v = v + 1
                  end do
                  t(r) = scalab * w
            end do
      end subroutine jab_tile


      subroutine jcd_tile(t, rho, ab0, gabcd, nab, ncd, scalcd)
            double precision, dimension(:), intent(out) :: t
            double precision, dimension(:), intent(in)  :: rho
            integer, intent(in)                         :: ab0
            double precision, dimension(:), intent(in)  :: gabcd
            integer, intent(in)                         :: nab, ncd
            double precision, intent(in)                :: scalcd

            double precision rab
            integer :: r, s, v

            v = 1
            t(1:ncd) = ZERO
            do r = 0, nab-1
                  rab = rho(ab0+r) * scalcd
                  do s = 1, ncd
                        t(s) = t(s) + rab * gabcd(v)
                        v = v + 1
                  end do
            end do
      end subroutine jcd_tile


      subroutine batchfint_tile(kmatrix, jmatrix, k1, k2, kk3, kk4, n, rho, &
            rhomax, erimax, ierimax, rhoerimax, irhoerimax, &
            kscal, jscal, mask, antisymmetric, Kappa)
            !
            ! Determine which batches of integrals are needed to construct
            ! the Fock matrix (stage 1). Multiply the two-electron integrals
            ! by Rho (symmetric or antisymmetric) and update the Coulomb/exchange
            ! matrices (stage 2).
            ! 
            ! The list of atomic centers K1, K2, KK3, KK4 (canonical ordering assumed)
            ! is used to call the exchange and Coulomb screening subroutines for each
            ! permutation of centers. 
            !
            double precision, dimension(:), intent(inout)    :: kmatrix
            double precision, dimension(:), intent(inout)    :: jmatrix
            integer, intent(in)                              :: k1, k2
            integer, dimension(:), intent(in)                :: kk3, kk4
            integer, intent(in)                              :: n
            double precision, dimension(:), intent(in)       :: rho
            double precision, dimension(:, :), intent(in)    :: rhomax
            double precision, dimension(:, :), intent(in)    :: erimax
            integer, dimension(:, :), intent(in)             :: ierimax
            double precision, dimension(:, :), intent(in)    :: rhoerimax
            integer, dimension(:, :), intent(in)             :: irhoerimax
            double precision, intent(in)                     :: kscal, jscal
            integer, intent(in)                              :: mask
            logical, intent(in)                              :: antisymmetric
            real(F64), intent(in)                            :: Kappa

            double precision, dimension(0:1) :: dscal
            integer :: iscalab, iscalcd
            integer :: id, ida, idb
            integer :: k3, k4
            integer :: n1, n2, n3, n4
            integer :: m1, m2, m3, m4
            integer :: nshell1, nshell2, nshell3, nshell4
            integer :: i12, i34
            integer :: i, v, aa, bb, cc, dd
            integer :: flags
            integer :: nquart
            integer :: nprv, nqq
            integer, dimension(MAX_ATOMNSHELL**4) :: fintpermflag, fprv, qprv
            double precision, dimension(MAX_ATOMNSHELL) :: onxwork
            integer, dimension(MAX_ATOMNSHELL) :: ionxwork

            dscal(0) = jscal
            dscal(1) = two * jscal
            nqq = 0

            !$omp parallel default(shared) &
            !$omp private(i, k3, k4, i12, i34, id, ida, idb) &
            !$omp private(nshell1, nshell2, nshell3, nshell4) &
            !$omp private(nquart, n1, n2, n3, n4, m1, m2, m3, m4) &
            !$omp private(v, aa, bb, cc, dd) &
            !$omp private(iscalab, iscalcd) &
            !$omp private(flags, nprv, onxwork, ionxwork) &
            !$omp private(fintpermflag, fprv, qprv)

            !$omp do schedule(guided)
            do i = 1, n
                  k3 = kk3(i)
                  k4 = kk4(i)

                  nshell1 = sh0(k1 + 1) - sh0(k1)
                  nshell2 = sh0(k2 + 1) - sh0(k2)
                  nshell3 = sh0(k3 + 1) - sh0(k3)
                  nshell4 = sh0(k4 + 1) - sh0(k4)

                  n1 = nshell2 * nshell3 * nshell4
                  n2 = nshell3 * nshell4
                  n3 = nshell4
                  n4 = 1

                  nquart = nshell1 * n1

                  m1 = sh0(k1)
                  m2 = sh0(k2)
                  m3 = sh0(k3)
                  m4 = sh0(k4)

                  i12 = k1 * natom + k2
                  i34 = k3 * natom + k4

                  fintpermflag(1:nquart) = 0
                  !
                  ! Build a list of unique integrals contributing
                  ! to the HF exchange matrix. For each unique integral,
                  ! FINTPERMFLAG controls which permutations contribute
                  ! to the HF exchange/Coulomb matrix.
                  !
                  ! scalab = jscal
                  ! scalcd = jscal
                  !
                  iscalab = 0
                  iscalcd = 0
                  if (i12 .ne. i34) then
                        !
                        ! Exchange matrix
                        !
                        call onx(rhomax, erimax, ierimax, k1, k2, k3, k4, &
                              m1, m2, m3, m4, n1, n2, n3, n4, &
                              fintpermflag, EXCH_PERM_ABCD, onxwork, ionxwork)
                        call onx(rhomax, erimax, ierimax, k3, k4, k1, k2, &
                              m3, m4, m1, m2, n3, n4, n1, n2, &
                              fintpermflag, EXCH_PERM_CDAB, onxwork, ionxwork)
                        !
                        ! Coulomb matrix
                        !
                        call coulscreen(rhoerimax, irhoerimax, erimax, &
                              ierimax, k1, k2, k3, k4, m1, m2, m3, m4, &
                              n1, n2, n3, n4, fintpermflag, COUL_PERM_ABCD)
                        call coulscreen(rhoerimax, irhoerimax, erimax, &
                              ierimax, k3, k4, k1, k2, m3, m4, m1, m2, &
                              n3, n4, n1, n2, fintpermflag, COUL_PERM_CDAB)

                        if ((k1 .ne. k2) .and. (k3 .ne. k4)) then
                              !
                              ! Coulomb contribution scaling
                              !
                              ! scalab = jscal * two
                              ! scalcd = jscal * two    
                              !
                              iscalab = 1
                              iscalcd = 1
                              
                              call onx(rhomax, erimax, ierimax, k2, k1, k4, k3, &
                                    m2, m1, m4, m3, n2, n1, n4, n3, &
                                    fintpermflag, EXCH_PERM_BADC, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k4, k3, k2, k1, &
                                    m4, m3, m2, m1, n4, n3, n2, n1, &
                                    fintpermflag, EXCH_PERM_DCBA, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k1, k2, k4, k3, &
                                    m1, m2, m4, m3, n1, n2, n4, n3, &
                                    fintpermflag, EXCH_PERM_ABDC, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k4, k3, k1, k2, &
                                    m4, m3, m1, m2, n4, n3, n1, n2, &
                                    fintpermflag, EXCH_PERM_DCAB, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k2, k1, k3, k4, &
                                    m2, m1, m3, m4, n2, n1, n3, n4, &
                                    fintpermflag, EXCH_PERM_BACD, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k3, k4, k2, k1, &
                                    m3, m4, m2, m1, n3, n4, n2, n1, &
                                    fintpermflag, EXCH_PERM_CDBA, onxwork, ionxwork)
                        else if (k3 .ne. k4) then
                              !
                              ! scalab = jscal * two
                              !
                              iscalab = 1
                              call onx(rhomax, erimax, ierimax, k1, k2, k4, k3, &
                                    m1, m2, m4, m3, n1, n2, n4, n3, &
                                    fintpermflag, EXCH_PERM_ABDC, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k4, k3, k1, k2, &
                                    m4, m3, m1, m2, n4, n3, n1, n2, &
                                    fintpermflag, EXCH_PERM_DCAB, onxwork, ionxwork)
                        else if (k1 .ne. k2) then
                              !
                              ! scalcd = jscal * two
                              !
                              iscalcd = 1
                              call onx(rhomax, erimax, ierimax, k2, k1, k3, k4, &
                                    m2, m1, m3, m4, n2, n1, n3, n4, &
                                    fintpermflag, EXCH_PERM_BACD, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k3, k4, k2, k1, &
                                    m3, m4, m2, m1, n3, n4, n2, n1, &
                                    fintpermflag, EXCH_PERM_CDBA, onxwork, ionxwork)
                        end if
                  else 
                        !
                        ! Exchange matrix
                        !
                        call onx(rhomax, erimax, ierimax, k1, k2, k3, k4, &
                              m1, m2, m3, m4, n1, n2, n3, n4, &
                              fintpermflag, EXCH_PERM_ABCD, onxwork, ionxwork)
                        !
                        ! Coulomb matrix
                        !
                        call coulscreen(rhoerimax, irhoerimax, erimax, &
                              ierimax, k1, k2, k3, k4, m1, m2, m3, m4, &
                              n1, n2, n3, n4, fintpermflag, &
                              COUL_PERM_ABCD)

                        if ((k1 .ne. k2) .and. (k3 .ne. k4)) then
                              !
                              ! scalab = two * jscal
                              ! scalcd = two * jscal
                              !
                              iscalab = 1
                              iscalcd = 1

                              call onx(rhomax, erimax, ierimax, k2, k1, k4, k3, &
                                    m2, m1, m4, m3, n2, n1, n4, n3, &
                                    fintpermflag, EXCH_PERM_BADC, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k1, k2, k4, k3, &
                                    m1, m2, m4, m3, n1, n2, n4, n3, &
                                    fintpermflag, EXCH_PERM_ABDC, onxwork, ionxwork)
                              call onx(rhomax, erimax, ierimax, k2, k1, k3, k4, &
                                    m2, m1, m3, m4, n2, n1, n3, n4, &
                                    fintpermflag, EXCH_PERM_BACD, onxwork, ionxwork)
                        else if (k3 .ne. k4) then
                              !
                              ! scalab = two * jscal
                              !
                              iscalab = 1
                              call onx(rhomax, erimax, ierimax, k1, k2, k4, k3, &
                                    m1, m2, m4, m3, n1, n2, n4, n3, &
                                    fintpermflag, EXCH_PERM_ABDC, onxwork, ionxwork)
                        else if (k1 .ne. k2) then
                              !
                              ! scalcd = two * jscal
                              !
                              iscalcd = 1
                              call onx(rhomax, erimax, ierimax, k2, k1, k3, k4, &
                                    m2, m1, m3, m4, n2, n1, n3, n4, &
                                    fintpermflag, EXCH_PERM_BACD, onxwork, ionxwork)
                        end if
                  end if
                  !
                  ! For each integral in FINTPERMFLAG list find 
                  ! what permutations should be contributed
                  ! to the HF exchange matrix.
                  !
                  nprv = 0
                  v = 1
                  do aa = 1, nshell1
                        do bb = 1, nshell2
                              do cc = 1, nshell3
                                    do dd = 1, nshell4
                                          flags = iand(fintpermflag(v), mask)
                                          if (flags .gt. 0) then
                                                nprv = nprv + 1
                                                call flagpack(i, iscalab, iscalcd, flags, fprv(nprv))
                                                call ipack(aa, bb, cc, dd, qprv(nprv))
                                          end if
                                          v = v + 1
                                    end do
                              end do
                        end do
                  end do

                  if (nprv > 0) then
                        !$omp critical
                        ff(nqq+1:nqq+nprv) = fprv(1:nprv)
                        qq(nqq+1:nqq+nprv) = qprv(1:nprv)
                        nqq = nqq + nprv
                        !$omp end critical
                  end if
            end do
            !$omp end do

            if (antisymmetric) then
                  !$omp do schedule(guided)
                  do v = 1, nqq
                        !
                        ! Antisymmetric density matrix. This code used in the case of complex
                        ! density matrices, e.g., in real-time TDDFT propagation.
                        ! Only the exchange part is computed because the Coulomb part
                        ! is exactly zero when Rho is antisymmetric.
                        !
                        call iunpack(qq(v), aa, bb, cc, dd)
                        call flagunpack(ff(v), i, iscalab, iscalcd, flags)
                        k3 = kk3(i)
                        k4 = kk4(i)
                        aa = sh0(k1) + aa - 1
                        bb = sh0(k2) + bb - 1
                        cc = sh0(k3) + cc - 1
                        dd = sh0(k4) + dd - 1
                        call exchange_antisymm_tile(kmatrix, flags, aa, bb, cc, dd, &
                              k1, k2, k3, k4, kscal, rho, Kappa)
                  end do
                  !$omp end do nowait
            else
                  !$omp do schedule(guided)
                  do v = 1, nqq
                        !
                        ! Symmetric density matrix
                        !
                        call iunpack(qq(v), aa, bb, cc, dd)
                        call flagunpack(ff(v), i, iscalab, iscalcd, flags)
                        k3 = kk3(i)
                        k4 = kk4(i)
                        aa = sh0(k1) + aa - 1
                        bb = sh0(k2) + bb - 1
                        cc = sh0(k3) + cc - 1
                        dd = sh0(k4) + dd - 1
                        call fockcontrib_tile(kmatrix, jmatrix, flags, aa, bb, cc, dd, &
                              k1, k2, k3, k4, kscal, dscal(iscalab), dscal(iscalcd), rho, Kappa)
                  end do
                  !$omp end do nowait
            end if
            !$omp end parallel
      end subroutine batchfint_tile


      subroutine fock2eldrv_tile(kmatrix, jmatrix, rho, rhomax, erimax, ierimax, &
            rhoerimax, irhoerimax, kscal, jscal, mask, antisymmetric, Kappa, brapair)
            !
            ! Integral-direct algorithm  for HF exchange / Coulomb matrices.
            ! The base for the implemented algorithm is ONX (linear scaling
            ! for large systems), but it is modified to use the full permutational
            ! symmetry of electron repulsion integrals.
            !
            double precision, dimension(:, :), intent(inout) :: kmatrix
            double precision, dimension(:, :), intent(inout) :: jmatrix
            double precision, dimension(:, :), intent(in)    :: rho
            double precision, dimension(:, :), intent(in)    :: rhomax
            double precision, dimension(:, :), intent(in)    :: erimax
            integer, dimension(:, :), intent(in)             :: ierimax
            double precision, dimension(:, :), intent(in)    :: rhoerimax
            integer, dimension(:, :), intent(in)             :: irhoerimax
            double precision, intent(in)                     :: kscal, jscal
            integer, intent(in)                              :: mask
            logical, intent(in)                              :: antisymmetric
            real(F64), intent(in)                            :: Kappa
            integer, dimension(2), optional, intent(in)      :: brapair

            integer :: k1, k2, k3, k4
            integer :: k4_max, n

            call matrix2tile(RHO_TILE, rho)
            KMATRIX_TILE = ZERO
            JMATRIX_TILE = ZERO

            if (present(brapair)) then
                  n = 0
                  k1 = brapair(1)
                  k2 = brapair(2)
                  do k3 = 1, k1
                        if (k3 .eq. k1) then
                              k4_max = k2
                        else
                              k4_max = k3
                        end if

                        do k4 = 1, k4_max
                              n = n + 1
                              kk3(n) = k3
                              kk4(n) = k4

                              if (n .eq. MAX_NBATCH) then
                                    call batchfint_tile(kmatrix_tile, jmatrix_tile, k1, k2, kk3, kk4, n, RHO_TILE, &
                                          rhomax, erimax, ierimax, rhoerimax, &
                                          irhoerimax, kscal, jscal, mask, antisymmetric, Kappa)
                                    n = 0
                              end if
                        end do
                  end do
                  
                  if (n .gt. 0) then
                        call batchfint_tile(kmatrix_tile, jmatrix_tile, k1, k2, kk3, kk4, n, RHO_TILE, &
                              rhomax, erimax, ierimax, rhoerimax, &
                              irhoerimax, kscal, jscal, mask, antisymmetric, Kappa)
                  end if
            else
                  do k1 = 1, NATOM
                        do k2 = 1, k1
                              n = 0
                              do k3 = 1, k1
                                    if (k3 .eq. k1) then
                                          k4_max = k2
                                    else
                                          k4_max = k3
                                    end if

                                    do k4 = 1, k4_max
                                          n = n + 1
                                          kk3(n) = k3
                                          kk4(n) = k4

                                          if (n .eq. MAX_NBATCH) then
                                                call batchfint_tile(kmatrix_tile, jmatrix_tile, &
                                                      k1, k2, kk3, kk4, n, RHO_TILE, &
                                                      rhomax, erimax, ierimax, rhoerimax, &
                                                      irhoerimax, kscal, jscal, mask, antisymmetric, Kappa)
                                                n = 0
                                          end if
                                    end do
                              end do
                              if (n .gt. 0) then
                                    call batchfint_tile(kmatrix_tile, jmatrix_tile, k1, k2, kk3, kk4, n, &
                                          RHO_TILE, rhomax, erimax, ierimax, &
                                          rhoerimax, irhoerimax, kscal, jscal, mask, antisymmetric, Kappa)
                              end if
                        end do
                  end do
            end if

            if (SEPKSCONTRIB) then
                  call tile2matrix(kmatrix, kmatrix_tile)
                  call tile2matrix(jmatrix, jmatrix_tile)
            else
                  call tile2matrix(kmatrix, kmatrix_tile)
                  call tile2matrix(kmatrix, jmatrix_tile)
            end if
      end subroutine fock2eldrv_tile
end module fock2el
