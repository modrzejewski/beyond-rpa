module density_matrix_ground

      use cc3_intermediates
      use s_gen
      use basis
      use symmetry
      use linalg
      use scfutils

      implicit none

      integer, private :: dm_nocc0, dm_nvirt0, dm_nocc1, dm_nvirt1, dm_nocc, dm_nvirt, dm_npair
      integer, private :: qbj, qbj2
      integer, private :: qck, qck2
      integer, private :: q00
      integer, private :: nidx_ccsd


contains

      subroutine density_ground_init(nc0, nc1, nv0, nv1, nc, nv, dmmo)
            integer, intent(in) :: nc0, nc1
            integer, intent(in) :: nv0, nv1
            integer, intent(in) :: nc, nv
            double precision, dimension(:,:), intent(out) :: dmmo

            dm_nocc = nc
            dm_nvirt = nv
            dm_npair = dm_nocc * dm_nvirt
            dm_nocc0 = nc0
            dm_nocc1 = nc1
            dm_nvirt0 = nv0
            dm_nvirt1 = nv1
            nidx_ccsd = dm_npair + (dm_npair * (dm_npair + 1)) / 2 

            dmmo = ZERO

      end subroutine density_ground_init

      subroutine density_free()

      end subroutine density_free

      subroutine dmmo_to_dmao(dmao, dmmo, mocoeff, nactive)
            real(F64), dimension(:,:), intent(in)  :: mocoeff
            real(F64), dimension(:,:), intent(in)  :: dmmo
            real(F64), dimension(:,:), intent(out) :: dmao
            integer, intent(in) :: nactive
            integer :: k, l, p, q, i
            real(F64) :: sum
            real(F64), dimension(:), allocatable :: w
            real(F64), dimension(:, :), allocatable :: dmaocopy

            allocate(w(nactive))
            allocate(dmaocopy(nactive, nactive))

            dmao = zero

            do k = 1, CC_NORB
                  do l = 1, CC_NORB
                        sum = zero
                        do p = 1, nactive
                              do q = 1, nactive
                                    sum = sum + mocoeff(k, p) * dmmo(p, q) * mocoeff(l, q)
                              end do
                        end do
                        dmao(k, l) = sum
                  end do
            end do

            dmaocopy = dmmo
            call evd_cc(dmaocopy, w, nactive)

      end subroutine dmmo_to_dmao
      
      subroutine density_check(dmao, overlap, dens)
            real(F64), dimension(:,:), intent(in) :: dmao
            real(F64), dimension(:,:), intent(in) :: overlap
            real(F64), intent(out) :: dens
            real(F64) :: dens2, sum

            integer :: k, l, i

            dens = zero

            do k = 1, CC_NORB
                  do l = 1, CC_NORB
                        dens = dens + overlap(l, k) * dmao(k, l)
                  end do
            end do
            
            print*, ''
            print*, 'gestosc', dens
            print*, ''
            dens2 = zero
            do k = 1, CC_NORB
                  dens2 = dens2 + overlap(k, k) * dmao(k, k)
                  sum = zero
                  do l = 1, k-1
                        sum = sum + overlap(l, k) * dmao(l, k)
                  end do
                  dens2 = dens2 + 2.d+0*sum
            end do
            print*, 'dens2', dens2

            
      end subroutine density_check

      subroutine dmao_to_dmao_sym(dmao, dmao_sym)

            real(F64), dimension(:, :), intent(in) :: dmao
            real(F64), dimension(:, :), intent(out) :: dmao_sym
            integer :: n
            integer :: i, j

            n = size(dmao, 1)

            dmao_sym = zero
            do i = 1, n
                  do j = 1, n

                        dmao_sym(i, j) = dmao(i, j) + dmao(j, i)
                  end do
            end do

            dmao_sym = 0.5d+0 * dmao_sym 
            
      end subroutine dmao_to_dmao_sym

      subroutine dipmom(dmao, overlap, dip)
            double precision, dimension(:,:), intent(in) :: dmao
            real(F64), dimension(:,:), intent(in) :: overlap
            real(F64), dimension(:), intent(out) :: dip
            double precision, dimension(:, :), allocatable :: dipxao, dipyao, dipzao
            real(F64) :: diplen
            integer :: i, j
            integer :: nft2, l1, l2

            allocate(dipxao(CC_NORB, CC_NORB))
            allocate(dipyao(CC_NORB, CC_NORB))
            allocate(dipzao(CC_NORB, CC_NORB))
            !
            ! Matrices of the electronic dipole moment operator
            ! (AO basis)
            !
            if (SLATER_BASIS) then
                  l1 = CC_NORB
                  l2 = l1 * (l1 + 1) / 2
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 4, dipxao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 5, dipyao)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 6, dipzao)
                  close(nft2)
            else
                  call dipole(dipxao, dipyao, dipzao, origin)
            end if

            call dipolevec(dip, dipxao, dipyao, dipzao, dmao)

            diplen = sqrt(dip(1)**2 + dip(2)**2 + dip(3)**2)

            call msg("DIPOLE MOMENT [A.U.]", underline=.true.)
            call dmsg("LENGTH", diplen)
            call dmsg("X", dip(1))
            call dmsg("Y", dip(2))
            call dmsg("Z", dip(3))
            call blankline()

            call msg("DIPOLE MOMENT [DEBYE]", underline=.true.)
            call dmsg("LENGTH", todebye(diplen))
            call dmsg("X", todebye(dip(1)))
            call dmsg("Y", todebye(dip(2)))
            call dmsg("Z", todebye(dip(3)))

            call blankline()

      end subroutine dipmom

      subroutine quadru(dmao, quad)
            double precision, dimension(:,:), intent(in) :: dmao
            real(F64), dimension(:), intent(out)         :: quad
            double precision, dimension(:, :), allocatable :: quadxxao, quadyyao, quadzzao
            double precision, dimension(:, :), allocatable :: quadyxao, quadzxao, quadzyao

            allocate(quadxxao(CC_NORB, CC_NORB))
            allocate(quadyyao(CC_NORB, CC_NORB))
            allocate(quadzzao(CC_NORB, CC_NORB))
            allocate(quadyxao(CC_NORB, CC_NORB))
            allocate(quadzxao(CC_NORB, CC_NORB))
            allocate(quadzyao(CC_NORB, CC_NORB))

             call quadrupole(quadxxao, quadyyao, quadzzao, quadyxao, quadzxao, quadzyao, origin, &
                   QUAD_TRACELESS_BUCKINGHAM)

            call quadrupolevec(quad, quadxxao, quadyyao, quadzzao, quadyxao, quadzxao, quadzyao, dmao)
                        
            call msg("TRACELESS QUADRUPOLE MOMENT [A.U.]", underline=.true.)
            call dmsg("XX", quad(1))
            call dmsg("YY", quad(2))
            call dmsg("ZZ", quad(3))
            call dmsg("XY", quad(4))
            call dmsg("XZ", quad(5))
            call dmsg("YZ", quad(6))

            call blankline()

            call msg("TRACELESS QUADRUPOLE MOMENT [BUCKINGHAM = DEBYE * ANGS]", underline=.true.)
            call dmsg("XX", toang(todebye((quad(1)))))
            call dmsg("YY", toang(todebye((quad(2)))))
            call dmsg("ZZ", toang(todebye((quad(3)))))
            call dmsg("XY", toang(todebye((quad(4)))))
            call dmsg("XZ", toang(todebye((quad(5)))))
            call dmsg("YZ", toang(todebye((quad(6)))))
            call blankline()

      end subroutine quadru


      function oneelprop(dm_nocc0, dm_nocc1, dm_nvirt0, dm_nvirt1, x, &
            dm_nocc, dm_nvirt, dm)
            ! ---------------------------------------------------------------------
            ! ONEELPROP    - Output, requested one electron propertie
            ! N       - Number of requested transition moments
            ! DM_NOCC0   - First occupied orbital
            ! DM_NOCC1   - Last occupied orbital
            ! DM_NVIRT0  - First virtual orbital
            ! DM_NVIRT1  - Last virtual orbital
            ! X       - Matrix of one electron operator
            ! DMAO    - Density matrix in AO basis
            !
            real(F64)  :: oneelprop
            integer, intent(in)     :: dm_nocc0, dm_nocc1
            integer, intent(in)     :: dm_nvirt0, dm_nvirt1
            integer, intent(in)     :: dm_nocc, dm_nvirt
            double precision, dimension(:, :), intent(in)       :: x
            double precision, dimension(:,:), intent(in)      :: dm
            integer :: i, j, a, b, k
            integer :: ij, ia, ai, ab

            oneelprop = zero

            !
            ! Occupied-occupied block
            !
            do j = dm_nocc0, dm_nocc1
                  do i = dm_nocc0, dm_nocc1
                        ij = (j - dm_nocc0) * dm_nocc + (i - dm_nocc0) + 1
                        oneelprop = oneelprop + x(j, i) * dm(i, j)
                  end do
            end do
            !
            ! Occupied-virtual block
            !
            k = dm_nocc*dm_nocc
            do i = dm_nocc0, dm_nocc1
                  do a = dm_nvirt0, dm_nvirt1
                        ai = (i - dm_nocc0) * dm_nvirt + (a - dm_nvirt0) + 1 + k
                        oneelprop = oneelprop + x(i, a) * dm(a, i)
                  end do
            end do
            !
            ! Virtual-occupied block
            !
            k = k + dm_nvirt * dm_nocc 
            do i = dm_nocc0, dm_nocc1            
                  do a = dm_nvirt0, dm_nvirt1
                        ia = (a - dm_nvirt0) * dm_nocc + (i - dm_nocc0) + 1 + k
                        oneelprop = oneelprop + x(a, i) * dm(i, a)
                  end do
            end do
            !
            ! Virtual-virtual block
            !
            k = k + dm_nvirt * dm_nocc 
            do b = dm_nvirt0, dm_nvirt1
                  do a = dm_nvirt0, dm_nvirt1
                        ab = (b - dm_nvirt0) * dm_nvirt + (a - dm_nvirt0) + 1 + k
                        oneelprop = oneelprop + x(b, a) * dm(a, b)
                  end do
            end do

      end function oneelprop


      subroutine generate_density_matrix_ground(t2, t1, s2, s1,  &
            dm_nocc, nactive, method, dmmo, irrep_idx, irrep0, irrep1, order, mbpt)
            integer, intent(in)                                                              :: dm_nocc, nactive
            double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
            double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
            double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
            double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1
            integer, intent(in)                                 :: method
            double precision, dimension(:,:), intent(inout)  :: dmmo
            integer, intent(in) :: irrep_idx
            integer, dimension(:, :), intent(in) :: irrep0
            integer, dimension(:, :), intent(in) :: irrep1
            integer, intent(in) :: order
            integer, intent(in) :: mbpt
            integer, dimension(:, :), allocatable :: isingles_ij
            integer, dimension(:, :), allocatable :: isingles_ai
            integer, dimension(:, :), allocatable :: isingles_ia
            integer, dimension(:, :), allocatable :: isingles_ab
            integer, dimension(:, :), allocatable :: isingles
            integer :: idims_ij, idims_ai, idims_ia, idims_ab

            type(tclock) :: time
            integer      :: ijbig, aibig, iabig, abbig
            integer      :: m0i, m1i, m0j, m1j
            integer      :: m0a, m1a, m0b, m1b
            integer      :: ntasks
            integer      :: pa, pb, pi, pj
            integer      :: into

            integer :: nthreads
            integer :: max_ntasks

            nthreads = OMP_NTHREAD
            max_ntasks = 100 * nthreads
            allocate(isingles(max_ntasks, 2))

            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ij, POINT_GROUP, idims_ij, .true., 1, 1)
            allocate(isingles_ij(2, idims_ij))
            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ij, POINT_GROUP, idims_ij, .false., 1, 1)

            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ai, POINT_GROUP, idims_ai, .true., 2, 1)
            allocate(isingles_ai(2, idims_ai))
            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ai, POINT_GROUP, idims_ai, .false., 2, 1)

            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ia, POINT_GROUP, idims_ia, .true., 1, 2)
            allocate(isingles_ia(2, idims_ia))
            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ia, POINT_GROUP, idims_ia, .false., 1, 2)

            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ab, POINT_GROUP, idims_ab, .true., 2, 2)
            allocate(isingles_ab(2, idims_ab))
            call irrep_singless_pq(irrep_idx, irrep0, irrep1, isingles_ab, POINT_GROUP, idims_ab, .false., 2, 2)

            call clock_start(time)
            ntasks = 0
            print*, 'LOOP OVER occ occ...'
            do ijbig = 1, idims_ij
                  call loop_boundaries_sp_qq(isingles_ij(1:2, ijbig), irrep0, irrep1, &
                        m0i, m1i, m0j, m1j, 1, 1)

                  do pi = m0i, m1i
                        do pj = m0j, m1j

                              ntasks = ntasks + 1

                              isingles(ntasks, 1) = pi
                              isingles(ntasks, 2) = pj

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dmmo_ij(method, dmmo, &
                                          isingles, dm_nocc,   &
                                          t1, t2, s1, s2, ntasks, nactive, mbpt)
                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then
                  call dotasks_dmmo_ij(method, dmmo, &
                        isingles, dm_nocc,   &
                        t1, t2, s1, s2, ntasks, nactive, mbpt)                        
            end if

            call dmsg("TOTAL TIME IJBIG", clock_readwall(time))

            print*, 'LOOP OVER virt occ...'
            call clock_start(time)
            ntasks = 0
            do aibig = 1, idims_ai
                  call loop_boundaries_sp_qq(isingles_ai(1:2, aibig), irrep0, irrep1, &
                        m0a, m1a, m0i, m1i, 2, 1)


                  do pa = m0a, m1a
                        do pi = m0i, m1i

                              ntasks = ntasks + 1

                              isingles(ntasks, 1) = pa
                              isingles(ntasks, 2) = pi

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dmmo_ai(method, dmmo, &
                                          isingles, dm_nocc,   &
                                          t1, t2, s1, s2, ntasks, nactive, mbpt)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then
                  call dotasks_dmmo_ai(method, dmmo, &
                        isingles, dm_nocc,   &
                        t1, t2, s1, s2, ntasks, nactive, mbpt)
            end if
            call dmsg("TOTAL TIME AIBIG", clock_readwall(time))

            print*, 'LOOP OVER occ virt...'
            call clock_start(time)
            ntasks = 0
            do iabig = 1, idims_ia
                  call loop_boundaries_sp_qq(isingles_ia(1:2, iabig), irrep0, irrep1, &
                        m0i, m1i, m0a, m1a, 1, 2)

                  do pa = m0a, m1a
                        do pi = m0i, m1i

                              ntasks = ntasks + 1

                              isingles(ntasks, 1) = pi
                              isingles(ntasks, 2) = pa

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dmmo_ia(method, dmmo, &
                                          isingles, dm_nocc,   &
                                          t1, t2, s1, s2, ntasks, nactive, mbpt)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  call dotasks_dmmo_ia(method, dmmo, &
                        isingles, dm_nocc,   &
                        t1, t2, s1, s2, ntasks, nactive, mbpt)

            end if
            call dmsg("TOTAL TIME IABIG", clock_readwall(time))                  

            print*, 'LOOP OVER virt virt...'
            call clock_start(time)
            ntasks = 0                              
            do abbig = 1, idims_ab
                  call loop_boundaries_sp_qq(isingles_ab(1:2, abbig), irrep0, irrep1, &
                        m0a, m1a, m0b, m1b, 2, 2)

                  do pa = m0a, m1a
                        do pb = m0b, m1b

                              ntasks = ntasks + 1
                              isingles(ntasks, 1) = pa
                              isingles(ntasks, 2) = pb

                              if (ntasks == max_ntasks)  then

                                    call dotasks_dmmo_ab(method, dmmo, &
                                          isingles, dm_nocc,   &
                                          t1, t2, s1, s2, ntasks, nactive, mbpt)

                                    ntasks = 0
                              end if
                        end do
                  end do
            end do

            if (ntasks .gt. 0)  then

                  call dotasks_dmmo_ab(method, dmmo, &
                        isingles, dm_nocc,   &
                        t1, t2, s1, s2, ntasks, nactive, mbpt)
                  
            end if
            call dmsg("TOTAL TIME ABBIG", clock_readwall(time))
            
            
      end subroutine generate_density_matrix_ground
      
      subroutine dotasks_dmmo_ij(theory, dmmo, isingles, dm_nocc,   &
            t1, t2, s1, s2, ntasks, nactive, mbpt)
            
            integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dmmo
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: dm_nocc
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: mbpt
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, j


           if (theory .eq. THEORY_CC3) then

                 !$omp parallel private(k, i, j) default(shared)                                                                                        
                 !$omp do schedule(dynamic)                                                                                                           
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dmmo(i, j)  = dmmo(i, j) + calc_D_oo_ground_mbpt_cc3(t2, t1, s2, s1, &
                             dm_nocc, nactive, i, j, mbpt)
                       if(i==j)then
                             dmmo(i, j) = dmmo(i, j) + calc_D_oo_diag_ground_mbpt_cc3(t2, t1, s2, s1, &
                                   dm_nocc, nactive, i, mbpt)
                       end if
                 end do
                 !$omp end do                                                                                                                             
                 !$omp end parallel   
           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, i, j) default(shared)                                                                         
                 !$omp do schedule(dynamic)                                                                                                                 
                 do k = 1, ntasks                                                                                                                         
                       i = isingles(k, 1)
                       j = isingles(k, 2)
                       dmmo(i, j)  = dmmo(i, j) + calc_D_oo_ground_mbpt(t2, t1, s2, s1, &
                             dm_nocc, nactive, i, j, mbpt)
                       if(i==j)then
                             dmmo(i, j)  = dmmo(i, j) + calc_D_oo_diag_ground_mbpt(t2, t1, s2, s1, &
                                   dm_nocc, nactive, i, mbpt)
                       end if

                 end do
                 !$omp end do                                                                                                           
                 !$omp end parallel 
           end if

           ! if (mbpt == 3) then
           
           !       if (theory .eq. THEORY_CC3) then

           !             !$omp parallel private(k, i, j) default(shared)                                                                                        
           !             !$omp do schedule(dynamic)                                                                                                           
           !             do k = 1, ntasks
           !                   i = isingles(k, 1)
           !                   j = isingles(k, 2)
           !                   dmmo(i, j)  = calc_D_oo_ground_mbpt3_cc3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, i, j)
           !                   if(i==j)then
           !                         dmmo(i, j) = dmmo(i, j) + calc_D_oo_diag_ground_mbpt3_cc3(t2, t1, s2, s1, &
           !                               dm_nocc, nactive, i)
           !                   end if
           !             end do
           !             !$omp end do                                                                                                                             
           !             !$omp end parallel   
           !       else if (theory .eq. THEORY_CCSD) then

           !             !$omp parallel private(k, i, j) default(shared)                                                                         
           !             !$omp do schedule(dynamic)                                                                                                                 
           !             do k = 1, ntasks                                                                                                                         
           !                   i = isingles(k, 1)
           !                   j = isingles(k, 2)
           !                   dmmo(i, j)  = dmmo(i, j) + calc_D_oo_ground_mbpt3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, i, j)
           !                   if(i==j)then
           !                         dmmo(i, j)  = dmmo(i, j) + calc_D_oo_diag_ground_mbpt3(t2, t1, s2, s1, &
           !                               dm_nocc, nactive, i)
           !                   end if

           !             end do
           !             !$omp end do                                                                                                           
           !             !$omp end parallel 
           !       end if

           ! else if (mbpt == 4) then 
           
           !       if (theory .eq. THEORY_CC3) then

           !             !$omp parallel private(k, i, j) default(shared)                                                                                        
           !             !$omp do schedule(dynamic)                                                                                                           
           !             do k = 1, ntasks
           !                   i = isingles(k, 1)
           !                   j = isingles(k, 2)
           !                   dmmo(i, j)  = calc_D_oo_ground_mbpt4_cc3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, i, j)
           !                   if(i==j)then
           !                         dmmo(i, j)  = dmmo(i, j) + calc_D_oo_diag_ground_mbpt4_cc3(t2, t1, s2, s1, &
           !                               dm_nocc, nactive, i)
           !                   end if
           !             end do
           !             !$omp end do                                                                                                                             
           !             !$omp end parallel   
           !       else if (theory .eq. THEORY_CCSD) then

           !             !$omp parallel private(k, i, j) default(shared)                                                                         
           !             !$omp do schedule(dynamic)                                                                                                                 
           !             do k = 1, ntasks                                                                                                                         
           !                   i = isingles(k, 1)
           !                   j = isingles(k, 2)
           !                   dmmo(i, j)  = dmmo(i, j) + calc_D_oo_ground_mbpt4(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, i, j)
           !                   if(i==j)then
           !                         dmmo(i, j)  = dmmo(i, j) + calc_D_oo_diag_ground_mbpt4(t2, t1, s2, s1, &
           !                               dm_nocc, nactive, i)
           !                   end if

           !             end do
           !             !$omp end do                                                                                                                                    
           !             !$omp end parallel 
           !       end if
           ! end if

     end subroutine dotasks_dmmo_ij

     subroutine dotasks_dmmo_ai(theory, dmmo, isingles, dm_nocc,   &
            t1, t2, s1, s2, ntasks, nactive, mbpt)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dmmo
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: dm_nocc
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: mbpt
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: a, i
              
           if (theory .eq. THEORY_CC3) then

                 !$omp parallel private(k, a, i) default(shared)                                                                                             
                 !$omp do schedule(dynamic)                                                                                                                   
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       i = isingles(k, 2)
                       dmmo(a, i)  = dmmo(a, i) + calc_D_vo_ground_mbpt_cc3(t2, t1, s2, s1, &
                             dm_nocc, nactive, a, i, mbpt)
                 end do
                 !$omp end do                                                                                                                                 
                 !$omp end parallel                                                                                                                              
           else if (theory .eq. THEORY_CCSD) then

                 !$omp parallel private(k, a, i) default(shared)                                                                                             
                 !$omp do schedule(dynamic)                                                                                                                    
                 do k = 1, ntasks
                       a = isingles(k, 1)                                                                                                                     
                       i = isingles(k, 2) 
                       dmmo(a, i)  = dmmo(a, i) + calc_D_vo_ground_mbpt(t2, t1, s2, s1, &
                             dm_nocc, nactive, a, i, mbpt)      
                 end do
                 !$omp end do                                                                                                                             
                 !$omp end parallel 
           end if


           ! if (mbpt == 3)then

           !       if (theory .eq. THEORY_CC3) then

           !             !$omp parallel private(k, a, i) default(shared)                                                                                             
           !             !$omp do schedule(dynamic)                                                                                                                   
           !             do k = 1, ntasks
           !                   a = isingles(k, 1)
           !                   i = isingles(k, 2)
           !                   dmmo(a, i)  = calc_D_vo_ground_mbpt3_cc3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, a, i)
           !             end do
           !             !$omp end do                                                                                                                                 
           !             !$omp end parallel                                                                                                                              
           !       else if (theory .eq. THEORY_CCSD) then

           !             !$omp parallel private(k, a, i) default(shared)                                                                                             
           !             !$omp do schedule(dynamic)                                                                                                                    
           !             do k = 1, ntasks
           !                   a = isingles(k, 1)                                                                                                                     
           !                   i = isingles(k, 2) 
           !                   dmmo(a, i)  = calc_D_vo_ground_mbpt3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, a, i)      
           !             end do
           !             !$omp end do                                                                                                                             
           !             !$omp end parallel 
           !       end if

           ! else if(mbpt == 4) then

           !       if (theory .eq. THEORY_CC3) then
           !             !$omp parallel private(k, a, i) default(shared)                                                                                             
           !             !$omp do schedule(dynamic)                                                                                                                   
           !             do k = 1, ntasks
           !                   a = isingles(k, 1)
           !                   i = isingles(k, 2)
           !                   dmmo(a, i)  = calc_D_vo_ground_mbpt4_cc3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, a, i)
           !             end do
           !             !$omp end do                                                                                                                                 
           !             !$omp end parallel                                                                                                                              
           !       else if (theory .eq. THEORY_CCSD) then
                       
           !             !$omp parallel private(k, a, i) default(shared)                                                                                             
           !             !$omp do schedule(dynamic)                                                                                                                    
           !             do k = 1, ntasks
           !                   a = isingles(k, 1)                                                                                                                     
           !                   i = isingles(k, 2) 
           !                   dmmo(a, i)  = calc_D_vo_ground_mbpt4(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, a, i)      
           !             end do
           !             !$omp end do                                                                                                                             
           !             !$omp end parallel 
           !       end if
           ! end if

     end subroutine dotasks_dmmo_ai

     subroutine dotasks_dmmo_ia(theory, dmmo, isingles, dm_nocc,   &
            t1, t2, s1, s2, ntasks, nactive, mbpt)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dmmo
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: dm_nocc
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: mbpt
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: i, a
           
           if (theory .eq. THEORY_CC3) then
                 !$omp parallel private(k, i, a) default(shared)                                                                                       
                 !$omp do schedule(dynamic)                                                                                                                 
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dmmo(i, a)  = dmmo(i, a) + calc_D_ov_ground_mbpt_cc3(t2, t1, s2, s1, &
                             dm_nocc, nactive, i, a, mbpt)
                 end do
                 !$omp end do                                                                                                                             
                 !$omp end parallel                                                                                                                         
           else if (theory .eq. THEORY_CCSD) then
                 !$omp parallel private(k, i, a) default(shared)                                                                                            
                 !$omp do schedule(dynamic)                                                                                                                 
                 do k = 1, ntasks
                       i = isingles(k, 1)
                       a = isingles(k, 2)
                       dmmo(i, a)  = dmmo(i, a) + calc_D_ov_ground_mbpt(t2, t1, s2, s1, &
                             dm_nocc, nactive, i, a, mbpt)
                 end do
                 !$omp end do                                                                                                                                   
                 !$omp end parallel                                                                                                                          
           end if

           ! if (mbpt == 3)then
           !       if (theory .eq. THEORY_CC3) then
           !             !$omp parallel private(k, i, a) default(shared)                                                                                       
           !             !$omp do schedule(dynamic)                                                                                                                 
           !             do k = 1, ntasks
           !                   i = isingles(k, 1)
           !                   a = isingles(k, 2)
           !                   dmmo(i, a)  = calc_D_ov_ground_mbpt3_cc3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, i, a)
           !             end do
           !             !$omp end do                                                                                                                             
           !             !$omp end parallel                                                                                                                         
           !       else if (theory .eq. THEORY_CCSD) then
           !             !$omp parallel private(k, i, a) default(shared)                                                                                            
           !             !$omp do schedule(dynamic)                                                                                                                 
           !             do k = 1, ntasks
           !                   i = isingles(k, 1)
           !                   a = isingles(k, 2)
           !                   dmmo(i, a)  = calc_D_ov_ground_mbpt3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, i, a)
           !             end do
           !             !$omp end do                                                                                                                                   
           !             !$omp end parallel                                                                                                                          
           !       end if

           ! else if (mbpt == 4) then
                
           !       if (theory .eq. THEORY_CC3) then
           !             !$omp parallel private(k, i, a) default(shared)                                                                                       
           !             !$omp do schedule(dynamic)                                                                                                                 
           !             do k = 1, ntasks
           !                   i = isingles(k, 1)
           !                   a = isingles(k, 2)
           !                   dmmo(i, a)  = calc_D_ov_ground_mbpt4_cc3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, i, a)
           !             end do
           !             !$omp end do                                                                                                                             
           !             !$omp end parallel                                                                                                                         
           !       else if (theory .eq. THEORY_CCSD) then
           !             !$omp parallel private(k, i, a) default(shared)                                                                                            
           !             !$omp do schedule(dynamic)                                                                                                                 
           !             do k = 1, ntasks
           !                   i = isingles(k, 1)
           !                   a = isingles(k, 2)
           !                   dmmo(i, a)  = calc_D_ov_ground_mbpt4(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, i, a)
           !             end do
           !             !$omp end do                                                                                                                                   
           !             !$omp end parallel                                                                                                                          
           !       end if
           ! end if
     end subroutine dotasks_dmmo_ia

 
     subroutine dotasks_dmmo_ab(theory, dmmo, isingles, dm_nocc,   &
           t1, t2, s1, s2, ntasks, nactive, mbpt)

           integer, intent(in)                   :: theory
           real(F64), dimension(:,:)             :: dmmo
           integer, dimension(:,:), intent(in)   :: isingles
           integer, intent(in)                   :: dm_nocc
           double precision, dimension(:, :, :, :), intent(in) :: t2 
           double precision, dimension(:, :, :, :), intent(in) :: s2 
           double precision, dimension(:, :), intent(in)                  :: t1 
           double precision, dimension(:, :), intent(in)                  :: s1
           integer, intent(in)                   :: mbpt
           integer, intent(in)                   :: ntasks
           integer, intent(in)                   :: nactive
           integer :: k
           integer :: a, b

           if (theory .eq. THEORY_CC3) then
                 !$omp parallel private(k, a, b) default(shared)                                                                                                 
                 !$omp do schedule(dynamic)                                                                                                                         
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       b = isingles(k, 2)
                       dmmo(a, b) = dmmo(a, b) + calc_D_vv_ground_mbpt_cc3(t2, t1, s2, s1, &
                             dm_nocc, nactive, a, b, mbpt)
                 end do
                 !$omp end do                                                                                                                               
                 !$omp end parallel   
           else if (theory .eq. THEORY_CCSD) then
                 !$omp parallel private(k, a, b) default(shared)                                                                                      
                 !$omp do schedule(dynamic)                                                                                                          
                 do k = 1, ntasks
                       a = isingles(k, 1)
                       b = isingles(k, 2)
                       dmmo(a, b) = dmmo(a, b) + calc_D_vv_ground_mbpt(t2, t1, s2, s1, &
                             dm_nocc, nactive, a, b, mbpt)
                 end do
                 !$omp end do                                                                                                               
                 !$omp end parallel 
           end if
           
           ! if (mbpt == 3) then
                 
           !       if (theory .eq. THEORY_CC3) then
           !             !$omp parallel private(k, a, b) default(shared)                                                                                                 
           !             !$omp do schedule(dynamic)                                                                                                                         
           !             do k = 1, ntasks
           !                   a = isingles(k, 1)
           !                   b = isingles(k, 2)
           !                   dmmo(a, b) = calc_D_vv_ground_mbpt3_cc3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, a, b)
           !             end do
           !             !$omp end do                                                                                                                               
           !             !$omp end parallel   
           !       else if (theory .eq. THEORY_CCSD) then
           !             !$omp parallel private(k, a, b) default(shared)                                                                                      
           !             !$omp do schedule(dynamic)                                                                                                          
           !             do k = 1, ntasks
           !                   a = isingles(k, 1)
           !                   b = isingles(k, 2)
           !                   dmmo(a, b) = calc_D_vv_ground_mbpt3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, a, b)
           !             end do
           !             !$omp end do                                                                                                               
           !             !$omp end parallel 
           !       end if
           ! else if (mbpt == 4)then
           !       if (theory .eq. THEORY_CC3) then
           !             !$omp parallel private(k, a, b) default(shared)                                                                                                 
           !             !$omp do schedule(dynamic)                                                                                                                         
           !             do k = 1, ntasks
           !                   a = isingles(k, 1)
           !                   b = isingles(k, 2)
           !                   dmmo(a, b) = calc_D_vv_ground_mbpt4_cc3(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, a, b)
           !             end do
           !             !$omp end do                                                                                                                               
           !             !$omp end parallel   
           !       else if (theory .eq. THEORY_CCSD) then
           !             !$omp parallel private(k, a, b) default(shared)                                                                                      
           !             !$omp do schedule(dynamic)                                                                                                          
           !             do k = 1, ntasks
           !                   a = isingles(k, 1)
           !                   b = isingles(k, 2)
           !                   dmmo(a, b) = calc_D_vv_ground_mbpt4(t2, t1, s2, s1, &
           !                         dm_nocc, nactive, a, b)
           !             end do
           !             !$omp end do                                                                                                               
           !             !$omp end parallel 
           !       end if
           ! end if
           
     end subroutine dotasks_dmmo_ab

     function calc_D_oo_diag_ground_mbpt(t2, t1, s2, s1, dm_nocc, nactive, i, mbpt) 
           double precision :: calc_D_oo_diag_ground_mbpt
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: i, mbpt

           calc_D_oo_diag_ground_mbpt = zero

           select case (mbpt)
           case(0)
                 calc_D_oo_diag_ground_mbpt = calc_D_oo_diag_ground_mbpt0(t2, t1, s2, s1, dm_nocc, nactive, i) 
           end select

     end function calc_D_oo_diag_ground_mbpt

     function calc_D_oo_ground_mbpt(t2, t1, s2, s1, dm_nocc, nactive, i,j, mbpt) 
           double precision :: calc_D_oo_ground_mbpt
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: i,j, mbpt
           
           calc_D_oo_ground_mbpt = zero

           select case (mbpt)
           case(2) 
                 calc_D_oo_ground_mbpt = calc_D_oo_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
           case(4) 
                 calc_D_oo_ground_mbpt = calc_D_oo_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
           case(5) 
                 calc_D_oo_ground_mbpt = calc_D_oo_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
           end select

     end function calc_D_oo_ground_mbpt

     function calc_D_ov_ground_mbpt(t2, t1, s2, s1, dm_nocc, nactive, i,a, mbpt) 
           double precision :: calc_D_ov_ground_mbpt
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: i,a , mbpt
           
           calc_D_ov_ground_mbpt = zero

           select case (mbpt)
           case(2) 
                 calc_D_ov_ground_mbpt = calc_D_ov_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(3) 
                 calc_D_ov_ground_mbpt = calc_D_ov_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(4) 
                 calc_D_ov_ground_mbpt = calc_D_ov_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(5) 
                 calc_D_ov_ground_mbpt = calc_D_ov_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(6) 
                 calc_D_ov_ground_mbpt = calc_D_ov_ground_mbpt6(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(7) 
                 calc_D_ov_ground_mbpt = calc_D_ov_ground_mbpt7(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(8) 
                 calc_D_ov_ground_mbpt = calc_D_ov_ground_mbpt8(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           end select

     end function calc_D_ov_ground_mbpt

     function calc_D_vo_ground_mbpt(t2, t1, s2, s1, dm_nocc, nactive, a,i, mbpt) 
           double precision :: calc_D_vo_ground_mbpt
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: a,i , mbpt

           calc_D_vo_ground_mbpt = zero

           select case (mbpt)
           case(2) 
                 calc_D_vo_ground_mbpt = calc_D_vo_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
           end select

     end function calc_D_vo_ground_mbpt

     function calc_D_vv_ground_mbpt(t2, t1, s2, s1, dm_nocc, nactive, a,b, mbpt) 
           double precision :: calc_D_vv_ground_mbpt
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: a,b , mbpt

           calc_D_vv_ground_mbpt = zero

           select case (mbpt)
           case(2) 
                 calc_D_vv_ground_mbpt = calc_D_vv_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
           case(4) 
                 calc_D_vv_ground_mbpt = calc_D_vv_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
           case(5) 
                 calc_D_vv_ground_mbpt = calc_D_vv_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
           end select

     end function calc_D_vv_ground_mbpt

     function calc_D_oo_diag_ground_mbpt_cc3(t2, t1, s2, s1, dm_nocc, nactive, i, mbpt) 
           double precision :: calc_D_oo_diag_ground_mbpt_cc3
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: i , mbpt

           calc_D_oo_diag_ground_mbpt_cc3 = zero

           select case (mbpt)
           case(0)
                 calc_D_oo_diag_ground_mbpt_cc3 = calc_D_oo_diag_ground_mbpt0_cc3(t2, t1, s2, s1, dm_nocc, nactive, i)
           end select


     end function calc_D_oo_diag_ground_mbpt_cc3


     function calc_D_oo_ground_mbpt_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j, mbpt) 
           double precision :: calc_D_oo_ground_mbpt_cc3
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: i,j , mbpt

           calc_D_oo_ground_mbpt_cc3 = zero

           select case (mbpt)
           case(2) 
                 calc_D_oo_ground_mbpt_cc3 = calc_D_oo_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
           case(4) 
                 calc_D_oo_ground_mbpt_cc3 = calc_D_oo_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
           case(5) 
                 calc_D_oo_ground_mbpt_cc3 = calc_D_oo_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
           case(8) 
                 calc_D_oo_ground_mbpt_cc3 = calc_D_oo_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
           end select


     end function calc_D_oo_ground_mbpt_cc3

     function calc_D_ov_ground_mbpt_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a, mbpt) 
           double precision :: calc_D_ov_ground_mbpt_cc3
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: i,a , mbpt

           calc_D_ov_ground_mbpt_cc3 = zero

           select case (mbpt)
           case(2) 
                 calc_D_ov_ground_mbpt_cc3 = calc_D_ov_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(3) 
                 calc_D_ov_ground_mbpt_cc3 = calc_D_ov_ground_mbpt3_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(4) 
                 calc_D_ov_ground_mbpt_cc3 = calc_D_ov_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(5) 
                 calc_D_ov_ground_mbpt_cc3 = calc_D_ov_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(6) 
                 calc_D_ov_ground_mbpt_cc3 = calc_D_ov_ground_mbpt6_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(7) 
                 calc_D_ov_ground_mbpt_cc3 = calc_D_ov_ground_mbpt7_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           case(8) 
                 calc_D_ov_ground_mbpt_cc3 = calc_D_ov_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
           end select

     end function calc_D_ov_ground_mbpt_cc3

     function calc_D_vo_ground_mbpt_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,i, mbpt) 
           double precision :: calc_D_vo_ground_mbpt_cc3
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: a,i , mbpt

           calc_D_vo_ground_mbpt_cc3 = zero

           select case (mbpt)
           case(2) 
                 calc_D_vo_ground_mbpt_cc3 = calc_D_vo_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
           end select

     end function calc_D_vo_ground_mbpt_cc3

     function calc_D_vv_ground_mbpt_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b, mbpt) 
           double precision :: calc_D_vv_ground_mbpt_cc3
           integer, intent(in) :: dm_nocc, nactive
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
           double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
           integer, intent(in) :: a,b , mbpt

           calc_D_vv_ground_mbpt_cc3 = zero

           select case (mbpt)
           case(2) 
                 calc_D_vv_ground_mbpt_cc3 = calc_D_vv_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
           case(4) 
                 calc_D_vv_ground_mbpt_cc3 = calc_D_vv_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
           case(5) 
                 calc_D_vv_ground_mbpt_cc3 = calc_D_vv_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
           case(8) 
                 calc_D_vv_ground_mbpt_cc3 = calc_D_vv_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
           end select

     end function calc_D_vv_ground_mbpt_cc3

   function calc_D_oo_diag_ground_mbpt0(t2, t1, s2, s1, dm_nocc, nactive, i) 
    double precision :: calc_D_oo_diag_ground_mbpt0
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) ::  i
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = one

    term(0) = term(0) * 2.0d+0 


    calc_D_oo_diag_ground_mbpt0 = 0.d+0 
    do s = 0, 0
    calc_D_oo_diag_ground_mbpt0 = calc_D_oo_diag_ground_mbpt0 + term(s)
    end do

    end function calc_D_oo_diag_ground_mbpt0
    
    function calc_D_oo_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
          double precision :: calc_D_oo_ground_mbpt2
          integer, intent(in) :: dm_nocc, nactive
          double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
          double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
          double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
          double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
          integer, intent(in) :: i,j 
          integer :: s ,k,b,a 
          double precision, dimension(0:1) :: term 
           term = 0.d+0 

          do k = 1, dm_nocc 
                do b = dm_nocc + 1, nactive 
                      do a = dm_nocc + 1, nactive 
                            term(0) = term(0) + s2(a,b,k,j) * t2(a,b,i,k)
                            term(1) = term(1) + s2(a,b,j,k) * t2(a,b,i,k)
                      end do
                end do
          end do

          term(0) = term(0) * 2.0d+0 
          term(1) = term(1) * (-4.0d+0) 


          calc_D_oo_ground_mbpt2 = 0.d+0 
          do s = 0, 1
                calc_D_oo_ground_mbpt2 = calc_D_oo_ground_mbpt2 + term(s)
          end do

    end function calc_D_oo_ground_mbpt2


 
    function calc_D_ov_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt2
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a
    integer :: b, j, c, k
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 

    term(0) = term(0) + t1(a,i)

term(0) = term(0) * 2.0d+0 


    calc_D_ov_ground_mbpt2 = 0.d+0 
    do s = 0, 0
    calc_D_ov_ground_mbpt2 = calc_D_ov_ground_mbpt2 + term(s)
    end do

    end function calc_D_ov_ground_mbpt2
    
    function calc_D_vo_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
    double precision :: calc_D_vo_ground_mbpt2
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 

    term(0) = term(0) + s1(a,i)

term(0) = term(0) * 2.0d+0 


    calc_D_vo_ground_mbpt2 = 0.d+0 
    do s = 0, 0
    calc_D_vo_ground_mbpt2 = calc_D_vo_ground_mbpt2 + term(s)
    end do

    end function calc_D_vo_ground_mbpt2
    
    function calc_D_vv_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
    double precision :: calc_D_vv_ground_mbpt2
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,b 
    integer :: s ,j,i,c 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 



    do j = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
term(0) = term(0) + s2(a,c,j,i) * t2(b,c,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 

do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do i = 1, dm_nocc 
term(1) = term(1) + s2(a,c,i,j) * t2(b,c,i,j)
end do 
end do 
end do 

term(1) = term(1) * 4.0d+0 


    calc_d_vv_ground_mbpt2 = 0.d+0 
    do s = 0, 1
    calc_D_vv_ground_mbpt2 = calc_D_vv_ground_mbpt2 + term(s)
    end do

    end function calc_D_vv_ground_mbpt2

    function calc_D_ov_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,j,b 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,j) * t2(a,b,i,j)
end do 
end do 

term(0) = term(0) * 4.0d+0 

do b = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
term(1) = term(1) + s1(b,j) * t2(a,b,j,i)
end do 
end do 

term(1) = term(1) * (-2.0d+0) 


    calc_D_ov_ground_mbpt3 = 0.d+0 
    do s = 0, 1
    calc_D_ov_ground_mbpt3 = calc_D_ov_ground_mbpt3 + term(s)
    end do

    end function calc_D_ov_ground_mbpt3

    function calc_D_oo_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
    double precision :: calc_D_oo_ground_mbpt4
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,j 
    integer :: s ,a 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do a = dm_nocc + 1, nactive 
term(0) = term(0) + s1(a,j) * t1(a,i)
end do 

term(0) = term(0) * (-2.0d+0) 


    calc_D_oo_ground_mbpt4 = 0.d+0 
    do s = 0, 0
    calc_D_oo_ground_mbpt4 = calc_D_oo_ground_mbpt4 + term(s)
    end do

    end function calc_D_oo_ground_mbpt4
    
    function calc_D_ov_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt4
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,k,j,c,b 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do k = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s2(b,c,k,j) * t1(a,j) * t2(b,c,i,k)
term(1) = term(1) + s2(b,c,k,j) * t1(b,i) * t2(a,c,j,k)
end do 
end do 
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * 2.0d+0 

do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(2) = term(2) + s2(b,c,j,k) * t1(a,j) * t2(b,c,i,k)
term(3) = term(3) + s2(b,c,j,k) * t1(b,i) * t2(a,c,j,k)
end do 
end do 
end do 
end do 

term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (-4.0d+0) 


    calc_D_ov_ground_mbpt4 = 0.d+0 
    do s = 0, 3
    calc_D_ov_ground_mbpt4 = calc_D_ov_ground_mbpt4 + term(s)
    end do

    end function calc_D_ov_ground_mbpt4
    
    function calc_D_vv_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
    double precision :: calc_D_vv_ground_mbpt4
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,b 
    integer :: s ,i 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do i = 1, dm_nocc 
term(0) = term(0) + s1(a,i) * t1(b,i)
end do 

term(0) = term(0) * 2.0d+0 


    calc_D_vv_ground_mbpt4 = 0.d+0 
    do s = 0, 0
    calc_D_vv_ground_mbpt4 = calc_D_vv_ground_mbpt4 + term(s)
    end do

    end function calc_D_vv_ground_mbpt4

    function calc_D_oo_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
    double precision :: calc_D_oo_ground_mbpt5
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,j 
    integer :: s ,k,b,a 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do a = dm_nocc + 1, nactive 
term(0) = term(0) + s1(a,k) * s1(b,j) * t2(a,b,i,k)
term(1) = term(1) + s1(b,k) * s1(a,j) * t2(a,b,i,k)
end do 
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-4.0d+0) 


    calc_D_oo_ground_mbpt5 = 0.d+0 
    do s = 0, 1
    calc_D_oo_ground_mbpt5 = calc_D_oo_ground_mbpt5 + term(s)
    end do

    end function calc_D_oo_ground_mbpt5
    
    function calc_D_ov_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt5
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,l,k,j,d,c,b 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do l = 1, dm_nocc 
do k = 1, dm_nocc 
do j = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,k) * s2(c,d,l,j) * t2(a,d,j,l) * t2(b,c,i,k)
term(1) = term(1) + s1(c,k) * s2(b,d,l,j) * t2(a,d,j,l) * t2(b,c,i,k)
term(2) = term(2) + s1(b,k) * s2(c,d,l,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(3) = term(3) + s1(c,k) * s2(b,d,l,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(4) = term(4) + s1(c,k) * s2(b,d,l,j) * t2(a,c,j,l) * t2(b,d,i,k)
term(5) = term(5) + s1(c,k) * s2(b,d,l,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(6) = term(6) + s1(b,k) * s2(c,d,l,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(7) = term(7) + s1(b,k) * s2(c,d,l,j) * t2(a,c,j,l) * t2(b,d,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = -term(0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 

do l = 1, dm_nocc 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(8) = term(8) + s1(b,j) * s2(c,d,k,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(9) = term(9) + s1(c,j) * s2(b,d,k,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(10) = term(10) + s1(b,k) * s2(c,d,j,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(11) = term(11) + s1(b,k) * s2(c,d,j,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(12) = term(12) + s1(b,j) * s2(c,d,k,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(13) = term(13) + s1(c,j) * s2(b,d,k,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(14) = term(14) + s1(c,k) * s2(b,d,j,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(15) = term(15) + s1(c,k) * s2(b,d,j,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(16) = term(16) + s1(c,j) * s2(b,d,k,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(17) = term(17) + s1(c,k) * s2(b,d,j,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(18) = term(18) + s1(c,j) * s2(b,d,k,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(19) = term(19) + s1(c,k) * s2(b,d,j,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(20) = term(20) + s1(b,k) * s2(c,d,j,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(21) = term(21) + s1(b,j) * s2(c,d,k,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(22) = term(22) + s1(b,k) * s2(c,d,j,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(23) = term(23) + s1(b,j) * s2(c,d,k,l) * t2(a,c,j,k) * t2(b,d,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * 2.0d+0 
term(9) = term(9) * (-2.0d+0) 
term(10) = -term(10) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * 4.0d+0 
term(14) = term(14) * 4.0d+0 
term(15) = term(15) * (-8.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * 4.0d+0 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-8.0d+0) 
term(20) = -term(20) 
term(21) = term(21) * 2.0d+0 
term(22) = term(22) * 2.0d+0 
term(23) = term(23) * (-4.0d+0) 


    calc_D_ov_ground_mbpt5 = 0.d+0 
    do s = 0, 23
    calc_D_ov_ground_mbpt5 = calc_D_ov_ground_mbpt5 + term(s)
    end do

    end function calc_D_ov_ground_mbpt5
    
    function calc_D_vv_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
    double precision :: calc_D_vv_ground_mbpt5
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,b 
    integer :: s ,i,c,j 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
term(0) = term(0) + s1(c,j) * s1(a,i) * t2(b,c,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 

do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do i = 1, dm_nocc 
term(1) = term(1) + s1(c,j) * s1(a,i) * t2(b,c,i,j)
end do 
end do 
end do 

term(1) = term(1) * 4.0d+0 


    calc_D_vv_ground_mbpt5 = 0.d+0 
    do s = 0, 1
    calc_D_vv_ground_mbpt5 = calc_D_vv_ground_mbpt5 + term(s)
    end do

    end function calc_D_vv_ground_mbpt5

    function calc_D_ov_ground_mbpt6(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt6
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,j,b 
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,j) * t1(a,j) * t1(b,i)
end do 
end do 

term(0) = term(0) * (-2.0d+0) 


    calc_D_ov_ground_mbpt6 = 0.d+0 
    do s = 0, 0
    calc_D_ov_ground_mbpt6 = calc_D_ov_ground_mbpt6 + term(s)
    end do

    end function calc_D_ov_ground_mbpt6

    function calc_D_ov_ground_mbpt7(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt7
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,k,c,j,b 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,k) * s1(c,j) * t1(a,j) * t2(b,c,i,k)
term(1) = term(1) + s1(b,j) * s1(c,k) * t1(a,j) * t2(b,c,i,k)
term(2) = term(2) + s1(b,k) * s1(c,j) * t1(b,i) * t2(a,c,j,k)
term(3) = term(3) + s1(b,j) * s1(c,k) * t1(b,i) * t2(a,c,j,k)
end do 
end do 
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-4.0d+0) 


    calc_D_ov_ground_mbpt7 = 0.d+0 
    do s = 0, 3
    calc_D_ov_ground_mbpt7 = calc_D_ov_ground_mbpt7 + term(s)
    end do

    end function calc_D_ov_ground_mbpt7

    function calc_D_ov_ground_mbpt8(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt8
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,l,k,d,c,j,b 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    do l = 1, dm_nocc 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,k) * s1(c,l) * s1(d,j) * t2(a,d,j,l) * t2(b,c,i,k)
term(1) = term(1) + s1(b,j) * s1(c,k) * s1(d,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(2) = term(2) + s1(b,k) * s1(c,j) * s1(d,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(3) = term(3) + s1(b,k) * s1(c,l) * s1(d,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(4) = term(4) + s1(b,k) * s1(c,j) * s1(d,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(5) = term(5) + s1(b,j) * s1(c,k) * s1(d,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(6) = term(6) + s1(b,k) * s1(c,l) * s1(d,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(7) = term(7) + s1(b,k) * s1(c,j) * s1(d,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(8) = term(8) + s1(b,k) * s1(c,j) * s1(d,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(9) = term(9) + s1(b,j) * s1(c,k) * s1(d,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(10) = term(10) + s1(b,k) * s1(c,l) * s1(d,j) * t2(a,c,j,l) * t2(b,d,i,k)
term(11) = term(11) + s1(b,j) * s1(c,k) * s1(d,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(12) = term(12) + s1(c,k) * s1(b,j) * s1(d,l) * t2(a,d,j,l) * t2(b,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999996d+0 
term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999996d+0 
term(4) = term(4) * 1.9999999999999996d+0 
term(5) = term(5) * (-2.999999999999999d+0) 
term(6) = -term(6) 
term(7) = term(7) * 1.9999999999999996d+0 
term(8) = -term(8) 
term(9) = term(9) * 1.9999999999999996d+0 
term(10) = term(10) * 1.9999999999999996d+0 
term(11) = term(11) * (-3.999999999999999d+0) 
term(12) = -term(12) 


    calc_D_ov_ground_mbpt8 = 0.d+0 
    do s = 0, 12
    calc_D_ov_ground_mbpt8 = calc_D_ov_ground_mbpt8 + term(s)
    end do

    end function calc_D_ov_ground_mbpt8

    function calc_D_oo_diag_ground_mbpt0_cc3(t2, t1, s2, s1, dm_nocc, nactive, i) 
    double precision :: calc_D_oo_diag_ground_mbpt0_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) ::  i
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = one

term(0) = term(0) * 2.0d+0 


    calc_D_oo_diag_ground_mbpt0_cc3 = 0.d+0 
    do s = 0, 0
    calc_D_oo_diag_ground_mbpt0_cc3 = calc_D_oo_diag_ground_mbpt0_cc3 + term(s)
    end do

    end function calc_D_oo_diag_ground_mbpt0_cc3

    function calc_D_oo_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
    double precision :: calc_D_oo_ground_mbpt2_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,j 
    integer :: s ,k,b,a 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do a = dm_nocc + 1, nactive 
term(0) = term(0) + s2(a,b,k,j) * t2(a,b,i,k)
term(1) = term(1) + s2(a,b,j,k) * t2(a,b,i,k)
end do 
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-4.0d+0) 


    calc_D_oo_ground_mbpt2_cc3 = 0.d+0 
    do s = 0, 1
    calc_D_oo_ground_mbpt2_cc3 = calc_D_oo_ground_mbpt2_cc3 + term(s)
    end do

    end function calc_D_oo_ground_mbpt2_cc3
    
    function calc_D_ov_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt2_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + t1(a,i)

term(0) = term(0) * 2.0d+0 


    calc_D_ov_ground_mbpt2_cc3 = 0.d+0 
    do s = 0, 0
    calc_D_ov_ground_mbpt2_cc3 = calc_D_ov_ground_mbpt2_cc3 + term(s)
    end do

    end function calc_D_ov_ground_mbpt2_cc3
    
    function calc_D_vo_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
    double precision :: calc_D_vo_ground_mbpt2_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,i 
    integer :: s  
    double precision, dimension(0:0) :: term 
    term = 0.d+0 
    term(0) = term(0) + s1(a,i)

term(0) = term(0) * 2.0d+0 


    calc_D_vo_ground_mbpt2_cc3 = 0.d+0 
    do s = 0, 0
    calc_D_vo_ground_mbpt2_cc3 = calc_D_vo_ground_mbpt2_cc3 + term(s)
    end do

    end function calc_D_vo_ground_mbpt2_cc3
    
    function calc_D_vv_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
    double precision :: calc_D_vv_ground_mbpt2_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,b 
    integer :: s ,j,i,c 
    double precision, dimension(0:1) :: term 
    term = 0.d+0 
    do j = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
term(0) = term(0) + s2(a,c,j,i) * t2(b,c,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 

do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do i = 1, dm_nocc 
term(1) = term(1) + s2(a,c,i,j) * t2(b,c,i,j)
end do 
end do 
end do 

term(1) = term(1) * 4.0d+0 


    calc_D_vv_ground_mbpt2_cc3 = 0.d+0 
    do s = 0, 1
    calc_D_vv_ground_mbpt2_cc3 = calc_D_vv_ground_mbpt2_cc3 + term(s)
    end do

    end function calc_D_vv_ground_mbpt2_cc3

    function calc_D_ov_ground_mbpt3_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt3_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,j,b,k,c 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
term(0) = term(0) + s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,i,k,j)
end do 
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
term(1) = term(1) + s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 

term(1) = term(1) * 3.9999999999999996d+0 

do k = 1, dm_nocc 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
term(2) = term(2) + s2(b,c,k,j) * t3(dm_nocc, nactive, a,b,c,j,i,k)
term(3) = term(3) + s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 

term(2) = term(2) * 1.9999999999999998d+0 
term(3) = term(3) * (-3.9999999999999996d+0) 

do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(4) = term(4) + s1(b,j) * t2(a,b,i,j)
end do 
end do 

term(4) = term(4) * 4.0d+0 

do b = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
term(5) = term(5) + s1(b,j) * t2(a,b,j,i)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    calc_D_ov_ground_mbpt3_cc3 = 0.d+0 
    do s = 0, 5
    calc_D_ov_ground_mbpt3_cc3 = calc_D_ov_ground_mbpt3_cc3 + term(s)
    end do

    end function calc_D_ov_ground_mbpt3_cc3

    function calc_D_oo_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
    double precision :: calc_D_oo_ground_mbpt4_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,j 
    integer :: s ,a,l,k,b,c 
    double precision, dimension(0:4) :: term 
    term = 0.d+0 
    do a = dm_nocc + 1, nactive 
term(0) = term(0) + s1(a,j) * t1(a,i)
end do 

term(0) = term(0) * (-2.0d+0) 

do l = 1, dm_nocc 
do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do a = dm_nocc + 1, nactive 
term(1) = term(1) + t3(dm_nocc, nactive, a,b,c,i,k,l) * t3(dm_nocc, nactive, a,b,c,l,j,k)
term(2) = term(2) + t3(dm_nocc, nactive, a,b,c,i,k,l) * t3(dm_nocc, nactive, a,b,c,j,l,k)
term(3) = term(3) + t3(dm_nocc, nactive, a,b,c,i,k,l) * t3(dm_nocc, nactive, a,b,c,k,j,l)
end do 
end do 
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 1.9999999999999998d+0 
term(3) = term(3) * 4.000000000000001d+0 

do l = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do a = dm_nocc + 1, nactive 
term(4) = term(4) + t3(dm_nocc, nactive, a,b,c,i,k,l) * t3(dm_nocc, nactive, a,b,c,j,k,l)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * (-4.0d+0) 


    calc_D_oo_ground_mbpt4_cc3 = 0.d+0 
    do s = 0, 4
    calc_D_oo_ground_mbpt4_cc3 = calc_D_oo_ground_mbpt4_cc3 + term(s)
    end do

    end function calc_D_oo_ground_mbpt4_cc3
    
    function calc_D_ov_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt4_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,k,l,c,d,j,b 
    double precision, dimension(0:9) :: term 
    term = 0.d+0 
    do k = 1, dm_nocc 
do l = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do d = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,l,j,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999991d+0) 

do k = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do l = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do b = dm_nocc + 1, nactive 
term(1) = term(1) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,j,l,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(1) = term(1) * 3.9999999999999982d+0 

do j = 1, dm_nocc 
do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do l = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do b = dm_nocc + 1, nactive 
term(2) = term(2) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,k,l,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * (-1.9999999999999991d+0) 

do l = 1, dm_nocc 
do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do d = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(3) = term(3) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,k,j,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(3) = term(3) * 3.9999999999999982d+0 

do j = 1, dm_nocc 
do l = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do b = dm_nocc + 1, nactive 
term(4) = term(4) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,l,k,j)
end do 
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * 3.9999999999999982d+0 

do l = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do b = dm_nocc + 1, nactive 
term(5) = term(5) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,j,k,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * (-7.9999999999999964d+0) 

do k = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do b = dm_nocc + 1, nactive 
term(6) = term(6) + s2(b,c,k,j) * t1(a,j) * t2(b,c,i,k)
term(7) = term(7) + s2(b,c,k,j) * t1(b,i) * t2(a,c,j,k)
end do 
end do 
end do 
end do 

term(6) = term(6) * 2.0d+0 
term(7) = term(7) * 2.0d+0 

do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(8) = term(8) + s2(b,c,j,k) * t1(a,j) * t2(b,c,i,k)
term(9) = term(9) + s2(b,c,j,k) * t1(b,i) * t2(a,c,j,k)
end do 
end do 
end do 
end do 

term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (-4.0d+0) 


    calc_D_ov_ground_mbpt4_cc3 = 0.d+0 
    do s = 0, 9
    calc_D_ov_ground_mbpt4_cc3 = calc_D_ov_ground_mbpt4_cc3 + term(s)
    end do

    end function calc_D_ov_ground_mbpt4_cc3
    
    function calc_D_vv_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
    double precision :: calc_D_vv_ground_mbpt4_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,b 
    integer :: s ,i,k,j,c,d 
    double precision, dimension(0:6) :: term 
    term = 0.d+0 
    do i = 1, dm_nocc 
term(0) = term(0) + s1(a,i) * t1(b,i)
end do 

term(0) = term(0) * 2.0d+0 

do k = 1, dm_nocc 
do i = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do d = dm_nocc + 1, nactive 
term(1) = term(1) + t3(dm_nocc, nactive, a,c,d,i,j,k) * t3(dm_nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 


do k = 1, dm_nocc 
do j = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do d = dm_nocc + 1, nactive 
term(2) = term(2) + t3(dm_nocc, nactive, a,c,d,i,j,k) * t3(dm_nocc, nactive, b,c,d,i,k,j)
term(3) = term(3) + t3(dm_nocc, nactive, a,c,d,i,k,j) * t3(dm_nocc, nactive, b,c,d,j,i,k)
term(4) = term(4) + t3(dm_nocc, nactive, a,c,d,i,j,k) * t3(dm_nocc, nactive, b,c,d,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 
term(4) = term(4) * (-2.0000000000000004d+0) 

do j = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
term(5) = term(5) + t3(dm_nocc, nactive, a,c,d,i,k,j) * t3(dm_nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * (-2.0000000000000004d+0) 

do k = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
term(6) = term(6) + t3(dm_nocc, nactive, a,c,d,i,j,k) * t3(dm_nocc, nactive, b,c,d,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * 4.000000000000001d+0 


    calc_D_vv_ground_mbpt4_cc3 = 0.d+0 
    do s = 0, 6
    calc_D_vv_ground_mbpt4_cc3 = calc_D_vv_ground_mbpt4_cc3 + term(s)
    end do

    end function calc_D_vv_ground_mbpt4_cc3

    function calc_D_oo_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
    double precision :: calc_D_oo_ground_mbpt5_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,j 
    integer :: s ,k,b,a,l,c 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do a = dm_nocc + 1, nactive 
term(0) = term(0) + s1(a,k) * s1(b,j) * t2(a,b,i,k)
term(1) = term(1) + s1(b,k) * s1(a,j) * t2(a,b,i,k)
end do 
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-4.0d+0) 

do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do l = 1, dm_nocc 
do a = dm_nocc + 1, nactive 
term(2) = term(2) + s1(a,k) * s2(b,c,l,j) * t3(dm_nocc, nactive, a,b,c,i,l,k)
term(3) = term(3) + s1(b,k) * s2(a,c,l,j) * t3(dm_nocc, nactive, a,b,c,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * 1.9999999999999998d+0 
term(3) = term(3) * (-1.9999999999999998d+0) 

do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do l = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do a = dm_nocc + 1, nactive 
term(4) = term(4) + s1(a,k) * s2(b,c,j,l) * t3(dm_nocc, nactive, a,b,c,i,l,k)
term(5) = term(5) + s1(a,j) * s2(b,c,k,l) * t3(dm_nocc, nactive, a,b,c,i,l,k)
term(6) = term(6) + s1(b,j) * s2(a,c,k,l) * t3(dm_nocc, nactive, a,b,c,i,l,k)
term(7) = term(7) + s1(b,k) * s2(a,c,j,l) * t3(dm_nocc, nactive, a,b,c,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(4) = -term(4) 
term(5) = term(5) * 1.9999999999999998d+0 
term(6) = term(6) * (-1.9999999999999998d+0) 
term(7) = term(7) * 3.9999999999999996d+0 

do l = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do a = dm_nocc + 1, nactive 
term(8) = term(8) + s1(b,k) * s2(a,c,l,j) * t3(dm_nocc, nactive, a,b,c,i,k,l)
term(9) = term(9) + s1(a,k) * s2(b,c,l,j) * t3(dm_nocc, nactive, a,b,c,i,k,l)
term(10) = term(10) + s1(a,k) * s2(b,c,j,l) * t3(dm_nocc, nactive, a,b,c,i,k,l)
term(11) = term(11) + s1(a,j) * s2(b,c,k,l) * t3(dm_nocc, nactive, a,b,c,i,k,l)
term(12) = term(12) + s1(b,j) * s2(a,c,k,l) * t3(dm_nocc, nactive, a,b,c,i,k,l)
term(13) = term(13) + s1(b,k) * s2(a,c,j,l) * t3(dm_nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * 3.9999999999999996d+0 
term(9) = -term(9) 
term(10) = term(10) * 1.9999999999999998d+0 
term(11) = term(11) * (-3.9999999999999996d+0) 
term(12) = term(12) * 3.9999999999999996d+0 
term(13) = term(13) * (-7.999999999999999d+0) 


    calc_D_oo_ground_mbpt5_cc3 = 0.d+0 
    do s = 0, 13
    calc_D_oo_ground_mbpt5_cc3 = calc_D_oo_ground_mbpt5_cc3 + term(s)
    end do

    end function calc_D_oo_ground_mbpt5_cc3
    
    function calc_D_ov_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt5_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,l,k,j,d,c,b 
    double precision, dimension(0:23) :: term 
    term = 0.d+0 
    do l = 1, dm_nocc 
do k = 1, dm_nocc 
do j = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,k) * s2(c,d,l,j) * t2(a,d,j,l) * t2(b,c,i,k)
term(1) = term(1) + s1(c,k) * s2(b,d,l,j) * t2(a,d,j,l) * t2(b,c,i,k)
term(2) = term(2) + s1(b,k) * s2(c,d,l,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(3) = term(3) + s1(c,k) * s2(b,d,l,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(4) = term(4) + s1(c,k) * s2(b,d,l,j) * t2(a,c,j,l) * t2(b,d,i,k)
term(5) = term(5) + s1(c,k) * s2(b,d,l,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(6) = term(6) + s1(b,k) * s2(c,d,l,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(7) = term(7) + s1(b,k) * s2(c,d,l,j) * t2(a,c,j,l) * t2(b,d,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = -term(0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 4.0d+0 
term(6) = -term(6) 
term(7) = term(7) * 2.0d+0 

do l = 1, dm_nocc 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(8) = term(8) + s1(b,j) * s2(c,d,k,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(9) = term(9) + s1(c,j) * s2(b,d,k,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(10) = term(10) + s1(b,k) * s2(c,d,j,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(11) = term(11) + s1(b,k) * s2(c,d,j,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(12) = term(12) + s1(b,j) * s2(c,d,k,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(13) = term(13) + s1(c,j) * s2(b,d,k,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(14) = term(14) + s1(c,k) * s2(b,d,j,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(15) = term(15) + s1(c,k) * s2(b,d,j,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(16) = term(16) + s1(c,j) * s2(b,d,k,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(17) = term(17) + s1(c,k) * s2(b,d,j,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(18) = term(18) + s1(c,j) * s2(b,d,k,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(19) = term(19) + s1(c,k) * s2(b,d,j,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(20) = term(20) + s1(b,k) * s2(c,d,j,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(21) = term(21) + s1(b,j) * s2(c,d,k,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(22) = term(22) + s1(b,k) * s2(c,d,j,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(23) = term(23) + s1(b,j) * s2(c,d,k,l) * t2(a,c,j,k) * t2(b,d,i,l)
end do 
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * 2.0d+0 
term(9) = term(9) * (-2.0d+0) 
term(10) = -term(10) 
term(11) = term(11) * 2.0d+0 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * 4.0d+0 
term(14) = term(14) * 4.0d+0 
term(15) = term(15) * (-8.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * 4.0d+0 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-8.0d+0) 
term(20) = -term(20) 
term(21) = term(21) * 2.0d+0 
term(22) = term(22) * 2.0d+0 
term(23) = term(23) * (-4.0d+0) 


    calc_D_ov_ground_mbpt5_cc3 = 0.d+0 
    do s = 0, 23
    calc_D_ov_ground_mbpt5_cc3 = calc_D_ov_ground_mbpt5_cc3 + term(s)
    end do

    end function calc_D_ov_ground_mbpt5_cc3
    
    function calc_D_vv_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
    double precision :: calc_D_vv_ground_mbpt5_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,b 
    integer :: s ,i,c,j,k,d 
    double precision, dimension(0:13) :: term 
    term = 0.d+0 
    do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
term(0) = term(0) + s1(c,j) * s1(a,i) * t2(b,c,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 

do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do i = 1, dm_nocc 
term(1) = term(1) + s1(c,j) * s1(a,i) * t2(b,c,i,j)
end do 
end do 
end do 

term(1) = term(1) * 4.0d+0 

do i = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
term(2) = term(2) + s1(a,i) * s2(c,d,j,k) * t3(dm_nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 


do j = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
term(3) = term(3) + s1(a,i) * s2(c,d,j,k) * t3(dm_nocc, nactive, b,c,d,i,k,j)
term(4) = term(4) + s1(c,i) * s2(a,d,j,k) * t3(dm_nocc, nactive, b,c,d,i,k,j)
term(5) = term(5) + s1(c,j) * s2(a,d,i,k) * t3(dm_nocc, nactive, b,c,d,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(3) = term(3) * (-1.9999999999999998d+0) 
term(4) = term(4) * 2.0d+0 
term(5) = term(5) * (-4.0d+0) 

do k = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do d = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
term(6) = term(6) + s1(c,j) * s2(a,d,k,i) * t3(dm_nocc, nactive, b,c,d,i,j,k)
term(7) = term(7) + s1(a,i) * s2(c,d,j,k) * t3(dm_nocc, nactive, b,c,d,i,j,k)
term(8) = term(8) + s1(c,i) * s2(a,d,j,k) * t3(dm_nocc, nactive, b,c,d,i,j,k)
term(9) = term(9) + s1(c,j) * s2(a,d,i,k) * t3(dm_nocc, nactive, b,c,d,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * 3.9999999999999996d+0 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * 8.0d+0 

do k = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do d = dm_nocc + 1, nactive 
do i = 1, dm_nocc 
term(10) = term(10) + s1(a,i) * s2(c,d,k,j) * t3(dm_nocc, nactive, b,c,d,j,i,k)
term(11) = term(11) + s1(a,i) * s2(c,d,j,k) * t3(dm_nocc, nactive, b,c,d,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(11) = term(11) * (-1.9999999999999998d+0) 

do i = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do d = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
term(12) = term(12) + s1(a,i) * s2(c,d,k,j) * t3(dm_nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(12) = term(12) * (-1.9999999999999998d+0) 

do j = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do d = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
term(13) = term(13) + s1(c,j) * s2(a,d,k,i) * t3(dm_nocc, nactive, b,c,d,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(13) = term(13) * 2.0d+0 


    calc_D_vv_ground_mbpt5_cc3 = 0.d+0 
    do s = 0, 13
    calc_D_vv_ground_mbpt5_cc3 = calc_D_vv_ground_mbpt5_cc3 + term(s)
    end do

    end function calc_D_vv_ground_mbpt5_cc3
    

    function calc_D_ov_ground_mbpt6_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt6_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,j,b,k,c 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,i,k,j)
end do 
end do 
end do 
end do 

term(0) = term(0) * (-1.9999999999999998d+0) 

do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
term(1) = term(1) + s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,i,j,k)
end do 
end do 
end do 
end do 

term(1) = term(1) * 3.9999999999999996d+0 

do k = 1, dm_nocc 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
term(2) = term(2) + s1(b,k) * s1(c,j) * t3(dm_nocc, nactive, a,b,c,j,i,k)
term(3) = term(3) + s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,j,i,k)
term(4) = term(4) + s1(c,j) * s1(b,k) * t3(dm_nocc, nactive, a,b,c,j,i,k)
end do 
end do 
end do 
end do 

term(3) = term(3) * (-3.9999999999999996d+0) 

do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(5) = term(5) + s1(b,j) * t1(a,j) * t1(b,i)
end do 
end do 

term(5) = term(5) * (-2.0d+0) 


    calc_D_ov_ground_mbpt6_cc3 = 0.d+0 
    do s = 0, 5
    calc_D_ov_ground_mbpt6_cc3 = calc_D_ov_ground_mbpt6_cc3 + term(s)
    end do

    end function calc_D_ov_ground_mbpt6_cc3

    function calc_D_ov_ground_mbpt7_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt7_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,k,c,j,b 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,k) * s1(c,j) * t1(a,j) * t2(b,c,i,k)
term(1) = term(1) + s1(b,j) * s1(c,k) * t1(a,j) * t2(b,c,i,k)
term(2) = term(2) + s1(b,k) * s1(c,j) * t1(b,i) * t2(a,c,j,k)
term(3) = term(3) + s1(b,j) * s1(c,k) * t1(b,i) * t2(a,c,j,k)
end do 
end do 
end do 
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-4.0d+0) 


    calc_D_ov_ground_mbpt7_cc3 = 0.d+0 
    do s = 0, 3
    calc_D_ov_ground_mbpt7_cc3 = calc_D_ov_ground_mbpt7_cc3 + term(s)
    end do

    end function calc_D_ov_ground_mbpt7_cc3

    function calc_D_oo_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
    double precision :: calc_D_oo_ground_mbpt8_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,j 
    integer :: s ,k,b,l,c,a 
    double precision, dimension(0:3) :: term 
    term = 0.d+0 
    do k = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do l = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do a = dm_nocc + 1, nactive 
term(0) = term(0) + s1(a,k) * s1(c,l) * s1(b,j) * t3(dm_nocc, nactive, a,b,c,i,l,k)
term(1) = term(1) + s1(b,k) * s1(c,l) * s1(a,j) * t3(dm_nocc, nactive, a,b,c,i,l,k)
end do 
end do 
end do 
end do 
end do 

term(0) = term(0) * (-2.0000000000000004d+0) 
term(1) = term(1) * 2.0d+0 

do l = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do a = dm_nocc + 1, nactive 
term(2) = term(2) + s1(a,k) * s1(c,l) * s1(b,j) * t3(dm_nocc, nactive, a,b,c,i,k,l)
term(3) = term(3) + s1(b,k) * s1(c,l) * s1(a,j) * t3(dm_nocc, nactive, a,b,c,i,k,l)
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * 4.000000000000001d+0 
term(3) = term(3) * (-4.0d+0) 


    calc_D_oo_ground_mbpt8_cc3 = 0.d+0 
    do s = 0, 3
    calc_D_oo_ground_mbpt8_cc3 = calc_D_oo_ground_mbpt8_cc3 + term(s)
    end do

    end function calc_D_oo_ground_mbpt8_cc3
    
    function calc_D_ov_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
    double precision :: calc_D_ov_ground_mbpt8_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: i,a 
    integer :: s ,l,k,d,c,j,b 
    double precision, dimension(0:12) :: term 
    term = 0.d+0 
    do l = 1, dm_nocc 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do b = dm_nocc + 1, nactive 
term(0) = term(0) + s1(b,k) * s1(c,l) * s1(d,j) * t2(a,d,j,l) * t2(b,c,i,k)
term(1) = term(1) + s1(b,j) * s1(c,k) * s1(d,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(2) = term(2) + s1(b,k) * s1(c,j) * s1(d,l) * t2(a,d,j,k) * t2(b,c,i,l)
term(3) = term(3) + s1(b,k) * s1(c,l) * s1(d,j) * t2(a,d,j,k) * t2(b,c,i,l)
term(4) = term(4) + s1(b,k) * s1(c,j) * s1(d,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(5) = term(5) + s1(b,j) * s1(c,k) * s1(d,l) * t2(a,d,j,l) * t2(b,c,i,k)
term(6) = term(6) + s1(b,k) * s1(c,l) * s1(d,j) * t2(a,c,j,k) * t2(b,d,i,l)
term(7) = term(7) + s1(b,k) * s1(c,j) * s1(d,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(8) = term(8) + s1(b,k) * s1(c,j) * s1(d,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(9) = term(9) + s1(b,j) * s1(c,k) * s1(d,l) * t2(a,c,j,l) * t2(b,d,i,k)
term(10) = term(10) + s1(b,k) * s1(c,l) * s1(d,j) * t2(a,c,j,l) * t2(b,d,i,k)
term(11) = term(11) + s1(b,j) * s1(c,k) * s1(d,l) * t2(a,c,j,k) * t2(b,d,i,l)
term(12) = term(12) + s1(c,k) * s1(b,j) * s1(d,l) * t2(a,d,j,l) * t2(b,c,i,k)
end do 
end do 
end do 
end do 
end do 
end do 

term(0) = -term(0) 
term(1) = term(1) * 1.9999999999999996d+0 
term(2) = -term(2) 
term(3) = term(3) * 1.9999999999999996d+0 
term(4) = term(4) * 1.9999999999999996d+0 
term(5) = term(5) * (-2.999999999999999d+0) 
term(6) = -term(6) 
term(7) = term(7) * 1.9999999999999996d+0 
term(8) = -term(8) 
term(9) = term(9) * 1.9999999999999996d+0 
term(10) = term(10) * 1.9999999999999996d+0 
term(11) = term(11) * (-3.999999999999999d+0) 
term(12) = -term(12) 


    calc_D_ov_ground_mbpt8_cc3 = 0.d+0 
    do s = 0, 12
    calc_D_ov_ground_mbpt8_cc3 = calc_D_ov_ground_mbpt8_cc3 + term(s)
    end do

    end function calc_D_ov_ground_mbpt8_cc3
    
    function calc_D_vv_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
    double precision :: calc_D_vv_ground_mbpt8_cc3
    integer, intent(in) :: dm_nocc, nactive
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
    double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
    integer, intent(in) :: a,b 
    integer :: s ,i,j,c,k,d 
    double precision, dimension(0:5) :: term 
    term = 0.d+0 
    do i = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
term(0) = term(0) + s1(c,j) * s1(d,k) * s1(a,i) * t3(dm_nocc, nactive, b,c,d,j,k,i)
term(1) = term(1) + s1(c,k) * s1(d,j) * s1(a,i) * t3(dm_nocc, nactive, b,c,d,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(1) = term(1) * (-2.0000000000000004d+0) 

do j = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do k = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
term(2) = term(2) + s1(c,j) * s1(d,k) * s1(a,i) * t3(dm_nocc, nactive, b,c,d,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(2) = term(2) * (-2.0000000000000004d+0) 

do k = 1, dm_nocc 
do j = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do i = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
term(3) = term(3) + s1(c,k) * s1(d,j) * s1(a,i) * t3(dm_nocc, nactive, b,c,d,j,i,k)
term(4) = term(4) + s1(c,j) * s1(d,k) * s1(a,i) * t3(dm_nocc, nactive, b,c,d,j,i,k)
end do 
end do 
end do 
end do 
end do 

term(4) = term(4) * (-2.0000000000000004d+0) 

do k = 1, dm_nocc 
do i = 1, dm_nocc 
do c = dm_nocc + 1, nactive 
do j = 1, dm_nocc 
do d = dm_nocc + 1, nactive 
term(5) = term(5) + s1(c,j) * s1(d,k) * s1(a,i) * t3(dm_nocc, nactive, b,c,d,i,j,k)
end do 
end do 
end do 
end do 
end do 

term(5) = term(5) * 4.000000000000001d+0 


    calc_D_vv_ground_mbpt8_cc3 = 0.d+0 
    do s = 0, 5
    calc_D_vv_ground_mbpt8_cc3 = calc_D_vv_ground_mbpt8_cc3 + term(s)
    end do

    end function calc_D_vv_ground_mbpt8_cc3



    function antiherm(dmao, n)
          real(F64) :: antiherm
          real(F64), dimension(:,:), intent(in) :: dmao
          integer, intent(in) :: n
          integer :: p,q

          antiherm = zero
          do p = 1, n
                do q = 1, n
                      antiherm = antiherm + (dmao(p, q) - dmao(q, p))**2
                end do
          end do

          antiherm = sqrt(antiherm)

    end function antiherm


!--------------------------------------------------------------------------------------------------------------------------------------------------------

     ! function calc_D_oo_diag_ground_mbpt0(t2, t1, s2, s1, dm_nocc, nactive, i) 
     !       double precision :: calc_D_oo_diag_ground_mbpt0
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = one

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_oo_diag_ground_mbpt0 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_oo_diag_ground_mbpt0 = calc_D_oo_diag_ground_mbpt0 + term(s)
     !       end do

     ! end function calc_D_oo_diag_ground_mbpt0

     ! function calc_D_oo_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt2
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,k,b,a 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,b,k,j) * t2(a,b,i,k)
     !                         term(1) = term(1) + s2(a,b,j,k) * t2(a,b,i,k)
     !                         term(2) = term(2) + s2(a,b,j,k) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(3) = term(3) + s2(a,b,k,j) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 


     !       calc_D_oo_ground_mbpt2 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_oo_ground_mbpt2 = calc_D_oo_ground_mbpt2 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt2

     ! function calc_D_ov_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt2
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = term(0) + t1(a,i)

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_ov_ground_mbpt2 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_ov_ground_mbpt2 = calc_D_ov_ground_mbpt2 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt2

     ! function calc_D_vo_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
     !       double precision :: calc_D_vo_ground_mbpt2
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = term(0) + s1(a,i)

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_vo_ground_mbpt2 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_vo_ground_mbpt2 = calc_D_vo_ground_mbpt2 + term(s)
     !       end do

     ! end function calc_D_vo_ground_mbpt2

     ! function calc_D_vv_ground_mbpt2(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt2
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,c,i 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do i = 1, dm_nocc 
     !                         term(0) = term(0) + s2(a,c,i,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 

     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(1) = term(1) + s2(a,c,i,j) * t2(c,b,j,i)
     !                         term(2) = term(2) + s2(c,a,i,j) * t2(c,b,j,i)
     !                         term(3) = term(3) + s2(c,a,i,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * 2.0d+0 
     !       term(2) = -term(2) 
     !       term(3) = term(3) * 2.0d+0 


     !       calc_D_vv_ground_mbpt2 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_vv_ground_mbpt2 = calc_D_vv_ground_mbpt2 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt2

     ! function calc_D_oo_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive) 
     !       double precision :: calc_D_oo_ground_mbpt3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 

     !       calc_D_oo_ground_mbpt3 = 0.d+0 

     ! end function calc_D_oo_ground_mbpt3

     ! function calc_D_ov_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,b 
     !       double precision, dimension(0:1) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   term(0) = term(0) + s1(b,j) * t2(b,a,j,i)
     !                   term(1) = term(1) + s1(b,j) * t2(b,a,i,j)
     !             end do
     !       end do

     !       term(0) = term(0) * 4.0d+0 
     !       term(1) = term(1) * (-2.0d+0) 


     !       calc_D_ov_ground_mbpt3 = 0.d+0 
     !       do s = 0, 1
     !             calc_D_ov_ground_mbpt3 = calc_D_ov_ground_mbpt3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt3

     ! function calc_D_oo_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt4
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,a 
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       do a = dm_nocc + 1, nactive 
     !             term(0) = term(0) + s1(a,j) * t1(a,i)
     !       end do

     !       term(0) = term(0) * (-2.0d+0) 


     !       calc_D_oo_ground_mbpt4 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_oo_ground_mbpt4 = calc_D_oo_ground_mbpt4 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt4

     ! function calc_D_ov_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt4
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,k,j,b,c 
     !       double precision, dimension(0:7) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(0) = term(0) + s2(c,b,k,j) * t1(a,j) * t2(c,b,i,k)
     !                               term(1) = term(1) + s2(c,b,k,j) * t1(b,i) * t2(c,a,j,k)
     !                               term(2) = term(2) + s2(c,b,k,j) * t1(c,i) * t2(b,a,j,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * (-2.0d+0) 

     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(3) = term(3) + s2(c,b,k,j) * t1(a,k) * t2(c,b,i,j)
     !                               term(4) = term(4) + s2(c,b,k,j) * t1(a,k) * t2(c,b,j,i)
     !                               term(5) = term(5) + s2(c,b,k,j) * t1(a,j) * t2(c,b,k,i)
     !                               term(6) = term(6) + s2(c,b,k,j) * t1(b,i) * t2(c,a,k,j)
     !                               term(7) = term(7) + s2(c,b,k,j) * t1(c,i) * t2(b,a,k,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 
     !       term(5) = term(5) * (-2.0d+0) 
     !       term(6) = term(6) * (-2.0d+0) 


     !       calc_D_ov_ground_mbpt4 = 0.d+0 
     !       do s = 0, 7
     !             calc_D_ov_ground_mbpt4 = calc_D_ov_ground_mbpt4 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt4

     ! function calc_D_vv_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt4
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,i 
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       do i = 1, dm_nocc 
     !             term(0) = term(0) + s1(a,i) * t1(b,i)
     !       end do

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_vv_ground_mbpt4 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_vv_ground_mbpt4 = calc_D_vv_ground_mbpt4 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt4

     ! function calc_D_oo_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt5
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,k,b,a 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s1(a,k) * s1(b,j) * t2(a,b,i,k)
     !                         term(1) = term(1) + s1(a,j) * s1(b,k) * t2(a,b,i,k)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(2) = term(2) + s1(a,j) * s1(b,k) * t2(a,b,k,i)
     !                         term(3) = term(3) + s1(a,k) * s1(b,j) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 


     !       calc_D_oo_ground_mbpt5 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_oo_ground_mbpt5 = calc_D_oo_ground_mbpt5 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt5

     ! function calc_D_ov_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt5
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,d,b,l,k,c 
     !       double precision, dimension(0:47) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do d = dm_nocc + 1, nactive 
     !                   do b = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do k = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(0) = term(0) + s1(d,l) * s2(c,b,k,j) * t2(c,d,l,i) * t2(b,a,k,j)
     !                                           term(1) = term(1) + s1(d,l) * s2(c,b,k,j) * t2(c,d,k,i) * t2(b,a,l,j)
     !                                           term(2) = term(2) + s1(d,l) * s2(c,b,k,j) * t2(c,a,l,j) * t2(b,d,k,i)
     !                                           term(3) = term(3) + s1(d,l) * s2(c,b,k,j) * t2(c,a,k,j) * t2(b,d,l,i)
     !                                           term(4) = term(4) + s1(d,l) * s2(c,b,k,j) * t2(a,d,k,j) * t2(c,b,l,i)
     !                                           term(5) = term(5) + s1(d,l) * s2(c,b,k,j) * t2(a,d,l,j) * t2(c,b,k,i)
     !                                           term(6) = term(6) + s1(d,l) * s2(c,b,k,j) * t2(c,b,l,i) * t2(a,d,k,j)
     !                                           term(7) = term(7) + s1(d,l) * s2(c,b,k,j) * t2(c,b,k,i) * t2(a,d,l,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 
     !       term(1) = term(1) * 2.0d+0 
     !       term(2) = -term(2) 
     !       term(3) = term(3) * 2.0d+0 
     !       term(4) = term(4) * (-0.75d+0) 
     !       term(5) = term(5) * 1.5d+0 
     !       term(6) = term(6) * (-0.25d+0) 
     !       term(7) = term(7) * 0.5d+0 

     !       do l = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               do k = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(8) = term(8) + s1(d,l) * s2(c,b,k,j) * t2(c,d,j,i) * t2(b,a,k,l)
     !                                           term(9) = term(9) + s1(d,l) * s2(c,b,k,j) * t2(c,d,i,j) * t2(b,a,k,l)
     !                                           term(10) = term(10) + s1(d,l) * s2(c,b,k,j) * t2(c,d,i,l) * t2(b,a,k,j)
     !                                           term(11) = term(11) + s1(d,l) * s2(c,b,k,j) * t2(c,d,k,i) * t2(b,a,j,l)
     !                                           term(12) = term(12) + s1(d,l) * s2(c,b,k,j) * t2(c,b,i,l) * t2(a,d,k,j)
     !                                           term(13) = term(13) + s1(d,l) * s2(c,b,k,j) * t2(c,b,i,j) * t2(a,d,k,l)
     !                                           term(14) = term(14) + s1(d,l) * s2(c,b,k,j) * t2(c,a,j,l) * t2(b,d,k,i)
     !                                           term(15) = term(15) + s1(d,l) * s2(c,b,k,j) * t2(c,a,k,l) * t2(b,d,i,j)
     !                                           term(16) = term(16) + s1(d,l) * s2(c,b,k,j) * t2(c,a,k,j) * t2(b,d,i,l)
     !                                           term(17) = term(17) + s1(d,l) * s2(c,b,k,j) * t2(c,a,k,l) * t2(b,d,j,i)
     !                                           term(18) = term(18) + s1(d,l) * s2(c,b,k,j) * t2(a,d,k,l) * t2(c,b,j,i)
     !                                           term(19) = term(19) + s1(d,l) * s2(c,b,k,j) * t2(a,d,j,l) * t2(c,b,k,i)
     !                                           term(20) = term(20) + s1(d,l) * s2(c,b,k,j) * t2(a,d,k,j) * t2(c,b,i,l)
     !                                           term(21) = term(21) + s1(d,l) * s2(c,b,k,j) * t2(a,d,k,l) * t2(c,b,i,j)
     !                                           term(22) = term(22) + s1(d,l) * s2(c,b,k,j) * t2(c,b,j,i) * t2(a,d,k,l)
     !                                           term(23) = term(23) + s1(d,l) * s2(c,b,k,j) * t2(c,b,k,i) * t2(a,d,j,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = term(8) * 2.0d+0 
     !       term(9) = -term(9) 
     !       term(10) = term(10) * 2.0d+0 
     !       term(11) = term(11) * (-4.0d+0) 
     !       term(12) = term(12) * 1.5d+0 
     !       term(13) = term(13) * (-3.0d+0) 
     !       term(14) = term(14) * 2.0d+0 
     !       term(15) = term(15) * 2.0d+0 
     !       term(16) = term(16) * (-4.0d+0) 
     !       term(17) = term(17) * (-4.0d+0) 
     !       term(18) = term(18) * 1.5d+0 
     !       term(19) = term(19) * (-3.0d+0) 
     !       term(20) = term(20) * 0.5d+0 
     !       term(21) = -term(21) 
     !       term(22) = term(22) * 0.5d+0 
     !       term(23) = -term(23) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               do l = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(24) = term(24) + s1(d,l) * s2(c,b,k,j) * t2(c,d,j,i) * t2(b,a,l,k)
     !                                           term(25) = term(25) + s1(d,l) * s2(c,b,k,j) * t2(c,d,l,i) * t2(b,a,j,k)
     !                                           term(26) = term(26) + s1(d,l) * s2(c,b,k,j) * t2(c,d,i,k) * t2(b,a,l,j)
     !                                           term(27) = term(27) + s1(d,l) * s2(c,b,k,j) * t2(c,d,i,j) * t2(b,a,l,k)
     !                                           term(28) = term(28) + s1(d,l) * s2(c,b,k,j) * t2(c,b,i,k) * t2(a,d,l,j)
     !                                           term(29) = term(29) + s1(d,l) * s2(c,b,k,j) * t2(c,b,i,j) * t2(a,d,l,k)
     !                                           term(30) = term(30) + s1(d,l) * s2(c,b,k,j) * t2(c,a,j,k) * t2(b,d,l,i)
     !                                           term(31) = term(31) + s1(d,l) * s2(c,b,k,j) * t2(c,a,l,k) * t2(b,d,i,j)
     !                                           term(32) = term(32) + s1(d,l) * s2(c,b,k,j) * t2(c,a,l,j) * t2(b,d,i,k)
     !                                           term(33) = term(33) + s1(d,l) * s2(c,b,k,j) * t2(c,a,l,k) * t2(b,d,j,i)
     !                                           term(34) = term(34) + s1(d,l) * s2(c,b,k,j) * t2(a,d,l,k) * t2(c,b,j,i)
     !                                           term(35) = term(35) + s1(d,l) * s2(c,b,k,j) * t2(a,d,j,k) * t2(c,b,l,i)
     !                                           term(36) = term(36) + s1(d,l) * s2(c,b,k,j) * t2(a,d,l,j) * t2(c,b,i,k)
     !                                           term(37) = term(37) + s1(d,l) * s2(c,b,k,j) * t2(a,d,l,k) * t2(c,b,i,j)
     !                                           term(38) = term(38) + s1(d,l) * s2(c,b,k,j) * t2(c,b,j,i) * t2(a,d,l,k)
     !                                           term(39) = term(39) + s1(d,l) * s2(c,b,k,j) * t2(c,b,l,i) * t2(a,d,j,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(24) = -term(24) 
     !       term(25) = term(25) * 2.0d+0 
     !       term(26) = -term(26) 
     !       term(27) = term(27) * 2.0d+0 
     !       term(28) = term(28) * (-0.75d+0) 
     !       term(29) = term(29) * 1.5d+0 
     !       term(30) = -term(30) 
     !       term(31) = -term(31) 
     !       term(32) = term(32) * 2.0d+0 
     !       term(33) = term(33) * 2.0d+0 
     !       term(34) = term(34) * (-0.75d+0) 
     !       term(35) = term(35) * 1.5d+0 
     !       term(36) = term(36) * (-0.25d+0) 
     !       term(37) = term(37) * 0.5d+0 
     !       term(38) = term(38) * (-0.25d+0) 
     !       term(39) = term(39) * 0.5d+0 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do j = 1, dm_nocc 
     !                         do d = dm_nocc + 1, nactive 
     !                               do b = dm_nocc + 1, nactive 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(40) = term(40) + s1(d,l) * s2(c,b,k,j) * t2(c,d,i,k) * t2(b,a,j,l)
     !                                           term(41) = term(41) + s1(d,l) * s2(c,b,k,j) * t2(c,d,i,l) * t2(b,a,j,k)
     !                                           term(42) = term(42) + s1(d,l) * s2(c,b,k,j) * t2(c,b,i,l) * t2(a,d,j,k)
     !                                           term(43) = term(43) + s1(d,l) * s2(c,b,k,j) * t2(c,b,i,k) * t2(a,d,j,l)
     !                                           term(44) = term(44) + s1(d,l) * s2(c,b,k,j) * t2(c,a,j,l) * t2(b,d,i,k)
     !                                           term(45) = term(45) + s1(d,l) * s2(c,b,k,j) * t2(c,a,j,k) * t2(b,d,i,l)
     !                                           term(46) = term(46) + s1(d,l) * s2(c,b,k,j) * t2(a,d,j,l) * t2(c,b,i,k)
     !                                           term(47) = term(47) + s1(d,l) * s2(c,b,k,j) * t2(a,d,j,k) * t2(c,b,i,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(40) = term(40) * 2.0d+0 
     !       term(41) = term(41) * (-4.0d+0) 
     !       term(42) = term(42) * (-0.75d+0) 
     !       term(43) = term(43) * 1.5d+0 
     !       term(44) = -term(44) 
     !       term(45) = term(45) * 2.0d+0 
     !       term(46) = term(46) * 0.5d+0 
     !       term(47) = term(47) * (-0.25d+0) 


     !       calc_D_ov_ground_mbpt5 = 0.d+0 
     !       do s = 0, 47
     !             calc_D_ov_ground_mbpt5 = calc_D_ov_ground_mbpt5 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt5


     ! function calc_D_vv_ground_mbpt5(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt5
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,i,c 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s1(a,i) * s1(c,j) * t2(c,b,i,j)
     !                         term(1) = term(1) + s1(c,i) * s1(a,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 
     !       term(1) = term(1) * 2.0d+0 

     !       do i = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(2) = term(2) + s1(a,i) * s1(c,j) * t2(c,b,j,i)
     !                         term(3) = term(3) + s1(c,i) * s1(a,j) * t2(c,b,j,i)
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * 2.0d+0 
     !       term(3) = -term(3) 


     !       calc_D_vv_ground_mbpt5 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_vv_ground_mbpt5 = calc_D_vv_ground_mbpt5 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt5


     ! function calc_D_ov_ground_mbpt6(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt6
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,b 
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   term(0) = term(0) + s1(b,j) * t1(b,i) * t1(a,j)
     !             end do
     !       end do

     !       term(0) = term(0) * (-2.0d+0) 


     !       calc_D_ov_ground_mbpt6 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_ov_ground_mbpt6 = calc_D_ov_ground_mbpt6 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt6

     ! function calc_D_ov_ground_mbpt7(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt7
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,k,b,j,c 
     !       double precision, dimension(0:7) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do j = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(0) = term(0) + s1(c,k) * s1(b,j) * t1(a,j) * t2(c,b,i,k)
     !                         end do
     !                   end do
     !             end do
     !       end do


     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(1) = term(1) + s1(c,k) * s1(b,j) * t1(a,k) * t2(c,b,i,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do j = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(2) = term(2) + s1(c,k) * s1(b,j) * t1(a,k) * t2(c,b,j,i)
     !                               term(3) = term(3) + s1(c,k) * s1(b,j) * t1(a,j) * t2(c,b,k,i)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(4) = term(4) + s1(c,k) * s1(b,j) * t1(b,i) * t2(c,a,j,k)
     !                               term(5) = term(5) + s1(c,k) * s1(b,j) * t1(c,i) * t2(b,a,j,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(5) = term(5) * (-2.0d+0) 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(6) = term(6) + s1(c,k) * s1(b,j) * t1(b,i) * t2(c,a,k,j)
     !                               term(7) = term(7) + s1(c,k) * s1(b,j) * t1(c,i) * t2(b,a,k,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * (-2.0d+0) 


     !       calc_D_ov_ground_mbpt7 = 0.d+0 
     !       do s = 0, 7
     !             calc_D_ov_ground_mbpt7 = calc_D_ov_ground_mbpt7 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt7


     ! function calc_D_ov_ground_mbpt8(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt8
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,d,l,k,c,b 
     !       double precision, dimension(0:23) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do d = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do k = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(0) = term(0) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,l,i) * t2(b,a,k,j)
     !                                           term(1) = term(1) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,k,i) * t2(b,a,l,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = term(0) * (-0.7499999999999998d+0) 
     !       term(1) = term(1) * 1.4999999999999996d+0 

     !       do l = 1, dm_nocc 
     !             do d = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(2) = term(2) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,j,i) * t2(b,a,k,l)
     !                                           term(3) = term(3) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,k,i) * t2(b,a,j,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * 1.4999999999999996d+0 
     !       term(3) = term(3) * (-2.999999999999999d+0) 

     !       do k = 1, dm_nocc 
     !             do d = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(4) = term(4) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,j,i) * t2(b,a,l,k)
     !                                           term(5) = term(5) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,l,i) * t2(b,a,j,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * (-0.7499999999999998d+0) 
     !       term(5) = term(5) * 1.4999999999999996d+0 

     !       do j = 1, dm_nocc 
     !             do d = dm_nocc + 1, nactive 
     !                   do b = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do k = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(6) = term(6) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,k,j) * t2(c,b,l,i)
     !                                           term(7) = term(7) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,l,j) * t2(c,b,k,i)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * (-0.7499999999999998d+0) 
     !       term(7) = term(7) * 1.4999999999999996d+0 

     !       do k = 1, dm_nocc 
     !             do d = dm_nocc + 1, nactive 
     !                   do b = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do j = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(8) = term(8) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,l,k) * t2(c,b,j,i)
     !                                           term(9) = term(9) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,j,k) * t2(c,b,l,i)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = term(8) * (-0.7499999999999998d+0) 
     !       term(9) = term(9) * 1.4999999999999996d+0 

     !       do l = 1, dm_nocc 
     !             do d = dm_nocc + 1, nactive 
     !                   do b = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do j = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(10) = term(10) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,k,l) * t2(c,b,j,i)
     !                                           term(11) = term(11) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,j,l) * t2(c,b,k,i)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(10) = term(10) * 1.4999999999999996d+0 
     !       term(11) = term(11) * (-2.999999999999999d+0) 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               do j = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(12) = term(12) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,j,k) * t2(c,b,i,l)
     !                                           term(13) = term(13) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,j,l) * t2(c,b,i,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(12) = term(12) * (-0.24999999999999997d+0) 
     !       term(13) = term(13) * 0.49999999999999994d+0 

     !       do l = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               do k = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(14) = term(14) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,k,j) * t2(c,b,i,l)
     !                                           term(15) = term(15) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,k,l) * t2(c,b,i,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(14) = term(14) * 0.49999999999999994d+0 
     !       term(15) = -term(15) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               do l = 1, dm_nocc 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(16) = term(16) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,l,j) * t2(c,b,i,k)
     !                                           term(17) = term(17) + s1(d,l) * s1(c,k) * s1(b,j) * t2(a,d,l,k) * t2(c,b,i,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(16) = term(16) * (-0.24999999999999997d+0) 
     !       term(17) = term(17) * 0.49999999999999994d+0 

     !       do l = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(18) = term(18) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,i,j) * t2(b,a,k,l)
     !                                           term(19) = term(19) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,i,l) * t2(b,a,k,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(18) = term(18) * (-0.24999999999999997d+0) 
     !       term(19) = term(19) * 0.49999999999999994d+0 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(20) = term(20) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,i,k) * t2(b,a,j,l)
     !                                           term(21) = term(21) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,i,l) * t2(b,a,j,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(20) = term(20) * 0.49999999999999994d+0 
     !       term(21) = -term(21) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(22) = term(22) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,i,k) * t2(b,a,l,j)
     !                                           term(23) = term(23) + s1(d,l) * s1(c,k) * s1(b,j) * t2(c,d,i,j) * t2(b,a,l,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(22) = term(22) * (-0.24999999999999997d+0) 
     !       term(23) = term(23) * 0.49999999999999994d+0 


     !       calc_D_ov_ground_mbpt8 = 0.d+0 
     !       do s = 0, 23
     !             calc_D_ov_ground_mbpt8 = calc_D_ov_ground_mbpt8 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt8


     ! function calc_D_oo_diag_ground_mbpt0_cc3(t2, t1, s2, s1, dm_nocc, nactive, i) 
     !       double precision :: calc_D_oo_diag_ground_mbpt0_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = one

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_oo_diag_ground_mbpt0_cc3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_oo_diag_ground_mbpt0_cc3 = calc_D_oo_diag_ground_mbpt0_cc3 + term(s)
     !       end do
     ! end function calc_D_oo_diag_ground_mbpt0_cc3

     ! function calc_D_oo_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt2_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,k,b,a 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,b,k,j) * t2(a,b,i,k)
     !                         term(1) = term(1) + s2(a,b,j,k) * t2(a,b,i,k)
     !                         term(2) = term(2) + s2(a,b,j,k) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(3) = term(3) + s2(a,b,k,j) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 


     !       calc_D_oo_ground_mbpt2_cc3 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_oo_ground_mbpt2_cc3 = calc_D_oo_ground_mbpt2_cc3 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt2_cc3

     ! function calc_D_ov_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt2_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = term(0) + t1(a,i)

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_ov_ground_mbpt2_cc3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_ov_ground_mbpt2_cc3 = calc_D_ov_ground_mbpt2_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt2_cc3

     ! function calc_D_vo_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
     !       double precision :: calc_D_vo_ground_mbpt2_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = term(0) + s1(a,i)

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_vo_ground_mbpt2_cc3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_vo_ground_mbpt2_cc3 = calc_D_vo_ground_mbpt2_cc3 + term(s)
     !       end do

     ! end function calc_D_vo_ground_mbpt2_cc3

     ! function calc_D_vv_ground_mbpt2_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt2_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,c,i 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do i = 1, dm_nocc 
     !                         term(0) = term(0) + s2(a,c,i,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 

     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(1) = term(1) + s2(a,c,i,j) * t2(c,b,j,i)
     !                         term(2) = term(2) + s2(c,a,i,j) * t2(c,b,j,i)
     !                         term(3) = term(3) + s2(c,a,i,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * 2.0d+0 
     !       term(2) = -term(2) 
     !       term(3) = term(3) * 2.0d+0 


     !       calc_D_vv_ground_mbpt2_cc3 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_vv_ground_mbpt2_cc3 = calc_D_vv_ground_mbpt2_cc3 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt2_cc3


     ! function calc_D_ov_ground_mbpt3_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt3_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,b,k,c 
     !       double precision, dimension(0:7) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(0) = term(0) + s2(c,b,k,j) * t3(dm_nocc, nactive, c,b,a,j,k,i)
     !                               term(1) = term(1) + s2(c,b,k,j) * t3(dm_nocc, nactive, c,b,a,i,k,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = term(0) * (-1.9999999999999998d+0) 

     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do j = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(2) = term(2) + s2(c,b,k,j) * t3(dm_nocc, nactive, c,b,a,k,j,i)
     !                               term(3) = term(3) + s2(c,b,k,j) * t3(dm_nocc, nactive, c,b,a,i,j,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * 3.9999999999999996d+0 
     !       term(3) = term(3) * (-1.9999999999999998d+0) 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(4) = term(4) + s2(c,b,k,j) * t3(dm_nocc, nactive, c,b,a,k,i,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * (-1.9999999999999998d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(5) = term(5) + s2(c,b,k,j) * t3(dm_nocc, nactive, c,b,a,j,i,k)
     !                         end do
     !                   end do
     !             end do
     !       end do


     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   term(6) = term(6) + s1(b,j) * t2(b,a,j,i)
     !                   term(7) = term(7) + s1(b,j) * t2(b,a,i,j)
     !             end do
     !       end do

     !       term(6) = term(6) * 4.0d+0 
     !       term(7) = term(7) * (-2.0d+0) 


     !       calc_D_ov_ground_mbpt3_cc3 = 0.d+0 
     !       do s = 0, 7
     !             calc_D_ov_ground_mbpt3_cc3 = calc_D_ov_ground_mbpt3_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt3_cc3


     ! function calc_D_oo_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt4_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,l,k,b,c,a 
     !       double precision, dimension(0:18) :: term 
     !       term = 0.d+0 
     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(0) = term(0) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(1) = term(1) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(2) = term(2) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(3) = term(3) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(4) = term(4) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(5) = term(5) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(6) = term(6) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = term(0) * (-0.33333333333333337d+0) 
     !       term(1) = term(1) * 0.6666666666666667d+0 
     !       term(2) = term(2) * (-0.33333333333333337d+0) 
     !       term(3) = term(3) * (-0.33333333333333337d+0) 
     !       term(4) = term(4) * 0.6666666666666667d+0 
     !       term(5) = term(5) * 0.6666666666666667d+0 
     !       term(6) = term(6) * 0.6666666666666667d+0 

     !       do a = dm_nocc + 1, nactive 
     !             term(7) = term(7) + s1(a,j) * t1(a,i)
     !       end do

     !       term(7) = term(7) * (-2.0d+0) 

     !       do k = 1, dm_nocc 
     !             do l = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(8) = term(8) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(9) = term(9) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(10) = term(10) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(11) = term(11) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(12) = term(12) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                                     term(13) = term(13) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                                     term(14) = term(14) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = term(8) * 0.6666666666666667d+0 
     !       term(9) = term(9) * (-0.33333333333333337d+0) 
     !       term(10) = term(10) * 0.6666666666666667d+0 
     !       term(11) = term(11) * (-1.3333333333333335d+0) 
     !       term(12) = term(12) * (-0.33333333333333337d+0) 
     !       term(13) = term(13) * 0.6666666666666667d+0 
     !       term(14) = term(14) * (-0.33333333333333337d+0) 

     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(15) = term(15) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(16) = term(16) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(17) = term(17) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                                     term(18) = term(18) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(15) = term(15) * 0.6666666666666667d+0 
     !       term(16) = term(16) * (-1.3333333333333335d+0) 
     !       term(17) = term(17) * 0.6666666666666667d+0 
     !       term(18) = term(18) * (-1.3333333333333335d+0) 


     !       calc_D_oo_ground_mbpt4_cc3 = 0.d+0 
     !       do s = 0, 18
     !             calc_D_oo_ground_mbpt4_cc3 = calc_D_oo_ground_mbpt4_cc3 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt4_cc3

     ! function calc_D_ov_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt4_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,k,j,b,c,l,d 
     !       double precision, dimension(0:31) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(0) = term(0) + s2(c,b,k,j) * t1(a,j) * t2(c,b,i,k)
     !                               term(1) = term(1) + s2(c,b,k,j) * t1(b,i) * t2(c,a,j,k)
     !                               term(2) = term(2) + s2(c,b,k,j) * t1(c,i) * t2(b,a,j,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * (-2.0d+0) 

     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(3) = term(3) + s2(c,b,k,j) * t1(a,k) * t2(c,b,i,j)
     !                               term(4) = term(4) + s2(c,b,k,j) * t1(a,k) * t2(c,b,j,i)
     !                               term(5) = term(5) + s2(c,b,k,j) * t1(a,j) * t2(c,b,k,i)
     !                               term(6) = term(6) + s2(c,b,k,j) * t1(b,i) * t2(c,a,k,j)
     !                               term(7) = term(7) + s2(c,b,k,j) * t1(c,i) * t2(b,a,k,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 
     !       term(5) = term(5) * (-2.0d+0) 
     !       term(6) = term(6) * (-2.0d+0) 

     !       do k = 1, dm_nocc 
     !             do l = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     do d = dm_nocc + 1, nactive 
     !                                           term(8) = term(8) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,i,l) * t2(c,a,j,k)
     !                                           term(9) = term(9) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,i,k) * t2(c,a,j,l)
     !                                           term(10) = term(10) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,i,k) * t2(c,a,l,j)
     !                                           term(11) = term(11) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,i,j) * t2(c,a,l,k)
     !                                           term(12) = term(12) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,i,l) * t2(c,a,k,j)
     !                                           term(13) = term(13) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,i,j) * t2(c,a,k,l)
     !                                           term(14) = term(14) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,k,j) * t2(b,c,l,i)
     !                                           term(15) = term(15) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,l,j) * t2(b,c,k,i)
     !                                           term(16) = term(16) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,k,j) * t2(b,c,i,l)
     !                                           term(17) = term(17) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,k,l) * t2(b,c,i,j)
     !                                           term(18) = term(18) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,l,k) * t2(b,c,i,j)
     !                                           term(19) = term(19) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,l,j) * t2(b,c,i,k)
     !                                           term(20) = term(20) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,k,i) * t2(c,a,j,l)
     !                                           term(21) = term(21) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,j,i) * t2(c,a,k,l)
     !                                           term(22) = term(22) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,j,i) * t2(c,a,l,k)
     !                                           term(23) = term(23) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,l,i) * t2(c,a,j,k)
     !                                           term(24) = term(24) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,k,i) * t2(c,a,l,j)
     !                                           term(25) = term(25) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,b,l,i) * t2(c,a,k,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = term(8) * (-0.7499999999999998d+0) 
     !       term(9) = term(9) * 1.4999999999999996d+0 
     !       term(10) = term(10) * (-0.7499999999999998d+0) 
     !       term(11) = term(11) * 1.4999999999999996d+0 
     !       term(12) = term(12) * 1.4999999999999996d+0 
     !       term(13) = term(13) * (-2.999999999999999d+0) 
     !       term(14) = term(14) * (-0.7499999999999998d+0) 
     !       term(15) = term(15) * 1.4999999999999996d+0 
     !       term(16) = term(16) * 0.49999999999999994d+0 
     !       term(17) = term(17) * (-0.24999999999999997d+0) 
     !       term(18) = term(18) * 0.49999999999999994d+0 
     !       term(19) = -term(19) 
     !       term(20) = term(20) * (-0.24999999999999997d+0) 
     !       term(21) = term(21) * 0.49999999999999994d+0 
     !       term(22) = term(22) * (-0.24999999999999997d+0) 
     !       term(23) = term(23) * 0.49999999999999994d+0 
     !       term(24) = term(24) * 0.49999999999999994d+0 
     !       term(25) = -term(25) 

     !       do k = 1, dm_nocc 
     !             do l = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do j = 1, dm_nocc 
     !                                     do d = dm_nocc + 1, nactive 
     !                                           term(26) = term(26) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,k,l) * t2(b,c,j,i)
     !                                           term(27) = term(27) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,j,l) * t2(b,c,k,i)
     !                                           term(28) = term(28) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,j,k) * t2(b,c,l,i)
     !                                           term(29) = term(29) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,l,k) * t2(b,c,j,i)
     !                                           term(30) = term(30) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,j,k) * t2(b,c,i,l)
     !                                           term(31) = term(31) + t3(dm_nocc, nactive, d,b,c,l,j,k) * t2(d,a,j,l) * t2(b,c,i,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(26) = term(26) * 1.4999999999999996d+0 
     !       term(27) = term(27) * (-0.7499999999999998d+0) 
     !       term(28) = term(28) * 1.4999999999999996d+0 
     !       term(29) = term(29) * (-2.999999999999999d+0) 
     !       term(30) = term(30) * (-0.24999999999999997d+0) 
     !       term(31) = term(31) * 0.49999999999999994d+0 


     !       calc_D_ov_ground_mbpt4_cc3 = 0.d+0 
     !       do s = 0, 31
     !             calc_D_ov_ground_mbpt4_cc3 = calc_D_ov_ground_mbpt4_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt4_cc3


     ! function calc_D_vv_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt4_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,k,j,i,d,c 
     !       double precision, dimension(0:12) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do i = 1, dm_nocc 
     !                         do d = dm_nocc + 1, nactive 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(0) = term(0) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,k,i,j)
     !                                     term(1) = term(1) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,i,k,j)
     !                                     term(2) = term(2) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,j,i,k)
     !                                     term(3) = term(3) + t3(dm_nocc, nactive, c,d,a,i,j,k) * t3(dm_nocc, nactive, c,d,b,k,i,j)
     !                                     term(4) = term(4) + t3(dm_nocc, nactive, c,d,a,i,j,k) * t3(dm_nocc, nactive, c,d,b,i,k,j)
     !                                     term(5) = term(5) + t3(dm_nocc, nactive, c,d,a,i,j,k) * t3(dm_nocc, nactive, c,d,b,j,i,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = term(0) * 0.6666666666666669d+0 
     !       term(1) = term(1) * (-1.3333333333333337d+0) 
     !       term(2) = term(2) * (-1.3333333333333337d+0) 
     !       term(3) = term(3) * 0.33333333333333337d+0 
     !       term(4) = term(4) * (-0.6666666666666667d+0) 
     !       term(5) = term(5) * (-0.6666666666666667d+0) 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do j = 1, dm_nocc 
     !                                     term(6) = term(6) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,k,j,i)
     !                                     term(7) = term(7) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,i,j,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * (-1.3333333333333337d+0) 
     !       term(7) = term(7) * 0.6666666666666669d+0 

     !       do i = 1, dm_nocc 
     !             term(8) = term(8) + s1(a,i) * t1(b,i)
     !       end do

     !       term(8) = term(8) * 2.0d+0 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do j = 1, dm_nocc 
     !                         do d = dm_nocc + 1, nactive 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(9) = term(9) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,j,k,i)
     !                                     term(10) = term(10) + t3(dm_nocc, nactive, c,d,a,i,j,k) * t3(dm_nocc, nactive, c,d,b,j,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(9) = term(9) * 2.6666666666666674d+0 
     !       term(10) = term(10) * 0.33333333333333337d+0 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(11) = term(11) + t3(dm_nocc, nactive, c,d,a,i,j,k) * t3(dm_nocc, nactive, c,d,b,k,j,i)
     !                                     term(12) = term(12) + t3(dm_nocc, nactive, c,d,a,i,j,k) * t3(dm_nocc, nactive, c,d,b,i,j,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(11) = term(11) * (-0.6666666666666667d+0) 
     !       term(12) = term(12) * 1.3333333333333335d+0 


     !       calc_D_vv_ground_mbpt4_cc3 = 0.d+0 
     !       do s = 0, 12
     !             calc_D_vv_ground_mbpt4_cc3 = calc_D_vv_ground_mbpt4_cc3 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt4_cc3


     ! function calc_D_oo_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt5_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,k,b,a,l,c 
     !       double precision, dimension(0:21) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s1(a,k) * s1(b,j) * t2(a,b,i,k)
     !                         term(1) = term(1) + s1(a,j) * s1(b,k) * t2(a,b,i,k)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(2) = term(2) + s1(a,j) * s1(b,k) * t2(a,b,k,i)
     !                         term(3) = term(3) + s1(a,k) * s1(b,j) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 

     !       do l = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(4) = term(4) + s1(a,l) * s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(5) = term(5) + s1(a,j) * s2(b,c,l,k) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(6) = term(6) + s1(a,j) * s2(b,c,l,k) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                                     term(7) = term(7) + s1(a,l) * s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = -term(4) 
     !       term(5) = term(5) * 1.9999999999999998d+0 
     !       term(6) = -term(6) 
     !       term(7) = term(7) * 1.9999999999999998d+0 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(8) = term(8) + s1(a,j) * s2(b,c,l,k) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(9) = term(9) + s1(a,k) * s2(b,c,l,j) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(10) = term(10) + s1(a,l) * s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = -term(8) 
     !       term(9) = term(9) * 1.9999999999999998d+0 
     !       term(10) = term(10) * 1.9999999999999998d+0 

     !       do l = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do c = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(11) = term(11) + s1(a,k) * s2(b,c,l,j) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(12) = term(12) + s1(a,k) * s2(b,c,l,j) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(11) = -term(11) 
     !       term(12) = term(12) * 1.9999999999999998d+0 

     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do c = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(13) = term(13) + s1(a,l) * s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(14) = term(14) + s1(a,k) * s2(b,c,l,j) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(15) = term(15) + s1(a,j) * s2(b,c,l,k) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(16) = term(16) + s1(a,l) * s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                                     term(17) = term(17) + s1(a,j) * s2(b,c,l,k) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                                     term(18) = term(18) + s1(a,k) * s2(b,c,l,j) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(13) = term(13) * 1.9999999999999998d+0 
     !       term(14) = term(14) * 1.9999999999999998d+0 
     !       term(15) = term(15) * (-3.9999999999999996d+0) 
     !       term(16) = -term(16) 
     !       term(17) = term(17) * 1.9999999999999998d+0 
     !       term(18) = term(18) * (-3.9999999999999996d+0) 

     !       do k = 1, dm_nocc 
     !             do l = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(19) = term(19) + s1(a,k) * s2(b,c,l,j) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(20) = term(20) + s1(a,j) * s2(b,c,l,k) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(21) = term(21) + s1(a,l) * s2(b,c,j,k) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(19) = -term(19) 
     !       term(20) = term(20) * 1.9999999999999998d+0 
     !       term(21) = term(21) * (-3.9999999999999996d+0) 


     !       calc_D_oo_ground_mbpt5_cc3 = 0.d+0 
     !       do s = 0, 21
     !             calc_D_oo_ground_mbpt5_cc3 = calc_D_oo_ground_mbpt5_cc3 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt5_cc3

     ! function calc_D_ov_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt5_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,l,k,c,b,j,d 
     !       double precision, dimension(0:47) :: term 
     !       term = 0.d+0 
     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               do j = 1, dm_nocc 
     !                                     do d = dm_nocc + 1, nactive 
     !                                           term(0) = term(0) + s1(d,l) * s2(b,c,j,k) * t2(d,b,i,l) * t2(c,a,j,k)
     !                                           term(1) = term(1) + s1(d,l) * s2(b,c,j,k) * t2(d,b,i,k) * t2(c,a,j,l)
     !                                           term(2) = term(2) + s1(d,l) * s2(b,c,j,k) * t2(d,b,k,i) * t2(c,a,j,l)
     !                                           term(3) = term(3) + s1(d,l) * s2(b,c,j,k) * t2(d,b,j,i) * t2(c,a,k,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 
     !       term(1) = term(1) * 2.0d+0 
     !       term(2) = -term(2) 
     !       term(3) = term(3) * 2.0d+0 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(4) = term(4) + s1(d,l) * s2(b,c,j,k) * t2(b,c,i,j) * t2(d,a,l,k)
     !                                           term(5) = term(5) + s1(d,l) * s2(b,c,j,k) * t2(b,c,i,k) * t2(d,a,l,j)
     !                                           term(6) = term(6) + s1(d,l) * s2(b,c,j,k) * t2(b,a,k,j) * t2(d,c,l,i)
     !                                           term(7) = term(7) + s1(d,l) * s2(b,c,j,k) * t2(b,a,l,j) * t2(d,c,k,i)
     !                                           term(8) = term(8) + s1(d,l) * s2(b,c,j,k) * t2(b,a,l,k) * t2(d,c,i,j)
     !                                           term(9) = term(9) + s1(d,l) * s2(b,c,j,k) * t2(b,a,l,j) * t2(d,c,i,k)
     !                                           term(10) = term(10) + s1(d,l) * s2(b,c,j,k) * t2(d,a,k,j) * t2(b,c,l,i)
     !                                           term(11) = term(11) + s1(d,l) * s2(b,c,j,k) * t2(d,a,l,j) * t2(b,c,k,i)
     !                                           term(12) = term(12) + s1(d,l) * s2(b,c,j,k) * t2(d,a,l,k) * t2(b,c,i,j)
     !                                           term(13) = term(13) + s1(d,l) * s2(b,c,j,k) * t2(d,a,l,j) * t2(b,c,i,k)
     !                                           term(14) = term(14) + s1(d,l) * s2(b,c,j,k) * t2(b,c,l,i) * t2(d,a,k,j)
     !                                           term(15) = term(15) + s1(d,l) * s2(b,c,j,k) * t2(b,c,k,i) * t2(d,a,l,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * 1.5d+0 
     !       term(5) = term(5) * (-3.0d+0) 
     !       term(6) = term(6) * 2.0d+0 
     !       term(7) = -term(7) 
     !       term(8) = -term(8) 
     !       term(9) = term(9) * 2.0d+0 
     !       term(10) = term(10) * (-0.75d+0) 
     !       term(11) = term(11) * 1.5d+0 
     !       term(12) = term(12) * 0.5d+0 
     !       term(13) = -term(13) 
     !       term(14) = term(14) * (-0.25d+0) 
     !       term(15) = term(15) * 0.5d+0 

     !       do k = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do b = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do j = 1, dm_nocc 
     !                                     do d = dm_nocc + 1, nactive 
     !                                           term(16) = term(16) + s1(d,l) * s2(b,c,j,k) * t2(d,b,l,i) * t2(c,a,j,k)
     !                                           term(17) = term(17) + s1(d,l) * s2(b,c,j,k) * t2(d,b,j,i) * t2(c,a,l,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(16) = term(16) * 2.0d+0 
     !       term(17) = -term(17) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               do l = 1, dm_nocc 
     !                                     do d = dm_nocc + 1, nactive 
     !                                           term(18) = term(18) + s1(d,l) * s2(b,c,j,k) * t2(d,b,i,k) * t2(c,a,l,j)
     !                                           term(19) = term(19) + s1(d,l) * s2(b,c,j,k) * t2(d,b,i,j) * t2(c,a,l,k)
     !                                           term(20) = term(20) + s1(d,l) * s2(b,c,j,k) * t2(d,b,k,i) * t2(c,a,l,j)
     !                                           term(21) = term(21) + s1(d,l) * s2(b,c,j,k) * t2(d,b,l,i) * t2(c,a,k,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(18) = -term(18) 
     !       term(19) = term(19) * 2.0d+0 
     !       term(20) = term(20) * 2.0d+0 
     !       term(21) = term(21) * (-4.0d+0) 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do j = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               do b = dm_nocc + 1, nactive 
     !                                     do d = dm_nocc + 1, nactive 
     !                                           term(22) = term(22) + s1(d,l) * s2(b,c,j,k) * t2(d,b,i,l) * t2(c,a,k,j)
     !                                           term(23) = term(23) + s1(d,l) * s2(b,c,j,k) * t2(d,b,i,j) * t2(c,a,k,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(22) = term(22) * 2.0d+0 
     !       term(23) = term(23) * (-4.0d+0) 

     !       do k = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(24) = term(24) + s1(d,l) * s2(b,c,j,k) * t2(b,a,l,k) * t2(d,c,j,i)
     !                                           term(25) = term(25) + s1(d,l) * s2(b,c,j,k) * t2(b,a,j,k) * t2(d,c,l,i)
     !                                           term(26) = term(26) + s1(d,l) * s2(b,c,j,k) * t2(d,a,j,k) * t2(b,c,l,i)
     !                                           term(27) = term(27) + s1(d,l) * s2(b,c,j,k) * t2(d,a,l,k) * t2(b,c,j,i)
     !                                           term(28) = term(28) + s1(d,l) * s2(b,c,j,k) * t2(b,c,l,i) * t2(d,a,j,k)
     !                                           term(29) = term(29) + s1(d,l) * s2(b,c,j,k) * t2(b,c,j,i) * t2(d,a,l,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(24) = term(24) * 2.0d+0 
     !       term(25) = term(25) * (-4.0d+0) 
     !       term(26) = term(26) * 1.5d+0 
     !       term(27) = term(27) * (-3.0d+0) 
     !       term(28) = term(28) * 0.5d+0 
     !       term(29) = -term(29) 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do j = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(30) = term(30) + s1(d,l) * s2(b,c,j,k) * t2(b,c,i,j) * t2(d,a,k,l)
     !                                           term(31) = term(31) + s1(d,l) * s2(b,c,j,k) * t2(b,c,i,l) * t2(d,a,k,j)
     !                                           term(32) = term(32) + s1(d,l) * s2(b,c,j,k) * t2(b,a,k,j) * t2(d,c,i,l)
     !                                           term(33) = term(33) + s1(d,l) * s2(b,c,j,k) * t2(b,a,k,l) * t2(d,c,i,j)
     !                                           term(34) = term(34) + s1(d,l) * s2(b,c,j,k) * t2(d,a,k,l) * t2(b,c,i,j)
     !                                           term(35) = term(35) + s1(d,l) * s2(b,c,j,k) * t2(d,a,k,j) * t2(b,c,i,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(30) = term(30) * (-0.75d+0) 
     !       term(31) = term(31) * 1.5d+0 
     !       term(32) = -term(32) 
     !       term(33) = term(33) * 2.0d+0 
     !       term(34) = term(34) * (-0.25d+0) 
     !       term(35) = term(35) * 0.5d+0 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(36) = term(36) + s1(d,l) * s2(b,c,j,k) * t2(b,c,i,l) * t2(d,a,j,k)
     !                                           term(37) = term(37) + s1(d,l) * s2(b,c,j,k) * t2(b,c,i,k) * t2(d,a,j,l)
     !                                           term(38) = term(38) + s1(d,l) * s2(b,c,j,k) * t2(b,a,k,l) * t2(d,c,j,i)
     !                                           term(39) = term(39) + s1(d,l) * s2(b,c,j,k) * t2(b,a,j,l) * t2(d,c,k,i)
     !                                           term(40) = term(40) + s1(d,l) * s2(b,c,j,k) * t2(b,a,j,k) * t2(d,c,i,l)
     !                                           term(41) = term(41) + s1(d,l) * s2(b,c,j,k) * t2(b,a,j,l) * t2(d,c,i,k)
     !                                           term(42) = term(42) + s1(d,l) * s2(b,c,j,k) * t2(d,a,k,l) * t2(b,c,j,i)
     !                                           term(43) = term(43) + s1(d,l) * s2(b,c,j,k) * t2(d,a,j,l) * t2(b,c,k,i)
     !                                           term(44) = term(44) + s1(d,l) * s2(b,c,j,k) * t2(d,a,j,k) * t2(b,c,i,l)
     !                                           term(45) = term(45) + s1(d,l) * s2(b,c,j,k) * t2(d,a,j,l) * t2(b,c,i,k)
     !                                           term(46) = term(46) + s1(d,l) * s2(b,c,j,k) * t2(b,c,k,i) * t2(d,a,j,l)
     !                                           term(47) = term(47) + s1(d,l) * s2(b,c,j,k) * t2(b,c,j,i) * t2(d,a,k,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(36) = term(36) * (-0.75d+0) 
     !       term(37) = term(37) * 1.5d+0 
     !       term(38) = -term(38) 
     !       term(39) = term(39) * 2.0d+0 
     !       term(40) = term(40) * 2.0d+0 
     !       term(41) = term(41) * (-4.0d+0) 
     !       term(42) = term(42) * 1.5d+0 
     !       term(43) = term(43) * (-0.75d+0) 
     !       term(44) = term(44) * (-0.25d+0) 
     !       term(45) = term(45) * 0.5d+0 
     !       term(46) = term(46) * (-0.25d+0) 
     !       term(47) = term(47) * 0.5d+0 


     !       calc_D_ov_ground_mbpt5_cc3 = 0.d+0 
     !       do s = 0, 47
     !             calc_D_ov_ground_mbpt5_cc3 = calc_D_ov_ground_mbpt5_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt5_cc3


     ! function calc_D_vv_ground_mbpt5_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt5_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,i,c,k,d 
     !       double precision, dimension(0:21) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s1(a,i) * s1(c,j) * t2(c,b,i,j)
     !                         term(1) = term(1) + s1(c,i) * s1(a,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 
     !       term(1) = term(1) * 2.0d+0 

     !       do i = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(2) = term(2) + s1(a,i) * s1(c,j) * t2(c,b,j,i)
     !                         term(3) = term(3) + s1(c,i) * s1(a,j) * t2(c,b,j,i)
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * 2.0d+0 
     !       term(3) = -term(3) 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do i = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(4) = term(4) + s1(a,i) * s2(d,c,j,k) * t3(dm_nocc, nactive, d,c,b,k,i,j)
     !                                     term(5) = term(5) + s1(d,i) * s2(a,c,j,k) * t3(dm_nocc, nactive, d,c,b,k,i,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(5) = term(5) * (-2.6666666666666665d+0) 

     !       do i = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(6) = term(6) + s1(a,i) * s2(d,c,j,k) * t3(dm_nocc, nactive, d,c,b,k,j,i)
     !                                     term(7) = term(7) + s1(d,i) * s2(a,c,j,k) * t3(dm_nocc, nactive, d,c,b,k,j,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * (-1.9999999999999998d+0) 
     !       term(7) = term(7) * 1.3333333333333333d+0 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(8) = term(8) + s1(d,i) * s2(a,c,j,k) * t3(dm_nocc, nactive, d,c,b,i,j,k)
     !                                     term(9) = term(9) + s1(a,i) * s2(d,c,j,k) * t3(dm_nocc, nactive, d,c,b,i,j,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = term(8) * (-2.6666666666666665d+0) 

     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(10) = term(10) + s1(a,i) * s2(d,c,j,k) * t3(dm_nocc, nactive, d,c,b,i,k,j)
     !                                     term(11) = term(11) + s1(d,i) * s2(a,c,j,k) * t3(dm_nocc, nactive, d,c,b,i,k,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(10) = term(10) * (-1.9999999999999998d+0) 
     !       term(11) = term(11) * 5.333333333333333d+0 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do i = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(12) = term(12) + s1(a,i) * s2(d,c,j,k) * t3(dm_nocc, nactive, d,c,b,j,i,k)
     !                                     term(13) = term(13) + s1(d,i) * s2(a,c,j,k) * t3(dm_nocc, nactive, d,c,b,j,i,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(12) = term(12) * (-1.9999999999999998d+0) 
     !       term(13) = term(13) * 1.3333333333333333d+0 

     !       do i = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(14) = term(14) + s1(a,i) * s2(d,c,j,k) * t3(dm_nocc, nactive, d,c,b,j,k,i)
     !                                     term(15) = term(15) + s1(d,i) * s2(a,c,j,k) * t3(dm_nocc, nactive, d,c,b,j,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(14) = term(14) * 3.9999999999999996d+0 
     !       term(15) = term(15) * (-2.6666666666666665d+0) 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do i = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(16) = term(16) + s1(c,i) * s2(d,a,j,k) * t3(dm_nocc, nactive, c,d,b,k,i,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(16) = term(16) * 0.6666666666666666d+0 

     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(17) = term(17) + s1(c,i) * s2(d,a,j,k) * t3(dm_nocc, nactive, c,d,b,i,k,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(17) = term(17) * (-1.3333333333333333d+0) 

     !       do i = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(18) = term(18) + s1(c,i) * s2(d,a,j,k) * t3(dm_nocc, nactive, c,d,b,j,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(18) = term(18) * 0.6666666666666666d+0 

     !       do i = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(19) = term(19) + s1(c,i) * s2(d,a,j,k) * t3(dm_nocc, nactive, c,d,b,k,j,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(19) = term(19) * (-1.3333333333333333d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do i = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(20) = term(20) + s1(c,i) * s2(d,a,j,k) * t3(dm_nocc, nactive, c,d,b,j,i,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(20) = term(20) * (-1.3333333333333333d+0) 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(21) = term(21) + s1(c,i) * s2(d,a,j,k) * t3(dm_nocc, nactive, c,d,b,i,j,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(21) = term(21) * 2.6666666666666665d+0 


     !       calc_D_vv_ground_mbpt5_cc3 = 0.d+0 
     !       do s = 0, 21
     !             calc_D_vv_ground_mbpt5_cc3 = calc_D_vv_ground_mbpt5_cc3 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt5_cc3


     ! function calc_D_ov_ground_mbpt6_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt6_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,b,k,c 
     !       double precision, dimension(0:6) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   term(0) = term(0) + s1(b,j) * t1(b,i) * t1(a,j)
     !             end do
     !       end do

     !       term(0) = term(0) * (-2.0d+0) 

     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(1) = term(1) + s1(c,k) * s1(b,j) * t3(dm_nocc, nactive, c,b,a,j,k,i)
     !                               term(2) = term(2) + s1(c,k) * s1(b,j) * t3(dm_nocc, nactive, c,b,a,i,k,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-1.9999999999999998d+0) 

     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do j = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(3) = term(3) + s1(c,k) * s1(b,j) * t3(dm_nocc, nactive, c,b,a,k,j,i)
     !                               term(4) = term(4) + s1(c,k) * s1(b,j) * t3(dm_nocc, nactive, c,b,a,i,j,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * 3.9999999999999996d+0 
     !       term(4) = term(4) * (-1.9999999999999998d+0) 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(5) = term(5) + s1(c,k) * s1(b,j) * t3(dm_nocc, nactive, c,b,a,k,i,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(5) = term(5) * (-1.9999999999999998d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(6) = term(6) + s1(c,k) * s1(b,j) * t3(dm_nocc, nactive, c,b,a,j,i,k)
     !                         end do
     !                   end do
     !             end do
     !       end do



     !       calc_D_ov_ground_mbpt6_cc3 = 0.d+0 
     !       do s = 0, 6
     !             calc_D_ov_ground_mbpt6_cc3 = calc_D_ov_ground_mbpt6_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt6_cc3


     ! function calc_D_ov_ground_mbpt7_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt7_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,k,b,j,c 
     !       double precision, dimension(0:7) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do j = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(0) = term(0) + s1(c,k) * s1(b,j) * t1(a,j) * t2(c,b,i,k)
     !                         end do
     !                   end do
     !             end do
     !       end do


     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(1) = term(1) + s1(c,k) * s1(b,j) * t1(a,k) * t2(c,b,i,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do j = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               term(2) = term(2) + s1(c,k) * s1(b,j) * t1(a,k) * t2(c,b,j,i)
     !                               term(3) = term(3) + s1(c,k) * s1(b,j) * t1(a,j) * t2(c,b,k,i)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(4) = term(4) + s1(c,k) * s1(b,j) * t1(b,i) * t2(c,a,j,k)
     !                               term(5) = term(5) + s1(c,k) * s1(b,j) * t1(c,i) * t2(b,a,j,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(5) = term(5) * (-2.0d+0) 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(6) = term(6) + s1(c,k) * s1(b,j) * t1(b,i) * t2(c,a,k,j)
     !                               term(7) = term(7) + s1(c,k) * s1(b,j) * t1(c,i) * t2(b,a,k,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * (-2.0d+0) 


     !       calc_D_ov_ground_mbpt7_cc3 = 0.d+0 
     !       do s = 0, 7
     !             calc_D_ov_ground_mbpt7_cc3 = calc_D_ov_ground_mbpt7_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt7_cc3


     ! function calc_D_oo_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt8_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,l,b,k,c,a 
     !       double precision, dimension(0:17) :: term 
     !       term = 0.d+0 
     !       do l = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(0) = term(0) + s1(a,l) * s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(1) = term(1) + s1(a,j) * s1(b,l) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(2) = term(2) + s1(a,k) * s1(b,l) * s1(c,j) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(3) = term(3) + s1(a,l) * s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                                     term(4) = term(4) + s1(a,j) * s1(b,l) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                                     term(5) = term(5) + s1(a,k) * s1(b,l) * s1(c,j) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = term(0) * (-0.33333333333333337d+0) 
     !       term(1) = term(1) * 0.6666666666666667d+0 
     !       term(2) = term(2) * (-0.33333333333333337d+0) 
     !       term(3) = term(3) * 0.6666666666666667d+0 
     !       term(4) = term(4) * (-0.33333333333333337d+0) 
     !       term(5) = term(5) * 0.6666666666666667d+0 

     !       do k = 1, dm_nocc 
     !             do l = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(6) = term(6) + s1(a,k) * s1(b,l) * s1(c,j) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(7) = term(7) + s1(a,j) * s1(b,l) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(8) = term(8) + s1(a,l) * s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * (-0.33333333333333337d+0) 
     !       term(7) = term(7) * 0.6666666666666667d+0 
     !       term(8) = term(8) * (-1.3333333333333335d+0) 

     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(9) = term(9) + s1(a,l) * s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(10) = term(10) + s1(a,k) * s1(b,l) * s1(c,j) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(11) = term(11) + s1(a,j) * s1(b,l) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(12) = term(12) + s1(a,l) * s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                                     term(13) = term(13) + s1(a,j) * s1(b,l) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                                     term(14) = term(14) + s1(a,k) * s1(b,l) * s1(c,j) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(9) = term(9) * 0.6666666666666667d+0 
     !       term(10) = term(10) * 0.6666666666666667d+0 
     !       term(11) = term(11) * (-1.3333333333333335d+0) 
     !       term(12) = term(12) * (-0.33333333333333337d+0) 
     !       term(13) = term(13) * 0.6666666666666667d+0 
     !       term(14) = term(14) * (-1.3333333333333335d+0) 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(15) = term(15) + s1(a,j) * s1(b,l) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(16) = term(16) + s1(a,k) * s1(b,l) * s1(c,j) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(17) = term(17) + s1(a,l) * s1(b,j) * s1(c,k) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(15) = term(15) * (-0.33333333333333337d+0) 
     !       term(16) = term(16) * 0.6666666666666667d+0 
     !       term(17) = term(17) * 0.6666666666666667d+0 


     !       calc_D_oo_ground_mbpt8_cc3 = 0.d+0 
     !       do s = 0, 17
     !             calc_D_oo_ground_mbpt8_cc3 = calc_D_oo_ground_mbpt8_cc3 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt8_cc3

     ! function calc_D_ov_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt8_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,l,k,b,j,d,c 
     !       double precision, dimension(0:23) :: term 
     !       term = 0.d+0 
     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(0) = term(0) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,i,l) * t2(c,a,j,k)
     !                                           term(1) = term(1) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,i,k) * t2(c,a,j,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = term(0) * (-0.7499999999999998d+0) 
     !       term(1) = term(1) * 1.4999999999999996d+0 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(2) = term(2) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,i,k) * t2(c,a,l,j)
     !                                           term(3) = term(3) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,i,j) * t2(c,a,l,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * (-0.7499999999999998d+0) 
     !       term(3) = term(3) * 1.4999999999999996d+0 

     !       do l = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(4) = term(4) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,i,l) * t2(c,a,k,j)
     !                                           term(5) = term(5) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,i,j) * t2(c,a,k,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * 1.4999999999999996d+0 
     !       term(5) = term(5) * (-2.999999999999999d+0) 

     !       do j = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(6) = term(6) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,k,j) * t2(b,c,l,i)
     !                                           term(7) = term(7) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,l,j) * t2(b,c,k,i)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * (-0.7499999999999998d+0) 
     !       term(7) = term(7) * 1.4999999999999996d+0 

     !       do l = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(8) = term(8) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,k,l) * t2(b,c,j,i)
     !                                           term(9) = term(9) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,j,l) * t2(b,c,k,i)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = term(8) * 1.4999999999999996d+0 
     !       term(9) = term(9) * (-0.7499999999999998d+0) 

     !       do k = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(10) = term(10) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,j,k) * t2(b,c,l,i)
     !                                           term(11) = term(11) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,l,k) * t2(b,c,j,i)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(10) = term(10) * 1.4999999999999996d+0 
     !       term(11) = term(11) * (-2.999999999999999d+0) 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(12) = term(12) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,j,k) * t2(b,c,i,l)
     !                                           term(13) = term(13) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,j,l) * t2(b,c,i,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(12) = term(12) * (-0.24999999999999997d+0) 
     !       term(13) = term(13) * 0.49999999999999994d+0 

     !       do l = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(14) = term(14) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,k,j) * t2(b,c,i,l)
     !                                           term(15) = term(15) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,k,l) * t2(b,c,i,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(14) = term(14) * 0.49999999999999994d+0 
     !       term(15) = term(15) * (-0.24999999999999997d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(16) = term(16) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,l,k) * t2(b,c,i,j)
     !                                           term(17) = term(17) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,a,l,j) * t2(b,c,i,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(16) = term(16) * 0.49999999999999994d+0 
     !       term(17) = -term(17) 

     !       do l = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do k = 1, dm_nocc 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(18) = term(18) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,k,i) * t2(c,a,j,l)
     !                                           term(19) = term(19) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,j,i) * t2(c,a,k,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(18) = term(18) * (-0.24999999999999997d+0) 
     !       term(19) = term(19) * 0.49999999999999994d+0 

     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(20) = term(20) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,j,i) * t2(c,a,l,k)
     !                                           term(21) = term(21) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,l,i) * t2(c,a,j,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(20) = term(20) * (-0.24999999999999997d+0) 
     !       term(21) = term(21) * 0.49999999999999994d+0 

     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do c = dm_nocc + 1, nactive 
     !                                           term(22) = term(22) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,k,i) * t2(c,a,l,j)
     !                                           term(23) = term(23) + s1(d,l) * s1(b,j) * s1(c,k) * t2(d,b,l,i) * t2(c,a,k,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(22) = term(22) * 0.49999999999999994d+0 
     !       term(23) = -term(23) 


     !       calc_D_ov_ground_mbpt8_cc3 = 0.d+0 
     !       do s = 0, 23
     !             calc_D_ov_ground_mbpt8_cc3 = calc_D_ov_ground_mbpt8_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt8_cc3

     ! function calc_D_vv_ground_mbpt8_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt8_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,k,c,i,d 
     !       double precision, dimension(0:11) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do i = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(0) = term(0) + s1(a,i) * s1(d,j) * s1(c,k) * t3(dm_nocc, nactive, d,c,b,k,i,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = term(0) * 0.6666666666666669d+0 

     !       do i = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(1) = term(1) + s1(a,i) * s1(d,j) * s1(c,k) * t3(dm_nocc, nactive, d,c,b,k,j,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-1.3333333333333337d+0) 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(2) = term(2) + s1(a,i) * s1(d,j) * s1(c,k) * t3(dm_nocc, nactive, d,c,b,i,j,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * 0.6666666666666669d+0 

     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(3) = term(3) + s1(a,i) * s1(d,j) * s1(c,k) * t3(dm_nocc, nactive, d,c,b,i,k,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-1.3333333333333337d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do i = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(4) = term(4) + s1(a,i) * s1(d,j) * s1(c,k) * t3(dm_nocc, nactive, d,c,b,j,i,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * (-1.3333333333333337d+0) 

     !       do i = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     term(5) = term(5) + s1(a,i) * s1(d,j) * s1(c,k) * t3(dm_nocc, nactive, d,c,b,j,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(5) = term(5) * 2.6666666666666674d+0 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do i = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(6) = term(6) + s1(c,i) * s1(d,j) * s1(a,k) * t3(dm_nocc, nactive, c,d,b,k,i,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * 0.33333333333333337d+0 

     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(7) = term(7) + s1(c,i) * s1(d,j) * s1(a,k) * t3(dm_nocc, nactive, c,d,b,i,k,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(7) = term(7) * (-0.6666666666666667d+0) 

     !       do i = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(8) = term(8) + s1(c,i) * s1(d,j) * s1(a,k) * t3(dm_nocc, nactive, c,d,b,j,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = term(8) * 0.33333333333333337d+0 

     !       do i = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(9) = term(9) + s1(c,i) * s1(d,j) * s1(a,k) * t3(dm_nocc, nactive, c,d,b,k,j,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(9) = term(9) * (-0.6666666666666667d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do i = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(10) = term(10) + s1(c,i) * s1(d,j) * s1(a,k) * t3(dm_nocc, nactive, c,d,b,j,i,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(10) = term(10) * (-0.6666666666666667d+0) 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do j = 1, dm_nocc 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(11) = term(11) + s1(c,i) * s1(d,j) * s1(a,k) * t3(dm_nocc, nactive, c,d,b,i,j,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(11) = term(11) * 1.3333333333333335d+0 


     !       calc_D_vv_ground_mbpt8_cc3 = 0.d+0 
     !       do s = 0, 11
     !             calc_D_vv_ground_mbpt8_cc3 = calc_D_vv_ground_mbpt8_cc3 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt8_cc3



!___________________________________________________________________________________________________________________________________
     
     ! function calc_D_oo_diag_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive, i) 
     !       double precision :: calc_D_oo_diag_ground_mbpt3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = one

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_oo_diag_ground_mbpt3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_oo_diag_ground_mbpt3 = calc_D_oo_diag_ground_mbpt3 + term(s)
     !       end do

     ! end function calc_D_oo_diag_ground_mbpt3

     ! function calc_D_oo_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,k,b,a 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,b,k,j) * t2(a,b,i,k)
     !                         term(1) = term(1) + s2(a,b,j,k) * t2(a,b,i,k)
     !                         term(2) = term(2) + s2(a,b,j,k) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(3) = term(3) + s2(a,b,k,j) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 


     !       calc_D_oo_ground_mbpt3 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_oo_ground_mbpt3 = calc_D_oo_ground_mbpt3 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt3

     ! function calc_D_ov_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,b 
     !       double precision, dimension(0:2) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   term(0) = term(0) + s1(b,j) * t2(b,a,j,i)
     !                   term(1) = term(1) + s1(b,j) * t2(b,a,i,j)
     !             end do
     !       end do

     !       term(0) = term(0) * 4.0d+0 
     !       term(1) = term(1) * (-2.0d+0) 

     !       term(2) = term(2) + t1(a,i)

     !       term(2) = term(2) * 2.0d+0 


     !       calc_D_ov_ground_mbpt3 = 0.d+0 
     !       do s = 0, 2
     !             calc_D_ov_ground_mbpt3 = calc_D_ov_ground_mbpt3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt3

     ! function calc_D_vo_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
     !       double precision :: calc_D_vo_ground_mbpt3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = term(0) + s1(a,i)

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_vo_ground_mbpt3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_vo_ground_mbpt3 = calc_D_vo_ground_mbpt3 + term(s)
     !       end do

     ! end function calc_D_vo_ground_mbpt3

     ! function calc_D_vv_ground_mbpt3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,i,c 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,c,i,j) * t2(b,c,j,i)
     !                         term(1) = term(1) + s2(c,a,i,j) * t2(c,b,j,i)
     !                         term(2) = term(2) + s2(c,a,i,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 
     !       term(1) = -term(1) 
     !       term(2) = term(2) * 2.0d+0 

     !       do j = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do i = 1, dm_nocc 
     !                         term(3) = term(3) + s2(a,c,i,j) * t2(b,c,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * 2.0d+0 


     !       calc_D_vv_ground_mbpt3 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_vv_ground_mbpt3 = calc_D_vv_ground_mbpt3 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt3

     ! function calc_D_oo_diag_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i) 
     !       double precision :: calc_D_oo_diag_ground_mbpt4
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = one

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_oo_diag_ground_mbpt4 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_oo_diag_ground_mbpt4 = calc_D_oo_diag_ground_mbpt4 + term(s)
     !       end do

     ! end function calc_D_oo_diag_ground_mbpt4


     ! function calc_D_oo_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt4
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,k,b,a 
     !       double precision, dimension(0:4) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,b,k,j) * t2(a,b,i,k)
     !                         term(1) = term(1) + s2(a,b,j,k) * t2(a,b,i,k)
     !                         term(2) = term(2) + s2(a,b,j,k) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(3) = term(3) + s2(a,b,k,j) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 

     !       do a = dm_nocc + 1, nactive 
     !             term(4) = term(4) + s1(a,j) * t1(a,i)
     !       end do

     !       term(4) = term(4) * (-2.0d+0) 


     !       calc_D_oo_ground_mbpt4 = 0.d+0 
     !       do s = 0, 4
     !             calc_D_oo_ground_mbpt4 = calc_D_oo_ground_mbpt4 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt4

     ! function calc_D_ov_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt4
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,b,k,c 
     !       double precision, dimension(0:10) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   term(0) = term(0) + s1(b,j) * t2(b,a,j,i)
     !                   term(1) = term(1) + s1(b,j) * t2(b,a,i,j)
     !             end do
     !       end do

     !       term(0) = term(0) * 4.0d+0 
     !       term(1) = term(1) * (-2.0d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(2) = term(2) + s2(b,c,j,k) * t1(a,k) * t2(b,c,i,j)
     !                               term(3) = term(3) + s2(b,c,j,k) * t1(c,i) * t2(b,a,k,j)
     !                               term(4) = term(4) + s2(b,c,j,k) * t1(b,i) * t2(a,c,k,j)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(2) = term(2) * 0.5d+0 
     !       term(3) = term(3) * 0.5d+0 
     !       term(4) = term(4) * 0.5d+0 

     !       do k = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do j = 1, dm_nocc 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(5) = term(5) + s2(b,c,j,k) * t1(a,j) * t2(b,c,i,k)
     !                               term(6) = term(6) + s2(b,c,j,k) * t1(a,j) * t2(b,c,k,i)
     !                               term(7) = term(7) + s2(b,c,j,k) * t1(a,k) * t2(b,c,j,i)
     !                               term(8) = term(8) + s2(b,c,j,k) * t1(c,i) * t2(b,a,j,k)
     !                               term(9) = term(9) + s2(b,c,j,k) * t1(b,i) * t2(a,c,j,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(5) = -term(5) 
     !       term(6) = term(6) * 0.5d+0 
     !       term(7) = -term(7) 
     !       term(8) = -term(8) 
     !       term(9) = -term(9) 

     !       term(10) = term(10) + t1(a,i)

     !       term(10) = term(10) * 2.0d+0 


     !       calc_D_ov_ground_mbpt4 = 0.d+0 
     !       do s = 0, 10
     !             calc_D_ov_ground_mbpt4 = calc_D_ov_ground_mbpt4 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt4

     ! function calc_D_vo_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
     !       double precision :: calc_D_vo_ground_mbpt4
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = term(0) + s1(a,i)

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_vo_ground_mbpt4 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_vo_ground_mbpt4 = calc_D_vo_ground_mbpt4 + term(s)
     !       end do

     ! end function calc_D_vo_ground_mbpt4

     ! function calc_D_vv_ground_mbpt4(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt4
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,i,c 
     !       double precision, dimension(0:4) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,c,i,j) * t2(b,c,j,i)
     !                         term(1) = term(1) + s2(c,a,i,j) * t2(c,b,j,i)
     !                         term(2) = term(2) + s2(c,a,i,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 
     !       term(1) = -term(1) 
     !       term(2) = term(2) * 2.0d+0 

     !       do j = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do i = 1, dm_nocc 
     !                         term(3) = term(3) + s2(a,c,i,j) * t2(b,c,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * 2.0d+0 

     !       do i = 1, dm_nocc 
     !             term(4) = term(4) + s1(a,i) * t1(b,i)
     !       end do

     !       term(4) = term(4) * 2.0d+0 


     !       calc_D_vv_ground_mbpt4 = 0.d+0 
     !       do s = 0, 4
     !             calc_D_vv_ground_mbpt4 = calc_D_vv_ground_mbpt4 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt4

     ! function calc_D_oo_diag_ground_mbpt3_cc3(t2, t1, s2, s1, dm_nocc, nactive, i) 
     !       double precision :: calc_D_oo_diag_ground_mbpt3_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = one

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_oo_diag_ground_mbpt3_cc3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_oo_diag_ground_mbpt3_cc3 = calc_D_oo_diag_ground_mbpt3_cc3 + term(s)
     !       end do

     ! end function calc_D_oo_diag_ground_mbpt3_cc3



     ! function calc_D_oo_ground_mbpt3_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt3_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,k,b,a 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,b,k,j) * t2(a,b,i,k)
     !                         term(1) = term(1) + s2(a,b,j,k) * t2(a,b,i,k)
     !                         term(2) = term(2) + s2(a,b,j,k) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(3) = term(3) + s2(a,b,k,j) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 


     !       calc_D_oo_ground_mbpt3_cc3 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_oo_ground_mbpt3_cc3 = calc_D_oo_ground_mbpt3_cc3 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt3_cc3

     ! function calc_D_ov_ground_mbpt3_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt3_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,k,c,b 
     !       double precision, dimension(0:8) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(0) = term(0) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,k,i,j)
     !                               term(1) = term(1) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,i,k,j)
     !                               term(2) = term(2) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,j,k,i)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(0) = term(0) * (-1.9999999999999998d+0) 
     !       term(2) = term(2) * (-1.9999999999999998d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(3) = term(3) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,j,i,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * 3.9999999999999996d+0 

     !       do k = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do j = 1, dm_nocc 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(4) = term(4) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,i,j,k)
     !                               term(5) = term(5) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,k,j,i)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * (-1.9999999999999998d+0) 

     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   term(6) = term(6) + s1(b,j) * t2(b,a,j,i)
     !                   term(7) = term(7) + s1(b,j) * t2(b,a,i,j)
     !             end do
     !       end do

     !       term(6) = term(6) * 4.0d+0 
     !       term(7) = term(7) * (-2.0d+0) 

     !       term(8) = term(8) + t1(a,i)

     !       term(8) = term(8) * 2.0d+0 


     !       calc_D_ov_ground_mbpt3_cc3 = 0.d+0 
     !       do s = 0, 8
     !             calc_D_ov_ground_mbpt3_cc3 = calc_D_ov_ground_mbpt3_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt3_cc3

     ! function calc_D_vo_ground_mbpt3_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
     !       double precision :: calc_D_vo_ground_mbpt3_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = term(0) + s1(a,i)

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_vo_ground_mbpt3_cc3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_vo_ground_mbpt3_cc3 = calc_D_vo_ground_mbpt3_cc3 + term(s)
     !       end do

     ! end function calc_D_vo_ground_mbpt3_cc3

     ! function calc_D_vv_ground_mbpt3_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt3_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,i,c 
     !       double precision, dimension(0:3) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,c,i,j) * t2(b,c,j,i)
     !                         term(1) = term(1) + s2(c,a,i,j) * t2(c,b,j,i)
     !                         term(2) = term(2) + s2(c,a,i,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 
     !       term(1) = -term(1) 
     !       term(2) = term(2) * 2.0d+0 

     !       do j = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do i = 1, dm_nocc 
     !                         term(3) = term(3) + s2(a,c,i,j) * t2(b,c,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * 2.0d+0 


     !       calc_D_vv_ground_mbpt3_cc3 = 0.d+0 
     !       do s = 0, 3
     !             calc_D_vv_ground_mbpt3_cc3 = calc_D_vv_ground_mbpt3_cc3 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt3_cc3

     ! function calc_D_oo_diag_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i) 
     !       double precision :: calc_D_oo_diag_ground_mbpt4_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = one

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_oo_diag_ground_mbpt4_cc3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_oo_diag_ground_mbpt4_cc3 = calc_D_oo_diag_ground_mbpt4_cc3 + term(s)
     !       end do

     ! end function calc_D_oo_diag_ground_mbpt4_cc3



     ! function calc_D_oo_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,j) 
     !       double precision :: calc_D_oo_ground_mbpt4_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,j 
     !       integer :: s ,k,b,a,l,c 
     !       double precision, dimension(0:25) :: term 
     !       term = 0.d+0 
     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,b,k,j) * t2(a,b,i,k)
     !                         term(1) = term(1) + s2(a,b,j,k) * t2(a,b,i,k)
     !                         term(2) = term(2) + s2(a,b,j,k) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(1) = term(1) * (-2.0d+0) 

     !       do b = dm_nocc + 1, nactive 
     !             do k = 1, dm_nocc 
     !                   do a = dm_nocc + 1, nactive 
     !                         term(3) = term(3) + s2(a,b,k,j) * t2(a,b,k,i)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * (-2.0d+0) 

     !       do k = 1, dm_nocc 
     !             do l = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(4) = term(4) + t3(dm_nocc, nactive, a,b,c,i,l,k) * t3(dm_nocc, nactive, a,b,c,l,k,j)
     !                                     term(5) = term(5) + t3(dm_nocc, nactive, a,b,c,i,l,k) * t3(dm_nocc, nactive, a,b,c,l,j,k)
     !                                     term(6) = term(6) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                                     term(7) = term(7) + t3(dm_nocc, nactive, a,b,c,k,l,i) * t3(dm_nocc, nactive, a,b,c,l,j,k)
     !                                     term(8) = term(8) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                                     term(9) = term(9) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(10) = term(10) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(11) = term(11) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,l,i,k)
     !                                     term(12) = term(12) + t3(dm_nocc, nactive, a,b,c,l,i,k) * t3(dm_nocc, nactive, a,b,c,k,l,j)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * (-0.33333333333333337d+0) 
     !       term(5) = term(5) * 0.6666666666666667d+0 
     !       term(6) = term(6) * (-0.33333333333333337d+0) 
     !       term(7) = term(7) * (-0.16666666666666666d+0) 
     !       term(8) = term(8) * (-0.16666666666666666d+0) 
     !       term(9) = term(9) * (-0.16666666666666666d+0) 
     !       term(10) = term(10) * 0.6666666666666667d+0 
     !       term(11) = term(11) * (-1.3333333333333335d+0) 
     !       term(12) = term(12) * (-0.16666666666666666d+0) 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do b = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(13) = term(13) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,i,k,l)
     !                                     term(14) = term(14) + t3(dm_nocc, nactive, a,b,c,i,l,k) * t3(dm_nocc, nactive, a,b,c,k,j,l)
     !                                     term(15) = term(15) + t3(dm_nocc, nactive, a,b,c,k,l,i) * t3(dm_nocc, nactive, a,b,c,k,j,l)
     !                                     term(16) = term(16) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,l,k,i)
     !                                     term(17) = term(17) + t3(dm_nocc, nactive, a,b,c,k,l,i) * t3(dm_nocc, nactive, a,b,c,l,k,j)
     !                                     term(18) = term(18) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(19) = term(19) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(20) = term(20) + t3(dm_nocc, nactive, a,b,c,l,j,k) * t3(dm_nocc, nactive, a,b,c,k,i,l)
     !                                     term(21) = term(21) + t3(dm_nocc, nactive, a,b,c,l,i,k) * t3(dm_nocc, nactive, a,b,c,k,j,l)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(13) = term(13) * 0.6666666666666667d+0 
     !       term(14) = term(14) * (-0.33333333333333337d+0) 
     !       term(15) = term(15) * 0.6666666666666667d+0 
     !       term(16) = term(16) * 0.3333333333333333d+0 
     !       term(17) = term(17) * 0.3333333333333333d+0 
     !       term(18) = term(18) * 0.6666666666666667d+0 
     !       term(19) = term(19) * (-0.33333333333333337d+0) 
     !       term(20) = term(20) * 0.3333333333333333d+0 
     !       term(21) = term(21) * 0.3333333333333333d+0 

     !       do k = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   do l = 1, dm_nocc 
     !                         do c = dm_nocc + 1, nactive 
     !                               do a = dm_nocc + 1, nactive 
     !                                     term(22) = term(22) + t3(dm_nocc, nactive, a,b,c,i,l,k) * t3(dm_nocc, nactive, a,b,c,k,l,j)
     !                                     term(23) = term(23) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,i,l,k)
     !                                     term(24) = term(24) + t3(dm_nocc, nactive, a,b,c,j,l,k) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                                     term(25) = term(25) + t3(dm_nocc, nactive, a,b,c,k,l,j) * t3(dm_nocc, nactive, a,b,c,k,l,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(22) = term(22) * 0.6666666666666667d+0 
     !       term(23) = term(23) * (-1.3333333333333335d+0) 
     !       term(24) = term(24) * 0.6666666666666667d+0 
     !       term(25) = term(25) * (-1.3333333333333335d+0) 


     !       calc_D_oo_ground_mbpt4_cc3 = 0.d+0 
     !       do s = 0, 25
     !             calc_D_oo_ground_mbpt4_cc3 = calc_D_oo_ground_mbpt4_cc3 + term(s)
     !       end do

     ! end function calc_D_oo_ground_mbpt4_cc3

     ! function calc_D_ov_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, i,a) 
     !       double precision :: calc_D_ov_ground_mbpt4_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: i,a 
     !       integer :: s ,j,b,k,l,c,d 
     !       double precision, dimension(0:20) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do b = dm_nocc + 1, nactive 
     !                   term(0) = term(0) + s1(b,j) * t2(b,a,j,i)
     !                   term(1) = term(1) + s1(b,j) * t2(b,a,i,j)
     !             end do
     !       end do

     !       term(0) = term(0) * 4.0d+0 
     !       term(1) = term(1) * (-2.0d+0) 

     !       do k = 1, dm_nocc 
     !             do l = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do d = dm_nocc + 1, nactive 
     !                               do j = 1, dm_nocc 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(2) = term(2) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,l,j,k)
     !                                           term(3) = term(3) + t2(a,c,l,k) * t2(b,d,j,i) * t3(dm_nocc, nactive, b,c,d,l,j,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(2) = -term(2) 
     !       term(3) = -term(3) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(4) = term(4) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,j,l,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * 1.9999999999999991d+0 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do l = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(5) = term(5) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,k,l,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(5) = -term(5) 

     !       do l = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do d = dm_nocc + 1, nactive 
     !                               do j = 1, dm_nocc 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(6) = term(6) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,k,j,l)
     !                                           term(7) = term(7) + t2(a,c,l,k) * t2(b,d,j,i) * t3(dm_nocc, nactive, b,c,d,k,j,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(6) = term(6) * 1.9999999999999991d+0 
     !       term(7) = term(7) * 1.9999999999999991d+0 

     !       do j = 1, dm_nocc 
     !             do l = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(8) = term(8) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,l,k,j)
     !                                           term(9) = term(9) + t2(a,c,l,k) * t2(b,d,j,i) * t3(dm_nocc, nactive, b,c,d,l,k,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(8) = term(8) * 1.9999999999999991d+0 
     !       term(9) = term(9) * 1.9999999999999991d+0 

     !       do l = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do k = 1, dm_nocc 
     !                               do d = dm_nocc + 1, nactive 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(10) = term(10) + t2(a,c,j,k) * t2(b,d,i,l) * t3(dm_nocc, nactive, b,c,d,j,k,l)
     !                                           term(11) = term(11) + t2(a,c,l,k) * t2(b,d,j,i) * t3(dm_nocc, nactive, b,c,d,j,k,l)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(10) = term(10) * (-3.9999999999999982d+0) 
     !       term(11) = term(11) * (-3.9999999999999982d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do d = dm_nocc + 1, nactive 
     !                               do l = 1, dm_nocc 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(12) = term(12) + t2(a,c,l,k) * t2(b,d,j,i) * t3(dm_nocc, nactive, b,c,d,j,l,k)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(12) = term(12) * 1.9999999999999991d+0 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do d = dm_nocc + 1, nactive 
     !                               do l = 1, dm_nocc 
     !                                     do b = dm_nocc + 1, nactive 
     !                                           term(13) = term(13) + t2(a,c,l,k) * t2(b,d,j,i) * t3(dm_nocc, nactive, b,c,d,k,l,j)
     !                                     end do
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(13) = -term(13) 

     !       do j = 1, dm_nocc 
     !             do k = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(14) = term(14) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,k,i,j)
     !                               term(15) = term(15) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,i,k,j)
     !                               term(16) = term(16) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,j,k,i)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(14) = term(14) * (-1.9999999999999998d+0) 
     !       term(16) = term(16) * (-1.9999999999999998d+0) 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(17) = term(17) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,j,i,k)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(17) = term(17) * 3.9999999999999996d+0 

     !       do k = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do j = 1, dm_nocc 
     !                         do b = dm_nocc + 1, nactive 
     !                               term(18) = term(18) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,i,j,k)
     !                               term(19) = term(19) + s2(b,c,j,k) * t3(dm_nocc, nactive, b,a,c,k,j,i)
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(18) = term(18) * (-1.9999999999999998d+0) 

     !       term(20) = term(20) + t1(a,i)

     !       term(20) = term(20) * 2.0d+0 


     !       calc_D_ov_ground_mbpt4_cc3 = 0.d+0 
     !       do s = 0, 20
     !             calc_D_ov_ground_mbpt4_cc3 = calc_D_ov_ground_mbpt4_cc3 + term(s)
     !       end do

     ! end function calc_D_ov_ground_mbpt4_cc3

     ! function calc_D_vo_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,i) 
     !       double precision :: calc_D_vo_ground_mbpt4_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,i 
     !       integer :: s  
     !       double precision, dimension(0:0) :: term 
     !       term = 0.d+0 
     !       term(0) = term(0) + s1(a,i)

     !       term(0) = term(0) * 2.0d+0 


     !       calc_D_vo_ground_mbpt4_cc3 = 0.d+0 
     !       do s = 0, 0
     !             calc_D_vo_ground_mbpt4_cc3 = calc_D_vo_ground_mbpt4_cc3 + term(s)
     !       end do

     ! end function calc_D_vo_ground_mbpt4_cc3

     ! function calc_D_vv_ground_mbpt4_cc3(t2, t1, s2, s1, dm_nocc, nactive, a,b) 
     !       double precision :: calc_D_vv_ground_mbpt4_cc3
     !       integer, intent(in) :: dm_nocc, nactive
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: t2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: t1 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc+1:nactive,dm_nocc,dm_nocc), intent(in) :: s2 
     !       double precision, dimension(dm_nocc+1:nactive,dm_nocc), intent(in)                  :: s1 
     !       integer, intent(in) :: a,b 
     !       integer :: s ,j,i,c,k,d 
     !       double precision, dimension(0:15) :: term 
     !       term = 0.d+0 
     !       do j = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do c = dm_nocc + 1, nactive 
     !                         term(0) = term(0) + s2(a,c,i,j) * t2(b,c,j,i)
     !                         term(1) = term(1) + s2(c,a,i,j) * t2(c,b,j,i)
     !                         term(2) = term(2) + s2(c,a,i,j) * t2(c,b,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(0) = -term(0) 
     !       term(1) = -term(1) 
     !       term(2) = term(2) * 2.0d+0 

     !       do j = 1, dm_nocc 
     !             do c = dm_nocc + 1, nactive 
     !                   do i = 1, dm_nocc 
     !                         term(3) = term(3) + s2(a,c,i,j) * t2(b,c,i,j)
     !                   end do
     !             end do
     !       end do

     !       term(3) = term(3) * 2.0d+0 

     !       do k = 1, dm_nocc 
     !             do j = 1, dm_nocc 
     !                   do i = 1, dm_nocc 
     !                         do d = dm_nocc + 1, nactive 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(4) = term(4) + t3(dm_nocc, nactive, b,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,a,k,i,j)
     !                                     term(5) = term(5) + t3(dm_nocc, nactive, b,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,a,j,i,k)
     !                                     term(6) = term(6) + t3(dm_nocc, nactive, b,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,a,i,k,j)
     !                                     term(7) = term(7) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,k,i,j)
     !                                     term(8) = term(8) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,i,k,j)
     !                                     term(9) = term(9) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,j,i,k)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(4) = term(4) * 0.6666666666666669d+0 
     !       term(5) = term(5) * (-1.3333333333333337d+0) 
     !       term(6) = term(6) * (-1.3333333333333337d+0) 
     !       term(7) = term(7) * 0.33333333333333337d+0 
     !       term(8) = term(8) * (-0.6666666666666667d+0) 
     !       term(9) = term(9) * (-0.6666666666666667d+0) 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do d = dm_nocc + 1, nactive 
     !                         do c = dm_nocc + 1, nactive 
     !                               do j = 1, dm_nocc 
     !                                     term(10) = term(10) + t3(dm_nocc, nactive, b,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,a,k,j,i)
     !                                     term(11) = term(11) + t3(dm_nocc, nactive, b,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,a,i,j,k)
     !                                     term(12) = term(12) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,i,j,k)
     !                                     term(13) = term(13) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,k,j,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(10) = term(10) * (-1.3333333333333337d+0) 
     !       term(11) = term(11) * 0.6666666666666669d+0 
     !       term(12) = term(12) * 0.33333333333333337d+0 
     !       term(13) = term(13) * (-0.6666666666666667d+0) 

     !       do k = 1, dm_nocc 
     !             do i = 1, dm_nocc 
     !                   do j = 1, dm_nocc 
     !                         do d = dm_nocc + 1, nactive 
     !                               do c = dm_nocc + 1, nactive 
     !                                     term(14) = term(14) + t3(dm_nocc, nactive, b,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,a,j,k,i)
     !                                     term(15) = term(15) + t3(dm_nocc, nactive, a,d,c,i,j,k) * t3(dm_nocc, nactive, d,c,b,j,k,i)
     !                               end do
     !                         end do
     !                   end do
     !             end do
     !       end do

     !       term(14) = term(14) * 2.6666666666666674d+0 
     !       term(15) = term(15) * 1.3333333333333335d+0 


     !       calc_D_vv_ground_mbpt4_cc3 = 0.d+0 
     !       do s = 0, 15
     !             calc_D_vv_ground_mbpt4_cc3 = calc_D_vv_ground_mbpt4_cc3 + term(s)
     !       end do

     ! end function calc_D_vv_ground_mbpt4_cc3




end module density_matrix_ground



