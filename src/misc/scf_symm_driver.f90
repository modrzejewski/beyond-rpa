module scf_symm_driver
      use symmetry
      use slater_parser
      use math_constants
      use arithmetic
      use gparam
      use basis
      use display
      use string
      use periodic
      use parser
      use ecpint
      use gridfunc
      use orbextension
      use scf
      use scf_definitions
      use io

      implicit none

contains 

      subroutine scf_symm_slater(mocoeff, eorb, nactive, erhf, &
            nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, overlap, enucl)

            real(F64), dimension(:, :), allocatable, intent(out) ::  mocoeff
            real(F64), dimension(:), allocatable, intent(out) ::  eorb
            integer, intent(out) :: nactive
            real(F64), intent(out) :: erhf
            integer, intent(out) :: nocc0, nocc1, nvirt0, nvirt1
            integer, dimension(:, :), allocatable, intent(out) :: irrep0, irrep1
            real(F64), dimension(:, :), allocatable, intent(out) ::  overlap
            real(F64), intent(out) :: enucl

            real(F64), dimension(:, :), allocatable       :: kinetic, attraction
!            character(:), allocatable :: file_scf, file_1e, file_aux
            integer :: nft1, nft3, nft2
            integer, parameter :: rl1 = 8
            integer :: charge_a, charge_b
            integer :: l1, l2, i, j
            integer :: offset
            integer :: order
            real(F64) :: int_dist
            integer, dimension(:), allocatable :: rep
            real(F64), dimension(:, :), allocatable :: work1, work2
            integer :: workdim
            integer :: nocc, nvirt
            integer(I64) :: itemp
            real(F64)    :: rtemp
            integer :: l3
            real(F64), dimension(:), allocatable :: temp1, temp2, temp3


            ! file_scf = ROOTDIR//'slater-basis'//DIRSEP//'scfdata_be2_atcetcc2.F'
            ! file_aux = ROOTDIR//'slater-basis'//DIRSEP//'aux_be2_atcetcc2.F'
            ! file_1e = ROOTDIR//'slater-basis'//DIRSEP//'file1E_be2_atcetcc2.F'

            open(newunit=nft1, file=SLATER_FILE_AUX, access='stream',     form='unformatted', status='old')
            open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
            open(newunit=nft3, file=SLATER_FILE_SCF, access='sequential', form='unformatted', status='old')

            read(nft1) itemp
            read(nft1) itemp
            CC_NORB = itemp
            read(nft1) itemp
            read(nft1) rtemp
            int_dist = rtemp
            read(nft1) itemp
            charge_a = itemp
            read(nft1) itemp
            charge_b = itemp
            read(nft1) itemp
            order = itemp
            read(nft1) itemp
            allocate(rep(order))
            do i = 1, order
                  read(nft1) itemp
                  rep(i) = itemp
                  print*, 'rep', i, rep(i)
            end do



            close(nft1)

            print*, 'cc_norb', CC_NORB
            print*, charge_a, charge_b

            allocate(mocoeff(CC_NORB, CC_NORB))
            allocate(eorb(CC_NORB))
            allocate(overlap(CC_NORB, CC_NORB))

            l3 = CC_NORB * CC_NORB
            allocate(temp1(0:l3-1))

            read(nft3) 
            read(nft3)
            read(nft3)
            read(nft3) eorb

            close(nft3)

            print*, 'CC_NORB', CC_NORB
            print*, charge_a, charge_b, order, int_dist
            !
            ! First and last active occupied orbitals
            !
            nocc0 = 1
            nocc1 = (charge_a + charge_b)/2
            nocc = nocc1 - nocc0 + 1
            !
            ! First and last active virtual orbitals
            !                  
            nvirt0 = nocc + 1
            nvirt1 = CC_NORB
            nvirt = nvirt1 - nvirt0 + 1


            l1 = CC_NORB
            l2 = l1 * (l1 + 1) / 2
            call read_lower_triangle_norec(SLATER_FILE_SCF, 2, l2, CC_NORB, mocoeff)


            !
            ! Reading one-electron integrals
            !
            call read_lower_triangle(nft2, l2, 0, overlap)

            close(nft2)
            call smfill(mocoeff)
            call smfill(overlap)
            
            !                                                                                                                                                              
            ! Diagonalize Fock matrix in blocks                                                                                                                            
            !                                                                                                                                                                
            offset = 0
            do i = 1, order
                  workdim = rep(i)
                  if (workdim .ne.0)then
                        allocate(work1(workdim, workdim))
                        allocate(work2(workdim, workdim))
                        work1 = mocoeff(offset+1:offset + workdim, offset+1:offset+workdim)
                        work2 = overlap(offset+1:offset + workdim, offset+1:offset+workdim)
                        call gvd(work1, work2, eorb(offset+1:offset + workdim))

                        mocoeff(:, offset+1:offset+workdim) = zero
                        mocoeff(offset+1:offset + workdim, offset+1:offset+workdim) = work1
                        
                        offset = offset + workdim

                        deallocate(work1)
                        deallocate(work2)
                  end if

            end do

            do i = 1, CC_NORB
                  print*, eorb(i)
            end do

            allocate(irrep0(2, order))
            allocate(irrep1(2, order))
            call read_irrep2(mocoeff, eorb, order, irrep0, irrep1, rep, nocc)

            print*, 'irrep przed frozen'
            write(*, '(8I4)') irrep0(1, :)
            write(*, '(8I4)') irrep1(1, :)
            write(*, '(8I4)') irrep0(2, :)
            write(*, '(8I4)') irrep1(2, :)
            
            erhf = erhf_slater(eorb, mocoeff, nocc0, nocc1, nvirt0, nvirt1)
            if (abs(int_dist).gt.1.d-10) then
                  enucl = dble(charge_a)*dble(charge_b) / int_dist
            else
                  enucl = zero
            end if

            nactive = nocc + nvirt
            print*, 'mocoeff slater'
            do i = 1, CC_NORB
                  do j = 1, CC_NORB
                        if (abs( mocoeff(i, j)).gt.1.d-8)then
                              write(*,'(2I6, F26.11)')i, j,  mocoeff(i, j)
                        end if
                  end do
            end do
            print*, 'koniec'
      end subroutine scf_symm_slater


!       subroutine slater_test()

!             double precision, dimension(:, :), allocatable :: gvxao, gvxao2
!             integer :: nft2, l1, l2, i

!             allocate(gvxao(CC_NORB, CC_NORB))
!             allocate(gvxao2(CC_NORB, CC_NORB))
!             gvxao = zero
!             l1 = CC_NORB
!             l2 = l1 * (l1 + 1) / 2

!             print*, 'ttttttttt'
!             open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
!             call read_whole_matrix(nft2, l2, 29, 32, gvxao)
!             close(nft2)

!             open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
!             call read_whole_matrix(nft2, l2, 32, gvxao2)
!             close(nft2)
           
! !            gvxao = gvxao + gvxao2
!             print*, ''
!    !         do i = 1, CC_NORB
!                   print*, 'aa',  gvxao(7,97), gvxao(97, 7)
!          print*, 'aa',  gvxao2(7,97), gvxao2(97, 7)
         
!     !        end do

            
!             print*, 'snieznon'

!             deallocate(gvxao)


!       end subroutine slater_test



      subroutine scf_symm_gauss(mocoeff, eorb, nactive, erhf, &
            nocc0, nocc1, nvirt0, nvirt1, irrep0, irrep1, overlap, enucl)
            
            real(F64), dimension(:, :), allocatable, intent(out) ::  mocoeff
            real(F64), dimension(:), allocatable, intent(out) ::  eorb
            integer, intent(out) :: nactive
            real(F64), intent(out) :: erhf
            integer, intent(out) :: nocc0, nocc1, nvirt0, nvirt1
            integer, dimension(:, :), allocatable, intent(out) :: irrep0, irrep1
            real(F64), dimension(:, :), allocatable, intent(out) ::  overlap
            real(F64), intent(out) :: enucl

            real(F64), dimension(:, :), allocatable ::  rho_ao
            real(F64), dimension(:, :), allocatable ::  kinetic
            real(F64), dimension(:, :), allocatable ::  attraction
            real(F64), dimension(:, :), allocatable ::  hbare
            real(F64), dimension(:, :), allocatable ::  f_ao
            real(F64), dimension(:, :), allocatable ::  c
            real(F64), dimension(:, :), allocatable ::  eigv
            real(F64), dimension(:, :), allocatable ::  w

            real(F64), dimension(1, 1) :: dummy2
            real(F64), dimension(1) :: dummy1
            real(F64) :: eel, exc

            type(TXCDef) :: hartree_fock
            type(TGridDiag) :: gdiag
            type(TSCFOutput)  :: scfresults
            type(TSCFParams)    :: scfinput

            integer :: rep_dim
            integer :: idx0, idx1
            integer, dimension(:), allocatable :: rep, rep2
            integer :: i, j, k
            integer :: nexcluded
            integer :: order
            real(F64), dimension(:), allocatable :: p_c

            real(F64), dimension(:), allocatable :: work_evd
            integer, dimension(:), allocatable :: iwork_evd
            integer :: lwork, liwork


            if (CC_FROZEN .gt. NE/2)then
                  call msg("PARSER ERROR: BAD VALUE OF FROZEN ORBITALS SPECIFIED", &
                        priority=MSG_ERROR)
                  stop
            end if


            allocate(mocoeff(CC_NORB, CC_NORB)) 
            allocate(eorb(CC_NORB))

            if (POINT_GROUP == C2v) then
                  order = 4
            else if (POINT_GROUP == D2h) then
                  order = 8
            end if
            print*, 'order', order
            allocate(rho_ao(CC_NORB, CC_NORB))
            allocate(overlap(CC_NORB, CC_NORB))
            allocate(kinetic(CC_NORB, CC_NORB))
            allocate(attraction(CC_NORB, CC_NORB))
            allocate(hbare(CC_NORB, CC_NORB))
            allocate(f_ao(CC_NORB, CC_NORB))
            call stv(overlap, kinetic, attraction)
            call pseudopot(attraction)
            call smfill(overlap)
            call smfill(kinetic)
            call smfill(attraction)

            hbare = kinetic + attraction

            !
            ! Perform self-consistent RHF
            !
            call scfinput%init(XCF_HF)
            call scfinput%AUXIntegral(AUX_NONE, AUX_NONE)
            call ksdriver(scfresults, scfinput, rho_ao, eorb, &
                  outmatrix=SCF_OUT_RHO_AO)
            erhf = scfresults%EtotDFT
            enucl = scfresults%EtotDFT - scfresults%EelDFT
            nexcluded = scfresults%nexcluded

            allocate(rep(order))
            allocate(rep2(order))
            !
            ! Symmetrize basis
            !
            call spherical_basis(w)
            call symmetrize_arbitrary(w, rep)
            !
            ! Generate Fock matrix
            !
            call xcf_define(hartree_fock, XCF_HF, AUX_NONE, .false.)
            call ksgen(hartree_fock, f_ao, eel, exc, rho_ao, hbare, &
                  dummy2, gdiag, dummy1, dummy2, oaotransf=.false.)
            call smfill(f_ao)
!            print*, '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
            !
            ! Diagonalize Fock matrix in symmetry blocks
            !
            rep2 = 0
            allocate(p_c(CC_NORB))
            do i = 1, order
                  if (rep(i) .ne. 0) then
                        idx0 = sum(rep(1:i-1)) + 1
                        idx1 = idx0 + rep(i) - 1
                        rep_dim = idx1 - idx0 + 1

                        call nonredundant_basis(c, overlap, LINDEP_THRESH, w(:, idx0:idx1))

                        rep2(i) = size(c, dim=2)
                        idx0 = sum(rep2(1:i-1)) + 1
                        idx1 = idx0 + rep2(i) - 1
                        rep_dim = idx1 - idx0 + 1
                        
                        allocate(eigv(rep(i), rep(i)))
                        call diagonalize_transformed(eigv, eorb(idx0:idx1), f_ao, c)
                        call gemmwrap_simple('N', 'N', CC_NORB, rep_dim, rep_dim, one, &
                              c, eigv, zero, mocoeff(:, idx0:idx1))
                        deallocate(eigv)
                  end if
            end do

            allocate(irrep0(2, order))
            allocate(irrep1(2, order))
            call sort_mo_symm(mocoeff, eorb, rep2, irrep0, irrep1, nactive)

            nocc0 = 1
            nocc1 = ne/2
            nvirt0 = nocc1 - nocc0 + 1 + 1
            nvirt1 = nactive

            print*, 'NORB', NORB
            print*, 'CC_NORB', CC_NORB
            print*, 'CC_FROZEN', CC_FROZEN
            print*, 'nocc0', nocc0
            print*, 'nocc1', nocc1
            print*, 'nvirt0', nvirt0
            print*, 'nvirt1', nvirt1
            print*, 'nactive', nactive

            
            deallocate(rho_ao)
            deallocate(kinetic)
            deallocate(attraction)
            deallocate(hbare)
            deallocate(f_ao)
            deallocate(rep)
            deallocate(rep2)

      end subroutine scf_symm_gauss

      function erhf_slater(eorb, mocoeff, nocc0, nocc1, nvirt0, nvirt1)

            real(F64) :: erhf_slater
            real(F64), dimension(:), intent(in) :: eorb
            real(F64), dimension(:,:), intent(in) :: mocoeff
            integer, intent(in) :: nocc0, nocc1, nvirt0, nvirt1

            integer :: nft1, nft2
            integer :: charge_a, charge_b
            real(F64) :: int_dist
            real(F64), dimension(:, :), allocatable :: t_super
            character(:), allocatable :: file_1e, file_aux
            integer, parameter :: rl1 = 8
            integer :: l1, l2
            real(F64), dimension(:,:)    , allocatable                     :: Cocc
            real(F64), dimension(:,:)    , allocatable                     :: Cvirt
            real(F64), dimension(:,:)    , allocatable                     :: kinetic
            real(F64), dimension(:,:)    , allocatable                     :: attraction
            integer :: nocc, nvirt
            real(F64), dimension(:,:), allocatable :: oo
            integer :: i
            real(F64) :: rval
            integer :: ival

            nocc = nocc1 - nocc0 + 1
            nvirt = nvirt1 - nvirt0 + 1

            allocate(Cocc(CC_NORB, 1:nocc))
            allocate(Cvirt(CC_NORB, 1:nvirt))

            allocate(t_super(CC_NORB, CC_NORB))
            allocate(attraction(CC_NORB, CC_NORB))
            allocate(kinetic(CC_NORB, CC_NORB))
            allocate(oo(nocc,nocc))


            Cvirt = mocoeff(:, nvirt0: nvirt1)
            Cocc = mocoeff(:, nocc0: nocc1)

            open(newunit=nft1, file=SLATER_FILE_AUX, access='stream',     form='unformatted', status='old')

            read(nft1) ival
            read(nft1) ival
            read(nft1) ival
            read(nft1) int_dist
            read(nft1) charge_a
            read(nft1) charge_b
            close(nft1)

            l1 = CC_NORB
            l2 = l1 * (l1 + 1) / 2
            
                        open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
            call read_lower_triangle(nft2, l2, 1, kinetic)
            close(nft2)
            open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
            call read_lower_triangle(nft2, l2, 2, attraction)
            close(nft2)

            t_super = attraction + kinetic

            call smfill(t_super)
            call atbc3(oo, Cocc,  t_super, Cocc,  nocc,  nocc, CC_NORB)

            erhf_slater = zero
            do i = 1, nocc
                  erhf_slater = erhf_slater + oo(i, i) + eorb(i)
            end do

            if (abs(int_dist).gt.1.d-10)then
                  erhf_slater = erhf_slater + dble(charge_a)*dble(charge_b) / int_dist
            end if
            print*, 'erhf_slater', erhf_slater


            deallocate(Cocc)
            deallocate(Cvirt)
            deallocate(t_super)
            deallocate(attraction)
            deallocate(kinetic)
            deallocate(oo)

      end function erhf_slater

      subroutine spherical_basis(w)
            !
            ! Generate a matrix whose columns represent solid harmonic basis functions
            !
            real(F64), dimension(:, :), allocatable, intent(out) :: w

            integer :: ss, s, l, p, q, m, p0, v
            integer :: n_spher, l_max
            integer :: lx, ly, lz
            real(F64), dimension(:), allocatable :: c_spher
            real(F64) :: angdep
            integer :: n_indep
            integer :: nang

            n_spher = 0
            l_max = 0
            do ss = 1, NSHELL
                  s = SH(ss)
                  l = SHTYPE(s)
                  l_max = max(l_max, l)
                  n_spher = n_spher + 2 * l + 1
            end do

            allocate(w(NORB, n_spher))
            allocate(c_spher(numxyz(l_max)))

            w = ZERO                 
            q = 1
            do ss = 1, NSHELL
                  s = SH(ss)
                  l = SHTYPE(s)
                  nang = nfunc(l)
                  p0 = SHPOS(ss)
                  do m = -l, l
                        call rshu(c_spher, l, m)
                        do v = 1, nang
                              lx = ll(v, l)
                              ly = mm(v, l)
                              lz = nn(v, l)
                              !
                              ! The ANGDEP factor eliminates the dependence of normalized
                              ! Gaussians on the the index of the X^L Y^M Z^N function. Thanks
                              ! to this, the transformation to the solid harmonics basis is
                              ! as simple as for uncontracted primitives without any
                              ! LMN-dependent normalization.
                              !
                              angdep = CNTRNORM(v, s) / CNTRNORM(1, s)
                              w(p0 + v - 1, q) = c_spher(lxlylzpos(lx, ly, lz)) / angdep
                        end do
                        q = q + 1
                  end do
            end do
      end subroutine spherical_basis


      subroutine nonredundant_basis(c, overlap, lindep, vector_space)
            !
            ! Compute the OAO transformation matrix (C) with column vectors
            ! satisfying the following conditions:
            ! 1) the column vectors of C transform the HF (KS) generalized eigenproblem
            !    into an ordinary eigenvalue equation with unit overlap
            ! 2) the column vectors of C are linear combinations of the columns
            !    of VECTOR_SPACE
            ! 3) the columns of C span a linear independent subspace of VECTOR_SPACE,
            !    with the threshold for linear dependencies LINDEP
            ! 
            real(F64), dimension(:, :), allocatable, intent(out) :: c
            real(F64), dimension(:, :), contiguous, intent(in)   :: overlap
            real(F64), intent(in)                                :: lindep
            real(F64), dimension(:, :), intent(in)                 :: vector_space

            integer :: n_ao, n_basis, n_indep, p
            real(F64), dimension(:, :), allocatable :: st
            integer :: lwork, liwork
            real(F64), dimension(:), allocatable :: eig

            n_ao = size(vector_space, dim=1)
            n_basis = size(vector_space, dim=2)


            allocate(st(n_basis, n_basis))
            allocate(eig(n_basis))
            !
            ! Diagonalize the transformed overlap matrix to eliminate
            ! the eigenvectors corresponding to small eigenvalues of the
            ! Gram matrix
            !
            call diagonalize_transformed(st, eig, overlap, vector_space)
            n_indep = 0
            do p = 1, n_basis
                  if (eig(p) < lindep) then
                        exit
                  else
                        n_indep = n_indep + 1
                        st(:, n_indep) = st(:, n_indep) / sqrt(eig(n_indep))
                  end if
            end do

            !
            ! Compute the final vectors in AO basis
            !
            allocate(c(n_ao, n_indep))
            call gemmwrap("N", "N", n_ao, n_indep, n_basis, ONE, vector_space, st, ZERO, c)

      end subroutine nonredundant_basis

   
      subroutine diagonalize_transformed(at, eig, a, c)
            !
            ! Diagonalize the transformed matrix At = C^T A C
            !
            real(F64), dimension(:, :), intent(out) :: at
            real(F64), dimension(:), intent(out)    :: eig
            real(F64), dimension(:, :), intent(in)  :: a
            real(F64), dimension(:, :), intent(in)  :: c

            real(F64), dimension(:, :), allocatable :: work_transf
            real(F64), dimension(:), allocatable :: work_evd
            integer, dimension(:), allocatable :: iwork_evd
            integer :: m, n, lwork, liwork

            m = size(c, dim=1)
            n = size(c, dim=2)
            allocate(work_transf(n, m))

            call atba_transform(at, c, a, work_transf)
            call dsyevdquery(n, lwork, liwork, "V")
            allocate(work_evd(lwork))
            allocate(iwork_evd(liwork))
            call dsyevdwrap(at, eig, n, "V", work_evd, iwork_evd)

      end subroutine diagonalize_transformed

     subroutine atba_transform(c, a, b, scratch)
            real(F64), dimension(:, :), contiguous, intent(out) :: c
            real(F64), dimension(:, :), contiguous, intent(in)  :: a
            real(F64), dimension(:, :), contiguous, intent(in)  :: b
            real(F64), dimension(:, :), contiguous, intent(out) :: scratch

            integer :: n, k
            integer :: lda, ldb, ldc
            external :: dgemm

            n = size(a, dim=2)
            k = size(b, dim=2)
            lda = size(a, dim=1)
            ldb = size(b, dim=1)
            ldc = size(c, dim=1)

            if ((size(c, dim=1) .ne. size(a, dim=2)) .or. &
                  (size(a, dim=1) .ne. size(b, dim=2)) .or. &
                  (size(b, dim=1) .ne. size(b, dim=2))) then
                  call msg("INCONSISTENT DIMENSIONS ON ENTRY TO ATBA_TRANSFORM", MSG_ERROR)
                  stop
            end if

            if (size(scratch) < n * k) then
                  call msg("SCRATCH TOO SMALL FOR ATBA_TRANSFORM ", MSG_ERROR)
                  stop
            end if
            !
            ! SCRATCH <- A^T B
            !
            call dgemm("T", "N", n, k, k, ONE, a, lda, b, ldb, ZERO, scratch, n)
            !
            ! C <- SCRATCH A
            !
            call dgemm("N", "N", n, n, k, ONE, scratch, n, a, lda, ZERO, c, ldc)
      end subroutine atba_transform



end module scf_symm_driver
