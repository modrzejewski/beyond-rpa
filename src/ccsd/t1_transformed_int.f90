module t1_transformed_int

      use math_constants
      use gparam
      use basis
      use common_sub
      use slater_parser
      use scf
      use linalg
      use omp_lib
      use cc_gparams
      use cmpidx
      use io

      implicit none
      save

      ! real(F64), dimension(:,:,:,:), allocatable, save :: toooo
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tovoo
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tvooo
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tvovo
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tovov
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tvoov
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tvvoo
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tvvov
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tvvvo
      ! real(F64), dimension(:,:,:,:), allocatable, save :: tvvvv

      ! real(F64), dimension(:,:), allocatable, save :: too
      ! real(F64), dimension(:,:), allocatable, save :: tvo
      ! real(F64), dimension(:,:), allocatable, save :: tov
      ! real(F64), dimension(:,:), allocatable, save :: tvv
      
      integer, private :: nv, nv0

contains

      subroutine t1_transformed_integrals_init(nocc, nactive, nvirt, nvirt0)

            integer, intent(in) :: nocc
            integer, intent(in) :: nactive
            integer, intent(in) :: nvirt
            integer, intent(in) :: nvirt0
            integer(I64) :: vecsize_byte
            real(F64) :: vecsize_gib

            nv = nvirt
            nv0 = nvirt0


            allocate(tovoo(nocc, nocc + 1: nactive, nocc, nocc))
            allocate(tvvov(nocc+1: nactive, nocc+1: nactive, nocc, nocc+1: nactive))
            allocate(tvvvo(nocc+1: nactive, nocc+1: nactive, nocc+1: nactive, nocc))
            allocate(tvvoo(nocc+1: nactive, nocc+1: nactive, nocc, nocc))
            !allocate(tvvvv(nocc+1: nactive, nocc+1: nactive, nocc+1:nactive, nocc+1: nactive))
            allocate(ftvvvv(nvirt**2 * (nvirt**2 +1)/2))
            allocate(tovov(nocc, nocc+1: nactive, nocc, nocc+1: nactive))
            allocate(tvooo(nocc+1: nactive, nocc, nocc, nocc))
            allocate(tvoov(nocc+1: nactive, nocc, nocc, nocc+1:nactive))
            allocate(tvovo(nocc+1: nactive, nocc, nocc+1:nactive, nocc))
            allocate(toooo(nocc, nocc, nocc, nocc))
            allocate(too(nocc, nocc))
            allocate(tvo(nocc+1:nactive, nocc))
            allocate(tov(nocc, nocc+1:nactive))
            allocate(tvv(nocc+1:nactive, nocc+1:nactive))

            vecsize_byte = vectorsize(size(ftvvvv, dim=1, kind=I64))
            vecsize_gib = frombyte(vecsize_byte, GIBIBYTE)
            call dmsg("TVVVV INTEGRALS MEMORY REQUIREMENT", vecsize_gib, fmt="ES10.3")
            
      contains

            function vectorsize(n)
                  !                                                                                                                                          
                  ! Size of a single vector in bytes                                                                                                                           
                  !                                                                                                                                       
                  integer(I64) :: vectorsize
                  integer(I64), intent(in) :: n
                  
                  real(F64), dimension(1) :: t
                  
                  vectorsize = n * io_size_byte(t)
            end function vectorsize


      end subroutine t1_transformed_integrals_init

      subroutine t1_transformed_integrals_free()

            deallocate(tovoo)
            deallocate(tvvov)
            deallocate(tvvvo)
            deallocate(tvvoo)
            deallocate(ftvvvv)
            deallocate(tovov)
            deallocate(tvooo)
            deallocate(tvoov)
            deallocate(tvovo)
            deallocate(toooo)
            deallocate(too)
            deallocate(tvo)
            deallocate(tov)
            deallocate(tvv)
            
      end subroutine t1_transformed_integrals_free

      subroutine t1_transformed_integrals2(t1, mocoeff, nocc, nvirt, nactive, nocc0, nocc1, nvirt0, nvirt1)

            integer, intent(in)      :: nocc, nvirt
            integer, intent(in)      :: nactive
            integer, intent(in)      :: nocc0, nocc1
            integer, intent(in)      :: nvirt0, nvirt1
            real(F64), dimension(nocc + 1:nactive, nocc), intent(in)        :: t1
            real(F64), dimension(:,:), intent(in)                           :: mocoeff


            real(F64), dimension(max_nfunc**4)                             :: g_pqrs
            integer :: shell_max, shellp, shellq, shellr, shells
            integer :: shellpq, shellrs
            real(F64), dimension(:,:), allocatable                         :: C
            real(F64), dimension(:,:), allocatable                         :: XXmx
            real(F64), dimension(:,:), allocatable                         :: YYmx
            real(F64), dimension(:,:), allocatable                         :: XXocc
            real(F64), dimension(:,:), allocatable                         :: XXvirt
            real(F64), dimension(:,:), allocatable                         :: YYocc
            real(F64), dimension(:,:), allocatable                         :: YYvirt

            real(F64), dimension(:,:,:,:), allocatable                     :: t_super
            real(F64), dimension(:,:,:,:), allocatable                     :: u_super_vo
            real(F64), dimension(:,:,:,:), allocatable                     :: u_super_ov
            real(F64), dimension(:,:,:,:), allocatable                     :: u_super_vv
            real(F64), dimension(:,:,:,:), allocatable                     :: u_super_oo

            real(F64), dimension(:,:), allocatable                         :: u_vo
            real(F64), dimension(:,:), allocatable                         :: u_oo
            real(F64), dimension(:,:), allocatable                         :: u_ov
            real(F64), dimension(:,:), allocatable                         :: u_vv
            real(F64), dimension(:,:), allocatable                         :: w_vo
            real(F64), dimension(:,:), allocatable                         :: w_ov
            real(F64), dimension(:,:), allocatable                         :: w_vv
            real(F64), dimension(:,:), allocatable                         :: w_oo
            real(F64), dimension(:,:), allocatable                         :: work2

            integer :: k
            integer(I64) :: slp, slq, slr, sls
            integer :: p, q, r, s
            integer :: pp, qq, rr, ss
            integer :: shp, shq, shr, shs
            integer :: pshrt, qshrt

            integer :: oc1, vt1, oc2, vt2, oc3, vt3, oc4, vt4

            real(F64), dimension(:,:), allocatable                   :: t_super1
            real(F64), dimension(:,:), allocatable                   :: attraction
            real(F64), dimension(:,:), allocatable                   :: kinetic
            real(F64), dimension(:,:), allocatable                   :: overlap

!            character(:), allocatable :: file_1e, file_2e, file_aux
            integer :: nft1, nft2, nft3, l2
            integer, parameter :: rl1 = 8, rl3 = 16
            integer :: total_twoe
            integer(I64) :: label
            real(F64) :: val, ttt1, ttt2, valtemp
            integer :: l1
            integer :: v
            integer :: vvvv_idx
            integer :: vtbra, vtket

            allocate(C(CC_NORB, nactive))
            allocate(t_super1(CC_NORB, CC_NORB))
            allocate(attraction(CC_NORB, CC_NORB))
            allocate(kinetic(CC_NORB, CC_NORB))
            allocate(overlap(CC_NORB, CC_NORB))
            allocate(XXmx(CC_NORB, nactive))
            allocate(YYmx(CC_NORB, nactive))
            allocate(XXocc(CC_NORB, nocc))
            allocate(XXvirt(CC_NORB, nvirt))
            allocate(YYocc(CC_NORB, nocc))
            allocate(YYvirt(CC_NORB, nvirt))
            if (SLATER_BASIS) then
                  allocate(t_super(CC_NORB, CC_NORB, CC_NORB, CC_NORB))
            else
                  allocate(t_super(CC_NORB, CC_NORB, MAX_NFUNC, MAX_NFUNC))
            end if

            allocate(u_super_vo(CC_NORB, CC_NORB, nocc+1:nactive, nocc))
            allocate(u_super_ov(CC_NORB, CC_NORB, nocc, nocc+1:nactive))
            allocate(u_super_vv(CC_NORB, CC_NORB, nocc+1:nactive, nocc+1:nactive))
            allocate(u_super_oo(CC_NORB, CC_NORB, nocc, nocc))
            allocate(u_vo(nvirt, nocc))
            allocate(u_ov(nocc, nvirt))
            allocate(u_oo(nocc, nocc))
            allocate(u_vv(nvirt, nvirt))
            allocate(w_vo(nvirt, nocc))
            allocate(w_ov(nocc, nvirt))
            allocate(w_oo(nocc, nocc))
            allocate(w_vv(nvirt,nvirt))
            allocate(work2(CC_NORB, CC_NORB))

            C(:, 1:nocc) = mocoeff(:, nocc0:nocc1)
            C(:, nocc+1:nactive) = mocoeff(:, nvirt0:nvirt1)

            call big_xy_matrix(t1, C, XXmx, YYmx, nocc, nactive)

            XXocc  = XXmx(:, 1:nocc)
            XXvirt = XXmx(:, nocc+1: nactive)
            YYocc  = YYmx(:, 1:nocc)
            YYvirt = YYmx(:, nocc+1: nactive)


            if (SLATER_BASIS) then
!                  file_1e = ROOTDIR//'slater-basis'//DIRSEP//'file1E_be2_atcetcc2.F'
                  l1 = CC_NORB
                  l2 = l1 * (l1 + 1) / 2
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 0, overlap)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 1, kinetic)
                  close(nft2)
                  open(newunit=nft2, file=SLATER_FILE_1E,  access='stream',     form='unformatted', status='old')
                  call read_lower_triangle(nft2, l2, 2, attraction)
                  close(nft2)

            else
                  call stv(overlap, kinetic, attraction)
                  call pseudopot(attraction)
            end if

         !   if (not (SLATER_BASIS)) then
         !         call pseudopot(attraction)
         !   end if
            
            t_super1 = attraction + kinetic
            
            call smfill(t_super1)

            call atbc4(too, XXocc,   t_super1, YYocc,  nocc,  nocc, CC_NORB, work2)
            call atbc4(w_vo, XXvirt, t_super1, YYocc,  nvirt, nocc, CC_NORB, work2)
            call atbc4(w_ov, XXocc,  t_super1, YYvirt, nocc,  nvirt, CC_NORB, work2)
            call atbc4(w_vv, XXvirt, t_super1, YYvirt, nvirt, nvirt, CC_NORB, work2)
            
            tvo(nocc+1:nactive,1:nocc)  = w_vo(1:nvirt,1:nocc)
            tov(1:nocc, nocc+1:nactive) = w_ov(1:nocc,1:nvirt)
            tvv(nocc+1:nactive,nocc+1:nactive) = w_vv(1:nvirt,1:nvirt)

            if (SLATER_BASIS) then

                  ! file_2e = ROOTDIR//'slater-basis'//DIRSEP//'file2E_be2_atcetcc2.F'
                  ! file_aux = ROOTDIR//'slater-basis'//DIRSEP//'aux_be2_atcetcc2.F'
                  open(newunit=nft1, file=SLATER_FILE_AUX, access='stream', form='unformatted', status='old')
                  read(nft1) valtemp
                  read(nft1) valtemp
                  read(nft1) total_twoe
                  close(nft1)

                  open(newunit=nft3, file=SLATER_FILE_2E, access='stream',  form='unformatted', status='old')
                  
                  t_super = zero

                  do k = 1, total_twoe
                        read(nft3) label, val
                        slq = ishft(label, -48)
                        slp = iand(ishft(label, -32), 65535)
                        sls = iand(ishft(label, -16), 65535)
                        slr = iand(label, 65535)

                        ! if (r .eq. s) val = val*FRAC12                                                                                                         
                        ! if (p .eq. q) val = val*FRAC12                                                                                                         
                        ! if (r .eq. p .and. s .eq. q) val = val*FRAC12                                                                                          
                        t_super(slr, sls, slp, slq) = val
                        t_super(sls, slr, slp, slq) = val
                        t_super(slr, sls, slq, slp) = val
                        t_super(sls, slr, slq, slp) = val

                        t_super(slp, slq, slr, sls) = val
                        t_super(slp, slq, sls, slr) = val
                        t_super(slq, slp, slr, sls) = val
                        t_super(slq, slp, sls, slr) = val

                  end do
                  
                  close(nft3)
                  do slp = 1, CC_NORB
                        do slq = 1, CC_NORB
                              
                              call atbc4(u_ov, XXocc,  t_super(:, :, slp, slq), YYvirt, nocc,  nvirt, CC_NORB, work2)
                              call atbc4(u_vo, XXvirt, t_super(:, :, slp, slq), YYocc,  nvirt, nocc, CC_NORB, work2)
                              call atbc4(u_oo, XXocc,  t_super(:, :, slp, slq), YYocc,  nocc,  nocc, CC_NORB, work2)
                              call atbc4(u_vv, XXvirt, t_super(:, :, slp, slq), YYvirt, nvirt, nvirt, CC_NORB, work2)

                              do vt2 = nocc + 1, nactive
                                    do vt3 = nocc + 1, nactive
                                          u_super_vv(slp, slq, vt3, vt2) = u_vv(vt3-nocc,vt2-nocc)
                                    end do
                              end do

                              do vt2 = nocc + 1, nactive
                                    do oc1 =1, nocc
                                          u_super_ov(slp, slq, oc1, vt2) = u_ov(oc1,vt2-nocc)
                                    end do
                              end do

                              do vt2 = nocc + 1, nactive
                                    do oc1 =1, nocc
                                          u_super_vo(slp, slq, vt2, oc1) = u_vo(vt2-nocc, oc1)
                                    end do
                              end do


                              do oc2 = 1, nocc
                                    do oc1 = 1, nocc
                                          u_super_oo(slp, slq, oc1, oc2) = u_oo(oc1,oc2)
                                    end do
                              end do
                        end do
                  end do

            else

                  !
                  ! Convert (PQ|RS)--> (IJ|KL)
                  !
                  ! Two-step transformation:
                  ! 1. (PQ|KL) = (PQ|RS) C_{RK} C_{SL}
                  ! 2. (IJ|KL) = (PQ|KL) C_{PI} C_{QJ}

                  !$ ttt1 = omp_get_wtime()
                  shell_max = ((NSHELL + 1) * NSHELL) / 2

                  do shellpq = 1, shell_max
                        call decode_pq(shellpq, NSHELL, shellp, shellq)
                        
                        p = SHATOM(shellp)
                        q = SHATOM(shellq)
                        shp = SH(shellp)
                        shq = SH(shellq)

                        !$omp parallel private(shellrs, shellr, shells, g_pqrs, r, s, shr, shs, v, pp, qq, rr, ss, pshrt, qshrt) &
                        !$omp default(shared)     
                        !$omp do schedule(guided) 
                        do shellrs = 1, shell_max
                              call decode_pq(shellrs, NSHELL, shellr, shells)
                              
                              r = SHATOM(shellr)
                              s = SHATOM(shells)
                              shr = SH(shellr)
                              shs = SH(shells)

                              call ints2e(shq, q, shp, p, shr, r, shs, s, g_pqrs)

                              v = 1
                              do qq = SHPOS(shellq), SHPOS(shellq+1) - 1
                              do pp = SHPOS(shellp), SHPOS(shellp+1) - 1
                                    do rr = SHPOS(shellr), SHPOS(shellr+1) - 1
                                    do ss = SHPOS(shells), SHPOS(shells+1) - 1

                                          pshrt = pp-SHPOS(shellp)+1
                                          qshrt = qq-SHPOS(shellq)+1

                                          t_super(rr, ss, pshrt, qshrt) = g_pqrs(v)
                                          t_super(ss, rr, pshrt, qshrt) = g_pqrs(v)
                                          
                                          v = v + 1
                                          
                                    end do
                                    end do
                                    
                              end do
                              end do
                        end do
                        !$omp end do nowait   
                        !$omp end parallel

                        do qq = SHPOS(shellq), SHPOS(shellq+1) - 1
                              do pp = SHPOS(shellp), SHPOS(shellp+1) - 1

                                    pshrt= pp-SHPOS(shellp)+1
                                    qshrt= qq-SHPOS(shellq)+1
                                    call atbc4(u_ov, XXocc,  t_super(:,: , pshrt, qshrt), YYvirt, nocc,  nvirt, CC_NORB, work2)
                                    call atbc4(u_vo, XXvirt, t_super(:,: , pshrt, qshrt), YYocc,  nvirt, nocc,  CC_NORB, work2)
                                    call atbc4(u_oo, XXocc,  t_super(:,: , pshrt, qshrt), YYocc,  nocc,  nocc,  CC_NORB, work2)
                                    call atbc4(u_vv, XXvirt, t_super(:,: , pshrt, qshrt), YYvirt, nvirt, nvirt, CC_NORB, work2)

                                    do vt2 = nocc + 1, nactive
                                          do vt3 = nocc + 1, nactive
                                                u_super_vv(pp, qq, vt3, vt2) = u_vv(vt3-nocc,vt2-nocc)
                                                u_super_vv(qq, pp, vt3, vt2) = u_vv(vt3-nocc,vt2-nocc)
                                          end do
                                    end do

                                    do vt2 = nocc + 1, nactive
                                          do oc1 =1, nocc
                                                u_super_ov(pp, qq, oc1, vt2) = u_ov(oc1,vt2-nocc)
                                                u_super_ov(qq, pp, oc1, vt2) = u_ov(oc1,vt2-nocc)
                                          end do
                                    end do

                                    do vt2 = nocc + 1, nactive
                                          do oc1 =1, nocc
                                                u_super_vo(pp, qq, vt2, oc1) = u_vo(vt2-nocc, oc1)
                                                u_super_vo(qq, pp, vt2, oc1) = u_vo(vt2-nocc, oc1)
                                          end do
                                    end do


                                    do oc2 = 1, nocc
                                          do oc1 = 1, nocc
                                                u_super_oo(pp, qq, oc1, oc2) = u_oo(oc1,oc2)
                                                u_super_oo(qq, pp, oc1, oc2) = u_oo(oc1,oc2)
                                          end do
                                    end do
                              end do
                        end do
                  end do

                  !$ ttt2 = omp_get_wtime()                                                                                                                          
                  print*, 'czas na 1 polowe transformation', ttt2-ttt1

            end if


            ! do vt2 = nocc + 1, nactive
            !       do vt1 = nocc + 1, nactive
            !             call atbc4(w_vv, XXvirt, u_super_vv(:,:,vt1, vt2), YYvirt, nvirt, nvirt, CC_NORB, work2)
            !             do vt4 = nocc + 1, nactive
            !                   do vt3 = nocc + 1, nactive
            !                         tvvvv(vt3,vt4,vt1,vt2) = w_vv(vt3-nocc,vt4-nocc)
            !                   end do
            !             end do
            !       end do
            ! end do

            do vtket = 1, nvirt**2
                  vt1 = (vtket - 1) / nvirt + 1 + nocc
                  vt2 = vtket - (vt1 - nocc - 1) * nvirt + nocc
                  call atbc4(w_vv, XXvirt, u_super_vv(:,:,vt1, vt2), YYvirt, nvirt, nvirt, CC_NORB, work2)
                  do vtbra = 1, vtket
                        vt3 =(vtbra - 1) / nvirt + 1+ nocc
                        vt4 =vtbra -(vt3 - nocc - 1) * nvirt + nocc
                        vvvv_idx = p_ge_q2pq(vtket, vtbra, nvirt**2)
                        ftvvvv(vvvv_idx) =  w_vv(vt3-nocc,vt4-nocc)
                  end do
            end do

            do oc1 = 1, nocc
                  do oc2 =  1, nocc
                        
                        call atbc4(w_ov, XXocc,  u_super_oo(:,:,oc2, oc1), YYvirt, nocc,  nvirt, CC_NORB, work2)
                        call atbc4(w_vo, XXvirt, u_super_oo(:,:,oc2, oc1), YYocc,  nvirt, nocc, CC_NORB, work2)
                        call atbc4(w_vv, XXvirt, u_super_oo(:,:,oc2, oc1), YYvirt, nvirt, nvirt, CC_NORB, work2)
                        call atbc4(w_oo, XXocc,  u_super_oo(:,:,oc2, oc1), YYocc,  nocc,  nocc, CC_NORB, work2)

                        do vt4 = nocc + 1, nactive
                              do oc3 = 1, nocc
                                    tovoo(oc3,vt4,oc2,oc1) = w_ov(oc3, vt4-nocc)
                              end do
                        end do

                        do oc3 = 1, nocc
                              do vt1 =  nocc + 1, nactive
                                    tvooo(vt1,oc3,oc2,oc1) = w_vo(vt1-nocc, oc3)
                              end do
                        end do

                        do vt3 = nocc + 1, nactive
                              do vt1 =  nocc + 1, nactive
                                    tvvoo(vt1,vt3,oc2,oc1) = w_vv(vt1-nocc, vt3-nocc)
                              end do
                        end do

                        do oc4 = 1, nocc
                              do oc3 = 1, nocc
                                    toooo(oc3,oc4,oc2,oc1) = w_oo(oc3, oc4)
                              end do
                        end do

                  end do
            end do


            do vt2 = nocc + 1, nactive
                  do oc1 = 1, nocc

                        call atbc4(w_vo, XXvirt, u_super_vo(:,:,vt2, oc1), YYocc, nvirt, nocc, CC_NORB, work2)
                        call atbc4(w_vv, XXvirt, u_super_vo(:,:,vt2, oc1), YYvirt, nvirt, nvirt, CC_NORB, work2)
                        call atbc4(w_ov, XXocc,  u_super_vo(:,:,vt2, oc1), YYvirt, nocc, nvirt, CC_NORB, work2)

                        do oc2 = 1, nocc
                              do  vt3= nocc + 1, nactive
                                    tvovo(vt3,oc2,vt2, oc1) = w_vo(vt3-nocc, oc2)
                              end do
                        end do
                        do vt3 =  nocc + 1, nactive
                              do vt1 = nocc + 1, nactive
                                    tvvvo(vt1,vt3,vt2,oc1) = w_vv(vt1-nocc, vt3-nocc)
                              end do
                        end do
                  end do
            end do

            do vt2 = nocc + 1, nactive
                  do oc1 = 1, nocc

                        call atbc4(w_vv, XXvirt, u_super_ov(:,:,oc1,vt2), YYvirt, nvirt, nvirt, CC_NORB, work2)
                        call atbc4(w_ov, XXocc,  u_super_ov(:,:,oc1,vt2), YYvirt, nocc,  nvirt, CC_NORB, work2)
                        call atbc4(w_vo, XXvirt, u_super_ov(:,:,oc1,vt2), YYocc,  nvirt, nocc, CC_NORB, work2)

                        do vt4 = nocc + 1, nactive
                              do vt3 = nocc + 1, nactive
                                    tvvov(vt3,vt4,oc1,vt2) = w_vv(vt3-nocc,vt4-nocc)
                              end do
                        end do

                        do oc2 = 1, nocc
                              do  vt3= nocc + 1, nactive
                                    tovov(oc2,vt3,oc1,vt2) = w_ov(oc2,vt3-nocc)
                              end do
                        end do

                        do oc2 = 1, nocc
                              do  vt3= nocc + 1, nactive
                                    tvoov(vt3,oc2,oc1,vt2) = w_vo(vt3-nocc, oc2)
                              end do
                        end do

                  end do
            end do

            deallocate(C)
            deallocate(t_super1)
            deallocate(attraction)
            deallocate(kinetic)
            deallocate(overlap)
            deallocate(XXmx)
            deallocate(YYmx)
            deallocate(XXocc)
            deallocate(XXvirt)
            deallocate(YYocc)
            deallocate(YYvirt)
            deallocate(t_super)
            deallocate(u_super_vo)
            deallocate(u_super_ov)
            deallocate(u_super_vv)
            deallocate(u_super_oo)
            deallocate(u_vo)
            deallocate(u_ov)
            deallocate(u_oo)
            deallocate(u_vv)
            deallocate(w_vo)
            deallocate(w_ov)
            deallocate(w_oo)
            deallocate(w_vv)
            deallocate(work2)


      contains


            pure subroutine decode_pq(pq, n, p, q)
                  !                                                                                                                                   
                  ! Decode a lower-triangle compound index into individual                                                                            
                  ! indices:                                                                                                                             
                  ! PQ -> (P, Q)                     
                  ! Assumptions:
                  ! 0) P = 1, 2, ..., N,
                  !    Q = 1, 2, ..., N,
                  ! 1) P >= Q (diagonal indices admissible)
                  !
                  ! An example of how this algorithm traverses an N=3 triangle:
                  !
                  !      Q
                  !    1
                  ! P  2 5
                  !    3 6 4
                  !
                  integer, intent(in)  :: pq
                  integer, intent(in)  :: n
                  integer, intent(out) :: p
                  integer, intent(out) :: q

                  integer :: q_base
                  integer :: v
                  integer :: interval1
                  integer :: in1, in2
                  !
                  ! pq = (q_base - 1) * (n + 1) + v
                  !
                  q_base = (pq - 1) / (n + 1) + 1
                  v = pq - (n + 1) * (q_base - 1)
                  !
                  ! Decide if v is in interval_1 or interval_2:
                  ! in1 == 1 and in2 == 0 if v <= INTERVAL1
                  ! in1 == 0 and in2 == 1 if v > INTERVAL1
                  !
                  interval1 = n - q_base + 1
                  in2 = v / (interval1 + 1)
                  !
                  ! 1 -> 0, 0 -> 1
                  !
                  in1 = ieor((in2), 1)

                  p = in1 * (q_base + v - 1) + in2 * (v - interval1 + n - q_base)
                  q = in1 * q_base + in2 * interval1

            end subroutine decode_pq


      end subroutine t1_transformed_integrals2


      subroutine big_xy_matrix(t1, C, XXmx, YYmx, nocc, nactive)

            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc + 1:nactive,nocc), intent(in) :: t1
            real(F64), dimension(:,:), intent(in)  :: C
            real(F64), dimension(:,:), intent(out) :: XXmx
            real(F64), dimension(:,:), intent(out) :: YYmx
            real(F64), dimension(:,:), allocatable :: xmx
            real(F64), dimension(:,:), allocatable :: ymx

            allocate(xmx(nactive, nactive))
            allocate(ymx(nactive, nactive))


            call small_xy_matrix(t1, xmx, ymx, nocc, nactive)
            call dgemm('n','n',CC_NORB,nactive,nactive,1.d+0,C,CC_NORB,xmx,nactive,0.d+0,XXmx,CC_NORB)
            call dgemm('n','n',CC_NORB,nactive,nactive,1.d+0,C,CC_NORB,ymx,nactive,0.d+0,YYmx,CC_NORB)

      end subroutine big_xy_matrix

      subroutine small_xy_matrix(t1, xmx, ymx, nocc, nactive)
            integer, intent(in) :: nocc, nactive
            real(F64), dimension(nocc + 1:nactive,nocc), intent(in) :: t1
            real(F64), dimension(:,:), intent(out) :: xmx
            real(F64), dimension(:,:), intent(out) :: ymx
            real(F64), dimension(:,:), allocatable :: imx
            integer :: i, a

            allocate(imx(nactive, nactive))
            call identity_mx(nactive, imx)

            xmx = imx
            ymx = imx

            do i = 1, nocc
                  do a = nocc + 1, nactive
                        xmx(i, a) = xmx(i, a) - t1(a, i)
                  end do
            end do

            do i = 1, nocc
                  do a = nocc + 1, nactive
                        ymx(a, i) = ymx(a, i) + t1(a, i)
                  end do
            end do

      end subroutine small_xy_matrix

      subroutine identity_mx(n, imx)
            integer, intent(in) :: n
            real(F64), dimension(:,:), intent(out) :: imx

            integer :: i

            imx = 0.d+0

            do i = 1, n
                  imx(i, i) = 1.d+0
            end do

      end subroutine identity_mx


      function read_ftvvvv(vt3, vt4, vt1, vt2)
            real(F64) :: read_ftvvvv
            integer, intent(in) :: vt3, vt4, vt1, vt2
            
            integer :: vtbra, vtket
            integer :: idx


            vtbra = (vt3 - nv0) * nv + (vt4 - nv0) + 1
            vtket = (vt1 - nv0) * nv + (vt2 - nv0) + 1
            if (vtbra .gt. vtket) then
                  idx = p_ge_q2pq(vtbra, vtket, nv**2)
            else
                  idx = p_ge_q2pq(vtket, vtbra, nv**2)
            end if
            
            read_ftvvvv = ftvvvv(idx)
            
      end function read_ftvvvv

end module t1_transformed_int
