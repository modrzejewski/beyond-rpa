module ccsd_transformed_integrals

      use math_constants
      use gparam
      use common_sub
      use linalg
      use scf
      use ecpint
      use slater_parser
      use omp_lib

      implicit none
      save

      double precision, allocatable :: oooo(:,:,:,:)
!      double precision, allocatable :: vvvv(:,:,:,:)
      double precision, allocatable :: vovo(:,:,:,:)
      double precision, allocatable :: vvvo(:,:,:,:)
      double precision, allocatable :: vvoo(:,:,:,:)
      double precision, allocatable :: vooo(:,:,:,:)

      double precision, allocatable :: oo(:,:)
      double precision, allocatable :: vo(:,:)
      double precision, allocatable :: ov(:,:)
      double precision, allocatable :: vv(:,:)
      
contains

      subroutine transformed_integrals_init(nocc, nactive)
            integer, intent(in) :: nocc
            integer, intent(in) :: nactive
            
            allocate(oooo(nocc, nocc, nocc, nocc))
!            allocate(vvvv(nocc+1: nactive,nocc+1: nactive,nocc+1: nactive,nocc+1: nactive))
            allocate(vovo(nocc+1: nactive, nocc, nocc+1: nactive, nocc))
            allocate(vvoo(nocc+1: nactive,nocc+1: nactive,nocc, nocc))
            allocate(vvvo(nocc+1: nactive,nocc+1: nactive,nocc+1:nactive,nocc))
            allocate(vooo(nocc+1: nactive, nocc, nocc, nocc))
            allocate(oo(nocc, nocc))
            allocate(vo(nocc+1:nactive, nocc))
            allocate(ov(nocc, nocc+1:nactive))
            allocate(vv(nocc+1:nactive, nocc+1:nactive))
            
      end subroutine transformed_integrals_init

      subroutine transformed_integrals_free()
            
            if (allocated(oooo)) deallocate(oooo)
            if (allocated(vovo))deallocate(vovo)
            if (allocated(vvoo))deallocate(vvoo)
            if (allocated(vvvo))deallocate(vvvo)
            if (allocated(vooo))deallocate(vooo)
            if (allocated(oo))deallocate(oo)
            if (allocated(vo))deallocate(vo)
            if (allocated(ov))deallocate(ov)
            if (allocated(vv))deallocate(vv)

      end subroutine transformed_integrals_free

      subroutine transformed_integrals(Corb, nocc, nvirt, nactive, nocc0, nocc1, nvirt0, nvirt1)

            real(F64), dimension(:,:), intent(in)    :: Corb
            integer, intent(in)                      :: nocc, nvirt
            integer, intent(in)                      :: nactive
            integer, intent(in)                      :: nocc0, nocc1
            integer, intent(in)                      :: nvirt0, nvirt1

            real(F64), dimension(max_nfunc**4)       :: g_pqrs
            integer :: shell_max, shellp
            integer :: shellq, shellr, shells
            integer :: shellpq, shellrs
            
            real(F64), dimension(:,:)    , allocatable    :: Cocc
            real(F64), dimension(:,:)    , allocatable    :: Cvirt
            real(F64), dimension(:,:,:,:), allocatable    :: t_super
            real(F64), dimension(:,:,:,:), allocatable    :: u_super_vo
            real(F64), dimension(:,:,:,:), allocatable    :: u_super_oo
            real(F64), dimension(:,:,:,:), allocatable    :: u_super_vv

            real(F64), dimension(:,:), allocatable        :: u_vo
            real(F64), dimension(:,:), allocatable        :: u_oo
            real(F64), dimension(:,:), allocatable        :: u_vv
            real(F64), dimension(:,:), allocatable        :: w_vo
            real(F64), dimension(:,:), allocatable        :: w_oo
            real(F64), dimension(:,:), allocatable        :: w_vv
            real(F64), dimension(:,:), allocatable        :: t_super1
            real(F64), dimension(:,:), allocatable        :: attraction
            real(F64), dimension(:,:), allocatable        :: kinetic
            real(F64), dimension(:,:), allocatable        :: overlap
            real(F64), dimension(:,:), allocatable        :: work2

            integer      :: l1, k
            integer(I64) :: slp, slq, slr, sls
            integer      :: p, q, r, s
            integer      :: pp, qq, rr, ss
            integer      :: shp, shq, shr, shs
            integer      :: v
            integer      :: oc1, vt1, oc2, vt2
            integer      :: oc3, vt3, oc4, vt4
            integer :: pshrt, qshrt

!            character(:), allocatable :: file_1e, file_2e, file_aux
            integer, parameter :: rl1 = 8, rl3 = 16
            integer :: nft1, nft2, nft3, l2

            integer :: total_twoe, i
            integer(I64) :: label
            real(F64) :: val, ttt1, ttt2, t_tot1, t_tot2, valtemp


            !$ t_tot1 = omp_get_wtime()
            allocate(Cocc(CC_NORB, 1:nocc))
            allocate(Cvirt(CC_NORB, 1:nvirt))

           if (SLATER_BASIS) then
                  allocate(t_super(CC_NORB, CC_NORB, CC_NORB, CC_NORB))
            else
                  allocate(t_super(CC_NORB, CC_NORB, MAX_NFUNC, MAX_NFUNC))
            end if

            allocate(u_super_vo(CC_NORB, CC_NORB, nocc+1:nactive, nocc))
            allocate(u_super_vv(CC_NORB, CC_NORB, nocc+1:nactive, nocc+1:nactive))
            allocate(u_super_oo(CC_NORB, CC_NORB, nocc, nocc))
            allocate(u_vo(nocc+1:nactive, nocc))
            allocate(u_oo(nocc, nocc))
            allocate(u_vv(nocc + 1:nactive, nocc+1:nactive))
            allocate(w_vo(nocc+1:nactive, nocc))
            allocate(w_oo(nocc, nocc))
            allocate(w_vv(nocc+1:nactive, nocc+1:nactive))
            allocate(t_super1(CC_NORB, CC_NORB))
            allocate(attraction(CC_NORB, CC_NORB))
            allocate(kinetic(CC_NORB, CC_NORB))
            allocate(overlap(CC_NORB, CC_NORB))
            allocate(work2(CC_NORB, CC_NORB))


            Cvirt = Corb(:, nvirt0: nvirt1)
            Cocc = Corb(:, nocc0: nocc1)

            
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

    !        if (not (SLATER_BASIS)) then
     !             call pseudopot(attraction)
     !       end if

            t_super1 = attraction + kinetic
            
            call smfill(t_super1)
            call atbc4(oo, Cocc,  t_super1, Cocc,  nocc,  nocc, CC_NORB, work2)
            call atbc4(vo, Cvirt, t_super1, Cocc,  nvirt, nocc, CC_NORB, work2)
            call atbc4(ov, Cocc,  t_super1, Cvirt, nocc,  nvirt, CC_NORB, work2)
            call atbc4(vv, Cvirt, t_super1, Cvirt, nvirt, nvirt, CC_NORB, work2)

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
                              
                              call atbc4(u_vo, Cvirt, t_super(:, :, slp, slq), Cocc, nvirt, nocc, CC_NORB, work2)
                              call atbc4(u_oo, Cocc,  t_super(:, :, slp, slq), Cocc, nocc,  nocc, CC_NORB, work2)
                              call atbc4(u_vv, Cvirt, t_super(:, :, slp, slq), Cvirt, nvirt, nvirt, CC_NORB, work2)
                              
                              
                              do vt2 = nocc + 1, nactive
                                    do vt3 = nocc + 1, nactive
                                          u_super_vv(slp, slq, vt3, vt2) = u_vv(vt3,vt2)
                                    end do
                              end do
                              
                              do vt2 = nocc + 1, nactive
                                    do oc1 =1, nocc
                                          u_super_vo(slp, slq, vt2, oc1) = u_vo(vt2, oc1)
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
                  !$ ttt1 = omp_get_wtime()
                  !
                  ! Transform (PQ|RS)--> (IJ|KL)
                  !
                  ! Two-step transformation:
                  ! 1. (PQ|KL) = (PQ|RS) C_{RK} C_{SL}
                  ! 2. (IJ|KL) = (PQ|KL) C_{PI} C_{QJ}

                  shell_max = ((NSHELL + 1) * NSHELL) / 2

                  do shellpq = 1, shell_max
                        call decode_pq(shellpq, NSHELL, shellp, shellq)

                        p = SHATOM(shellp)
                        q = SHATOM(shellq)
                        shp = SH(shellp)
                        shq = SH(shellq)

                        g_pqrs = zero

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

                                    call atbc4(u_vo, Cvirt, t_super(:,:, pshrt, qshrt), Cocc, nvirt, nocc, CC_NORB, work2)
                                    call atbc4(u_oo, Cocc,  t_super(:,:, pshrt, qshrt), Cocc, nocc,  nocc, CC_NORB, work2)
                                    call atbc4(u_vv, Cvirt, t_super(:,:, pshrt, qshrt), Cvirt, nvirt, nvirt, CC_NORB, work2)
                                    
                                    do oc1 = 1, nocc
                                          do vt1 =  nocc + 1, nactive
                                                u_super_vo(pp, qq, vt1, oc1) = u_vo(vt1, oc1)
                                                u_super_vo(qq, pp, vt1, oc1) = u_vo(vt1, oc1)
                                          end do
                                    end do
                                    
                                    do oc2 = 1, nocc
                                          do oc1 = 1, nocc
                                                u_super_oo(pp, qq, oc1, oc2) = u_oo(oc1,oc2)
                                                u_super_oo(qq, pp, oc1, oc2) = u_oo(oc1,oc2)
                                          end do
                                    end do
                                    
                                    do vt2 = nocc + 1, nactive
                                          do vt1 = nocc + 1, nactive
                                                u_super_vv(pp, qq, vt1, vt2) = u_vv(vt1,vt2)
                                                u_super_vv(qq, pp, vt1, vt2) = u_vv(vt1,vt2)
                                          end do
                                    end do
                              end do
                        end do
                  end do
                  
            end if

            !$ ttt2 = omp_get_wtime()
            print*, 'ccsd transform 1 polowa', ttt2-ttt1
            
            do oc1 = 1, nocc
                  do oc2 =  1, nocc
                        
                        w_vv = 0.d+0
                        w_oo = 0.d+0
                        w_vo = 0.d+0

                        call atbc4(w_vv, Cvirt, u_super_oo(:,:,oc2, oc1),  Cvirt, nvirt, nvirt, CC_NORB, work2)
                        call atbc4(w_oo, Cocc,  u_super_oo(:,:,oc2, oc1),  Cocc,  nocc,  nocc, CC_NORB, work2)
                        call atbc4(w_vo, Cvirt, u_super_oo(:,:,oc2, oc1),  Cocc,  nvirt, nocc, CC_NORB, work2)

                        do vt4 =  nocc + 1, nactive
                              do vt3 = nocc + 1, nactive
                                    vvoo(vt3,vt4,oc2,oc1) = w_vv(vt3, vt4)
                              end do
                        end do

                        do oc4 =  1, nocc
                              do oc3 = 1, nocc
                                    oooo(oc3,oc4,oc2,oc1) = w_oo(oc3, oc4)
                              end do
                        end do

                        do oc3 = 1, nocc
                              do vt1 =  nocc + 1, nactive
                                    vooo(vt1,oc3,oc2,oc1) = w_vo(vt1, oc3)
                              end do
                        end do

                  end do
            end do

            do oc1 =  1, nocc
                  do vt1 = nocc + 1, nactive

                        w_vo = 0.d+0
                        w_vv = 0.d+0

                        call atbc4(w_vo, Cvirt, u_super_vo(:,:,vt1, oc1), Cocc,   nvirt, nocc, CC_NORB, work2)
                        call atbc4(w_vv, Cvirt, u_super_vo(:,:,vt1, oc1), Cvirt,  nvirt, nvirt, CC_NORB, work2)


                        do oc2 = 1, nocc
                              do vt2 =  nocc + 1, nactive
                                    vovo(vt2,oc2,vt1,oc1) = w_vo(vt2, oc2)
                              end do
                        end do

                        do vt4 =  nocc + 1, nactive
                              do vt3 = nocc + 1, nactive
                                    vvvo(vt3,vt4,vt1,oc1) = w_vv(vt3, vt4)
                              end do
                        end do

                  end do
            end do

            ! do vt2 = nocc + 1, nactive
            !       do vt1 =nocc + 1, nactive

            !             w_vv = 0.d+0
            !             call atbc4(w_vv, Cvirt, u_super_vv(:,:,vt1, vt2), Cvirt, nvirt, nvirt, CC_NORB, work2)
            !             do vt4 = nocc + 1, nactive
            !                   do vt3 = nocc + 1, nactive
            !                         vvvv(vt3,vt4,vt1,vt2) = w_vv(vt3,vt4)
            !                   end do
            !             end do
            !       end do
            ! end do
            !$ t_tot2 = omp_get_wtime()
            print*, 'cala procedura', t_tot2 - t_tot1

            deallocate(Cocc)
            deallocate(Cvirt)
            deallocate(t_super)
            deallocate(u_super_vo)
            deallocate(u_super_vv)
            deallocate(u_super_oo)
            deallocate(u_vo)
            deallocate(u_oo)
            deallocate(u_vv)
            deallocate(w_vo)
            deallocate(w_oo)
            deallocate(w_vv)
            deallocate(t_super1)
            deallocate(attraction)
            deallocate(kinetic)
            deallocate(overlap)


      end subroutine transformed_integrals

end module ccsd_transformed_integrals
