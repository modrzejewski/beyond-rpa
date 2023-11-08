module common_sub

      use gparam
      use basis
      use ints
      use linalg
      use omp_lib

      implicit none

contains

     subroutine test_calki_cc()
            integer :: k, l

            integer, dimension(1:4)                                               :: centers
            integer, dimension(1:4)                                               :: shell
            double precision, dimension(max_nfunc**4)                             :: g_abcd
            integer :: m1, m2
            double precision, dimension(:,:,:,:), allocatable                     :: t_super
  
            integer :: cc, cd, ca, cb, k0, k1, l0, l1
            integer :: m, n
            integer :: m0, mm1, n0, n1
            integer :: m3, m4
            integer :: p, q, r, s
            integer :: v
            integer :: licz, licz2

            allocate(t_super(CC_NORB, CC_NORB, CC_NORB, CC_NORB))

            licz = 0
            licz2 = 0
            ca_loop: do ca = 1, natom
                        centers(1) = ca
                        k0 = sh0(centers(1))
                        k1 = sh0(centers(1) + 1) - 1
                        cb_loop:do cb = 1, natom
                              centers(2) = cb
                              l0 = sh0(centers(2))
                              l1 = sh0(centers(2) + 1) - 1
                              kloop: do k = k0, k1
                                    shell(1) = sh(k)
                                    m1 = shtype(shell(1))
                                    llopp: do l = l0, l1
                                          shell(2) = sh(l)
                                          m2 = shtype(shell(2))

      g_abcd = 0.d+0

            cc_loop:do cc = 1, natom
                  centers(3) = cc
                  m0 = sh0(centers(3))
                  mm1 = sh0(centers(3) + 1) - 1
                  cd_loop:do cd = 1, natom
                        centers(4) = cd
                        n0 = sh0(centers(4))
                        n1 = sh0(centers(4) + 1) - 1
                        mloop: do m = m0, mm1
                              shell(3) = sh(m)
                              m3 = shtype(shell(3))
                              nloop: do n = n0, n1
                                    shell(4) = sh(n)
                                    m4 = shtype(shell(4))
                                    
                                    
                                    call prim2(shell, centers, g_abcd)
                                    v = 1
                                    ploop: do p = shpos(k), shpos(k) + nfunc(m1) -1
                                          qloop: do q = shpos(l),shpos(l)+ nfunc(m2)-1
                                                rloop: do r = shpos(m), shpos(m) +  nfunc(m3) - 1
                                                      sloop: do s = shpos(n), shpos(n) + nfunc(m4) - 1

                                                            t_super(r,s,p,q) = g_abcd(v)

                                                            v = v + 1
                                                      end do sloop
                                                end do rloop
                                          end do qloop
                                    end do ploop


                              end do nloop
                        end do mloop
                  end do cd_loop
            end do cc_loop
              end do llopp
        end do kloop
  end do cb_loop
end do ca_loop

end subroutine test_calki_cc
            




      subroutine t_pqrs(k, l, centers, shell, m1, m2, g_abcd, t_super)
            integer, intent(in) :: k
            integer, intent(in) :: l
            integer, dimension(:), intent(inout) :: centers
            integer, dimension(:), intent(inout) :: shell
            integer, intent(in) :: m1
            integer, intent(in) :: m2
            double precision, dimension(max_nfunc**4), intent(inout) :: g_abcd
            double precision, dimension(:,:,:,:), intent(out) :: t_super
            integer :: cc, cd
            integer :: m, n
            integer :: m0, mm1, n0, n1
            integer :: m3, m4
            integer :: p, q, r, s
            integer :: v

            g_abcd = 0.d+0

            cc_loop:do cc = 1, natom
                  centers(3) = cc
                  m0 = sh0(centers(3))
                  mm1 = sh0(centers(3) + 1) - 1
                  cd_loop:do cd = 1, natom
                        centers(4) = cd
                        n0 = sh0(centers(4))
                        n1 = sh0(centers(4) + 1) - 1
                        mloop: do m = m0, mm1
                              shell(3) = sh(m)
                              m3 = shtype(shell(3))
                              nloop: do n = n0, n1
                                    shell(4) = sh(n)
                                    m4 = shtype(shell(4))
                                    
                                    call ints2e(shell(1), centers(1), shell(2), centers(2), &
                                          shell(3), centers(3), shell(4), centers(4), g_abcd)

                                    v = 1
                                    ploop: do p = shpos(k), shpos(k) + nfunc(m1) -1
                                          qloop: do q = shpos(l),shpos(l)+ nfunc(m2)-1
                                                rloop: do r = shpos(m), shpos(m) +  nfunc(m3) - 1
                                                      sloop: do s = shpos(n), shpos(n) + nfunc(m4) - 1
                                                            
                                                            t_super(r,s,p,q) = g_abcd(v)
                                                            v = v + 1
                                                      end do sloop
                                                end do rloop
                                          end do qloop
                                    end do ploop

                              end do nloop
                        end do mloop
                  end do cd_loop
            end do cc_loop


      end subroutine t_pqrs

      subroutine mp(C, t, n, start)
            !
            ! Prints matrix C
            !
            double precision, dimension(:,:), intent(in) :: C
            character(*), intent(in) :: t
            integer :: i
            integer, intent(in) :: start
            integer, intent(in) :: n
            print*, '------------------------------------------------------------'
            print*, t
            print*, '------------------------------------------------------------'
            do i = start, start + n - 1
                  write(*,'(9F15.10)')  C(i, :)
            end do
            print*, '------------------------------------------------------------'

      end subroutine mp

      ! subroutine prim1(shell, centers, V_efg, kinetic_e, overlap)
      !       integer, dimension(1:2), intent(in) :: shell
      !       integer, dimension(1:2), intent(in) :: centers
      !       integer, dimension(1:2) :: primitive
      !       double precision, dimension(:), intent(out) :: kinetic_e
      !       double precision, dimension(1:max_nfunc**2) :: kinetic_e_work
      !       double precision, dimension(:), intent(out) :: V_efg
      !       double precision, dimension(1:max_nfunc**2) :: V_efg_work
      !       double precision, dimension(:), intent(out) :: overlap
      !       double precision, dimension(1:max_nfunc**2) :: overlap_work
      !       integer :: p, q, v, v1, v2, v3, v4
      !       integer :: m1, m2, m3, m4

      !       m1 = shtype(shell(1))
      !       m2 = shtype(shell(2))

      !       kinetic_e = 0.d+0
      !       V_efg = 0.d+0
      !       overlap = 0.d+0

      !       call ints1e(shell(1), centers(1), shell(2), centers(2), &
      !              overlap, kinetic_e, V_efg)

      !       ! ploop: do p = 1, nprm(shell(1))
      !       !       primitive(1) = p
      !       !       qloop: do q = 1, nprm(shell(2))
      !       !             primitive(2) = q

      !       !             call angular_loop_1el(shell, primitive, centers,&
      !       !                   V_efg_work, kinetic_e_work, overlap_work)
      !       !             v = 1

      !       !             v1loop: do v1 = 1, nfunc(m1)
      !       !                   v2loop: do v2 = 1, nfunc(m2)

      !       !                         V_efg(v) = V_efg(v) + V_efg_work(v)&
      !       !                               * nrml(v1, p, shell(1)) * nrml(v2, q, shell(2))

      !       !                         kinetic_e(v) = kinetic_e(v) +&
      !       !                               kinetic_e_work(v) * nrml(v1, p, shell(1)) * nrml(v2, q, shell(2))

      !       !                         overlap(v) = overlap(v) + &
      !       !                               overlap_work(v) *  nrml(v1, p, shell(1)) * nrml(v2, q, shell(2))

      !       !                         v = v + 1
      !       !                   end do v2loop
      !       !             end do v1loop
      !       !       end do qloop
      !       ! end do ploop


      ! end subroutine prim1

      subroutine prim2(shell, centers, g_abcd)
            integer, dimension(1:4), intent(in) :: shell
            integer, dimension(1:4), intent(in) :: centers
            double precision, dimension(:), intent(out) :: g_abcd


            ! double precision, dimension(max_nfunc**4) :: g_abcd_work
            ! integer, dimension(1:4) :: primitive
            ! integer :: p, q, r, s, v, v1, v2, v3, v4
            ! integer :: m1, m2, m3, m4

            ! m1 = shtype(shell(1))
            ! m2 = shtype(shell(2))
            ! m3 = shtype(shell(3))
            ! m4 = shtype(shell(4))


            call ints2e(shell(1), centers(1), shell(2), centers(2), &
                  shell(3), centers(3), shell(4), centers(4), g_abcd)

            ! g_abcd = 0.d+0

            ! ploop: do p = 1, nprm(shell(1))
            !       primitive(1) = p
            !       qloop: do q = 1, nprm(shell(2))
            !             primitive(2) = q
            !             rloop: do r = 1, nprm(shell(3))
            !                   primitive(3) = r
            !                   sloop: do s = 1, nprm(shell(4))
            !                         primitive(4) = s

            !                         call angular_loop_2el(shell, primitive, centers, g_abcd_work)

            !                         v = 1
            !                         v1loop: do v1 = 1, nfunc(m1)
            !                               v2loop: do v2 = 1, nfunc(m2)
            !                                     v3loop: do v3 = 1, nfunc(m3)
            !                                           v4loop: do v4 = 1, nfunc(m4)
            !                                                 g_abcd(v) = g_abcd(v) +&
            !                                                       g_abcd_work(v) * nrml(v1, p, shell(1)) * nrml(v2, q, shell(2))&
            !                                                       *nrml(v3, r, shell(3))*nrml(v4, s, shell(4))
            !                                                 v = v + 1
            !                                           end do v4loop
            !                                     end do v3loop
            !                               end do v2loop
            !                         end do v1loop
            !                   end do sloop
            !             end do rloop
            !       end do qloop
            ! end do ploop

      end subroutine prim2      

      function scalar(a, b, N)
            double precision :: scalar
            double precision, dimension(:) :: a
            double precision, dimension(:) :: b
            integer, intent(in) :: N
            integer :: i

            scalar = 0.d+0
            do i = 1, N
                  scalar = scalar + a(i) * b(i)
            end do

      end function scalar

end module common_sub
