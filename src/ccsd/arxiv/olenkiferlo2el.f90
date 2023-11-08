module olenkiferlo2el
      use gparam
      use oboys
      use math_constants
      use grad_hg
      use hermite_coulomb
      use lmn

      implicit none
            

contains

      subroutine angular_loop_2el(shell, primitive, centers, g_abcd)
            ! shell - rozdzaje powloki
            integer, dimension(1:4), intent(in) :: shell

            ! primitive - rodzaje prymitywów
            integer, dimension(1:4), intent(in) :: primitive

            ! centers - indeksy centrów
            integer, dimension(1:4), intent(in) :: centers
            
            !two electron integrals
            double precision, dimension(:), intent(inout) :: g_abcd
            double precision, dimension(1:6, max_nfunc**4) :: g_g_abcd

            double precision :: expa
            double precision :: expb
            double precision :: expc
            double precision :: expd
            double precision, dimension(1:3) :: XYZab
            double precision, dimension(1:3) :: XYZcd
            double precision, dimension(1:3) :: XYZpa
            double precision, dimension(1:3) :: XYZpb
            double precision, dimension(1:3) :: XYZqc
            double precision, dimension(1:3) :: XYZqd
            double precision, dimension(1:3) :: XYZpq
            double precision :: Ax, Ay, Az
            double precision :: Bx, By, Bz
            double precision :: Cx, Cy, Cz
            double precision :: Dx, Dy, Dz
            double precision :: Px, Py, Pz
            double precision :: Qx, Qy, Qz
            double precision, dimension(1:3) :: Kab 
            double precision, dimension(1:3) :: Kcd 
            double precision :: C_abcd 
 

            !degrees of tables Rtuv and E
            integer :: deg_ab, deg_cd, deg_abcd
            double precision :: p, q
            double precision :: mua, mub, alpha

            !tables Rtuv, E
            double precision, allocatable :: Rtuv(:, :, :, :)
            double precision, allocatable :: EXab (:, :, :)
            double precision, allocatable :: EYab (:, :, :)
            double precision, allocatable :: EZab (:, :, :)
            double precision, allocatable :: EXcd (:, :, :)
            double precision, allocatable :: EYcd (:, :, :)
            double precision, allocatable :: EZcd (:, :, :)

            double precision, allocatable :: gEXab (:, :, :)
            double precision, allocatable :: gEYab (:, :, :)
            double precision, allocatable :: gEZab (:, :, :)
            double precision, allocatable :: gEXcd (:, :, :)
            double precision, allocatable :: gEYcd (:, :, :)
            double precision, allocatable :: gEZcd (:, :, :)

            !tables of momentum combinations
            integer, allocatable :: lmna(:,:)
            integer, allocatable :: lmnb(:,:)
            integer, allocatable :: lmnc(:,:)
            integer, allocatable :: lmnd(:,:)

            !number of momentum combinations for shell
            integer :: sa, sb, sc, sd
            integer :: m1, m2, m3, m4

            ! irrelevant
            character(len=2), dimension(1:6) :: name 

            !supplementary variables
            integer :: i
            integer :: lg, li, lj, lk, ll

            name(1) = 'Ax'
            name(2) = 'Ay'
            name(3) = 'Az'
            name(4) = 'Bx'
            name(5) = 'By'
            name(6) = 'Bz'

            expa = expn(primitive(1), shell(1))
            expb = expn(primitive(2), shell(2))
            expc = expn(primitive(3), shell(3))
            expd = expn(primitive(4), shell(4))
            
            !********************************
            ! Evaluation of parameters for given expa, expb, expc, expd
            p = expa + expb
            q = expc + expd
            mua = expa * expb / p
            mub = expc * expd / q
            alpha = p * q / (p + q)
            C_abcd = 2.d+0 * pi ** ( 5.d+0 / 2.d+0) / ( p * q *sqrt(p + q))
            !********************************

            Ax = atomr(1,centers(1))
            Ay = atomr(2,centers(1))
            Az = atomr(3,centers(1))

            Bx = atomr(1,centers(2))
            By = atomr(2,centers(2))
            Bz = atomr(3,centers(2))

            Cx = atomr(1,centers(3))
            Cy = atomr(2,centers(3))
            Cz = atomr(3,centers(3))

            Dx = atomr(1,centers(4))
            Dy = atomr(2,centers(4))
            Dz = atomr(3,centers(4))

            Px = (expa * Ax + expb * Bx) / p
            Py = (expa * Ay + expb * By) / p
            Pz = (expa * Az + expb * Bz) / p

            Qx = (expc * Cx + expd * Dx) / q
            Qy = (expc * Cy + expd * Dy) / q
            Qz = (expc * Cz + expd * Dz) / q
        
            !********************************
            ! Evaluation of parameters for given centers
            XYZpa = (/ Px - Ax, Py - Ay, Pz - Az/)
            XYZpb = (/ Px - Bx, Py - By, Pz - Bz/)
            XYZqc = (/ Qx - Cx, Qy - Cy, Qz - Cz/)
          
            XYZqd = (/ Qx - Dx, Qy - Dy, Qz - Dz/)
            XYZpq = (/ Px - Qx, Py - Qy, Pz - Qz/)
            XYZab = (/ Ax - Bx, Ay - By, Az - Bz/)
            XYZcd = (/ Cx - Dx, Cy - Dy, Cz - Dz/)
            Kab(1) = exp(-mua * XYZab(1)**2)
            Kab(2) = exp(-mua * XYZab(2)**2)
            Kab(3) = exp(-mua * XYZab(3)**2)
            Kcd(1) = exp(-mub * XYZcd(1)**2)
            Kcd(2) = exp(-mub * XYZcd(2)**2)
            Kcd(3) = exp(-mub * XYZcd(3)**2)
          
            !********************************
            ! Evaluation of number of angular functions
            m1 = shtype(shell(1))
            m2 = shtype(shell(2))
            m3 = shtype(shell(3))
            m4 = shtype(shell(4))
            sa = nfunco(m1)
            sb = nfunco(m2)
            sc = nfunco(m3)
            sd = nfunco(m4)
            !********************************

            allocate(lmna(1:3, 1:sa))
            allocate(lmnb(1:3, 1:sb))
            allocate(lmnc(1:3, 1:sc))
            allocate(lmnd(1:3, 1:sd))
  

            call ang_mom(m1, sa, lmna)
            call ang_mom(m2, sb, lmnb)
            call ang_mom(m3, sc, lmnc)
            call ang_mom(m4, sd, lmnd)

            deg_ab = m1 + m2
            deg_cd = m3 + m4
           
            deg_abcd = deg_ab + deg_cd + 1

            allocate(Rtuv(-2: deg_abcd, -2: deg_abcd, -2:deg_abcd, 0: deg_abcd))
            allocate(EXab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(EYab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(EZab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(EXcd (0:deg_cd, 0:deg_cd, 0:deg_cd))
            allocate(EYcd (0:deg_cd, 0:deg_cd, 0:deg_cd))
            allocate(EZcd (0:deg_cd, 0:deg_cd, 0:deg_cd))

            allocate(gEXab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(gEYab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(gEZab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(gEXcd (0:deg_cd, 0:deg_cd, 0:deg_cd))
            allocate(gEYcd (0:deg_cd, 0:deg_cd, 0:deg_cd))
            allocate(gEZcd (0:deg_cd, 0:deg_cd, 0:deg_cd))

            call R_tuv(deg_abcd, XYZpq(1), XYZpq(2), XYZpq(3), Rtuv, alpha)
          
            call hermite2(deg_ab, deg_cd, XYZab, XYZcd,XYZpa, XYZpb,&
                  XYZqc, XYZqd, Kab, Kcd, p, q, expa, expc, mua, mub, EXab, EYab, EZab,&
                  EXcd, EYcd, EZcd, gEXab, gEYab, gEZab, gEXcd, gEYcd, gEZcd)
          
            g_abcd = 0.d+0
            lg = 1
            do li = 1, sa
                  do lj = 1, sb
                        do lk = 1, sc
                              do ll = 1, sd
                                    call integral_lmnk(C_abcd,lg, li, lj, lk, ll, lmna, lmnb, lmnc, lmnd, Rtuv,EXab, EYab, EZab,&
                                          EXcd, EYcd, EZcd, gEXab, gEYab, gEZab, gEXcd, gEYcd,&
                                          gEZcd, g_abcd, g_g_abcd, expa, expb, p)
                                    lg = lg + 1
                              end do
                        end do
                  end do
            end do

            deallocate(lmna)
            deallocate(lmnb)
            deallocate(lmnc)
            deallocate(lmnd)
          
            deallocate(Rtuv)
            deallocate(EXab)
            deallocate(EYab)
            deallocate(EZab)
            deallocate(EXcd)
            deallocate(EYcd)
            deallocate(EZcd)

            deallocate(gEXab)
            deallocate(gEYab)
            deallocate(gEZab)
            deallocate(gEXcd)
            deallocate(gEYcd)
            deallocate(gEZcd)


      end subroutine angular_loop_2el

      subroutine integral_lmnk(C_abcd,lg,  li, lj, lk, ll, lmna, lmnb, lmnc, lmnd, Rtuv,EXab, EYab, EZab,&
            EXcd, EYcd, EZcd, gEXab, gEYab, gEZab, gEXcd, gEYcd, gEZcd, g_abcd, g_g_abcd, ae, be, p)
            
            double precision, intent(in) :: C_abcd
            double precision, dimension(-2:,-2:,-2:,0:), intent(in) :: Rtuv
            double precision, dimension(0:,0:,0:), intent(in) :: EXab
            double precision, dimension(0:,0:,0:), intent(in) :: EYab
            double precision, dimension(0:,0:,0:), intent(in) :: EZab
            double precision, dimension(0:,0:,0:), intent(in) :: EXcd
            double precision, dimension(0:,0:,0:), intent(in) :: EYcd
            double precision, dimension(0:,0:,0:), intent(in) :: EZcd

            double precision, dimension(0:,0:,0:), intent(in) :: gEXab
            double precision, dimension(0:,0:,0:), intent(in) :: gEYab
            double precision, dimension(0:,0:,0:), intent(in) :: gEZab
            double precision, dimension(0:,0:,0:), intent(in) :: gEXcd
            double precision, dimension(0:,0:,0:), intent(in) :: gEYcd
            double precision, dimension(0:,0:,0:), intent(in) :: gEZcd

            double precision, dimension(:) :: g_abcd
            double precision, dimension(:,:) :: g_g_abcd
            double precision, intent(in) :: ae
            double precision, intent(in) :: be
            double precision, intent(in) :: p
            integer, intent(in) :: lg,li, lj, lk, ll
            integer, dimension(:,:), intent(in) :: lmna(:,:)
            integer, dimension(:,:), intent(in) :: lmnb(:,:)
            integer, dimension(:,:), intent(in) :: lmnc(:,:)
            integer, dimension(:,:), intent(in) :: lmnd(:,:)

            double precision :: s3, s4, s2, s5, s6, s1
            double precision :: gs3x, gs4x, gs2x, gs5x, gs6x, grad_sum1x, grad_sum2x
            double precision :: gs3y1, gs3y2, gs3y, gs4y, gs2y1, gs2y2, gs5y, gs6y, grad_sum1y, grad_sum2y
            double precision :: gs2z1, gs2z2, gs3z1, gs3z2, gs2z, gs5z, gs6z, gs4z, grad_sum1z, grad_sum2z
            integer :: t, u, v, tau, ni, fi

            s1 = 0.d+0
            grad_sum1x = 0.d+0
            grad_sum2x = 0.d+0
            grad_sum1y = 0.d+0
            grad_sum2y = 0.d+0
            grad_sum1z = 0.d+0
            grad_sum2z = 0.d+0
            tloop: do t = 0, lmna(1, li) + lmnb(1, lj)
                  s2 = 0.d+0
                  gs2x = 0.d+0
                  gs2y1 = 0.d+0
                  gs2y2 = 0.d+0
                  gs2z1 = 0.d+0
                  gs2z2 = 0.d+0
                  uloop: do u = 0, lmna(2, li) + lmnb(2, lj)
                        s3 = 0.d+0
                        gs3x = 0.d+0
                        gs3y = 0.d+0
                        gs3z1 = 0.d+0
                        gs3z2 = 0.d+0
                        vloop: do v = 0, lmna(3, li) + lmnb(3, lj)
                              s4 = 0.d+0
                              gs4x = 0.d+0
                              gs4y = 0.d+0
                              gs4z = 0.d+0
                              tauloop: do tau = 0, lmnc(1, lk) + lmnd(1, ll)
                                    s5 = 0.d+0
                                    gs5x = 0.d+0
                                    gs5y = 0.d+0
                                    gs5z = 0.d+0
                                    niloop: do ni = 0, lmnc(2, lk) + lmnd(2, ll)
                                          s6 = 0.d+0
                                          gs6x = 0.d+0
                                          gs6y = 0.d+0
                                          gs6z = 0.d+0
                                          filoop: do fi = 0, lmnc(3, lk) + lmnd(3, ll)
                                                s6 = s6 + (-1)**(tau + ni + fi) &
                                                      * EZcd(fi, lmnc(3, lk), lmnd(3, ll) )&
                                                      * Rtuv(t + tau, u + ni, v + fi, 0)
                                              
                                              
                                                gs6x = gs6x + (-1)**(tau + ni + fi) &
                                                      * EZcd(fi, lmnc(3, lk), lmnd(3, ll) )&
                                                      * Rtuv(t + 1 + tau, u + ni, v + fi, 0)
                                                gs6y = gs6y + (-1)**(tau + ni + fi) &
                                                      * EZcd(fi, lmnc(3, lk), lmnd(3, ll) )&
                                                      * Rtuv(t  + tau, u + ni + 1, v + fi , 0)
                                                gs6z = gs6z + (-1)**(tau + ni + fi) &
                                                      * EZcd(fi, lmnc(3, lk), lmnd(3, ll) )&
                                                      * Rtuv(t  + tau, u + ni, v + fi + 1, 0)
                                          end do filoop
                                          s5 = s5 + s6 * EYcd(ni, lmnc(2, lk), lmnd(2, ll))
                                       
                                          gs5x = gs5x + gs6x * EYcd(ni, lmnc(2, lk), lmnd(2, ll))
                                          gs5y = gs5y + gs6y * EYcd(ni, lmnc(2, lk), lmnd(2, ll))
                                          gs5z = gs5z + gs6z * EYcd(ni, lmnc(2, lk), lmnd(2, ll))
                                    end do niloop
                                  
                                    s4 = s4 + s5 * EXcd(tau, lmnc(1, lk), lmnd(1, ll))
                                  
                                    gs4x = gs4x + gs5x * EXcd(tau, lmnc(1, lk), lmnd(1, ll))
                                    gs4y = gs4y + gs5y * EXcd(tau, lmnc(1, lk), lmnd(1, ll))
                                    gs4z = gs4z + gs5z * EXcd(tau, lmnc(1, lk), lmnd(1, ll))
                              end do tauloop
                              s3 = s3 + s4 *  EZab(v, lmna(3, li), lmnb(3, lj))
                           
                              gs3x = gs3x + gs4x *  EZab(v, lmna(3, li), lmnb(3, lj))
                              gs3y = gs3y + gs4y *  EZab(v, lmna(3, li), lmnb(3, lj))
                              gs3z1 = gs3z1 + s4 *  gEZab(v, lmna(3, li), lmnb(3, lj))
                              gs3z2 = gs3z2 + gs4z *  EZab(v, lmna(3, li), lmnb(3, lj))

                        end do vloop
                        s2 = s2 + s3 * EYab(u, lmna(2, li), lmnb(2, lj))
                      
                        gs2x = gs2x + gs3x * EYab(u, lmna(2, li), lmnb(2, lj))

                        gs2y1 = gs2y1 + s3 * gEYab(u, lmna(2, li), lmnb(2, lj))
                        gs2y2 = gs2y2 + gs3y * EYab(u, lmna(2, li), lmnb(2, lj))

                        gs2z1 = gs2z1 + gs3z1 * EYab(u, lmna(2, li), lmnb(2, lj))
                        gs2z2 = gs2z2 + gs3z2 * EYab(u, lmna(2, li), lmnb(2, lj))
                  end do uloop
                  s1 = s1 +s2 * EXab(t, lmna(1, li), lmnb(1, lj) )
                
                  grad_sum1x = grad_sum1x + s2 * gEXab(t, lmna(1, li), lmnb(1, lj) )
                  grad_sum2x = grad_sum2x + gs2x * EXab(t, lmna(1, li), lmnb(1, lj) )

                  grad_sum1y = grad_sum1y + gs2y1 * EXab(t, lmna(1, li), lmnb(1, lj) )
                  grad_sum2y = grad_sum2y + gs2y2 * EXab(t, lmna(1, li), lmnb(1, lj) )

                  grad_sum1z = grad_sum1z + gs2z1 * EXab(t, lmna(1, li), lmnb(1, lj) )
                  grad_sum2z = grad_sum2z + gs2z2 * EXab(t, lmna(1, li), lmnb(1, lj) )
            end do tloop

         
         
            g_abcd(lg) = C_abcd * s1
          !  g_g_abcd(1,lg) = C_abcd * ( grad_sum1x + (ae / p ) * grad_sum2x)
          !  g_g_abcd(2,lg) = C_abcd * ( grad_sum1y + (ae / p ) * grad_sum2y)
          !  g_g_abcd(3,lg) = C_abcd * ( grad_sum1z + (ae / p ) * grad_sum2z)
          !  g_g_abcd(4,lg) = C_abcd * (-grad_sum1x + (be / p ) * grad_sum2x)
          !  g_g_abcd(5,lg) = C_abcd * (-grad_sum1y + (be / p ) * grad_sum2y)
          !  g_g_abcd(6,lg) = C_abcd * (-grad_sum1z + (be / p ) * grad_sum2z)

      end subroutine integral_lmnk

      subroutine hermite2(deg_ab, deg_cd, XYZab, XYZcd, XYZpa, XYZpb, XYZqc,&
            XYZqd, Kab, Kcd, p, q,ae, ce, mua, mub, EXab, EYab, EZab,&
            EXcd, EYcd, EZcd, gEXab, gEYab, gEZab, gEXcd, gEYcd, gEZcd)
            ! hermite takes angular momentum of <GaGb|GcGd> and gives tables of 
            ! hermite_gussian coefficents to maximum allowed degree.
            !
            integer, intent(in) :: deg_ab, deg_cd
            double precision, dimension(1:3), intent(in) :: XYZpa
            double precision, dimension(1:3), intent(in) :: XYZpb
            double precision, dimension(1:3), intent(in) :: XYZqc
            double precision, dimension(1:3), intent(in) :: XYZqd
            double precision, dimension(1:3), intent(in) :: XYZab
            double precision, dimension(1:3), intent(in) :: XYZcd
            double precision, dimension(1:3), intent(in) :: Kab
            double precision, dimension(1:3), intent(in) :: Kcd
            double precision, intent(in) :: p
            double precision, intent(in) :: q
            double precision, intent(in) :: ae
            double precision, intent(in) :: ce
            double precision, intent(in) :: mua
            double precision, intent(in) :: mub
            double precision, dimension(0:, 0:, 0: ), intent(out) :: EXab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: EYab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: EZab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: EXcd
            double precision, dimension(0:, 0:, 0: ), intent(out) :: EYcd
            double precision, dimension(0:, 0:, 0: ), intent(out) :: EZcd

            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEXab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEYab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEZab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEXcd
            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEYcd
            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEZcd
            integer :: i, j, k

            call grad_coefficent_generator(deg_ab, Kab(1), p, ae, mua, XYZab(1), XYZpa(1), XYZpb(1), EXab, gEXab)
            call grad_coefficent_generator(deg_ab, Kab(2), p, ae, mua, XYZab(2), XYZpa(2), XYZpb(2), EYab, gEYab)
            call grad_coefficent_generator(deg_ab, Kab(3), p, ae, mua, XYZab(3), XYZpa(3), XYZpb(3), EZab, gEZab)
            call grad_coefficent_generator(deg_cd, Kcd(1), q, ce, mub, XYZcd(1), XYZqc(1), XYZqd(1), EXcd, gEXcd)
            call grad_coefficent_generator(deg_cd, Kcd(2), q, ce, mub, XYZcd(2), XYZqc(2), XYZqd(2), EYcd, gEYcd)
            call grad_coefficent_generator(deg_cd, Kcd(3), q, ce, mub, XYZcd(3), XYZqc(3), XYZqd(3), EZcd, gEZcd)
        

      end subroutine hermite2

end module olenkiferlo2el
