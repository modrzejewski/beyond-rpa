module olenkiferlo1el
      use gparam
      use oboys
      use math_constants
      use lmn
      use grad_hg
      use hermite_coulomb

      implicit none

contains

      subroutine angular_loop_1el(shell, primitive, centers, V_efg, kinetic_e, overlap)
            ! shell - rozdzaje powloki
            integer, dimension(1:2), intent(in) :: shell

            ! primitive - rodzaje prymitywów
            integer, dimension(1:2), intent(in) :: primitive

            ! centers - indeksy centrów
            integer, dimension(1:2), intent(in) :: centers
            
            double precision :: expa
            double precision :: expb
            double precision, dimension(1:3) :: XYZab
            double precision, dimension(1:3) :: XYZpa
            double precision, dimension(1:3) :: XYZpb
            double precision, dimension(1:3) :: XYZpc
            double precision :: Ax, Ay, Az
            double precision :: Bx, By, Bz
            double precision :: Cx, Cy, Cz
            double precision :: Px, Py, Pz
            double precision, dimension(1:3) :: Kab 
            double precision :: C_ab
           


            !degrees of tables Rtuv and E
            integer :: deg_ab, deg_abc
            double precision :: p
            double precision :: mu, alpha

            !tables Rtuv, E
            double precision, allocatable :: Rtuv(:, :, :, :)
            double precision, allocatable :: EXab (:, :, :)
            double precision, allocatable :: EYab (:, :, :)
            double precision, allocatable :: EZab (:, :, :)

            double precision, allocatable :: gEXab (:, :, :)
            double precision, allocatable :: gEYab (:, :, :)
            double precision, allocatable :: gEZab (:, :, :)

            !tables of momentum combinations
            integer, allocatable :: lmna(:,:)
            integer, allocatable :: lmnb(:,:)

            !number of momentum combinations for shell
            integer :: sa, sb, m1, m2

            !integral and grad_integral tables
            double precision, dimension(:), intent(out) :: V_efg
            double precision, dimension(1:9, max_nfunc**2) :: gVefg
            double precision, dimension(:), intent(inout) :: kinetic_e
            double precision, dimension(1:6,max_nfunc**2) :: gkinetic_e
            double precision, dimension(:), intent(out) :: overlap

            !supplementary variables
            integer :: i, k
            integer :: lg, li, lj

            expa = expn(primitive(1), shell(1))
            expb = expn(primitive(2), shell(2))

            !********************************
            ! Evaluation of parameters for given expa, expb, expc, expd
            p = expa + expb
            mu = expa * expb / p
            C_ab = (2.d+0 * pi / p) 
            !********************************

            Ax = atomr(1,centers(1))
            Ay = atomr(2,centers(1))
            Az = atomr(3,centers(1))

            Bx = atomr(1,centers(2))
            By = atomr(2,centers(2))
            Bz = atomr(3,centers(2))

            Px = (expa * Ax + expb * Bx) / p
            Py = (expa * Ay + expb * By) / p
            Pz = (expa * Az + expb * Bz) / p

            !********************************
            ! Evaluation of parameters for given centers
            XYZpa = (/ Px - Ax, Py - Ay, Pz - Az/)
            XYZpb = (/ Px - Bx, Py - By, Pz - Bz/)
            XYZab = (/ Ax - Bx, Ay - By, Az - Bz/)
           
            Kab(1) = exp(-mu * XYZab(1)**2)
            Kab(2) = exp(-mu * XYZab(2)**2)
            Kab(3) = exp(-mu * XYZab(3)**2)

            !********************************
            ! Evaluation of number of angular functions
            m1 = shtype(shell(1))
            m2 = shtype(shell(2))
            sa = nfunco(m1)
            sb = nfunco(m2)
            !********************************

            allocate(lmna(1:3, 1:sa))
            allocate(lmnb(1:3, 1:sb))

            call ang_mom(m1, sa, lmna)
            call ang_mom(m2, sb, lmnb)
         
            deg_ab = m1 + m2 + 2

            deg_abc = m1 + m2  + 1

            allocate(Rtuv(-2: deg_abc, -2: deg_abc, -2:deg_abc, 0: deg_abc))
            allocate(EXab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(EYab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(EZab (0:deg_ab, 0:deg_ab, 0:deg_ab))
         

            allocate(gEXab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(gEYab (0:deg_ab, 0:deg_ab, 0:deg_ab))
            allocate(gEZab (0:deg_ab, 0:deg_ab, 0:deg_ab))


            call hermite(deg_ab, XYZab, XYZpa, XYZpb, Kab, p, expa, mu, EXab, EYab, EZab,&
                  gEXab, gEYab, gEZab)

            V_efg = 0.d+0
            kinetic_e = 0.d+0
            overlap = 0.d+0
            
            do k = atom0, atom1
                  Cx = atomr(1,k)
                  Cy = atomr(2,k)
                  Cz = atomr(3,k)

                  XYZpc = (/ Px - Cx, Py - Cy, Pz -Cz/)
                  Rtuv = 0.d+0
                  call R_tuv(deg_abc, XYZpc(1), XYZpc(2), XYZpc(3), Rtuv,  p)
                  lg = 1
                  do li = 1, sa
                        do lj = 1, sb
                              call sub_cint(k,lg, li, lj, lmna(:,li), lmnb(:,lj), Rtuv,&
                                    EXab, EYab, EZab, gEXab, gEYab, gEZab,  V_efg, gVefg, expa, expb, p)
                              lg = lg + 1
                        end do
                  end do
            end do
            

            lg = 1
            do li = 1, sa
                  do lj = 1, sb
                        call total_kinetic(lg, expb, lmna(:,li), lmnb(:,lj), EXab,&
                              EYab, EZab,gEXab, gEYab, gEZab, kinetic_e, gkinetic_e, overlap, p)
                        lg = lg + 1
                  end do
            end do
            
            
            deallocate(lmna)
            deallocate(lmnb)
            deallocate(Rtuv)
            deallocate(EXab)
            deallocate(EYab)
            deallocate(gEXab) 
            deallocate(gEYab)
            deallocate(gEZab)
            

      end subroutine angular_loop_1el

      subroutine sub_cint(k,lg, li, lj, lmna, lmnb, Rtuv,&
            EXab, EYab, EZab, gEXab, gEYab, gEZab,  V_efg, gVefg, ae, be, p)
            integer, intent(in) :: k
            integer, intent(in) :: lg, li, lj
            integer, dimension(:), intent(in) :: lmna
            integer, dimension(:), intent(in) :: lmnb
            double precision, dimension(-2:,-2:,-2:,0:), intent(in) :: Rtuv
            double precision, dimension(0:,0:,0:), intent(in) :: EXab
            double precision, dimension(0:,0:,0:), intent(in) :: EYab
            double precision, dimension(0:,0:,0:), intent(in) :: EZab
            double precision, dimension(0:,0:,0:), intent(in) :: gEXab
            double precision, dimension(0:,0:,0:), intent(in) :: gEYab
            double precision, dimension(0:,0:,0:), intent(in) :: gEZab
            double precision, dimension(:), intent(inout) :: V_efg
            double precision, dimension(:, :), intent(out) :: gVefg
            double precision, intent(in) :: ae
            double precision, intent(in) :: be
            double precision, intent(in) :: p
            double precision :: C_efg
            integer ::  t, u, v
            double precision :: s1, s2, s3, o1, o2, o3
            double precision :: gs3x, gs3y, gs3z1, gs3z2, gs2x, gs2y1, gs2y2, gs2z1, gs2z2, gs1x1, gs1x2, gs1y1, gs1y2, gs1z1, gs1z2
            double precision :: work

            C_efg = (2.d+0 * pi / p) 

            s1 = 0.d+0
            gs1x1 = 0.d+0
            gs1x2 = 0.d+0
            gs1y1 = 0.d+0
            gs1y2 = 0.d+0
            gs1z1 = 0.d+0 
            gs1z2 =  0.d+0
            do t = 0, lmna(1) + lmnb(1)
                  s2 = 0.d+0
                  gs2x = 0.d+0
                  gs2y1 = 0.d+0
                  gs2y2 = 0.d+0
                  gs2z1 = 0.d+0 
                  gs2z2 =  0.d+0
                  do u = 0, lmna(2) + lmnb(2)
                        s3 = 0.d+0
                        gs3x = 0.d+0
                        gs3y = 0.d+0
                        gs3z1 = 0.d+0 
                        gs3z2 =  0.d+0
                        do v = 0, lmna(3) + lmnb(3)
                              s3 = s3 + Rtuv(t , u  , v , 0) * EZab(v, lmna(3), lmnb(3))
                              gs3x = gs3x + EZab(v, lmna(3), lmnb(3)) * Rtuv(t + 1 , u , v , 0)
                              gs3y = gs3y + EZab(v, lmna(3), lmnb(3)) * Rtuv(t , u + 1 , v , 0)
                              gs3z1 = gs3z1 + gEZab(v, lmna(3), lmnb(3))* Rtuv(t , u , v , 0) 
                              gs3z2 = gs3z2 + EZab(v, lmna(3), lmnb(3)) *&
                                    Rtuv(t , u , v + 1 , 0)  

                        end do
                        s2 = s2 + s3 * EYab(u, lmna(2), lmnb(2))
                        gs2x = gs2x + gs3x *  EYab(u, lmna(2), lmnb(2))
                        gs2y1 = gs2y1 + s3 * gEYab(u, lmna(2), lmnb(2))
                        gs2y2 = gs2y2 + gs3y * EYab(u, lmna(2), lmnb(2))
                        gs2z1 = gs2z1 + gs3z1 * EYab(u, lmna(2), lmnb(2))
                        gs2z2 = gs2z2 + gs3z2 * EYab(u, lmna(2), lmnb(2))


                  end do
                  s1 = s1 + s2 *  EXab(t, lmna(1), lmnb(1))
                  gs1x1 =  gs1x1 + s2 *   gEXab(t, lmna(1), lmnb(1))
                  gs1x2 =  gs1x2 + gs2x *  EXab(t, lmna(1), lmnb(1))
                  gs1y1 =  gs1y1 + gs2y1 * EXab(t, lmna(1), lmnb(1))
                  gs1y2 =  gs1y2 + gs2y2 * EXab(t, lmna(1), lmnb(1))
                  gs1z1 =  gs1z1 + gs2z1 * EXab(t, lmna(1), lmnb(1))
                  gs1z2 =  gs1z2 + gs2z2 * EXab(t, lmna(1), lmnb(1))

            end do

            work = V_efg(lg)
            V_efg(lg) = -dnuclz(k) * C_efg * s1 !pomnozenie przez ładunek jądra i elektronu
            V_efg(lg)  = work + V_efg(lg)
            gVefg(1,lg) = C_efg * (gs1x1 + (ae / p ) * gs1x2)
            gVefg(2,lg) = C_efg * (gs1y1 + (ae / p ) * gs1y2)
            gVefg(3,lg) = C_efg * (gs1z1 + (ae / p ) * gs1z2)
            gVefg(4,lg) = C_efg * (-gs1x1 + (be / p ) * gs1x2)
            gVefg(5,lg) = C_efg * (-gs1y1 + (be / p ) * gs1y2)
            gVefg(6,lg) = C_efg * (-gs1z1 + (be / p ) * gs1z2)
            gVefg(7,lg) = C_efg * (-gs1x2)
            gVefg(8,lg) = C_efg * (-gs1y2)
            gVefg(9,lg) = C_efg * (-gs1z2)

      end subroutine sub_cint

      subroutine total_kinetic(lg, be, lmna, lmnb, EXab,&
            EYab, EZab, gEXab, gEYab, gEZab, kinetic_e, gkinetic_e, overlap, p)
            integer, intent(in) :: lg
            integer, dimension(:), intent(in) :: lmna, lmnb
            double precision, dimension(0:,0:,0:), intent(in) :: EXab, EYab, EZab
            double precision, dimension(0:,0:,0:), intent(in) :: gEXab, gEYab, gEZab
            double precision, intent(in) :: be
            double precision, dimension(:), intent(out) :: kinetic_e
            double precision, dimension(:,:), intent(out) :: gkinetic_e
            double precision, dimension(:), intent(out) :: overlap
            double precision, intent(in) :: p
            double precision :: Ix, Iy, Iz
            double precision, dimension(1:3) :: dlmnb
            double precision :: o1, o2x, o2y, o2z, o3x, o3y, o3z
            double precision :: et, etx, etx2, eu, euy, euy2, ev, evz, evz2
            double precision :: get, getx, getx2, geu, geuy, geuy2, gev, gevz, gevz2
            double precision :: go1x, go1y, go1z, go2xx, go2xy, go2xz, go2yx, go2yy, go2yz, go2zx, go2zy, go2zz
            double precision :: go3xx,  go3xy, go3xz, go3yx, go3yy, go3yz, go3zx, go3zy, go3zz
            double precision ::  gIxAx,  gIxAy,  gIxAz, gIyAx, gIyAy, gIyAz, gIzAx, gIzAy, gIzAz
            double precision ::  gIxBx,  gIxBy,  gIxBz, gIyBx, gIyBy, gIyBz, gIzBx, gIzBy, gIzBz

            !double precision :: overlap
            integer :: lj, ig, id
            double precision :: const1

            dlmnb = dble(lmnb)
            et =   EXab(0, lmna(1), lmnb(1))

            if((lmnb(1)-2).lt.0)then
            etx = 0.d+0
            else
                  etx =  EXab(0, lmna(1), lmnb(1) - 2)
            end if

            etx2 = EXab(0, lmna(1), lmnb(1)+ 2 )

            eu = EYab(0, lmna(2), lmnb(2))

            if((lmnb(2)-2).lt.0)then
                  euy = 0.d+0
            else
                  euy = EYab(0, lmna(2), lmnb(2) - 2)
            end if
           
            euy2 =EYab(0, lmna(2), lmnb(2)+ 2 )

            ev = EZab(0, lmna(3), lmnb(3))

            if((lmnb(3)-2).lt.0)then
                  evz = 0.d+0
            else
                  evz = EZab(0, lmna(3), lmnb(3) - 2)                
            end if

            evz2 = EZab(0, lmna(3), lmnb(3) + 2)

            get = gEXab(0, lmna(1), lmnb(1))

            if((lmnb(1)-2).lt.0)then
                  getx = 0.d+0
            else
                  getx = gEXab(0, lmna(1), lmnb(1) - 2)
            end if
           
            getx2 = gEXab(0, lmna(1), lmnb(1)+ 2 )

            geu = gEYab(0, lmna(2), lmnb(2))

            if((lmnb(2)-2).lt.0)then
                  geuy = 0.d+0
            else
                  geuy = gEYab(0, lmna(2), lmnb(2) - 2)
            end if

            geuy2 = gEYab(0, lmna(2), lmnb(2)+ 2 )

            gev = gEZab(0, lmna(3), lmnb(3))
            
            if((lmnb(3)-2).lt.0)then
                  gevz = 0.d+0
            else
                  gevz = gEZab(0, lmna(3), lmnb(3) - 2)               
            end if

            gevz2 = gEZab(0, lmna(3), lmnb(3) + 2)

            !const1 = exp(-mua * (XYZab(1)**2 + XYZab(2) + XYZab(3)**2))
            const1 = (pi / p)**(1.5d+0)
            o1 = const1 * et * eu * ev
            go1x = const1 * get * eu * ev
            go1y = const1 * et * geu * ev
            go1z = const1 * et * eu * gev
          
            if((lmnb(1) - 2).lt.0)then
                  o2x = 0.d+0
                  go2xx = 0.d+0
                  go2xy = 0.d+0
                  go2xz = 0.d+0
            else
                  o2x = const1 * etx * eu * ev
                  go2xx =  const1 * getx * eu * ev
                  go2xy =  const1 * etx * geu * ev
                  go2xz =  const1 * etx * eu * gev
            end if
            if((lmnb(2) - 2).lt.0)then
                  o2y = 0.d+0
                  go2yx = 0.d+0
                  go2yy = 0.d+0
                  go2yz = 0.d+0
            else
                  o2y = const1 * et * euy * ev
                  go2yx =  const1 * get * euy * ev
                  go2yy =  const1 * et * geuy * ev
                  go2yz =  const1 * et * euy * gev
            end if
            if((lmnb(3) - 2).lt.0)then
                  o2z = 0.d+0
                  go2zx = 0.d+0
                  go2zy = 0.d+0
                  go2zz = 0.d+0
            else
                  o2z = const1 * et * eu * evz
                  go2yx =  const1 * get * eu * evz
                  go2yy =  const1 * et * geu * evz
                  go2yz =  const1 * et * eu * gevz
            end if



            o3x =  const1 * etx2 * eu * ev
            go3xx =  const1 * getx2 * eu * ev
            go3xy =  const1 * etx2 * geu * ev
            go3xz =  const1 * etx2 * eu * gev
            o3y =  const1 * et * euy2 * ev
            go3yx = const1 * get * euy2 * ev
            go3yy = const1 * et * geuy2 * ev
            go3yz = const1 * et * euy2 * gev
            o3z =  const1 * et * eu * evz2
            go3zx = const1 * get * eu * evz2
            go3zy = const1 * et * geu * evz2
            go3zz = const1 * et * eu * gevz2

            Ix = be * (2.d+0 * dlmnb(1) + 1.d+0) * o1 -&
                  2.d+0 * be **2 * o3x - dlmnb(1)* &
                  (dlmnb(1) - 1.d+0) * o2x / 2.d+0 
            gIxAx = be * (2.d+0 * dlmnb(1) + 1.d+0) * go1x -&
                  2.d+0 * be **2 * go3xx - dlmnb(1)* &
                  (dlmnb(1) - 1.d+0) * go2xx / 2.d+0
            gIxAy = be * (2.d+0 * dlmnb(1) + 1.d+0) * go1y -&
                  2.d+0 * be **2 * go3xy - dlmnb(1)* &
                  (dlmnb(1) - 1.d+0) * go2xy / 2.d+0 
            gIxAz = be * (2.d+0 * dlmnb(1) + 1.d+0) * go1z -&
                  2.d+0 * be **2 * go3xz - dlmnb(1)* &
                  (dlmnb(1) - 1.d+0) * go2xz / 2.d+0 
            gIxBx = -gIxAx
            gIxBy = -gIxAy
            gIxBz = -gIxAz


            Iy = be * (2.d+0 * dlmnb(2) + 1.d+0) * o1 -&
                  2.d+0 * be **2 * o3y - dlmnb(2)* &
                  (dlmnb(2) - 1.d+0) * o2y / 2.d+0 
            gIyAx = be * (2.d+0 * dlmnb(2) + 1.d+0) * go1x -&
                  2.d+0 * be **2 * go3yx - dlmnb(2)* &
                  (dlmnb(2) - 1.d+0) * go2yx / 2.d+0 
            gIyAy = be * (2.d+0 * dlmnb(2) + 1.d+0) * go1y -&
                  2.d+0 * be **2 * go3yy - dlmnb(2)* &
                  (dlmnb(2) - 1.d+0) * go2yy / 2.d+0 
            gIyAz = be * (2.d+0 * dlmnb(2) + 1.d+0) * go1z -&
                  2.d+0 * be **2 * go3yz - dlmnb(2)* &
                  (dlmnb(2) - 1.d+0) * go2yz / 2.d+0 
            gIyBx = -gIyAx
            gIyBy = -gIyAy
            gIyBz = gIyAz

            Iz = be * (2.d+0 * dlmnb(3) + 1.d+0) * o1 -&
                  2.d+0 * be **2 * o3z - dlmnb(3)* &
                  (dlmnb(3) - 1.d+0) * o2z / 2.d+0 
            gIzAx = be * (2.d+0 * dlmnb(3) + 1.d+0) * go1x -&
                  2.d+0 * be **2 * go3zx - dlmnb(3)* &
                  (dlmnb(3) - 1.d+0) * go2zx / 2.d+0 
            gIzAy =be * (2.d+0 * dlmnb(3) + 1.d+0) * go1y -&
                  2.d+0 * be **2 * go3zy - dlmnb(3)* &
                  (dlmnb(3) - 1.d+0) * go2zy / 2.d+0 
            gIzAz = be * (2.d+0 * dlmnb(3) + 1.d+0) * go1z -&
                  2.d+0 * be **2 * go3zz - dlmnb(3)* &
                  (dlmnb(3) - 1.d+0) * go2zz / 2.d+0 
            gIzBx = -gIzAx
            gIzBy = -gIzAy
            gIzBz = -gIzAz

            kinetic_e(lg) = Ix + Iy + Iz
            gkinetic_e(1,lg) = gIxAx + gIyAx + gIzAx
            gkinetic_e(2,lg) = gIxAy + gIyAy + gIzAy
            gkinetic_e(3,lg) = gIxAz + gIyAz + gIzAz
            gkinetic_e(4,lg) = gIxBx + gIyBx + gIzBx
            gkinetic_e(5,lg) = gIxBy + gIyBy + gIzBy
            gkinetic_e(6,lg) = gIxBz + gIyBz + gIzBz
            overlap(lg) = o1
      end subroutine total_kinetic

      subroutine hermite(deg_ab, XYZab, XYZpa, XYZpb, Kab, p, ae, mua, EXab, EYab, EZab,&
            gEXab, gEYab, gEZab)

            integer, intent(in) :: deg_ab
            double precision, dimension(:), intent(in) :: XYZpa
            double precision, dimension(:), intent(in) :: XYZpb
            double precision, dimension(:), intent(in) :: XYZab
            double precision, dimension(:), intent(in) :: Kab
            double precision, intent(in) :: p
            double precision, intent(in) :: ae
            double precision, intent(in) :: mua

            double precision, dimension(0:, 0:, 0: ), intent(out) :: EXab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: EYab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: EZab

            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEXab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEYab
            double precision, dimension(0:, 0:, 0: ), intent(out) :: gEZab

            call grad_coefficent_generator(deg_ab, Kab(1), p, ae, mua, XYZab(1), XYZpa(1), XYZpb(1), EXab, gEXab)
            call grad_coefficent_generator(deg_ab, Kab(2), p, ae, mua, XYZab(2), XYZpa(2), XYZpb(2), EYab, gEYab)
            call grad_coefficent_generator(deg_ab, Kab(3), p, ae, mua, XYZab(3), XYZpa(3), XYZpb(3), EZab, gEZab)
                                          

      end subroutine hermite



end module  olenkiferlo1el
