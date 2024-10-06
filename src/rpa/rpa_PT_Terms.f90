module rpa_PT_Terms
      use arithmetic      
      use real_linalg
      use rpa_definitions
      use clock

      implicit none

contains 

      subroutine rpa_PT_Order2(Energy, Zgk, Yga, Xgi, OccEnergies, VirtEnergies, NOcc, NVirt, NGridTHC)
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NGridTHC
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            real(F64), dimension(NGridTHC, NVirt), intent(in)      :: Yga
            real(F64), dimension(NGridTHC, NOcc), intent(in)       :: Xgi
            real(F64), dimension(NOcc), intent(in)                 :: OccEnergies
            real(F64), dimension(NVirt), intent(in)                :: VirtEnergies

            integer :: NCholesky
            integer :: a, i, b, j
            real(F64) :: Ea, Ei, Eb, Ej, Vaibj, Vajbi
            real(F64) :: EcSinglet, EcTriplet, EcDirectMP2, EcTotMP2
            real(F64), dimension(:, :, :), allocatable :: Rkai
            real(F64), dimension(:), allocatable :: YXg

            NCholesky = size(Zgk, dim=2)
            allocate(Rkai(NCholesky, NVirt, NOcc))
            !$omp parallel private(a, i, YXg)
            allocate(YXg(NGridTHC))
            !$omp do collapse(2)
            do i = 1, NOcc
                  do a = 1, NVirt
                        YXg(:) = Yga(:, a) * Xgi(:, i)
                        !
                        ! R(k,a,i) = Sum(g) Z(g,k)*YX(g;a,i)
                        !                        
                        call real_aTv_x(Rkai(:, a, i), Zgk, NGridTHC, YXg, NGridTHC, NCholesky, ONE, ZERO)                        
                  end do
            end do
            !$omp end do
            deallocate(YXg)
            !$omp end parallel

            EcSinglet = ZERO
            EcTriplet = ZERO
            EcDirectMP2 = ZERO
            EcTotMP2 = ZERO
            !$omp parallel do private(a, i, b, j, Vaibj, Vajbi, Ea, Ei, Eb, Ej) &
            !$omp reduction(+:EcSinglet,EcTriplet,EcDirectMP2, EcTotMP2) &
            !$omp collapse(4)
            do j = 1, NOcc
                  do i = 1, NOcc
                        do b = 1, NVirt
                              do a = 1, NVirt
                                    call real_vw_x(Vaibj, Rkai(:, a, i), Rkai(:, b, j), NCholesky)
                                    call real_vw_x(Vajbi, Rkai(:, a, j), Rkai(:, b, i), NCholesky)
                                    Ea = VirtEnergies(a)
                                    Eb = VirtEnergies(b)
                                    Ei = OccEnergies(i)
                                    Ej = OccEnergies(j)
                                    EcSinglet = EcSinglet + Vaibj**2 / (Ei + Ej - Ea - Eb)
                                    EcTriplet = EcTriplet + Vaibj * (Vaibj - Vajbi) / (Ei + Ej - Ea - Eb)
                                    EcDirectMP2 = EcDirectMP2 + TWO * Vaibj**2 / (Ei + Ej - Ea - Eb)
                                    EcTotMP2 = EcTotMP2 + Vaibj * (TWO * Vaibj - Vajbi) / (Ei + Ej - Ea - Eb)
                              end do
                        end do
                  end do
            end do
            !$omp end parallel do
            Energy(MP2_ENERGY_SINGLET_PAIR) = EcSinglet
            Energy(MP2_ENERGY_TRIPLET_PAIR) = EcTriplet
            Energy(MP2_ENERGY_DIRECT) = EcDirectMP2
            Energy(MP2_ENERGY_TOTAL) = EcTotMP2
      end subroutine rpa_PT_Order2


      subroutine rpa_PT_Order3(Energy, Zgk, Yga, Xgi, OccEnergies, VirtEnergies, NOcc, NVirt, NGridTHC)
            integer, intent(in)                                    :: NOcc
            integer, intent(in)                                    :: NVirt
            integer, intent(in)                                    :: NGridTHC
            real(F64), dimension(:), intent(inout)                 :: Energy
            real(F64), dimension(:, :), intent(in)                 :: Zgk
            real(F64), dimension(NGridTHC, NVirt), intent(in)      :: Yga
            real(F64), dimension(NGridTHC, NOcc), intent(in)       :: Xgi
            real(F64), dimension(NOcc), intent(in)                 :: OccEnergies
            real(F64), dimension(NVirt), intent(in)                :: VirtEnergies

            integer :: NCholesky
            real(F64), dimension(:, :, :), allocatable :: Rkai
            real(F64), dimension(:, :, :), allocatable :: Rkab
            real(F64), dimension(:, :, :), allocatable :: Rkij
            real(F64), dimension(:), allocatable :: YXg
            real(F64) :: Ec2a, Ec2b, Ec2c, Ec2d
            real(F64) :: Ec2g, Ec2h, Ec2i, Ec2j
            real(F64) :: Ec2e, Ec2f
            real(F64) :: Ec2k, Ec2l
            integer :: a, b, i, j

            NCholesky = size(Zgk, dim=2)
            allocate(Rkai(NCholesky, NVirt, NOcc))
            allocate(Rkab(NCholesky, NVirt, NVirt))
            allocate(Rkij(NCholesky, NOcc, NOcc))
            !$omp parallel private(a, b, i, j, YXg)
            allocate(YXg(NGridTHC))
            !$omp do collapse(2)
            do i = 1, NOcc
                  do a = 1, NVirt
                        YXg(:) = Yga(:, a) * Xgi(:, i)
                        !
                        ! R(k,a,i) = Sum(g) Z(g,k)*YX(g;a,i)
                        !                        
                        call real_aTv_x(Rkai(:, a, i), Zgk, NGridTHC, YXg, NGridTHC, NCholesky, ONE, ZERO)
                  end do
            end do
            !$omp end do
            !$omp do collapse(2)
            do b = 1, NVirt
                  do a = 1, NVirt
                        YXg(:) = Yga(:, a) * Yga(:, b)
                        call real_aTv_x(Rkab(:, a, b), Zgk, NGridTHC, YXg, NGridTHC, NCholesky, ONE, ZERO)
                  end do
            end do
            !$omp end do
            !$omp do collapse(2)
            do j = 1, NOcc
                  do i = 1, NOcc
                        YXg(:) = Xgi(:, i) * Xgi(:, j)
                        call real_aTv_x(Rkij(:, i, j), Zgk, NGridTHC, YXg, NGridTHC, NCholesky, ONE, ZERO)
                  end do
            end do
            deallocate(YXg)
            !$omp end parallel

            call rpa_PT_Order3_abcd(Ec2a, Ec2b, Ec2c, Ec2d, Rkai, OccEnergies, &
                  VirtEnergies, NOcc, NVirt, NCholesky)
            call rpa_PT_Order3_ghij(Ec2g, Ec2h, Ec2i, Ec2j, Rkai, Rkij, Rkab, &
                  OccEnergies, VirtEnergies, NOcc, NVirt, NCholesky)
            call rpa_PT_Order3_ef(Ec2e, Ec2f, Rkai, Rkij, OccEnergies, &
                  VirtEnergies, NOcc, NVirt, NCholesky)
            call rpa_PT_Order3_kl(Ec2k, Ec2l, Rkai, Rkab, OccEnergies, &
                  VirtEnergies, NOcc, NVirt, NCholesky)
            
            Energy(MP3_ENERGY_A) = Ec2a
            Energy(MP3_ENERGY_B) = Ec2b
            Energy(MP3_ENERGY_C) = Ec2c
            Energy(MP3_ENERGY_D) = Ec2d
            Energy(MP3_ENERGY_E) = Ec2e
            Energy(MP3_ENERGY_F) = Ec2f
            Energy(MP3_ENERGY_G) = Ec2g
            Energy(MP3_ENERGY_H) = Ec2h
            Energy(MP3_ENERGY_I) = Ec2i
            Energy(MP3_ENERGY_J) = Ec2j
            Energy(MP3_ENERGY_K) = Ec2k
            Energy(MP3_ENERGY_L) = Ec2l
            Energy(MP3_ENERGY_TOTAL) = &
                  Ec2a + Ec2b + Ec2c + Ec2d &
                  + Ec2g + Ec2h + Ec2i + Ec2j &
                  + Ec2e + Ec2f + Ec2k + Ec2l
      end subroutine rpa_PT_Order3


      subroutine rpa_PT_Order3_abcd(Ec2a, Ec2b, Ec2c, Ec2d, Rkai, &
            OccEnergies, VirtEnergies, NOcc, NVirt, NCholesky)
            
            real(F64), intent(out)                    :: Ec2a, Ec2b, Ec2c, Ec2d
            real(F64), dimension(:, :, :), intent(in) :: Rkai
            real(F64), dimension(:), intent(in)       :: OccEnergies
            real(F64), dimension(:), intent(in)       :: VirtEnergies
            integer, intent(in)                       :: NOcc
            integer, intent(in)                       :: NVirt
            integer, intent(in)                       :: NCholesky

            integer :: a, i, b, j, c, k
            real(F64) :: Ea, Ei, Eb, Ej, Ec, Ek
            real(F64) :: Taick, Tckbj, Tcjbk, Takci
            real(F64) :: Vaibj, Vaick, Vckbj, Vcjbk, Vakci

            Ec2a = ZERO
            Ec2b = ZERO
            Ec2d = ZERO
            !$omp parallel do private(a, i, b, j, c, k) &
            !$omp private(Vaibj, Vaick, Vckbj, Vcjbk, Vakci) &
            !$omp private(Ea, Ei, Eb, Ej, Ec, Ek) &
            !$omp private(Taick, Tckbj, Tcjbk, Takci) &
            !$omp reduction(+:Ec2a, Ec2b, Ec2d) &
            !$omp collapse(4)
            do j = 1, NOcc
                  do i = 1, NOcc
                        do b = 1, NVirt
                              do a = 1, NVirt
                                    call real_vw_x(Vaibj, Rkai(:, a, i), Rkai(:, b, j), NCholesky)
                                    Ea = VirtEnergies(a)
                                    Eb = VirtEnergies(b)
                                    Ei = OccEnergies(i)
                                    Ej = OccEnergies(j)
                                    do k = 1, NOcc
                                          do c = 1, NVirt
                                                call real_vw_x(Vaick, Rkai(:, a, i), Rkai(:, c, k), NCholesky)
                                                call real_vw_x(Vckbj, Rkai(:, c, k), Rkai(:, b, j), NCholesky)
                                                call real_vw_x(Vcjbk, Rkai(:, c, j), Rkai(:, b, k), NCholesky)
                                                call real_vw_x(Vakci, Rkai(:, a, k), Rkai(:, c, i), NCholesky)
                                                Ec = VirtEnergies(c)
                                                Ek = OccEnergies(k)
                                                Taick = Vaick / (Ei + Ek - Ea - Ec)
                                                Tckbj = Vckbj / (Ej + Ek - Eb - Ec)
                                                Tcjbk = Vcjbk / (Ej + Ek - Eb - Ec)
                                                Takci = Vakci / (Ei + Ek - Ea - Ec)
                                                Ec2a = Ec2a + Vaibj * Taick * Tckbj
                                                Ec2b = Ec2b + Vaibj * Taick * Tcjbk
                                                Ec2d = Ec2d + Vaibj * Takci * Tcjbk
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
            !$omp end parallel do            
            Ec2a = EIGHT * Ec2a ! +8 this is the exact MBPT prefactor of the corresponding 3rd order diagram 2a
            Ec2b = -FOUR * Ec2b ! -4 this is the exact MBPT prefactor of the corresponding 3rd order diagram 2b
            Ec2c = Ec2b         ! -4 this is the exact MBPT prefactor of the corresponding 3rd order diagram 2c
            Ec2d = TWO * Ec2d
      end subroutine rpa_PT_Order3_abcd


      subroutine rpa_PT_Order3_ghij(Ec2g, Ec2h, Ec2i, Ec2j, Rkai, Rkij, Rkab, &
            OccEnergies, VirtEnergies, NOcc, NVirt, NCholesky)
            
            real(F64), intent(out)                    :: Ec2g, Ec2h, Ec2i, Ec2j
            real(F64), dimension(:, :, :), intent(in) :: Rkai
            real(F64), dimension(:, :, :), intent(in) :: Rkij
            real(F64), dimension(:, :, :), intent(in) :: Rkab
            real(F64), dimension(:), intent(in)       :: OccEnergies
            real(F64), dimension(:), intent(in)       :: VirtEnergies
            integer, intent(in)                       :: NOcc
            integer, intent(in)                       :: NVirt
            integer, intent(in)                       :: NCholesky

            integer :: a, i, b, j, c, k
            real(F64) :: Ea, Ei, Eb, Ej, Ec, Ek
            real(F64) :: Tajck, Tckbi, Takcj, Tcibk
            real(F64) :: Vabij, Vajck, Vckbi, Vakcj, Vcibk

            Ec2g = ZERO
            Ec2h = ZERO
            Ec2i = ZERO
            Ec2j = ZERO
            !$omp parallel do private(a, i, b, j, c, k) &
            !$omp private(Vabij, Vajck, Vckbi, Vakcj, Vcibk) &
            !$omp private(Ea, Ei, Eb, Ej, Ec, Ek) &
            !$omp private(Tajck, Tckbi, Takcj, Tcibk) &
            !$omp reduction(+:Ec2g, Ec2h, Ec2i, Ec2j) &
            !$omp collapse(4)
            do j = 1, NOcc
                  do i = 1, NOcc
                        do b = 1, NVirt
                              do a = 1, NVirt
                                    call real_vw_x(Vabij, Rkab(:, a, b), Rkij(:, i, j), NCholesky)
                                    Ea = VirtEnergies(a)
                                    Eb = VirtEnergies(b)
                                    Ei = OccEnergies(i)
                                    Ej = OccEnergies(j)
                                    do k = 1, NOcc
                                          do c = 1, NVirt
                                                call real_vw_x(Vajck, Rkai(:, a, j), Rkai(:, c, k), NCholesky)
                                                call real_vw_x(Vckbi, Rkai(:, c, k), Rkai(:, b, i), NCholesky)
                                                call real_vw_x(Vakcj, Rkai(:, a, k), Rkai(:, c, j), NCholesky)
                                                call real_vw_x(Vcibk, Rkai(:, c, i), Rkai(:, b, k), NCholesky)
                                                Ec = VirtEnergies(c)
                                                Ek = OccEnergies(k)
                                                Tajck = Vajck / (Ej + Ek - Ea - Ec)
                                                Tckbi = Vckbi / (Ei + Ek - Eb - Ec)
                                                Takcj = Vakcj / (Ej + Ek - Ea - Ec)
                                                Tcibk = Vcibk / (Ei + Ek - Eb - Ec)
                                                Ec2g = Ec2g + Vabij * Tajck * Tckbi 
                                                Ec2h = Ec2h + Vabij * Takcj * Tcibk
                                                Ec2i = Ec2i + Vabij * Tajck * Tcibk                                 
                                                Ec2j = Ec2j + Vabij * Takcj * Tckbi
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
            !$omp end parallel do            
            Ec2g = -FOUR * Ec2g
            Ec2h = -FOUR * Ec2h
            Ec2i = TWO * Ec2i
            Ec2j = TWO * Ec2j
      end subroutine rpa_PT_Order3_ghij


      subroutine rpa_PT_Order3_ef(Ec2e, Ec2f, Rkai, Rkij, OccEnergies, &
            VirtEnergies, NOcc, NVirt, NCholesky)
            
            real(F64), intent(out)                    :: Ec2e, Ec2f
            real(F64), dimension(:, :, :), intent(in) :: Rkai
            real(F64), dimension(:, :, :), intent(in) :: Rkij
            real(F64), dimension(:), intent(in)       :: OccEnergies
            real(F64), dimension(:), intent(in)       :: VirtEnergies
            integer, intent(in)                       :: NOcc
            integer, intent(in)                       :: NVirt
            integer, intent(in)                       :: NCholesky

            integer :: a, i, b, j, k, l
            real(F64) :: Ea, Ei, Eb, Ej, Ek, El
            real(F64) :: Taibk, Tblaj, Tbjal
            real(F64) :: Vijkl, Vaibk, Vblaj, Vbjal

            Ec2e = ZERO
            Ec2f = ZERO
            !$omp parallel do private(a, i, b, j, k, l) &
            !$omp private(Ea, Ei, Eb, Ej, Ek, El) &
            !$omp private(Taibk, Tblaj, Tbjal) &
            !$omp private(Vijkl, Vaibk, Vblaj, Vbjal) &
            !$omp reduction(+:Ec2e, Ec2f) &
            !$omp collapse(4)
            do l = 1, NOcc
                  do k = 1, NOcc
                        do j = 1, NOcc
                              do i = 1, NOcc
                                    call real_vw_x(Vijkl, Rkij(:, i, j), Rkij(:, k, l), NCholesky)
                                    Ei = OccEnergies(i)
                                    Ej = OccEnergies(j)
                                    Ek = OccEnergies(k)
                                    El = OccEnergies(l)
                                    do b = 1, NVirt
                                          do a = 1, NVirt
                                                call real_vw_x(Vaibk, Rkai(:, a, i), Rkai(:, b, k), NCholesky)
                                                call real_vw_x(Vblaj, Rkai(:, b, l), Rkai(:, a, j), NCholesky)
                                                call real_vw_x(Vbjal, Rkai(:, b, j), Rkai(:, a, l), NCholesky)
                                                Ea = VirtEnergies(a)
                                                Eb = VirtEnergies(b)
                                                Taibk = Vaibk / (Ei + Ek - Ea - Eb)
                                                Tblaj = Vblaj / (Ej + El - Ea - Eb)
                                                Tbjal = Vbjal / (Ej + El - Ea - Eb)
                                                Ec2e = Ec2e + Vijkl * Taibk * Tblaj
                                                Ec2f = Ec2f + Vijkl * Taibk * Tbjal
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
            !$omp end parallel do            
            Ec2e = TWO * Ec2e
            Ec2f = -Ec2f
      end subroutine rpa_PT_Order3_ef


      subroutine rpa_PT_Order3_kl(Ec2k, Ec2l, Rkai, Rkab, OccEnergies, &
            VirtEnergies, NOcc, NVirt, NCholesky)
            
            real(F64), intent(out)                    :: Ec2k, Ec2l
            real(F64), dimension(:, :, :), intent(in) :: Rkai
            real(F64), dimension(:, :, :), intent(in) :: Rkab
            real(F64), dimension(:), intent(in)       :: OccEnergies
            real(F64), dimension(:), intent(in)       :: VirtEnergies
            integer, intent(in)                       :: NOcc
            integer, intent(in)                       :: NVirt
            integer, intent(in)                       :: NCholesky

            integer :: a, i, b, j, c, d
            real(F64) :: Ea, Ei, Eb, Ej, Ec, Ed
            real(F64) :: Taicj, Tdjbi, Tdibj
            real(F64) :: Vabcd, Vaicj, Vdjbi, Vdibj

            Ec2k = ZERO
            Ec2l = ZERO
            !$omp parallel do private(a, i, b, j, c, d) &
            !$omp private(Ea, Ei, Eb, Ej, Ec, Ed) &
            !$omp private(Taicj, Tdjbi, Tdibj) &
            !$omp private(Vabcd, Vaicj, Vdjbi, Vdibj) &
            !$omp reduction(+:Ec2k, Ec2l) &
            !$omp collapse(4)
            do d = 1, NVirt
                  do c = 1, NVirt
                        do b = 1, NVirt
                              do a = 1, NVirt
                                    call real_vw_x(Vabcd, Rkab(:, a, b), Rkab(:, c, d), NCholesky)
                                    Ea = VirtEnergies(a)
                                    Eb = VirtEnergies(b)
                                    Ec = VirtEnergies(c)
                                    Ed = VirtEnergies(d)
                                    do j = 1, NOcc
                                          do i = 1, NOcc
                                                call real_vw_x(Vaicj, Rkai(:, a, i), Rkai(:, c, j), NCholesky)
                                                call real_vw_x(Vdjbi, Rkai(:, d, j), Rkai(:, b, i), NCholesky)
                                                call real_vw_x(Vdibj, Rkai(:, d, i), Rkai(:, b, j), NCholesky)
                                                Ei = OccEnergies(i)
                                                Ej = OccEnergies(j)
                                                Taicj = Vaicj / (Ei + Ej - Ea - Ec)
                                                Tdjbi = Vdjbi / (Ei + Ej - Eb - Ed)
                                                Tdibj = Vdibj / (Ei + Ej - Eb - Ed)
                                                Ec2k = Ec2k + Vabcd * Taicj * Tdjbi
                                                Ec2l = Ec2l + Vabcd * Taicj * Tdibj
                                          end do
                                    end do
                              end do
                        end do
                  end do
            end do
            !$omp end parallel do            
            Ec2k = TWO * Ec2k
            Ec2l = -Ec2l
      end subroutine rpa_PT_Order3_kl
end module rpa_PT_Terms
