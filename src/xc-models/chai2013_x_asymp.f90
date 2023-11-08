module chai2013_x_asymp
      use arithmetic
      use math_constants
      use AtomicDensities
      
      implicit none

      real(F64), parameter :: CHAI2013_RHO_THRESH = 1.0E-10_F64

contains
      
      subroutine chai2013_x(Eps, Ex_Rho, OriginX, OriginY, OriginZ, R, SpherX, SpherY, SpherZ, &
            NSpher, Rho, Omega, EpsPrefac, Frac, AtomElementMap, RhoAtomic, System, AOBasis)
            !
            ! Local Fermi-Amaldi asymptotic correction (the LFAs variant)
            ! of Chai et al.
            !
            ! 1. Chi-Ruei Pan, Po-Tung Fang, and Jeng-Da Chai, Phys. Rev. A 87, 052510 (2013);
            !    doi: 10.1103/PhysRevA.87.052510
            !
            real(F64), dimension(:), intent(inout) :: Eps
            real(F64), dimension(:), intent(inout) :: Ex_Rho
            real(F64), intent(in)                  :: OriginX
            real(F64), intent(in)                  :: OriginY
            real(F64), intent(in)                  :: OriginZ
            real(F64), intent(in)                  :: R
            real(F64), dimension(:), intent(in)    :: SpherX
            real(F64), dimension(:), intent(in)    :: SpherY
            real(F64), dimension(:), intent(in)    :: SpherZ
            integer, intent(in)                    :: NSpher
            real(F64), dimension(:), intent(in)    :: Rho
            real(F64), intent(in)                  :: Omega
            real(F64), intent(in)                  :: EpsPrefac
            real(F64), intent(in)                  :: Frac
            integer, dimension(:), intent(in)      :: AtomElementMap
            real(F64), dimension(:, :), intent(in) :: RhoAtomic
            type(TSystem), intent(in)              :: System
            type(TAOBasis), intent(in)             :: AOBasis            

            integer :: k, s, i
            real(F64) :: Vx, HirshDenom, RhoI, LenRki
            real(F64), dimension(3) :: Rk, Rki

            associate( &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NPrimitives => AOBasis%NPrimitives, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxNShells => AOBasis%MaxNShells, &
                  AtomCoords => System%AtomCoords, &
                  RealAtoms => System%RealAtoms)                  

                  do k = 1, NSpher
                        if (Rho(k) > CHAI2013_RHO_THRESH) then
                              Rk(1) = OriginX + R * SpherX(k)
                              Rk(2) = OriginY + R * SpherY(k)
                              Rk(3) = OriginZ + R * SpherZ(k)
                              Vx = ZERO
                              HirshDenom = ZERO
                              do s = 1, 2
                                    do i = RealAtoms(1, s), RealAtoms(2, s)
                                          Rki = Rk - AtomCoords(:, i)                                          
                                          LenRki = norm2(Rki)
                                          !
                                          ! Compute the spherically-averaged density of the I-th atom to get
                                          ! the numerator of the Hirshfeld weight. The denominator will
                                          ! be accounted for after the loop over atoms is completed.
                                          !
                                          RhoI = RhoSpherValue(RhoAtomic(:, AtomElementMap(i)), LenRki, i, &
                                                ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
                                                AtomShellMap, AtomShellN, MaxNShells)
                                          Vx = Vx - RhoI * erf(omega * LenRki) / LenRki
                                          HirshDenom = HirshDenom + RhoI
                                    end do
                              end do
                              Vx = Vx / HirshDenom
                              !
                              ! Eqs. 15 and 17 in Ref. 1.
                              !
                              Eps(k) = Eps(k) + Frac * EpsPrefac * Vx
                              Ex_Rho(k) = Ex_Rho(k) + Frac * Vx                              
                        end if
                  end do
            end associate
      end subroutine chai2013_x


      subroutine u_chai2013_x(Eps, Ex_Rho, OriginX, OriginY, OriginZ, R, SpherX, SpherY, SpherZ, &
            NSpher, Rho, Omega, EpsPrefac, Frac, AtomElementMap, RhoAtomic, System, AOBasis)
            !
            ! Local Fermi-Amaldi asymptotic correction (the LFAs variant)
            ! of Chai et al.
            !
            ! 1. Chi-Ruei Pan, Po-Tung Fang, and Jeng-Da Chai, Phys. Rev. A 87, 052510 (2013);
            !    doi: 10.1103/PhysRevA.87.052510
            !
            real(F64), dimension(:), intent(inout)    :: Eps
            real(F64), dimension(:, :), intent(inout) :: Ex_Rho
            real(F64), intent(in)                     :: OriginX
            real(F64), intent(in)                     :: OriginY
            real(F64), intent(in)                     :: OriginZ
            real(F64), intent(in)                     :: R
            real(F64), dimension(:), intent(in)       :: SpherX
            real(F64), dimension(:), intent(in)       :: SpherY
            real(F64), dimension(:), intent(in)       :: SpherZ
            integer, intent(in)                       :: NSpher
            real(F64), dimension(:, :), intent(in)    :: Rho
            real(F64), intent(in)                     :: Omega
            real(F64), intent(in)                     :: EpsPrefac
            real(F64), intent(in)                     :: Frac
            integer, dimension(:), intent(in)         :: AtomElementMap
            real(F64), dimension(:, :), intent(in)    :: RhoAtomic
            type(TSystem), intent(in)                 :: System
            type(TAOBasis), intent(in)                :: AOBasis            

            integer :: k, s, i
            real(F64) :: Vx, HirshDenom, RhoI, LenRki
            real(F64), dimension(3) :: Rk, Rki

            associate( &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  CntrCoeffs => AOBasis%CntrCoeffs, &
                  Exponents => AOBasis%Exponents, &
                  NPrimitives => AOBasis%NPrimitives, &
                  ShellMomentum => AOBasis%ShellMomentum, &
                  AtomShellMap => AOBasis%AtomShellMap, &
                  AtomShellN => AOBasis%AtomShellN, &
                  MaxNShells => AOBasis%MaxNShells, &
                  AtomCoords => System%AtomCoords, &
                  RealAtoms => System%RealAtoms)                  

                  do k = 1, NSpher
                        if (TWO*Rho(1,k) > CHAI2013_RHO_THRESH &
                              .or. TWO*Rho(2,k) > CHAI2013_RHO_THRESH) then
                              Rk(1) = OriginX + R * SpherX(k)
                              Rk(2) = OriginY + R * SpherY(k)
                              Rk(3) = OriginZ + R * SpherZ(k)
                              Vx = ZERO
                              HirshDenom = ZERO
                              do s = 1, 2
                                    do i = RealAtoms(1, s), RealAtoms(2, s)
                                          Rki = Rk - AtomCoords(:, i)                                          
                                          LenRki = norm2(Rki)
                                          !
                                          ! Compute the spherically-averaged density of the I-th atom to get
                                          ! the numerator of the Hirshfeld weight. The denominator will
                                          ! be accounted for after the loop over atoms is completed.
                                          !
                                          RhoI = RhoSpherValue(RhoAtomic(:, AtomElementMap(i)), LenRki, i, &
                                                ShellParamsIdx, CntrCoeffs, Exponents, NPrimitives, ShellMomentum, &
                                                AtomShellMap, AtomShellN, MaxNShells)
                                          Vx = Vx - RhoI * erf(omega * LenRki) / LenRki
                                          HirshDenom = HirshDenom + RhoI
                                    end do
                              end do
                              Vx = Vx / HirshDenom
                              !
                              ! Eqs. 15 and 17 in Ref. 1.
                              !
                              Eps(k) = Eps(k) + Frac * EpsPrefac * Vx
                              if (TWO*Rho(1,k) > CHAI2013_RHO_THRESH) then
                                    Ex_Rho(1, k) = Ex_Rho(1, k) + Frac * Vx
                              end if
                              if (TWO*Rho(2,k) > CHAI2013_RHO_THRESH) then
                                    Ex_Rho(2, k) = Ex_Rho(2, k) + Frac * Vx
                              end if
                        end if
                  end do
            end associate
      end subroutine u_chai2013_x


      subroutine chai2013_free_x(Eps, Ex_Rho, OriginX, OriginY, OriginZ, R, SpherX, SpherY, SpherZ, &
            NSpher, Rho, Omega, EpsPrefac, Frac, System)
            !
            ! Local Fermi-Amaldi asymptotic correction (the LFAs variant)
            ! of Chai et al.
            !
            ! Variant intended for free atoms, for which the Hirshfeld weight
            ! equals one at all grid points.
            !
            ! 1. Chi-Ruei Pan, Po-Tung Fang, and Jeng-Da Chai, Phys. Rev. A 87, 052510 (2013);
            !    doi: 10.1103/PhysRevA.87.052510
            !
            real(F64), dimension(:), intent(inout) :: Eps
            real(F64), dimension(:), intent(inout) :: Ex_Rho
            real(F64), intent(in)                  :: OriginX
            real(F64), intent(in)                  :: OriginY
            real(F64), intent(in)                  :: OriginZ
            real(F64), intent(in)                  :: R
            real(F64), dimension(:), intent(in)    :: SpherX
            real(F64), dimension(:), intent(in)    :: SpherY
            real(F64), dimension(:), intent(in)    :: SpherZ
            integer, intent(in)                    :: NSpher
            real(F64), dimension(:), intent(in)    :: Rho
            real(F64), intent(in)                  :: Omega
            real(F64), intent(in)                  :: EpsPrefac
            real(F64), intent(in)                  :: Frac
            type(TSystem), intent(in)              :: System

            integer :: k
            real(F64) :: Vx, LenRki
            real(F64), dimension(3) :: Rk, Rki

            associate(AtomCoords => System%AtomCoords)                  
                  do k = 1, NSpher
                        if (Rho(k) > CHAI2013_RHO_THRESH) then
                              Rk(1) = OriginX + R * SpherX(k)
                              Rk(2) = OriginY + R * SpherY(k)
                              Rk(3) = OriginZ + R * SpherZ(k)
                              Rki = Rk - AtomCoords(:, 1)
                              LenRki = norm2(Rki)
                              Vx = -erf(omega * LenRki) / LenRki
                              !
                              ! Eqs. 15 and 17 in Ref. 1.
                              !
                              Eps(k) = Eps(k) + Frac * EpsPrefac * Vx
                              Ex_Rho(k) = Ex_Rho(k) + Frac * Vx                              
                        end if
                  end do
            end associate
      end subroutine chai2013_free_x


      subroutine u_chai2013_free_x(Eps, Ex_Rho, OriginX, OriginY, OriginZ, R, SpherX, SpherY, SpherZ, &
            NSpher, Rho, Omega, EpsPrefac, Frac, System)
            !
            ! Local Fermi-Amaldi asymptotic correction (the LFAs variant)
            ! of Chai et al.
            !
            ! Variant intended for free atoms, for which the Hirshfeld weight
            ! equals one at all grid points.
            !
            ! 1. Chi-Ruei Pan, Po-Tung Fang, and Jeng-Da Chai, Phys. Rev. A 87, 052510 (2013);
            !    doi: 10.1103/PhysRevA.87.052510
            !
            real(F64), dimension(:), intent(inout)    :: Eps
            real(F64), dimension(:, :), intent(inout) :: Ex_Rho
            real(F64), intent(in)                     :: OriginX
            real(F64), intent(in)                     :: OriginY
            real(F64), intent(in)                     :: OriginZ
            real(F64), intent(in)                     :: R
            real(F64), dimension(:), intent(in)       :: SpherX
            real(F64), dimension(:), intent(in)       :: SpherY
            real(F64), dimension(:), intent(in)       :: SpherZ
            integer, intent(in)                       :: NSpher
            real(F64), dimension(:, :), intent(in)    :: Rho
            real(F64), intent(in)                     :: Omega
            real(F64), intent(in)                     :: EpsPrefac
            real(F64), intent(in)                     :: Frac
            type(TSystem), intent(in)                 :: System

            integer :: k
            real(F64) :: Vx, LenRki
            real(F64), dimension(3) :: Rk, Rki

            associate(AtomCoords => System%AtomCoords)                  
                  do k = 1, NSpher
                        if (TWO*Rho(1,k) > CHAI2013_RHO_THRESH &
                              .or. TWO*Rho(2,k) > CHAI2013_RHO_THRESH) then
                              Rk(1) = OriginX + R * SpherX(k)
                              Rk(2) = OriginY + R * SpherY(k)
                              Rk(3) = OriginZ + R * SpherZ(k)
                              Rki = Rk - AtomCoords(:, 1)
                              LenRki = norm2(Rki)
                              Vx = -erf(omega * LenRki) / LenRki
                              !
                              ! Eqs. 15 and 17 in Ref. 1.
                              !
                              Eps(k) = Eps(k) + Frac * EpsPrefac * Vx
                              if (TWO*Rho(1,k) > CHAI2013_RHO_THRESH) then
                                    Ex_Rho(1, k) = Ex_Rho(1, k) + Frac * Vx
                              end if
                              if (TWO*Rho(2,k) > CHAI2013_RHO_THRESH) then
                                    Ex_Rho(2, k) = Ex_Rho(2, k) + Frac * Vx
                              end if
                        end if
                  end do
            end associate
      end subroutine u_chai2013_free_x
end module chai2013_x_asymp
