module slater_x_potential
      use arithmetic
      use math_constants
      use basis_sets

      implicit none

      real(F64), parameter :: SLATER_X_RHO_THRESH = 1.0E-10_F64

contains

      subroutine slater_RhoEff(RhoEff, RhoEffMatrix, XOrb, XShells, XNShells, &
            ShellParamsIdx, ShellLoc, NAngFunc, DeltaK)
            
            real(F64), intent(out)                 :: RhoEff
            real(F64), dimension(:, :), intent(in) :: RhoEffMatrix
            real(F64), dimension(:), intent(in)    :: XOrb
            integer, dimension(:), intent(in)      :: XShells
            integer, intent(in)                    :: XNShells
            integer, dimension(:), intent(in)      :: ShellParamsIdx
            integer, dimension(:), intent(in)      :: ShellLoc
            integer, dimension(:), intent(in)      :: NAngFunc
            integer, intent(in)                    :: DeltaK

            real(F64) :: sum_0_eff
            real(F64) :: p_val, q_val
            integer :: ShellP, ShellParamsP, NAngFuncP
            integer :: ShellQ, ShellParamsQ, NAngFuncQ
            integer :: uu, vv
            integer :: l, k
            integer :: p, q, p0, p1, q0, q1

            RhoEff = ZERO
            k = 1
            shloop1: do uu = 1, XNShells
                  ShellP = XShells(uu)
                  ShellParamsP = ShellParamsIdx(ShellP)
                  NAngFuncP = NAngFunc(ShellParamsP)
                  p0 = ShellLoc(ShellP)
                  p1 = ShellLoc(ShellP) + NAngFuncP - 1
                  ploop: do p = p0, p1 
                        p_val  = XOrb(k)
                        l = k
                        k = k + DeltaK
                        !
                        ! Diagonal element
                        !
                        sum_0_eff = (ONE/TWO) * RhoEffMatrix(p, p) * p_val
                        l = l + DeltaK
                        !
                        ! Diagonal block
                        !
                        do q = p + 1, p1
                              q_val  = XOrb(l)
                              l = l + DeltaK
                              sum_0_eff = sum_0_eff + RhoEffMatrix(q, p) * q_val
                        end do
                        do vv = uu + 1, XNShells
                              ShellQ = XShells(vv)
                              ShellParamsQ = ShellParamsIdx(ShellQ)
                              NAngFuncQ = NAngFunc(ShellParamsQ)
                              q0 = ShellLoc(ShellQ)
                              q1 = ShellLoc(ShellQ) + NAngFuncQ - 1
                              do q = q0, q1
                                    q_val  = XOrb(l)
                                    l = l + DeltaK
                                    sum_0_eff = sum_0_eff + RhoEffMatrix(q, p) * q_val
                              end do
                        end do
                        RhoEff = RhoEff + p_val * sum_0_eff
                  end do ploop
            end do shloop1
            RhoEff = TWO * RhoEff
      end subroutine slater_RhoEff


      subroutine slater_Y_Sphere(Exc, YRho, RhoEffMatrix, XRho, XOrb, &
            XShells, XNShells, XWeights, NSpher, DeltaK, AOBasis)
            !
            ! Compute the Slater potential for a batch of grid points.
            !
            ! 1. Della Sala, F. and Gorling, A. J. Chem. Phys. 115, 5718 (2001);
            !    doi: 10.1063/1.1398093
            !
            real(F64), intent(inout)                  :: Exc
            real(F64), dimension(:), intent(inout)    :: YRho
            real(F64), dimension(:, :), intent(in)    :: RhoEffMatrix
            real(F64), dimension(:), intent(in)       :: XRho
            real(F64), dimension(:, :), intent(in)    :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            integer, intent(in)                       :: NSpher
            integer, intent(in)                       :: DeltaK
            type(TAOBasis), intent(in)                :: AOBasis

            real(F64) :: RhoEff
            integer :: k

            associate ( &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellLoc => AOBasis%ShellLocCart &
                  )
                  do k = 1, NSpher
                        if (XRho(k) > SLATER_X_RHO_THRESH) then
                              call slater_RhoEff(RhoEff, RhoEffMatrix, XOrb(:, k), XShells(:, k), XNShells(k), &
                                    ShellParamsIdx, ShellLoc, NAngFunc, DeltaK)
                              !
                              ! Potential (written to the same array as derivatives
                              ! with respect to the density): Eq. 22 in Ref. 1
                              !
                              YRho(k) = YRho(k) + RhoEff / XRho(k)
                              !
                              ! Energy density: Eq. 25 in Ref. 1
                              !
                              Exc = Exc + XWeights(k) * (ONE/TWO) * RhoEff
                        end if
                  end do
            end associate
      end subroutine slater_Y_Sphere


      subroutine slater_YU_Sphere(Exc, YRho, RhoEffMatrix, XRho, XOrb, &
            XShells, XNShells, XWeights, NSpher, DeltaK, AOBasis)
            !
            ! Compute the Slater potential for a batch of grid points.
            !
            ! 1. Della Sala, F. and Gorling, A. J. Chem. Phys. 115, 5718 (2001);
            !    doi: 10.1063/1.1398093
            !
            real(F64), intent(inout)                  :: Exc
            real(F64), dimension(:, :), intent(inout) :: YRho
            real(F64), dimension(:, :, :), intent(in) :: RhoEffMatrix
            real(F64), dimension(:, :), intent(in)    :: XRho
            real(F64), dimension(:, :), intent(in)    :: XOrb
            integer, dimension(:, :), intent(out)     :: XShells
            integer, dimension(:), intent(out)        :: XNShells
            real(F64), dimension(:), intent(out)      :: XWeights
            integer, intent(in)                       :: NSpher
            integer, intent(in)                       :: DeltaK
            type(TAOBasis), intent(in)                :: AOBasis

            real(F64) :: RhoEff
            integer :: k, s

            associate ( &
                  NAngFunc => AOBasis%NAngFuncCart, &
                  ShellParamsIdx => AOBasis%ShellParamsIdx, &
                  ShellLoc => AOBasis%ShellLocCart &
                  )
                  do k = 1, NSpher
                        do s = 1, 2
                              if (TWO*XRho(s, k) > SLATER_X_RHO_THRESH) then
                                    call slater_RhoEff(RhoEff, RhoEffMatrix(:, :, s), XOrb(:, k), XShells(:, k), XNShells(k), &
                                          ShellParamsIdx, ShellLoc, NAngFunc, DeltaK)
                                    !
                                    ! Potential (written to the same array as derivatives
                                    ! with respect to the density): Eq. 22 in Ref. 1
                                    !
                                    YRho(s, k) = YRho(s, k) + RhoEff / XRho(s, k)
                                    !
                                    ! Energy density: Eq. 25 in Ref. 1
                                    !
                                    Exc = Exc + XWeights(k) * (ONE/TWO) * RhoEff
                              end if
                        end do
                  end do
            end associate
      end subroutine slater_YU_Sphere
end module slater_x_potential
