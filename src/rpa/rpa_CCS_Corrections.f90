module rpa_CCS_Corrections
      use arithmetic
      use real_linalg
      use display
      use string
      use rpa_definitions

      implicit none
      
contains

      subroutine rpa_CCS_hadamard_Z_YXU(YXUgh, Zgh, NGridTHC)
            real(F64), dimension(NGridTHC**2), intent(inout) :: YXUgh
            real(F64), dimension(NGridTHC**2), intent(in)    :: Zgh
            integer, intent(in)                              :: NGridTHC

            integer :: gh

            do gh = 1, NGridTHC**2
                  YXUgh(gh) = Zgh(gh) * YXUgh(gh)
            end do
      end subroutine rpa_CCS_hadamard_Z_YXU

      
      subroutine rpa_CCS_intermediate_WZYXUW(WZYXUWpq, ZYXUWgp, Z_YXUgh, Wgp, NGridTHC, NMO)
            real(F64), dimension(NMO, NMO), intent(out)          :: WZYXUWpq
            real(F64), dimension(NGridTHC, NMO), intent(out)     :: ZYXUWgp
            real(F64), dimension(NGridTHC, NGridTHC), intent(in) :: Z_YXUgh
            real(F64), dimension(NGridTHC, NMO), intent(in)      :: Wgp
            integer, intent(in)                                  :: NGridTHC, NMO

            call real_ab(ZYXUWgp, Z_YXUgh, Wgp)
            call real_aTb(WZYXUWpq, Wgp, ZYXUWgp)
      end subroutine rpa_CCS_intermediate_WZYXUW

      
      subroutine rpa_CCS_intermediate_WWUT(WWUTg, WUTgp, UTpq, Wgp, NGridTHC, NMO)
            real(F64), dimension(NGridTHC), intent(out)      :: WWUTg
            real(F64), dimension(NGridTHC, NMO), intent(out) :: WUTgp
            real(F64), dimension(NMO, NMO), intent(in)       :: UTpq
            real(F64), dimension(NGridTHC, NMO), intent(in)  :: Wgp
            integer, intent(in)                              :: NGridTHC
            integer, intent(in)                              :: NMO

            integer :: q

            call real_ab(WUTgp, Wgp, UTpq)
            WWUTg = ZERO
            do q = 1, NMO
                  WWUTg(:) = WWUTg(:) + Wgp(:, q) * WUTgp(:, q)
            end do
      end subroutine rpa_CCS_intermediate_WWUT

      
      subroutine rpa_CCS_YXU_Z_WW_UT(S, YXUgg, Zgh, WWUTg, NGridTHC)
            real(F64), intent(out)                               :: S
            real(F64), dimension(NGridTHC), intent(in)           :: YXUgg
            real(F64), dimension(NGridTHC, NGridTHC), intent(in) :: Zgh
            real(F64), dimension(NGridTHC), intent(in)           :: WWUTg
            integer, intent(in)                                  :: NGridTHC

            integer :: g, h

            S = ZERO
            do h = 1, NGridTHC
                  do g = 1, NGridTHC
                        S = S + Zgh(g, h) * YXUgg(g) * WWUTg(h)
                  end do
            end do
      end subroutine rpa_CCS_YXU_Z_WW_UT
      

      subroutine rpa_CCS_T1(Tai, hHFai, OccEnergies, VirtEnergies, NOcc, NVirt)
            !
            ! Compute the exact solution of the coupled-cluster singles amplitude equation
            !
            ! 0 = hHF(ai) + T(ai)*e(a) - e(i)*T(ai) - Sum(bj) T(aj)*F(bj)*T(bi)
            !
            ! The exact solution is obtained from the eigenproblem of the full hHF
            ! matrix
            !
            ! (eo    hHFov) (Xoo Xov)    (Xoo Xov) ( e'o   0 )
            ! (hHFvo   ev ) (Xvo Xvv)  = (Xvo Xvv) ( 0    e'v)
            !
            ! The blocks of the eigenvector matrix X satisfy the equation
            !
            ! 0 = hHFov + eo*Xov*Xvv**(-1) - Xov*Xvv**(-1)*ev - (Xov*Xvv**(-1))*hHFvo*(Xov*Xvv**(-1))
            !
            ! Comparison of the above equation with the matrix representation
            ! of the CCS amplitude equation yields
            !
            ! Tvo = -(Xvv**(-1))**T*Xov**T
            !
            ! or, equivalently, a linear system
            !
            ! Xvv**T Tvo =  -Xov**T
            !
            real(F64), dimension(NVirt, NOcc), intent(out) :: Tai
            real(F64), dimension(NVirt, NOcc), intent(in)  :: hHFai
            real(F64), dimension(NOcc), intent(in)         :: OccEnergies
            real(F64), dimension(NVirt), intent(in)        :: VirtEnergies
            integer, intent(in)                            :: NOcc
            integer, intent(in)                            :: NVirt

            real(F64), dimension(:, :), allocatable :: hHFpq
            real(F64), dimension(:), allocatable :: Ep
            real(F64), dimension(:, :), allocatable :: XvvT
            integer :: a0, a1, i0, i1, i, a
            integer :: NMO

            NMO = NOcc + NVirt
            allocate(hHFpq(NMO, NMO))
            allocate(Ep(NMO))
            a0 = NOcc + 1
            a1 = NOcc + NVirt
            i0 = 1
            i1 = NOcc
            hHFpq = ZERO
            hHFpq(a0:a1, i0:i1) = hHFai
            do i = 1, NOcc
                  hHFpq(i, i) = OccEnergies(i)
            end do
            do a = 1, NVirt
                  hHFpq(NOcc+a, NOcc+a) = VirtEnergies(a)
            end do
            call symmetric_eigenproblem(Ep, hHFpq, NMO, .true.)
            allocate(XvvT(Nvirt, NVirt))
            Tai = transpose(hHFpq(i0:i1, a0:a1)) 
            XvvT = transpose(hHFpq(a0:a1, a0:a1))
            call real_Axb_nonsymmetric_gesv(Tai, XvvT)
            Tai = -Tai
      end subroutine rpa_CCS_T1

      
      subroutine rpa_PT1_T1(Tai, hHFai, OccEnergies, VirtEnergies, NOcc, NVirt)
            real(F64), dimension(NVirt, NOcc), intent(out) :: Tai
            real(F64), dimension(NVirt, NOcc), intent(in)  :: hHFai
            real(F64), dimension(NOcc), intent(in)         :: OccEnergies
            real(F64), dimension(NVirt), intent(in)        :: VirtEnergies
            integer, intent(in)                            :: NOcc
            integer, intent(in)                            :: NVirt

            integer :: a, i

            do i = 1, NOcc
                  do a = 1, NVirt
                        Tai(a, i) = -hHFai(a, i) / (VirtEnergies(a) - OccEnergies(i))
                  end do
            end do
      end subroutine rpa_PT1_T1
end module rpa_CCS_Corrections
