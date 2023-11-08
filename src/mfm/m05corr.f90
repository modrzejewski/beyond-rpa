module m05corr
      use lsdpw92

      implicit none

contains
      
      Subroutine um05c(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy)
            ! -----------------------------------------------------------------------
            !  M06c evaluates the correlation part of the M06 suite of             
            !  functionals on the grids.                                           
            !  !!! Second derivatives are not available yet.                       
            !                                                                      
            !     *  Ref: (a) Zhao, Y., Schultz, N. E., and Truhlar, D. G. J.      
            !     *   Chem. Phys. 123, 161103 (2005).                              
            !     *       (b) Y. Zhao, Schultz, N. E., and D. G. Truhlar, J. Chem. 
            !     *   Theory Comput. 2, 364 (2006)                                  
            !     *       ijzy - 1 M05                                             
            !     *       ijzy - 2 M05-2X                                          
            !  YZ (12/08)                                                          
            ! -----------------------------------------------------------------------
            !
            integer, intent(in)                              :: ngrid 
            double precision, dimension(:, :), intent(in)    :: rho
            double precision, dimension(:, :), intent(in)    :: sigma
            double precision, dimension(:, :), intent(in)    :: tau
            double precision, dimension(:), intent(inout)    :: f
            double precision, dimension(:, :), intent(inout) :: vrho
            double precision, dimension(:, :), intent(inout) :: vsigma
            double precision, dimension(:, :), intent(inout) :: vtau
            integer, intent(in)                              :: ijzy

            double precision RS,RSP,Zeta,dZdA,dZdB,PotLC,dLdS,dLdZ, &
                  P, EUEG
            double precision PA,GAA,TauA,FA,FPA,FGA,FTA,EUA,ChiA,EUPA,ChiAP,ChiAG
            double precision PB,GBB,TauB,FB,FPB,FGB,FTB,EUB,ChiB,EUPB,ChiBP,ChiBG
            double precision sopp0, sopp1,sopp2, sopp3, sopp4
            double precision sss0, sss1, sss2, sss3, sss4
            double precision U, W, dUdChiA,dUdChiB,dUdPA,dUdPB,dUdGA,dUdGB, &
                  dWdU,dWdPA,dWdPB, dWdGA,dWdGB,EUEGPA,EUEGPB
            double precision, parameter :: COpp = 0.0031d0

            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: F1 = 1.0d0
            double precision, parameter :: F2 = 2.0d0
            double precision, parameter :: F3 = 3.0d0
            double precision, parameter :: F4 = 4.0d0
            double precision, parameter :: Pi34 = F3 / (F4*Pi)
            double precision, parameter :: F13 = F1 / F3

            double precision, parameter :: dtol = 1.d-10

            INTEGER i

            if (ijzy.eq.1) then
                  !   Parameters for M05 Correlation
                  sopp0= 1.00000d0
                  sopp1= 3.78569d0
                  sopp2= -14.15261d0
                  sopp3= -7.46589d0
                  sopp4= 17.94491d0

                  sss0=  1.00000d0
                  sss1=  3.77344d0
                  sss2=  -26.04463d0
                  sss3=  30.69913d0
                  sss4=  -9.22695d0
            else
                  !   Parameters for M05-2X Correlation
                  sopp0= 1.00000d+0
                  sopp1= 1.09297d+0
                  sopp2= -3.79171d+0
                  sopp3= 2.82810d+0
                  sopp4= -10.58909d+0

                  sss0=  1.00000d0
                  sss1=  -3.05430d0
                  sss2=  7.61854d0
                  sss3=  1.47665d0
                  sss4=  -11.92365d0
            endif

            DO i = 1,NGrid
                  PA = rho(1, i)
                  if (pa .gt. dtol) then
                        GAA   = sigma(1, i)
                        TauA =  tau(1, i)

                        call m05css(PA,GAA,TauA,FA,FPA,FGA,FTA,EUA, &
                              ChiA,EUPA,ChiAP,ChiAG,sss0,sss1,sss2,sss3,sss4)

                        f(i)         = f(i) + fa
                        vrho(1, i)   = vrho(1, i) + fpa
                        vsigma(1, i) = vsigma(1, i) + fga
                        vtau(1, i)   = vtau(1, i) + fta
                  ENDIF
                  PB = rho(2, i)
                  if (pb .gt. dtol) then
                        GBB   =  sigma(3, i)
                        TauB = tau(2, i) 

                        call m05css(PB,GBB,TauB,FB,FPB,FGB,FTB,EUB, &
                              ChiB,EUPB,ChiBP,ChiBG,sss0,sss1,sss2,sss3,sss4)

                        f(i)         = f(i) + fb
                        vrho(2, i)   = vrho(2, i) + fpb
                        vsigma(3, i) = vsigma(3, i) + fgb
                        vtau(2, i)   = vtau(2, i) + ftb
                  ENDIF
                  P=PA+PB
                  IF (PB.gt.DTol.and.PA.gt.DTol) THEN
                        RS = (Pi34/P) ** F13 
                        RSP = -RS/(F3*P)
                        Zeta = (PA-PB)/P
                        dZdA = (F1-Zeta)/P
                        dZdB = (-F1-Zeta)/P
                        !       
                        !      lsdac is a subroutine to evaluate the Perdew-Wang-91 correlation functional 
                        !      local spin density approximation (LSDA) to the correlation energy of a uniform 
                        !      electron gas. (Phys. Rev. B 45, 13244 (1992)). Users should provid their own
                        !      for this LSDA correlation functional or they may find this routine on Kieron 
                        !      Burke's Web site at http://www.chem.uci.edu/~kieron/dftold2/pubs/PBE.asc
                        !
                        Call lsdac(RS,Zeta,PotLC,dLdS,dLdZ)
                        EUEG = P*PotLC - EUA - EUB
                        U = COpp*(ChiA+ChiB)/(F1 + COpp*(ChiA+ChiB))
                        W = sopp0+U*(sopp1+U*(sopp2+U*(sopp3+U*sopp4)))
                        F(i) = F(i) + EUEG*W
                        dUdChiA =COpp/(F1 + COpp*(ChiA+ChiB))**2
                        dUdChiB =COpp/(F1 + COpp*(ChiA+ChiB))**2
                        dUdPA= dUdChiA*ChiAP
                        dUdPB= dUdChiB*ChiBP
                        dUdGA= dUdChiA*ChiAG
                        dUdGB= dUdChiB*ChiBG
                        dWdU =sopp1+U*(F2*sopp2+U*(F3*sopp3+U*F4*sopp4))
                        dWdPA= dWdU*dUdPA
                        dWdPB= dWdU*dUdPB
                        dWdGA= dWdU*dUdGA
                        dWdGB= dWdU*dUdGB
                        EUEGPA = PotLC + P*dLdS*RSP + P*dLdZ*dZdA - EUPA
                        EUEGPB = PotLC + P*dLdS*RSP + P*dLdZ*dZdB - EUPB

                        vrho(1, i)   = vrho(1, i) + EUEGPA*W + EUEG*dWdPA 
                        vsigma(1, i) = vsigma(1, i) + EUEG*dWdGA 
                        vrho(2, i)   = vrho(2, i) + EUEGPB*W + EUEG*dWdPB  
                        vsigma(3, i) = vsigma(3, i) + EUEG*dWdGB
                  ENDIF
            ENDDO
      end subroutine um05c


      Subroutine m05c(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy)
            ! -----------------------------------------------------------------------
            !  M06c evaluates the correlation part of the M06 suite of             
            !  functionals on the grids.                                           
            !  !!! Second derivatives are not available yet.                       
            !                                                                      
            !     *  Ref: (a) Zhao, Y., Schultz, N. E., and Truhlar, D. G. J.      
            !     *   Chem. Phys. 123, 161103 (2005).                              
            !     *       (b) Y. Zhao, Schultz, N. E., and D. G. Truhlar, J. Chem. 
            !     *   Theory Comput. 2, 364 (2006)                                  
            !     *       ijzy - 1 M05                                             
            !     *       ijzy - 2 M05-2X                                          
            !  YZ (12/08)                                                          
            ! -----------------------------------------------------------------------
            !
            integer, intent(in)                           :: ngrid 
            double precision, dimension(:), intent(in)    :: rho
            double precision, dimension(:), intent(in)    :: sigma
            double precision, dimension(:), intent(in)    :: tau
            double precision, dimension(:), intent(inout) :: f
            double precision, dimension(:), intent(inout) :: vrho
            double precision, dimension(:), intent(inout) :: vsigma
            double precision, dimension(:), intent(inout) :: vtau
            integer, intent(in)                           :: ijzy

            double precision RS,RSP,Zeta,dZdA,PotLC,dLdS,dLdZ, &
                  P, EUEG
            double precision PA,GAA,TauA,FA,FPA,FGA,FTA,EUA,ChiA,EUPA,ChiAP,ChiAG
            double precision sopp0, sopp1,sopp2, sopp3, sopp4
            double precision sss0, sss1, sss2, sss3, sss4
            double precision U, W, dUdChiA,dUdPA,dUdGA,&
                  dWdU,dWdPA,dWdGA,EUEGPA
            double precision, parameter :: COpp = 0.0031d0

            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: F1 = 1.0d0
            double precision, parameter :: F2 = 2.0d0
            double precision, parameter :: F3 = 3.0d0
            double precision, parameter :: F4 = 4.0d0
            double precision, parameter :: Pi34 = F3 / (F4*Pi)
            double precision, parameter :: F13 = F1 / F3

            double precision, parameter :: dtol = 1.d-10

            INTEGER i

            if (ijzy.eq.1) then
                  !   Parameters for M05 Correlation
                  sopp0= 1.00000d0
                  sopp1= 3.78569d0
                  sopp2= -14.15261d0
                  sopp3= -7.46589d0
                  sopp4= 17.94491d0

                  sss0=  1.00000d0
                  sss1=  3.77344d0
                  sss2=  -26.04463d0
                  sss3=  30.69913d0
                  sss4=  -9.22695d0
            else
                  !   Parameters for M05-2X Correlation
                  sopp0= 1.00000d+0
                  sopp1= 1.09297d+0
                  sopp2= -3.79171d+0
                  sopp3= 2.82810d+0
                  sopp4= -10.58909d+0

                  sss0=  1.00000d0
                  sss1=  -3.05430d0
                  sss2=  7.61854d0
                  sss3=  1.47665d0
                  sss4=  -11.92365d0
            endif

            DO i = 1,NGrid
                  if (rho(i) .gt. dtol) then
                        pa = 0.5d+0 * rho(i)
                        GAA   = 0.25d+0 * sigma(i)
                        TauA =  0.5d+0 * tau(i)

                        call m05css(PA,GAA,TauA,FA,FPA,FGA,FTA,EUA, &
                              ChiA,EUPA,ChiAP,ChiAG,sss0,sss1,sss2,sss3,sss4)

                        f(i)      = f(i) + 2.d+0 * fa
                        vrho(i)   = vrho(i) + fpa
                        vsigma(i) = vsigma(i) + 0.5d+0 * fga
                        vtau(i)   = vtau(i) + fta

                        P = rho(i)
                        RS = (Pi34/P) ** F13 
                        RSP = -RS/(F3*P)
                        Zeta = 0.d+0
                        dZdA = (F1-Zeta)/P
                        !       
                        !      lsdac is a subroutine to evaluate the Perdew-Wang-91 correlation functional 
                        !      local spin density approximation (LSDA) to the correlation energy of a uniform 
                        !      electron gas. (Phys. Rev. B 45, 13244 (1992)). Users should provid their own
                        !      for this LSDA correlation functional or they may find this routine on Kieron 
                        !      Burke's Web site at http://www.chem.uci.edu/~kieron/dftold2/pubs/PBE.asc
                        !
                        Call lsdac(RS,Zeta,PotLC,dLdS,dLdZ)
                        EUEG = P*PotLC - 2.d+0 * EUA
                        U = COpp*(2.d+0 * ChiA)/(F1 + COpp*(2.d+0 * ChiA))
                        W = sopp0+U*(sopp1+U*(sopp2+U*(sopp3+U*sopp4)))
                        F(i) = F(i) + EUEG*W
                        dUdChiA =COpp/(F1 + COpp*(2.d+0 * ChiA))**2
                        dUdPA= dUdChiA*ChiAP
                        dUdGA= dUdChiA*ChiAG
                        dWdU =sopp1+U*(F2*sopp2+U*(F3*sopp3+U*F4*sopp4))
                        dWdPA= dWdU*dUdPA
                        dWdGA= dWdU*dUdGA
                        EUEGPA = PotLC + P*dLdS*RSP + P*dLdZ*dZdA - EUPA

                        vrho(i)   = vrho(i) + EUEGPA*W + EUEG*dWdPA 
                        vsigma(i) = vsigma(i) + 0.5d+0 * EUEG*dWdGA 
                  end if
            ENDDO
      end subroutine m05c
      

      Subroutine m05css(PX,GX,TX,F,FP,FG,FT,EUEG,Chi,EUEGP, &
            ChiP,ChiG,sss0,sss1,sss2,sss3,sss4)
            !
            !     Compute the same-spin part of the m05 correlation functional for one grid
            !     point and one spin-case.
            !
            double precision PX, GX, TX, F, FP, FG, FT
            double precision EUEG, Chi, EUEGP, ChiP, ChiG
            double precision sss0,sss1, sss2, sss3, sss4
            double precision RS, Fscc, RSP, dFsccP, dFsccG
            double precision E, W, U, dFsccT, dUdChi, dWdU, dWdP, dWdG 
            double precision PotLC,dLdS,dLdZ
            double precision xi, dxi
            !
            ! Damping factor to improve numerical
            ! robustness:
            ! Grafenstein et al., J.Chem.Phys., 127, 214103(2007)
            !
            double precision :: damp_fac
            double precision :: damp_exp
            double precision :: damp_tau
            double precision :: damp_arg
            double precision, parameter :: damp_param = 1.d-5
            double precision, parameter :: damp_deriv = 2.d+0 / (damp_param)**2

            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: Pt25 = 0.25d0
            double precision, parameter :: F1   = 1.0d0
            double precision, parameter :: F2   = 2.0d0
            double precision, parameter :: F3   = 3.0d0 
            double precision, parameter :: F4   = 4.0d0
            double precision, parameter :: F8   = 8.0d0
            double precision, parameter :: Css  = 0.06d0
            double precision, parameter :: Pi34 = F3 / (F4*Pi)
            double precision, parameter :: F13 = F1 / F3
            double precision, parameter :: F83 = F8 / F3
            
            RS = (Pi34/PX) ** F13
            Call lsdac(RS,F1,PotLC,dLdS,dLdZ)
            EUEG = PX*PotLC
            !        DUEG = FDUEG*PX**F53
            Chi = GX/PX**F83
            U = Css*Chi/(F1 + Css*Chi)
            W = sss0+U*(sss1+U*(sss2+U*(sss3+U*sss4)))
            !
            ! Damping factor (see Grafenstein et. al)
            !
            damp_arg = (tx / damp_param)**2
            damp_exp = exp(-damp_arg)
            damp_fac = f1 - damp_exp
            damp_tau = damp_deriv * tx * damp_exp

            !Fscc=D/TX
            xi = pt25 * gx / (tx * px)
            dxi = damp_fac * xi
            !fscc = damp_fac * d / tx
            fscc = damp_fac * (f1 - xi)

            E = Fscc*W*EUEG
            F = E
            RSP = -RS/(F3*Px)
            ChiG = F1/PX**F83
            ChiP = -F83*Chi/PX
            ! dFsccP=Pt25*GX/(TX*PX**2)
            ! dFsccG=-Pt25/(TX*PX)
            ! dFsccT=Pt25*GX/(PX*TX**2)
            dFsccP = dxi / px
            dFsccG = -dxi / gx
            dFsccT = dxi / tx + damp_tau * (f1 - xi)

            dUdChi=Css/((F1+Css*Chi)**2)
            dWdU=sss1+U*(F2*sss2+U*(F3*sss3+U*F4*sss4))
            dWdP=dWdU*dUdChi*ChiP
            dWdG=dWdU*dUdChi*ChiG 
            EUEGP = PotLC + PX*dLdS*RSP
            FP = (dFsccP*W*EUEG  &
                  + Fscc*dWdP*EUEG &
                  + Fscc*W*EUEGP)
            FG = (dFsccG*W*EUEG &
                  + Fscc*dWdG*EUEG)
            FT = (dFsccT*W*EUEG)
      End Subroutine m05css
end module m05corr
