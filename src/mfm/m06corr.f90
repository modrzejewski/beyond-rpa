module m06corr
      use lsdpw92
      use vs98corr

      implicit none

contains

      Subroutine um06c(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy)
            ! ---------------------------------------------------------------------
            !  CM06 evaluates the M05 correlation part of the M06 suite of         
            !  functionals on the grids.                                           
            !  !!! Second derivatives are not available yet.                       
            !                                                                      
            !  Ref: (a) Zhao, Y.  and Truhlar, D. G. J. Chem. Phys. 125,           
            !    194101 (2006).                                                    
            !       (b) Y. Zhao and D. G. Truhlar, J. Phys. Chem. A (2006),        
            !    110(49),13126-13130.                                              
            !                                                                      
            !       ijzy - 1 M06-L                                                 
            !       ijzy - 2 M06-HF                                                
            !       ijzy - 3 M06                                                   
            !       ijzy - 4 M06-2X                                                
            !                                                                      
            !  YZ (12/08)                                                          
            ! ---------------------------------------------------------------------
            integer, intent(in)                              :: ngrid
            double precision, dimension(:, :), intent(in)    :: rho
            double precision, dimension(:, :), intent(in)    :: sigma
            double precision, dimension(:, :), intent(in)    :: tau
            double precision, dimension(:), intent(inout)    :: f
            double precision, dimension(:, :), intent(inout) :: vrho
            double precision, dimension(:, :), intent(inout) :: vsigma
            double precision, dimension(:, :), intent(inout) :: vtau
            integer, intent(in)                              :: ijzy

            INTEGER i
            double precision  RS,RSP,Zeta,dZdA,dZdB,PotLC,dLdS,dLdZ,&
                  P, EUEG
            double precision PA,GAA,TauA,FA,FPA,FGA,FTA,EUA,ChiA,EUPA,ChiAP,ChiAG
            double precision PB,GBB,TauB,FB,FPB,FGB,FTB,EUB,ChiB,EUPB,ChiBP,ChiBG
            double precision :: sopp0, sopp1,sopp2, sopp3, sopp4
            double precision :: sss0, sss1, sss2, sss3, sss4
            double precision U, W, dUdChiA,dUdChiB,dUdPA,dUdPB,dUdGA,dUdGB, &
                  dWdU,dWdPA,dWdPB, dWdGA,dWdGB,EUEGPA,EUEGPB

            double precision, parameter :: dtol = 1.d-10
            double precision, parameter :: COpp = 0.0031d0

            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: F1 = 1.0d0
            double precision, parameter :: F2 = 2.0d0
            double precision, parameter :: F3 = 3.0d0
            double precision, parameter :: F4 = 4.0d0
            double precision, parameter :: Pi34 = F3 / (F4*Pi)
            double precision, parameter :: F13 = F1 / F3

            if (ijzy.eq.1) then
                  !     Parameters for M06-L Correlation
                  sopp0= 6.042374D-01
                  sopp1= 1.776783D+02
                  sopp2= -2.513252D+02
                  sopp3= 7.635173D+01
                  sopp4= -1.255699D+01

                  sss0=  5.349466D-01
                  sss1=  5.396620D-01
                  sss2=  -3.161217D+01
                  sss3=  5.149592D+01
                  sss4=  -2.919613D+01
            elseif (ijzy.eq.2) then
                  !     Parameters for M06-HF Correlation
                  sopp0= 1.674634D+00
                  sopp1= 5.732017D+01
                  sopp2= 5.955416D+01
                  sopp3= -2.311007D+02
                  sopp4= 1.255199D+02

                  sss0=  1.023254D-01
                  sss1=  -2.453783D+00
                  sss2=  2.913180D+01
                  sss3=  -3.494358D+01
                  sss4=  2.315955D+01
            elseif (ijzy.eq.3) then
                  !     Parameters for M06 Correlation
                  sopp0= 3.741539D+00
                  sopp1= 2.187098D+02
                  sopp2= -4.531252D+02
                  sopp3= 2.936479D+02
                  sopp4= -6.287470D+01

                  sss0=  5.094055D-01
                  sss1=  -1.491085D+00
                  sss2=  1.723922D+01
                  sss3=  -3.859018D+01
                  sss4=  2.845044D+01
            else
                  !
                  ! if (ijzy.eq.4) then
                  !     Parameters for M06-2X Correlation
                  sopp0= 8.833596D-01
                  sopp1= 3.357972D+01
                  sopp2= -7.043548D+01
                  sopp3= 4.978271D+01
                  sopp4= -1.852891D+01

                  sss0=  3.097855D-01
                  sss1=  -5.528642D+00
                  sss2=  1.347420D+01
                  sss3=  -3.213623D+01
                  sss4=  2.846742D+01
            endif


            call uvs98c(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy+1, dtol)

            ! CALL VS98c(Tol,F,D1F,RA,RB,D1RA,D1RB,TA,TB,NGrid,ijzy+1)

            DO i = 1,NGrid
                  PA = rho(1, i)
                  IF (PA.gt.DTol) THEN
                        GAA   =  sigma(1, i) !D1RA(i,1)**2 + D1RA(i,2)**2 + D1RA(i,3)**2
                        TauA = tau(1, i) !TA(i)
                        call m06css(PA,GAA,TauA,FA,FPA,FGA,FTA,EUA, &
                              ChiA,EUPA,ChiAP,ChiAG, sss0, sss1, sss2, sss3, sss4)
                        F(i) = F(i) + FA
                        ! D1F(i,dRA)  = D1F(i,dRA) + FPA
                        vrho(1, i) = vrho(1, i) + fpa
                        ! D1F(i,dGA) = D1F(i,dGA) + FGA
                        vsigma(1, i) = vsigma(1, i) + fga
                        ! D1F(i,dTA)  = D1F(i,dTA) + FTA
                        vtau(1, i) = vtau(1, i) + fta
                  ENDIF
                  PB = rho(2, i)
                  IF (PB.gt.DTol) THEN
                        GBB   =  sigma(3, i) !D1RB(i,1)**2 + D1RB(i,2)**2 + D1RB(i,3)**2
                        TauB = tau(2, i) !TB(i)
                        call m06css(PB,GBB,TauB,FB,FPB,FGB,FTB,EUB, &
                              ChiB,EUPB,ChiBP,ChiBG, sss0, sss1, sss2, sss3, sss4)
                        F(i) = F(i) + FB
                        !D1F(i,dRB)  = D1F(i,dRB) + FPB
                        vrho(2, i) = vrho(2, i) + fpb
                        ! D1F(i,dGB) = D1F(i,dGB) + FGB
                        vsigma(3, i) = vsigma(3, i) + fgb
                        ! D1F(i,dTB)  = D1F(i,dTB) + FTB
                        vtau(2, i) = vtau(2, i) + ftb
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

                        !D1F(i,dRA) = D1F(i,dRA)+EUEGPA*W + EUEG*dWdPA
                        vrho(1, i) = vrho(1, i) + EUEGPA*W + EUEG*dWdPA

                        ! D1F(i,dGA) = D1F(i,dGA)+EUEG*dWdGA 
                        vsigma(1, i) = vsigma(1, i) + EUEG*dWdGA 

                        ! D1F(i,dRB) = D1F(i,dRB)+EUEGPB*W + EUEG*dWdPB  
                        vrho(2, i) = vrho(2, i) + EUEGPB*W + EUEG*dWdPB  

                        ! D1F(i,dGB) = D1F(i,dGB)+EUEG*dWdGB
                        vsigma(3, i) = vsigma(3, i) + EUEG*dWdGB
                  ENDIF
            ENDDO
      END Subroutine Um06c


      Subroutine m06css(PX,GX,TX,F,FP,FG,FT,EUEG,Chi,EUEGP, &
            ChiP,ChiG,sss0, sss1, sss2, sss3, sss4)
            !
            !     Compute the same-spin part of the m06 correlation functional for one grid
            !     point and one spin-case.
            !
            !
            double precision, intent(in) :: sss0, sss1, sss2, sss3, sss4
            double precision PX, GX, TX, F, FP, FG, FT
            double precision EUEG, Chi, EUEGP, ChiP, ChiG
            double precision RS, D, Fscc, RSP, dFsccP, dFsccG
            double precision E, W, U, dFsccT, dUdChi, dWdU, dWdP, dWdG 
            double precision PotLC,dLdS,dLdZ 

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
            D = TX - Pt25*GX/PX
            Chi = GX/PX**F83
            U = Css*Chi/(F1 + Css*Chi)
            W = sss0+U*(sss1+U*(sss2+U*(sss3+U*sss4)))
            Fscc=D/TX
            E = Fscc*W*EUEG
            F = E
            RSP = -RS/(F3*Px)
            ChiG = F1/PX**F83
            ChiP = -F83*Chi/PX
            dFsccP=Pt25*GX/(TX*PX**2)
            dFsccG=-Pt25/(TX*PX)
            dFsccT=Pt25*GX/(PX*TX**2)
            dUdChi=Css/((F1+Css*Chi)**2)
            dWdU=sss1+U*(F2*sss2+U*(F3*sss3+U*F4*sss4))
            dWdP=dWdU*dUdChi*ChiP
            dWdG=dWdU*dUdChi*ChiG 
            EUEGP = PotLC + PX*dLdS*RSP
            FP = dFsccP*W*EUEG  &
                  + Fscc*dWdP*EUEG &
                  + Fscc*W*EUEGP
            FG = dFsccG*W*EUEG &
                  + Fscc*dWdG*EUEG
            FT = dFsccT*W*EUEG
      End Subroutine m06css
end module m06corr
