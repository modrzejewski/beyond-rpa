module vs98corr
      use vs98exch
      use lsdpw92

      implicit none

contains

      Subroutine uvs98c(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy, dtol)
            !
            !       VS98c evaluates the correlation part of VS98 and its          
            !       variants on a grid.                                           
            !       !!! Second derivatives are not available yet.                 
            !                                                                     
            !       Ref:  T. V. Voorhis and G. E. Scuseria, J. Chem. Phys. 109,   
            !             400 (1998).                                             
            !            Y. Zhao and D. G. Truhlar, J. Chem. Phys. 125,           
            !             194101 (2006).                                          
            !                                                                     
            !           ijzy - 1 correlation functional in VS98                  
            !           ijzy - 2 correlation functional in M06-L                 
            !           ijzy - 3 correlation functional in M06-HF                
            !           ijzy - 4 correlation functional in M06                   
            !           ijzy - 5 correlation functional in M06-2X               
            !     
            !  YZ (12/08)  
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
            double precision, intent(in)                     :: dtol

            double precision :: RS,RSP,Zeta,dZdA,dZdB,PotLC,dLdS,dLdZ,&
                  P, EUEG, EUEGPA, EUEGPB
            double precision :: PA,GAA,TauA,FA,FPA,FGA,FTA,EUA,ChiA,EUPA,ChiAP, &
                  ChiAG,ZA,ZAP,ZAT
            double precision :: PB,GBB,TauB,FB,FPB,FGB,FTB,EUB,ChiB,EUPB,ChiBP, &
                  ChiBG,ZB,ZBP,ZBT
            double precision :: ZAB, XAB, kab, xk, zk
            double precision :: dgdx,dgdz,dgdPA,dgdGA,dgdTA,dgdPB,dgdGB,dgdTB
            double precision :: gcab
            double precision :: r7, r8, r9, r10, r11, r12
            double precision :: r13, r14, r15, r16, r17, r18

            double precision, parameter :: F1 = 1.0d0
            double precision, parameter :: F3 = 3.0d0
            double precision, parameter :: F4 = 4.0d0
            double precision, parameter :: F13 = F1 / F3
            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: Pi34 = F3 / (F4*Pi)

            double precision, parameter :: gab = 0.00304966d0
            integer :: i
            !
            ! Parameters for VS98
            !
            if (ijzy == 1) then
                  r7=   7.035010d-01
                  r8=   7.694574d-03
                  r9=   5.152765d-02
                  r10=   3.394308d-05
                  r11=  -1.269420d-03
                  r12=   1.296118d-03

                  r13=   3.270912d-01
                  r14=  -3.228915d-02
                  r15=  -2.942406d-02
                  r16=   2.134222d-03
                  r17=  -5.451559d-03
                  r18=   1.577575d-02

                  !     Parameters for M06-L
            elseif (ijzy == 2) then
                  r7=      3.957626D-01
                  r8=      -5.614546D-01
                  r9=      1.403963D-02
                  r10=     9.831442D-04
                  r11=     -3.577176D-03
                  r12=     0.000000D+00

                  r13=   4.650534D-01
                  r14=   1.617589D-01
                  r15=   1.833657D-01
                  r16=   4.692100D-04
                  r17=  -4.990573D-03
                  r18=   0.000000D+00
                  !     Parameters for M06-HF
            elseif (ijzy == 3) then
                  r7=    -6.746338D-01
                  r8=    -1.534002D-01
                  r9=    -9.021521D-02
                  r10=   -1.292037D-03
                  r11=   -2.352983D-04
                  r12=   0.000000D+00

                  r13=   8.976746D-01
                  r14=  -2.345830D-01
                  r15=   2.368173D-01
                  r16=  -9.913890D-04
                  r17=  -1.146165D-02
                  r18=   0.000000D+00
                  !     Parameters for M06
            elseif (ijzy == 4) then
                  r7= -2.741539D+00
                  r8= -6.720113D-01
                  r9= -7.932688D-02
                  r10=1.918681D-03
                  r11=-2.032902D-03
                  r12=0.000000D+00

                  r13=  4.905945D-01
                  r14= -1.437348D-01
                  r15=  2.357824D-01
                  r16=  1.871015D-03
                  r17= -3.788963D-03
                  r18=  0.000000D+00
            else
                  !
                  !  Parameters for M06-2X
                  ! elseif (ijzy == 5) then
                  !
                  r7=  1.166404D-01
                  r8=  -9.120847D-02
                  r9=  -6.726189D-02
                  r10= 6.720580D-05
                  r11= 8.448011D-04
                  r12= 0.000000D+00

                  r13=  6.902145D-01
                  r14=  9.847204D-02
                  r15=  2.214797D-01
                  r16= -1.968264D-03
                  r17= -6.775479D-03
                  r18=  0.000000D+00
            endif


            DO i = 1,NGrid
                  PA = rho(1, i)
                  IF (PA > DTol) THEN
                        GAA  =  sigma(1, i)
                        TauA = tau(1, i)

                        Call vs98ss(PA,GAA,TauA,FA,FPA,FGA,FTA,EUA,ZA, &
                              ChiA,EUPA,ChiAP,ChiAG,ZAP,ZAT,r13,r14,r15,r16,r17,r18)
                        F(i) = F(i) + FA
                        vrho(1, i)  = vrho(1, i) + FPA
                        vsigma(1, i) = vsigma(1, i) + FGA
                        vtau(1, i)  = vtau(1, i) + FTA
                  ENDIF
                  PB = rho(2, i)
                  IF (PB > DTol) THEN
                        GBB   =  sigma(3, i)
                        TauB = tau(2, i)

                        Call vs98ss(PB,GBB,TauB,FB,FPB,FGB,FTB,EUB,ZB, &
                              ChiB,EUPB,ChiBP,ChiBG,ZBP,ZBT,r13,r14,r15,r16,r17,r18)
                        F(i) = F(i) + FB
                        vrho(2, i) = vrho(2, i) + FPB
                        vsigma(3, i) = vsigma(3, i) + FGB
                        vtau(2, i)  = vtau(2, i) + FTB
                  ENDIF
                  P=PA+PB
                  IF (PB > DTol .AND. PA > DTol) THEN
                        RS = (Pi34/P) ** F13
                        RSP = -RS/(F3*P)
                        Zeta = (PA-PB)/P
                        dZdA = (F1-Zeta)/P
                        dZdB = (-F1-Zeta)/P
                        Call lsdac(RS,Zeta,PotLC,dLdS,dLdZ)
                        EUEG = P*PotLC - EUA - EUB
                        ZAB = ZA + ZB
                        XAB = ChiA+ChiB
                        kab = F1 + gab*(XAB+ZAB)
                        xk = XAB/kab
                        zk = ZAB/kab
                        call gvt4(gcab,dgdx,dgdz,xk,zk,kab,gab,r7,r8,r9,r10,r11,r12)
                        F(i) = F(i) + gcab*EUEG
                        dgdPA = dgdx*ChiAP + dgdz*ZAP
                        dgdGA = dgdx*ChiAG
                        dgdTA = dgdz*ZAT
                        dgdPB = dgdx*ChiBP + dgdz*ZBP
                        dgdGB = dgdx*ChiBG
                        dgdTB = dgdz*ZBT
                        EUEGPA = PotLC + P*dLdS*RSP + P*dLdZ*dZdA - EUPA
                        EUEGPB = PotLC + P*dLdS*RSP + P*dLdZ*dZdB - EUPB

                        vrho(1, i) = vrho(1, i) + EUEGPA*gcab + EUEG*dgdPA
                        vrho(2, i) = vrho(2, i) + EUEGPB*gcab + EUEG*dgdPB

                        vsigma(1, i) = vsigma(1, i) + EUEG*dgdGA
                        vsigma(3, i) = vsigma(3, i) + EUEG*dgdGB

                        vtau(1, i) = vtau(1, i) + EUEG*dgdTA
                        vtau(2, i) = vtau(2, i) + EUEG*dgdTB
                  ENDIF
            ENDDO
      END SUBROUTINE Uvs98c


      Subroutine vs98ss(PX,GX,TX,F,FP,FG,FT,EUEG,Z,Chi,EUEGP, &
            ChiP,ChiG,ZP,ZT,r13,r14,r15,r16,r17,r18)
            !
            !     Compute the same-spin part of the vs98 correlation
            !     functional for one grid point and one spin-case
            !
            double precision :: r13, r14, r15, r16, r17, r18
            double precision :: PX, GX, TX, F, FP, FG, FT, Z, ZP, ZT
            double precision :: EUEG, Chi, EUEGP, ChiP, ChiG
            double precision :: RS, D, RSP, PotLC, DX, DZ, dgdP, dgdG, dgdT
            double precision :: E,DP, DG, DT, rhoo, rho43, rho53, rho83
            double precision :: rho13, kc, xk, zk, gc, dgdx, dgdz
            double precision :: dLdS, dLdZ

            double precision, parameter :: F1 = 1.0d0
            double precision, parameter :: F3 = 3.0d0
            double precision, parameter :: F4 = 4.0d0
            double precision, parameter :: F5 = 5.0d+0
            double precision, parameter :: f8 = 8.d+0
            double precision, parameter :: f4o3 = F4 / F3
            double precision, parameter :: F13 = F1 / F3
            double precision, parameter :: F53 = F5 / F3
            double precision, parameter :: F83 = F8 / F3
            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: Pi34 = F3 / (F4*Pi)

            double precision, parameter :: gcc = 0.00515088d0
            double precision, parameter :: cf = 9.115599720d0

            rhoo = PX
            rho43 = rhoo**F4o3
            rho13 = rho43 / rhoo
            rho53 = rhoo**F53
            rho83 = rho53*rhoo

            RS = (Pi34/PX) ** F13
            Call lsdac(RS,F1,PotLC,dLdS,dLdZ)
            EUEG = PX*PotLC
            Chi = GX/rho83
            Z = (TX/rho53) - cf
            kc = F1 + gcc*(Chi + Z)
            xk = Chi/kc
            zk = Z/kc
            D = F1 - Chi/(F4*(Z + cf))
            call gvt4(gc,dgdx,dgdz,xk,zk,kc,gcc,r13,r14,r15,r16,r17,r18)
            E = D*EUEG*gc
            F = E

            RSP = -RS/(F3*Px)
            ChiG = F1/PX**F83
            ChiP = -F83*Chi/PX
            ZP = -F53 * TX/rho83
            ZT =  F1/rho53
            DZ = Chi/(F4*(Z + cf)*(Z + cf))
            DX = -F1/(F4*(Z + cf))
            DP = DZ*ZP + DX*ChiP
            DG = DX*ChiG
            DT = DZ*ZT
            dgdP = dgdx*ChiP + dgdz*ZP
            dgdG = dgdx*ChiG
            dgdT = dgdz*ZT
            EUEGP = PotLC + PX*dLdS*RSP
            FP = DP*EUEG*gc + D*EUEGP*gc + D*EUEG*dgdP
            FG = DG*EUEG*gc + D*EUEG*dgdG
            FT = DT*EUEG*gc + D*EUEG*dgdT
      end Subroutine vs98ss
end module vs98corr
