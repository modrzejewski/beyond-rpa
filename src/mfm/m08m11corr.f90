module m08m11corr
      use lsdpw92

      implicit none

contains

      Subroutine um08m11c(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy)
            !***********************************************************************
            !                                                                      *
            !  M08M11c evaluates the correlation part of the M08 and M11 suite of  *
            !  functionals on the grid.                                            *
            !  !!! Second derivatives are not available yet.                       *
            !                                                                      *
            !  Ref: (a) Zhao, Y.  and Truhlar, D. G. JCTC, 2008, 4 , 1849          *
            !       (b) Peverati, R. and Truhlar, D. G. J.P.C.Lett. 2011, 2, 2810  *
            !       (c) Peverati, R. and Truhlar, D. G. J.P.C.Lett. 2011, submitted*
            !                                                                      *
            !       ijzy - 1 M08-HX (a)                                            *
            !       ijzy - 2 M08-SO (a)                                            *
            !       ijzy - 3 M11 (b)                                               *
            !       ijzy - 4 M11-L (c)                                             *
            !                                                                      *
            !  OUTPUT:                                                             *
            !     F      - Functional values                                       *
            !     D1F    - First derivatives with respect to RA, RB, GA, GB        *
            !              TA, TB                                                  *
            !                                                                      *
            !  INPUT:                                                              *
            !     RA,B   - Spin densities                                          *
            !     D1RA,B - Spin density gradients                                  *
            !     TA,B   - Spin kinetic energy densities                           *
            !     NGrid  - number of grids                                         *
            !                                                                      *
            !  RP (11/11), YZ (12/08)                                              *
            !                                                                      *
            !***********************************************************************
            integer, intent(in)                              :: ngrid 
            double precision, dimension(:, :), intent(in)    :: rho
            double precision, dimension(:, :), intent(in)    :: sigma
            double precision, dimension(:, :), intent(in)    :: tau
            double precision, dimension(:), intent(inout)    :: f
            double precision, dimension(:, :), intent(inout) :: vrho
            double precision, dimension(:, :), intent(inout) :: vsigma
            double precision, dimension(:, :), intent(inout) :: vtau
            integer, intent(in)                              :: ijzy

            integer :: i
            double precision :: LSDA, rhoa, rhob, rhotot
            double precision :: taua, taub, tautot, rs, zeta, tauueg
            double precision :: tsig, wsig, fsig1, fsig2, y, grho, potlc
            double precision :: dlds, dldz, h, dhdr, dhdg, dhdz, gga, e1
            double precision :: e2, rsp, dzda, dzdb, dldra, dldrb, df1dw
            double precision :: df2dw, dwdt, dtdr, dtdtau, df1dr, df1dtau
            double precision :: df2dr, df2dtau, dhdra, dhdrb
            double precision :: dgrhody, dhdy, dhdga, dhdgb
            double precision :: dhdgc, dggadra, dggadrb, dggadga, dggadgb
            double precision :: dggadgc, de1dra, de1drb, de1dka, de1dkb
            double precision :: de2dra, de2drb, de2dka, de2dkb, de2dga
            double precision :: de2dgb, de2dgc, dlddra, dlddrb

            double precision :: at0, at1, at2, at3, at4, at5
            double precision :: at6, at7, at8, at9, at10, at11
            double precision :: bt0, bt1, bt2, bt3, bt4, bt5
            double precision :: bt6, bt7, bt8, bt9, bt10, bt11

            double precision, parameter :: F1 = 1.0D+00
            double precision, parameter :: F2 = 2.0D+00
            double precision, parameter :: F3 = 3.0D+00
            double precision, parameter :: F4 = 4.0D+00
            double precision, parameter :: F5 = 5.0D+00
            double precision, parameter :: F6 = 6.0D+00
            double precision, parameter :: F7 = 7.0D+00
            double precision, parameter :: F8 = 8.0D+00
            double precision, parameter :: F9 = 9.0D+00
            double precision, parameter :: F10 = 10.0D+00
            double precision, parameter :: F11 = 11.0D+00
            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: DTol = 1.d-10
            !
            double precision, parameter :: F1o3 = F1/F3 
            double precision, parameter :: F2o3 = F2/F3
            double precision, parameter :: F5o3 = F5/F3
            double precision, parameter :: Pi34   = F3/(F4*Pi)


            select case (ijzy)
            case (1)
                  !     Parameters for M08-HX
                  at0=    1.0000000D+00
                  at1=   -4.0661387D-01
                  at2=   -3.3232530D+00
                  at3=    1.5540980D+00
                  at4=    4.4248033D+01
                  at5=   -8.4351930D+01
                  at6=   -1.1955581D+02
                  at7=    3.9147081D+02
                  at8=    1.8363851D+02
                  at9=   -6.3268223D+02
                  at10=  -1.1297403D+02
                  at11=   3.3629312D+02
                  bt0=    1.3812334D+00
                  bt1=   -2.4683806D+00
                  bt2=   -1.1901501D+01
                  bt3=   -5.4112667D+01
                  bt4=    1.0055846D+01
                  bt5=    1.4800687D+02
                  bt6=    1.1561420D+02
                  bt7=    2.5591815D+02
                  bt8=    2.1320772D+02
                  bt9=   -4.8412067D+02
                  bt10=  -4.3430813D+02
                  bt11=   5.6627964D+01
            case (2)
                  !     Parameters for M08-SO 
                  at0=    1.0000000D+00
                  at1=    0.0000000D+00
                  at2=   -3.9980886D+00
                  at3=    1.2982340D+01
                  at4=    1.0117507D+02
                  at5=   -8.9541984D+01
                  at6=   -3.5640242D+02
                  at7=    2.0698803D+02
                  at8=    4.6037780D+02
                  at9=   -2.4510559D+02
                  at10=  -1.9638425D+02
                  at11=   1.1881459D+02
                  bt0=    1.0000000D+00
                  bt1=   -4.4117403D+00
                  bt2=   -6.4128622D+00
                  bt3=    4.7583635D+01
                  bt4=    1.8630053D+02
                  bt5=   -1.2800784D+02
                  bt6=   -5.5385258D+02
                  bt7=    1.3873727D+02
                  bt8=    4.1646537D+02
                  bt9=   -2.6626577D+02
                  bt10=   5.6676300D+01
                  bt11=   3.1673746D+02
            case (3)
                  !     Parameters for M11
                  at0=   1.0000000D+00
                  at1=   0.0000000D+00
                  at2=  -3.8933250D+00
                  at3=  -2.1688455D+00
                  at4=   9.3497200D+00
                  at5=  -1.9845140D+01
                  at6=   2.3455253D+00
                  at7=   7.9246513D+01
                  at8=   9.6042757D+00
                  at9=  -6.7856719D+01
                  at10= -9.1841067D+00
                  at11=  0.0000000D+00
                  bt0=   7.2239798D-01
                  bt1=   4.3730564D-01
                  bt2=  -1.6088809D+01
                  bt3=  -6.5542437D+01
                  bt4=   3.2057230D+01
                  bt5=   1.8617888D+02
                  bt6=   2.0483468D+01
                  bt7=  -7.0853739D+01
                  bt8=   4.4483915D+01
                  bt9=  -9.4484747D+01
                  bt10= -1.1459868D+02
                  bt11=  0.0000000D+00
            case default
                  !
                  ! if (ijzy.eq.4) then
                  !     Parameters for M11-L
                  !
                  at0=   1.000000D+00
                  at1=   0.000000D+00
                  at2=   2.750880D+00
                  at3=  -1.562287D+01
                  at4=   9.363381D+00
                  at5=   2.141024D+01
                  at6=  -1.424975D+01
                  at7=  -1.134712D+01
                  at8=   1.022365D+01
                  at9=   0.000000D+00
                  at10=  0.000000D+00
                  at11=  0.000000D+00
                  !
                  bt0=   1.000000D+00
                  bt1=  -9.082060D+00
                  bt2=   6.134682D+00
                  bt3=  -1.333216D+01
                  bt4=  -1.464115D+01
                  bt5=   1.713143D+01
                  bt6=   2.480738D+00
                  bt7=  -1.007036D+01
                  bt8=  -1.117521D-01
                  bt9=   0.000000D+00
                  bt10=  0.000000D+00
                  bt11=  0.000000D+00
            end select

            DO i = 1,NGrid
                  ! RhoA = RA(i)
                  ! RhoB = RB(i)
                  rhoa = rho(1, i)
                  rhob = rho(2, i)
                  Rhotot = RhoA + RhoB
                  ! TauA = TA(i)/F2
                  ! TauB = TB(i)/F2
                  taua = tau(1, i) / F2
                  taub = tau(2, i) / F2
                  Tautot = TauA + TauB
                  If(Rhotot.gt.DTol.and.Tautot.gt.DTol) then
                        RS = (Pi34/Rhotot)**F1o3
                        Zeta = (RhoA-RhoB)/Rhotot
                        TauUEG=F3*(F3*Pi*Pi)**(F2o3)*Rhotot**(F5o3)/F10
                        Tsig =TauUEG/Tautot
                        Wsig =(Tsig - F1)/(Tsig + F1)
                        Fsig1=(at0 + Wsig*(at1 + Wsig*(at2 + Wsig*(at3 + Wsig*( &
                              at4 + Wsig*(at5 + Wsig*(at6 + Wsig*(at7 + Wsig*( &
                              at8 + Wsig*(at9 + Wsig*(at10+Wsig*at11)))))))))))
                        Fsig2=(bt0 + Wsig*(bt1 + Wsig*(bt2 + Wsig*(bt3 + Wsig*( &
                              bt4 + Wsig*(bt5 + Wsig*(bt6 + Wsig*(bt7 + Wsig*( &
                              bt8 + Wsig*(bt9 + Wsig*(bt10+Wsig*bt11)))))))))))
                        ! Y = (D1RA(i,1) + D1RB(i,1))**F2 &
                        !       + (D1RA(i,2) + D1RB(i,2))**F2 &
                        !       + (D1RA(i,3) + D1RB(i,3))**F2
                        y = sigma(1, i) + 2.d+0 * sigma(2, i) + sigma(3, i)
                        GRho = Sqrt(Y)
                        !       
                        !      lsdac is a subroutine to evaluate the Perdew-Wang-91 correlation functional 
                        !      local spin density approximation (LSDA) to the correlation energy of a uniform 
                        !      electron gas. (Phys. Rev. B 45, 13244 (1992)). Users should provid their own
                        !      for this LSDA correlation functional or they may find this routine on Kieron 
                        !      Burke's Web site at http://www.chem.uci.edu/~kieron/dftold2/pubs/PBE.asc
                        !
                        Call lsdac(RS,Zeta,PotLC,dLdS,dLdZ)
                        LSDA = Rhotot*PotLC
                        !
                        !      PBEH0 is a subroutine to evaluate the H0 term in the PBE correlation functional
                        !      (Phys. Rev. Lett. 77, 3865 - 3868 (1996)) Users should provid their own
                        !      for this H0 subroutine or they may find this routine on Kieron
                        !      Burke's Web site at http://www.chem.uci.edu/~kieron/dftold2/pubs/PBE.asc
                        !
                        !Call PBEH0(Rhotot,GRho,RS,Zeta,PotLC,dLdS,dLdZ,H,dHdR,dHdG,dHdZ)
                        CALL PBEH0(RHOTOT,GRHO,ZETA,H,DHDR,DHDG,DHDZ)

                        GGA = Rhotot*H 
                        E1 = LSDA*Fsig1
                        E2 = GGA*Fsig2
                        F(i) = F(i)+ E1 +E2
                        !
                        !     functional derivatives
                        !
                        RSP = -RS/(F3*Rhotot)
                        dZdA = (F1-Zeta)/Rhotot
                        dZdB = (-F1-Zeta)/Rhotot
                        dLdRA = dLdS*RSP + dLdZ*dZdA
                        dLdRB = dLdS*RSP + dLdZ*dZdB
                        dF1dW=( at1 + Wsig*(F2  *at2 + Wsig*(F3*at3 + Wsig*( &
                              F4 *at4 + Wsig*(F5 *at5 + Wsig*(F6  *at6 + Wsig*( &
                              F7*at7 + Wsig*(F8*at8 + Wsig*(F9 *at9 + Wsig*( &
                              F10  *at10+ Wsig*F11*at11))))))))))
                        dF2dW=( bt1 + Wsig*(F2  *bt2 + Wsig*(F3*bt3 + Wsig*( &
                              F4 *bt4 + Wsig*(F5 *bt5 + Wsig*(F6  *bt6 + Wsig*( &
                              F7*bt7 + Wsig*(F8*bt8 + Wsig*(F9 *bt9 + Wsig*( &
                              F10  *bt10+ Wsig*F11*bt11))))))))))
                        dWdT = F2/((F1 + Tsig)**F2)
                        dTdR = Tsig*F5/(F3*Rhotot) 
                        dTdTau = -Tsig/Tautot
                        dF1dR = dF1dW*dWdT*dTdR
                        dF1dTau=dF1dW*dWdT*dTdTau
                        dF2dR = dF2dW*dWdT*dTdR
                        dF2dTau=dF2dW*dWdT*dTdTau
                        dLDdRA = PotLC + Rhotot*dLdRA
                        dLDdRB = PotLC + Rhotot*dLdRB
                        dHdRA = dHdR + dHdZ*dZdA
                        dHdRB = dHdR + dHdZ*dZdB
                        dGRhodY = F1/(F2*GRho)
                        dHdY = dHdG * dGRhodY
                        dHdGA = dHdY
                        dHdGB = dHdY
                        dHdGC = dHdY*F2  
                        dGGAdRA = H + Rhotot*dHdRA
                        dGGAdRB = H + Rhotot*dHdRB
                        dGGAdGA = Rhotot*dHdGA
                        dGGAdGB = dGGAdGA
                        dGGAdGC = Rhotot*dHdGC
                        !
                        dE1dRA = dLDdRA*Fsig1 + LSDA*dF1dR
                        dE1dRB = dLDdRB*Fsig1 + LSDA*dF1dR
                        dE1dKA = LSDA*dF1dTau
                        dE1dKB = dE1dKA
                        !
                        dE2dRA = dGGAdRA*Fsig2 + GGA*dF2dR
                        dE2dRB = dGGAdRB*Fsig2 + GGA*dF2dR 
                        dE2dKA = GGA*dF2dTau
                        dE2dKB = dE2dKA
                        dE2dGA = dGGAdGA*Fsig2
                        dE2dGB = dGGAdGB*Fsig2
                        dE2dGC = dGGAdGC*Fsig2   

                        vrho(1, i)=vrho(1, i) + dE1dRA + dE2dRA 
                        vrho(2, i)=vrho(2, i) + dE1dRB + dE2dRB  
                        vtau(1, i)=vtau(1, i) + (dE1dKA + dE2dKA)/F2
                        vtau(2, i)=vtau(2, i) + (dE1dKB + dE2dKA)/F2
                        vsigma(1, i)=vsigma(1, i) + dE2dGA
                        vsigma(3, i)=vsigma(3, i) + dE2dGB
                        vsigma(2, i)=vsigma(2, i) + dE2dGC 
                  Endif
            Enddo
            Return
      End Subroutine Um08m11c


      Subroutine m08m11c(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy)
            !***********************************************************************
            !                                                                      *
            !  M08M11c evaluates the correlation part of the M08 and M11 suite of  *
            !  functionals on the grid.                                            *
            !  !!! Second derivatives are not available yet.                       *
            !                                                                      *
            !  Ref: (a) Zhao, Y.  and Truhlar, D. G. JCTC, 2008, 4 , 1849          *
            !       (b) Peverati, R. and Truhlar, D. G. J.P.C.Lett. 2011, 2, 2810  *
            !       (c) Peverati, R. and Truhlar, D. G. J.P.C.Lett. 2011, submitted*
            !                                                                      *
            !       ijzy - 1 M08-HX (a)                                            *
            !       ijzy - 2 M08-SO (a)                                            *
            !       ijzy - 3 M11 (b)                                               *
            !       ijzy - 4 M11-L (c)                                             *
            !                                                                      *
            !  OUTPUT:                                                             *
            !     F      - Functional values                                       *
            !     D1F    - First derivatives with respect to RA, RB, GA, GB        *
            !              TA, TB                                                  *
            !                                                                      *
            !  INPUT:                                                              *
            !     RA,B   - Spin densities                                          *
            !     D1RA,B - Spin density gradients                                  *
            !     TA,B   - Spin kinetic energy densities                           *
            !     NGrid  - number of grids                                         *
            !                                                                      *
            !  RP (11/11), YZ (12/08)                                              *
            !                                                                      *
            !***********************************************************************
            integer, intent(in)                           :: ngrid 
            double precision, dimension(:), intent(in)    :: rho
            double precision, dimension(:), intent(in)    :: sigma
            double precision, dimension(:), intent(in)    :: tau
            double precision, dimension(:), intent(inout) :: f
            double precision, dimension(:), intent(inout) :: vrho
            double precision, dimension(:), intent(inout) :: vsigma
            double precision, dimension(:), intent(inout) :: vtau
            integer, intent(in)                           :: ijzy

            integer :: i
            double precision :: LSDA, rhoa, rhob, rhotot
            double precision :: taua, taub, tautot, rs, zeta, tauueg
            double precision :: tsig, wsig, fsig1, fsig2, y, grho, potlc
            double precision :: dlds, dldz, h, dhdr, dhdg, dhdz, gga, e1
            double precision :: e2, rsp, dzda, dzdb, dldra, dldrb, df1dw
            double precision :: df2dw, dwdt, dtdr, dtdtau, df1dr, df1dtau
            double precision :: df2dr, df2dtau, dhdra, dhdrb
            double precision :: dgrhody, dhdy, dhdga, dhdgb
            double precision :: dhdgc, dggadra, dggadrb, dggadga, dggadgb
            double precision :: dggadgc, de1dra, de1drb, de1dka, de1dkb
            double precision :: de2dra, de2drb, de2dka, de2dkb, de2dga
            double precision :: de2dgb, de2dgc, dlddra, dlddrb

            double precision :: at0, at1, at2, at3, at4, at5
            double precision :: at6, at7, at8, at9, at10, at11
            double precision :: bt0, bt1, bt2, bt3, bt4, bt5
            double precision :: bt6, bt7, bt8, bt9, bt10, bt11

            double precision, parameter :: F0 = 0.0D+00
            double precision, parameter :: F1 = 1.0D+00
            double precision, parameter :: F2 = 2.0D+00
            double precision, parameter :: F3 = 3.0D+00
            double precision, parameter :: F4 = 4.0D+00
            double precision, parameter :: F5 = 5.0D+00
            double precision, parameter :: F6 = 6.0D+00
            double precision, parameter :: F7 = 7.0D+00
            double precision, parameter :: F8 = 8.0D+00
            double precision, parameter :: F9 = 9.0D+00
            double precision, parameter :: F10 = 10.0D+00
            double precision, parameter :: F11 = 11.0D+00
            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: DTol = 1.d-10
            !
            double precision, parameter :: F1o3 = F1/F3 
            double precision, parameter :: F2o3 = F2/F3
            double precision, parameter :: F5o3 = F5/F3
            double precision, parameter :: Pi34   = F3/(F4*Pi)


            select case (ijzy)
            case (1)
                  !     Parameters for M08-HX
                  at0=    1.0000000D+00
                  at1=   -4.0661387D-01
                  at2=   -3.3232530D+00
                  at3=    1.5540980D+00
                  at4=    4.4248033D+01
                  at5=   -8.4351930D+01
                  at6=   -1.1955581D+02
                  at7=    3.9147081D+02
                  at8=    1.8363851D+02
                  at9=   -6.3268223D+02
                  at10=  -1.1297403D+02
                  at11=   3.3629312D+02
                  bt0=    1.3812334D+00
                  bt1=   -2.4683806D+00
                  bt2=   -1.1901501D+01
                  bt3=   -5.4112667D+01
                  bt4=    1.0055846D+01
                  bt5=    1.4800687D+02
                  bt6=    1.1561420D+02
                  bt7=    2.5591815D+02
                  bt8=    2.1320772D+02
                  bt9=   -4.8412067D+02
                  bt10=  -4.3430813D+02
                  bt11=   5.6627964D+01
            case (2)
                  !     Parameters for M08-SO 
                  at0=    1.0000000D+00
                  at1=    0.0000000D+00
                  at2=   -3.9980886D+00
                  at3=    1.2982340D+01
                  at4=    1.0117507D+02
                  at5=   -8.9541984D+01
                  at6=   -3.5640242D+02
                  at7=    2.0698803D+02
                  at8=    4.6037780D+02
                  at9=   -2.4510559D+02
                  at10=  -1.9638425D+02
                  at11=   1.1881459D+02
                  bt0=    1.0000000D+00
                  bt1=   -4.4117403D+00
                  bt2=   -6.4128622D+00
                  bt3=    4.7583635D+01
                  bt4=    1.8630053D+02
                  bt5=   -1.2800784D+02
                  bt6=   -5.5385258D+02
                  bt7=    1.3873727D+02
                  bt8=    4.1646537D+02
                  bt9=   -2.6626577D+02
                  bt10=   5.6676300D+01
                  bt11=   3.1673746D+02
            case (3)
                  !     Parameters for M11
                  at0=   1.0000000D+00
                  at1=   0.0000000D+00
                  at2=  -3.8933250D+00
                  at3=  -2.1688455D+00
                  at4=   9.3497200D+00
                  at5=  -1.9845140D+01
                  at6=   2.3455253D+00
                  at7=   7.9246513D+01
                  at8=   9.6042757D+00
                  at9=  -6.7856719D+01
                  at10= -9.1841067D+00
                  at11=  0.0000000D+00
                  bt0=   7.2239798D-01
                  bt1=   4.3730564D-01
                  bt2=  -1.6088809D+01
                  bt3=  -6.5542437D+01
                  bt4=   3.2057230D+01
                  bt5=   1.8617888D+02
                  bt6=   2.0483468D+01
                  bt7=  -7.0853739D+01
                  bt8=   4.4483915D+01
                  bt9=  -9.4484747D+01
                  bt10= -1.1459868D+02
                  bt11=  0.0000000D+00
            case default
                  !
                  ! ijzy == 4
                  ! Parameters for M11-L
                  !
                  at0=   1.000000D+00
                  at1=   0.000000D+00
                  at2=   2.750880D+00
                  at3=  -1.562287D+01
                  at4=   9.363381D+00
                  at5=   2.141024D+01
                  at6=  -1.424975D+01
                  at7=  -1.134712D+01
                  at8=   1.022365D+01
                  at9=   0.000000D+00
                  at10=  0.000000D+00
                  at11=  0.000000D+00
                  !
                  bt0=   1.000000D+00
                  bt1=  -9.082060D+00
                  bt2=   6.134682D+00
                  bt3=  -1.333216D+01
                  bt4=  -1.464115D+01
                  bt5=   1.713143D+01
                  bt6=   2.480738D+00
                  bt7=  -1.007036D+01
                  bt8=  -1.117521D-01
                  bt9=   0.000000D+00
                  bt10=  0.000000D+00
                  bt11=  0.000000D+00
            end select 

            DO i = 1,NGrid
                  ! RhoA = RA(i)
                  ! RhoB = RB(i)
                  rhoa = 0.5d+0 * rho(i)
                  rhob = rhoa
                  Rhotot = RhoA + RhoB
                  ! TauA = TA(i)/F2
                  ! TauB = TB(i)/F2
                  taua = tau(i) / F4
                  taub = taua
                  Tautot = TauA + TauB
                  If(Rhotot.gt.DTol.and.Tautot.gt.DTol) then
                        RS = (Pi34/Rhotot)**F1o3
                        Zeta = F0
                        TauUEG=F3*(F3*Pi*Pi)**(F2o3)*Rhotot**(F5o3)/F10
                        Tsig =TauUEG/Tautot
                        Wsig =(Tsig - F1)/(Tsig + F1)
                        Fsig1=(at0 + Wsig*(at1 + Wsig*(at2 + Wsig*(at3 + Wsig*( &
                              at4 + Wsig*(at5 + Wsig*(at6 + Wsig*(at7 + Wsig*( &
                              at8 + Wsig*(at9 + Wsig*(at10+Wsig*at11)))))))))))
                        Fsig2=(bt0 + Wsig*(bt1 + Wsig*(bt2 + Wsig*(bt3 + Wsig*( &
                              bt4 + Wsig*(bt5 + Wsig*(bt6 + Wsig*(bt7 + Wsig*( &
                              bt8 + Wsig*(bt9 + Wsig*(bt10+Wsig*bt11)))))))))))
                        y = sigma(i)
                        GRho = Sqrt(Y)
                        !       
                        !      lsdac is a subroutine to evaluate the Perdew-Wang-91 correlation functional 
                        !      local spin density approximation (LSDA) to the correlation energy of a uniform 
                        !      electron gas. (Phys. Rev. B 45, 13244 (1992)). Users should provid their own
                        !      for this LSDA correlation functional or they may find this routine on Kieron 
                        !      Burke's Web site at http://www.chem.uci.edu/~kieron/dftold2/pubs/PBE.asc
                        !
                        Call lsdac(RS,Zeta,PotLC,dLdS,dLdZ)
                        LSDA = Rhotot*PotLC
                        !
                        !      PBEH0 is a subroutine to evaluate the H0 term in the PBE correlation functional
                        !      (Phys. Rev. Lett. 77, 3865 - 3868 (1996)) Users should provid their own
                        !      for this H0 subroutine or they may find this routine on Kieron
                        !      Burke's Web site at http://www.chem.uci.edu/~kieron/dftold2/pubs/PBE.asc
                        !
                        !Call PBEH0(Rhotot,GRho,RS,Zeta,PotLC,dLdS,dLdZ,H,dHdR,dHdG,dHdZ)
                        CALL PBEH0(RHOTOT,GRHO,ZETA,H,DHDR,DHDG,DHDZ)

                        GGA = Rhotot*H 
                        E1 = LSDA*Fsig1
                        E2 = GGA*Fsig2
                        F(i) = F(i)+ E1 +E2
                        !
                        !     functional derivatives
                        !
                        RSP = -RS/(F3*Rhotot)
                        dZdA = (F1-Zeta)/Rhotot
                        dZdB = (-F1-Zeta)/Rhotot
                        dLdRA = dLdS*RSP + dLdZ*dZdA
                        dLdRB = dLdS*RSP + dLdZ*dZdB
                        dF1dW=( at1 + Wsig*(F2  *at2 + Wsig*(F3*at3 + Wsig*( &
                              F4 *at4 + Wsig*(F5 *at5 + Wsig*(F6  *at6 + Wsig*( &
                              F7*at7 + Wsig*(F8*at8 + Wsig*(F9 *at9 + Wsig*( &
                              F10  *at10+ Wsig*F11*at11))))))))))
                        dF2dW=( bt1 + Wsig*(F2  *bt2 + Wsig*(F3*bt3 + Wsig*( &
                              F4 *bt4 + Wsig*(F5 *bt5 + Wsig*(F6  *bt6 + Wsig*( &
                              F7*bt7 + Wsig*(F8*bt8 + Wsig*(F9 *bt9 + Wsig*( &
                              F10  *bt10+ Wsig*F11*bt11))))))))))
                        dWdT = F2/((F1 + Tsig)**F2)
                        dTdR = Tsig*F5/(F3*Rhotot) 
                        dTdTau = -Tsig/Tautot
                        dF1dR = dF1dW*dWdT*dTdR
                        dF1dTau=dF1dW*dWdT*dTdTau
                        dF2dR = dF2dW*dWdT*dTdR
                        dF2dTau=dF2dW*dWdT*dTdTau
                        dLDdRA = PotLC + Rhotot*dLdRA
                        dLDdRB = PotLC + Rhotot*dLdRB
                        dHdRA = dHdR + dHdZ*dZdA
                        dHdRB = dHdR + dHdZ*dZdB
                        dGRhodY = F1/(F2*GRho)
                        dHdY = dHdG * dGRhodY
                        dHdGA = dHdY
                        dHdGB = dHdY
                        dHdGC = dHdY*F2  
                        dGGAdRA = H + Rhotot*dHdRA
                        dGGAdRB = H + Rhotot*dHdRB
                        dGGAdGA = Rhotot*dHdGA
                        dGGAdGB = dGGAdGA
                        dGGAdGC = Rhotot*dHdGC
                        !
                        dE1dRA = dLDdRA*Fsig1 + LSDA*dF1dR
                        dE1dRB = dLDdRB*Fsig1 + LSDA*dF1dR
                        dE1dKA = LSDA*dF1dTau
                        dE1dKB = dE1dKA
                        !
                        dE2dRA = dGGAdRA*Fsig2 + GGA*dF2dR
                        dE2dRB = dGGAdRB*Fsig2 + GGA*dF2dR 
                        dE2dKA = GGA*dF2dTau
                        dE2dKB = dE2dKA
                        dE2dGA = dGGAdGA*Fsig2
                        dE2dGB = dGGAdGB*Fsig2
                        dE2dGC = dGGAdGC*Fsig2   

                        vrho(i) = vrho(i) + dE1dRA + dE2dRA 
                        vtau(i) = vtau(i) + (dE1dKA + dE2dKA)/F2
                        vsigma(i)= vsigma(i) + 0.25d+0 * (dE2dGA + dE2dGB + dE2dGC)
                  Endif
            Enddo
            Return
      End Subroutine m08m11c

    SUBROUTINE PBEH0(P,GP,Z,ENERGY,DHDP,DHDG,DHDZ)
          double precision :: p
          double precision :: gp
          double precision :: z
          double precision :: energy
          double precision :: dhdp
          double precision :: dhdg
          double precision :: dhdz
!    IMPLICIT DOUBLE PRECISION (A-H,O-Z)

    double precision :: t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11
    double precision :: t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22
    double precision :: t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33
    double precision :: t34, t35, t36, t37, t38, t39, t40, t41, t42, t43, t44
    double precision :: t45, t46, t47, t48, t49, t50, t51, t52, t53, t54, t55
    double precision :: t56, t57, t58, t59, t60, t61, t62, t63, t64, t65, t66
    double precision :: t67, t68, t69, t70, t71, t72, t73, t74, t75, t76, t77
    double precision :: t78, t79, t80, t81, t82, t83, t84, t85, t86, t87, t88
    double precision, parameter :: PI  = 3.1415926535897932384626433832795D+0

!     FUNCTIONAL ENERGY

    T1 = .3068528194400547D+0
    T2 = 1.0D+0-1.0D+0*Z
    T3 = Z+1.0D+0
    T4 = T3**(2.0D+0/3.0D+0)+T2**( &
    2.0D+0/3.0D+0)
    T5 = T4**3.0D+0
    T6 = PI**2.0D+0
    T7 = 1.442249570307408D+0
    T8 = 1.0D+0/T7
    T9 = 1.0D+0/T1
    T10 = GP**2.0D+0
    T11 = 1.0D+0/P**(7.0D+0/3.0D+0)
    T12 = 1.0D+0/T4**2.0D+0
    T13 = PI**(7.0D+0/3.0D+0)
    T14 = 1.259921049894873D+0
    T15 = 1.0D+0/(2.0D+0*T14-2.0D+0)
    T16 = Z**4.0D+0
    T17 = T3**(4.0D+0/3.0D+0)+T2** &
    (4.0D+0/3.0D+0)-2.0D+0
    T18 = 1.732050807568877D+0
    T19 = P**(1.0D+0/3.0D+0)
    T20 = PI**(1.0D+0/3.0D+0)
    T21 = SQRT(T19*T20)
    T22 = 1.0D+0/T21**3.0D+0
    T23 = 1.0D+0/T14
    T24 = 1.200936955176003D+0
    T25 = 1.0D+0/T21
    T26 = 2.080083823051904D+0
    T27 = 0.39685026299205D+0
    T28 = 1.0D+0/P**(2.0D+0/3.0D+0)
    T29 = 1.0D+0/PI**(2.0D+0/3.0D+0)
    T30 = .6299605249474366D+0
    T31 = 1.0D+0/T19
    T32 = 1.0D+0/T20
    T33 = 0.0621814D+0*(0.2137D+0* &
    T7*T30*T31*T32+1.0D+0)*LOG(16.08197949869254D+0/ &
    (3.5876D+0*T7*T30*T31*T32+0.49294D+0*T26* &
    T27*T28*T29+7.5957D+0*T23*T24*T25+0.8191D+0* &
    T18*T22)+1.0D+0)
    T34 = 2.718281828459045D+0**(8.0D+0* &
    T9*(-T15*T16*T17*(T33-0.0310907D+0*LOG( &
    32.16395899738507D+0/(6.1977D+0*T7*T30* &
    T31*T32+0.62517D+0*T26*T27*T28*T29+14.1189D+0* &
    T23*T24*T25+1.6831D+0*T18*T22)+1)*(0.20548D+0* &
    T7*T30*T31*T32+1))+T33-.01975167351202899D+0* &
    T15*(1.0D+0-T16)*T17*LOG(29.60874997779344D+0/ &
    (3.6231D+0*T7*T30*T31*T32+0.49671D+0*T26* &
    T27*T28*T29+10.357D+0*T23*T24*T25+0.44013D+0* &
    T18*T22)+1)*(0.11125D+0*T7*T30*T31*T32+ &
    1))*T6/T5)-1.0D+0
    T35 = 0.0166811376507873D+0*T10* &
    T11*T12*T13*T8*T9/T34

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

    ENERGY = 0.125D+0*T1*T5*LOG(0.0166811376507873D+0* &
    T10*T11*T12*T13*(T35+1.0D+0)*T8*T9/(2.782603533245138D-4* &
    GP**4.0D+0*PI**(14.0D+0/3.0D+0)/(T1**2.0D+0* &
    T26*T34**2.0D+0*T4**4.0D+0*P**(14.0D+0/ &
    3.0D+0))+T35+1.0D+0)+1.0D+0)/T6

!     FIRST FUNCTIONAL DERIVATIVE

    T1 = .3068528194400547D+0
    T2 = 1.0D+0-1.0D+0*Z
    T3 = Z+1.0D+0
    T4 = T3**(2.0D+0/3.0D+0)+T2**( &
    2.0D+0/3.0D+0)
    T5 = T4**3.0D+0
    T6 = PI**2.0D+0
    T7 = 1.0D+0/T6
    T8 = 1.442249570307408D+0
    T9 = 1.0D+0/T8
    T10 = 1.0D+0/T1
    T11 = GP**2.0D+0
    T12 = 1.0D+0/P**(7.0D+0/3.0D+0)
    T13 = T4**2.0D+0
    T14 = 1.0D+0/T13
    T15 = PI**(7.0D+0/3.0D+0)
    T16 = 1.0D+0/T5
    T17 = 1.259921049894873D+0
    T18 = 1.0D+0/(2.0D+0*T17-2.0D+0)
    T19 = Z**4.0D+0
    T20 = 1.0D+0-1.0D+0*T19
    T21 = T3**(4.0D+0/3.0D+0)+T2** &
    (4.0D+0/3.0D+0)-2.0D+0
    T22 = 1.732050807568877D+0
    T23 = P**(1.0D+0/3.0D+0)
    T24 = PI**(1.0D+0/3.0D+0)
    T25 = SQRT(T23*T24)
    T26 = 1.0D+0/T25**3.0D+0
    T27 = 1.0D+0/T17
    T28 = 1.200936955176003D+0
    T29 = 1.0D+0/T25
    T30 = 2.080083823051904D+0
    T31 = 0.39685026299205D+0
    T32 = 1.0D+0/P**(2.0D+0/3.0D+0)
    T33 = 1.0D+0/PI**(2.0D+0/3.0D+0)
    T34 = .6299605249474366D+0
    T35 = 1.0D+0/T23
    T36 = 1.0D+0/T24
    T37 = 3.6231D+0*T8*T34*T35*T36+ &
    0.49671D+0*T30*T31*T32*T33+10.357D+0*T27* &
    T28*T29+0.44013D+0*T22*T26
    T38 = 29.60874997779344D+0/T37+ &
    1.0D+0
    T39 = LOG(T38)
    T40 = 0.11125D+0*T8*T34*T35*T36+ &
    1.0D+0
    T41 = 3.5876D+0*T8*T34*T35*T36+ &
    0.49294D+0*T30*T31*T32*T33+7.5957D+0*T27* &
    T28*T29+0.8191D+0*T22*T26
    T42 = 16.08197949869254D+0/T41+ &
    1.0D+0
    T43 = LOG(T42)
    T44 = 0.2137D+0*T8*T34*T35*T36+ &
    1.0D+0
    T45 = 0.0621814D+0*T43*T44
    T46 = 6.1977D+0*T8*T34*T35*T36+ &
    0.62517D+0*T30*T31*T32*T33+14.1189D+0* &
    T27*T28*T29+1.6831D+0*T22*T26
    T47 = 32.16395899738507D+0/T46+ &
    1.0D+0
    T48 = LOG(T47)
    T49 = 0.20548D+0*T8*T34*T35*T36+ &
    1.0D+0
    T50 = T45-0.0310907D+0*T48*T49
    T51 = - &
    1.0D+0*T18*T19*T21*T50+T45-.01975167351202899D+0* &
    T18*T20*T21*T39*T40
    T52 = 2.718281828459045D+0**(8.0D+0* &
    T10*T16*T51*T6)
    T53 = T52-1.0D+0
    T54 = 1.0D+0/T53
    T55 = 0.0166811376507873D+0*T9* &
    T10*T11*T12*T14*T15*T54
    T56 = T55+1.0D+0
    T57 = 1.0D+0/T30
    T58 = 1.0D+0/T1**2.0D+0
    T59 = GP**4.0D+0
    T60 = 1.0D+0/P**(14.0D+0/3.0D+0)
    T61 = 1.0D+0/T4**4.0D+0
    T62 = PI**(14.0D+0/3.0D+0)
    T63 = 1.0D+0/T53**2.0D+0
    T64 = 2.782603533245138D-4*T57* &
    T58*T59*T60*T61*T62*T63+T55+1.0D+0
    T65 = 1.0D+0/T64
    T66 = 0.0166811376507873D+0*T9* &
    T10*T11*T12*T14*T15*T56*T65+1.0D+0
    T67 = 1.0D+0/T66
    T68 = 1.0D+0/P**(10.0D+0/3.0D+0)
    T69 = -.03892265451850371D+0*T9*T10*T11*T68*T14* &
    T15*T54
    T70 = 1.0D+0/T4**5.0D+0
    T71 = 1.0D+0/P**(4.0D+0/3.0D+0)
    T72 = -.004429388393333333D+0*T8*T34*T71*T43* &
    T36
    T73 = 1.0D+0/P**(5.0D+0/3.0D+0)
    T74 = 1.0D+0/T25**5.0D+0
    T75 = -1.0D+0*(-1.26595D+0*T27* &
    T28*T32*T26*T24-0.40955D+0*T22*T32*T74* &
    T24-1.195866666666667D+0*T8*T34*T71*T36- &
    .3286266666666666D+0*T30*T31*T73*T33)* &
    T44/(T41**2.0D+0*T42)
    T76 = -1.0D+0*T18*T19*T21*(T75+ &
    T72+1.0D+0*(-2.35315D+0*T27*T28*T32*T26* &
    T24-0.84155D+0*T22*T32*T74*T24-2.0659D+0* &
    T8*T34*T71*T36-0.41678D+0*T30*T31*T73* &
    T33)*T49/(T46**2.0D+0*T47)+.002129505678666666D+0* &
    T8*T34*T71*T48*T36)+T75+T72+.5848223626606717D+0* &
    T18*T20*T21*(-1.726166666666666D+0*T27* &
    T28*T32*T26*T24-0.220065D+0*T22*T32*T74* &
    T24-1.2077D+0*T8*T34*T71*T36-0.33114D+0* &
    T30*T31*T73*T33)*T40/(T37**2.0D+0*T38) &
    +7.324578927377418D-4*T18*T8*T34*T71*T20* &
    T21*T39*T36
    T77 = -.1334491012062984D+0*T9* &
    T58*T11*T12*T70*T76*PI**(13.0D+0/3.0D+0)* &
    T63*T52
    T78 = 1.0D+0/T64**2.0D+0
    T79 = 1.0D+0/T53**3.0D+0
    T80 = GP**3.0D+0
    T81 = T2**(1.0D+0/3.0D+0)
    T82 = T3**(1.0D+0/3.0D+0)
    T83 = .6666666666666666D+0/T82- &
    .6666666666666666D+0/T81
    T84 = -.03336227530157461D+0*T9* &
    T10*T11*T12*T83*T16*T15*T54
    T85 = 1.333333333333333D+0*T82- &
    1.333333333333333D+0*T81
    T86 = Z**3.0D+0
    T87 = 8.0D+0*T10*T16*T6*(-4.0D+0* &
    T18*T21*T50*T86-1.0D+0*T18*T19*T50*T85- &
    .01975167351202899D+0*T18*T20*T85*T39* &
    T40+.07900669404811597D+0*T18*T86*T21* &
    T39*T40)-24.0D+0*T10*T51*T6*T61*T83
    T88 = -0.0166811376507873D+0*T9* &
    T10*T11*T12*T14*T15*T87*T63*T52

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

    DHDP = 0.125D+0*T1*T5*T67*T7*(- &
    0.0166811376507873D+0*T10*T11*T12*T14* &
    T15*T56*T78*T9*(-.004452165653192221D+0* &
    T52*T57*T59*T60*T76*T79*PI**(20.0D+0/3.0D+0)/ &
    (T1**3.0D+0*T4**7.0D+0)-.001298548315514398D+0* &
    T57*T58*T59*T61*T62*T63/P**(17.0D+0/3.0D+0) &
    +T77+T69)+0.0166811376507873D+0*T9*T10* &
    T11*T12*T14*T15*T65*(T77+T69)-.03892265451850371D+0* &
    T9*T10*T11*T68*T14*T15*T56*T65)
    DHDG = 0.125D+0*T1*T5*(5.565207066490276D-4* &
    T57*T58*T80*T60*T61*T62*T65*T54+.03336227530157461D+0* &
    T9*T10*GP*T12*T14*T15*T56*T65-0.0166811376507873D+0* &
    T9*T10*T11*T12*T14*T15*(.03336227530157461D+0* &
    T9*T10*GP*T12*T14*T15*T54+.001113041413298055D+0* &
    T57*T58*T80*T60*T61*T62*T63)*T56*T78)* &
    T67*T7
    DHDZ = 0.125D+0*T1*T5*T67*T7*(- &
    0.0166811376507873D+0*T9*T10*T11*T12*T14* &
    T15*T56*T78*(T88-5.565207066490276D-4* &
    T57*T58*T59*T60*T61*T62*T87*T79*T52+T84- &
    .001113041413298055D+0*T57*T58*T59*T60* &
    T83*T70*T62*T63)+0.0166811376507873D+0* &
    T9*T10*T11*T12*T14*T15*T65*(T88+T84)-.03336227530157461D+0* &
    T9*T10*T11*T12*T83*T16*T15*T56*T65)+0.375D+0* &
    T1*T13*LOG(T66)*T7*T83

!     END OF CODE GENERATION

    RETURN
END SUBROUTINE PBEH0
end module m08m11corr
