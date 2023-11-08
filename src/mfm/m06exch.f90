module m06exch
      use vs98exch

      implicit none

contains

      Subroutine um06x(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy)
            ! ----------------------------------------------------------------------
            !                                                                      
            !  M06x evaluates the exchange part of the M06 suite of
            !  functionals on the grid.                                            
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

            integer i
            double precision at1, at2, at3, at4, at5, at6, at7, at8, at9
            double precision at10, at11, at0
            double precision rrho, rho43, rho13, rhoo, rho53
            double precision Gamma2, Gamma
            double precision TauUEG, Tsig, Wsig, Fsig
            double precision tauu, tauN
            double precision x, x2, En, Ed, E, dE, dEn, dEd
            double precision dFdW, dWdT, dTdR, dTdTau, dGGAdR, dFdR
            double precision dFdTau, dGGAdG
            
            double precision, parameter :: pi = 3.1415926535897932384626433832795d0
            double precision, parameter :: F1o3=1.d0/3.d0
            double precision, parameter :: F2o3=2.d0/3.d0
            double precision, parameter :: F3o2=3.d0/2.d0
            double precision, parameter :: F4o3=4.d0/3.d0
            double precision, parameter :: F3o5=3.d0/5.d0
            double precision, parameter :: F53=5.d0/3.d0
            double precision, parameter :: One=1.0d0
            double precision, parameter :: Two=2.0d0
            double precision, parameter :: Three=3.0d0
            double precision, parameter :: Four=4.0d0
            double precision, parameter :: Five=5.0d0
            double precision, parameter :: Six=6.0d0
            double precision, parameter :: Seven=7.0d0
            double precision, parameter :: Eight=8.0d0
            double precision, parameter :: Nine=9.0d0
            double precision, parameter :: F10=10.0d0
            double precision, parameter :: F11=11.d0
            double precision, parameter :: Ax = -F3o2*(F4o3*PI)**(-F1o3)

            double precision, parameter :: C1 = 3.36116D-03
            double precision, parameter :: C2 = 4.49267D-03

            double precision, parameter :: DTol=1.0d-10

            if (ijzy.eq.1) then
                  !     Parameters for M06-L
                  at0=    3.987756D-01
                  at1=    2.548219D-01
                  at2=    3.923994D-01
                  at3=    -2.103655D+00
                  at4=    -6.302147D+00
                  at5=    1.097615D+01
                  at6=    3.097273D+01
                  at7=    -2.318489D+01
                  at8=    -5.673480D+01
                  at9=    2.160364D+01
                  at10=   3.421814D+01
                  at11=   -9.049762D+00
            elseif (ijzy.eq.2) then
                  !     Parameters for M06-HF
                  at0=    1.179732D-01
                  at1=    -1.066708D+00
                  at2=    -1.462405D-01
                  at3=    7.481848D+00
                  at4=    3.776679D+00
                  at5=    -4.436118D+01
                  at6=    -1.830962D+01
                  at7=    1.003903D+02
                  at8=    3.864360D+01
                  at9=    -9.806018D+01
                  at10=   -2.557716D+01
                  at11=   3.590404D+01
            elseif (ijzy.eq.3) then
                  !     Parameters for M06
                  at0=    5.877943D-01
                  at1=    -1.371776D-01
                  at2=    2.682367D-01
                  at3=    -2.515898D+00
                  at4=    -2.978892D+00
                  at5=    8.710679D+00
                  at6=    1.688195D+01
                  at7=    -4.489724D+00
                  at8=    -3.299983D+01
                  at9=    -1.449050D+01
                  at10=   2.043747D+01
                  at11=   1.256504D+01
            else
           ! elseif (ijzy.eq.4) then
                  !     Parameters for M06-2X
                  at0=    4.600000D-01
                  at1=    -2.206052D-01
                  at2=    -9.431788D-02
                  at3=    2.164494D+00
                  at4=    -2.556466D+00
                  at5=    -1.422133D+01
                  at6=    1.555044D+01
                  at7=    3.598078D+01
                  at8=    -2.722754D+01
                  at9=    -3.924093D+01
                  at10=   1.522808D+01
                  at11=   1.522227D+01
            endif
            
            if (ijzy.LT.4) then
                  call uvs98x(ngrid, rho, sigma, tau, f, &
                        vrho, vsigma, vtau, ijzy+1, dtol)

                  ! call VS98x(DTol,1.0d0,F,D1F,RA,RB,D1RA,D1RB, &
                  !       TA,TB,NGrid,ijzy+1)
            end if

            DO i = 1,NGrid
                  IF ((rho(1, i).gt.DTol).and.(tau(1, i).gt.DTol)) THEN
                        !  rhoo = RA(i)
                        rhoo = rho(1, i)
                        rho43 = rhoo**F4o3  
                        rrho = 1.0d0/rhoo 
                        rho13 = rho43*rrho
                        rho53 = rhoo**F53
                        !
                      !  tauN = TA(i)
                        tauN = tau(1, i)
                        tauu = tauN
                        TauUEG=F3o5*((Six*pi*pi)**F2o3)*rho53
                        Tsig =TauUEG/tauN
                        Wsig =(Tsig-One)/(Tsig+One)
                        Fsig=(at0 + Wsig*(at1 + Wsig*(at2 + Wsig*(at3 + Wsig*( &
                              at4 + Wsig*(at5 + Wsig*(at6 + Wsig*(at7 + Wsig*( &
                              at8 + Wsig*(at9 + Wsig*(at10+Wsig*at11)))))))))))
                        Gamma2 =  sigma(1, i) !D1RA(i,1)**2 + D1RA(i,2)**2 + D1RA(i,3)**2
                        Gamma = dsqrt(Gamma2)
                        x = Gamma/rho43
                        x2 = x*x
                        En = C1*x2
                        Ed = One + C2*x2
                        E  = -En/Ed
                        F(i)=F(i)+(Ax+E)*Fsig*rho43
                        !
                        !     functional derivatives
                        !
                        dEn   = Two*C1*x
                        dEd   = Two*C2*x
                        dE    = -(dEn*Ed-En*dEd)/(Ed*Ed)
                        dFdW=( at1 + Wsig*(Two  *at2 + Wsig*(Three*at3 + Wsig*( &
                              Four *at4 + Wsig*(Five *at5 + Wsig*(Six  *at6 + Wsig*( &
                              Seven*at7 + Wsig*(Eight*at8 + Wsig*(Nine *at9 + Wsig*( &
                              F10  *at10+ Wsig*F11*at11))))))))))
                        dWdT = Two/((One + Tsig)**2)
                        dTdR = ((Six*PI*PI)**F2o3)*(rhoo**F2o3)/tauu
                        dTdTau = -TauUEG/tauu**2
                        dGGAdR = F4o3*rho13*(Ax+(E-x*dE))
                        dFdR = dFdW*dWdT*dTdR
                        dFdTau=dFdW*dWdT*dTdTau
                        dGGAdG =(dE/(Two*Gamma))
                        !        dF/dRhoa
                        !            D1F(i,GAB) = 0.0d0
                        !        dF/dRhoa
!                        D1F(i,dRA) = D1F(i,dRA) + dGGAdR*Fsig + (Ax+E)*rho43*dFdR
                        vrho(1, i) = vrho(1, i) + dGGAdR*Fsig + (Ax+E)*rho43*dFdR
                        !        dF/dGammaaa
!                        D1F(i,dGA) = D1F(i,dGA) + dGGAdG*Fsig 
                        vsigma(1, i) = vsigma(1, i) + dGGAdG*Fsig 
                        !        dF/dTaua
                        ! D1F(i,dTA) = D1F(i,dTA) + rho43*(Ax+E)*dFdTau
                        vtau(1, i) = vtau(1, i) + rho43*(Ax+E)*dFdTau
                  ENDIF
                  !
                  ! beta component
                  !
                  IF ((rho(2, i).gt.DTol).and.(tau(2, i).gt.DTol)) THEN
                        rhoo = rho(2, i)
                        rho43 = rhoo**F4o3
                        rrho = 1.0d0/rhoo       ! reciprocal of rho
                        rho13 = rho43*rrho
                        rho53 = rhoo**F53
                        !
                        tauN = tau(2, i)
                        tauu = tauN
                        TauUEG=F3o5*((Six*pi*pi)**F2o3)*rho53
                        Tsig =TauUEG/tauN
                        Wsig =(Tsig-One)/(Tsig+One)
                        Fsig=(at0 + Wsig*(at1 + Wsig*(at2 + Wsig*(at3 + Wsig*( &
                              at4 + Wsig*(at5 + Wsig*(at6 + Wsig*(at7 + Wsig*( &
                              at8 + Wsig*(at9 + Wsig*(at10+Wsig*at11)))))))))))
                        Gamma2 = sigma(3, i) !D1RB(i,1)**2 + D1RB(i,2)**2 + D1RB(i,3)**2
                        Gamma = dsqrt(Gamma2)
                        x = Gamma/rho43
                        x2 = x*x
                        En = C1*x2
                        Ed = One + C2*x2
                        E  = -En/Ed
                        F(i)=F(i)+(Ax+E)*Fsig*rho43
                        !
                        !     functional derivatives
                        !
                        dEn   = Two*C1*x
                        dEd   = Two*C2*x
                        dE    = -(dEn*Ed-En*dEd)/(Ed*Ed)
                        dFdW=(      at1 + Wsig*(Two  *at2 + Wsig*(Three*at3 + Wsig*( &
                              Four *at4 + Wsig*(Five *at5 + Wsig*(Six  *at6 + Wsig*( &
                              Seven*at7 + Wsig*(Eight*at8 + Wsig*(Nine *at9 + Wsig*( &
                              F10  *at10+ Wsig*F11*at11))))))))))
                        dWdT = Two/((One + Tsig)**2)
                        dTdR = ((Six*PI*PI)**F2o3)*(rhoo**F2o3)/tauu
                        dTdTau = -TauUEG/tauu**2
                        dGGAdR = F4o3*rho13*(Ax+(E-x*dE))
                        dFdR = dFdW*dWdT*dTdR
                        dFdTau=dFdW*dWdT*dTdTau
                        dGGAdG =(dE/(Two*Gamma))
                        !
                        !        dF/dRhob
                        ! D1F(i,dRB) = D1F(i,dRB) + dGGAdR*Fsig+ (Ax+E)*rho43*dFdR
                        vrho(2, i) = rho(2, i) + dGGAdR*Fsig+ (Ax+E)*rho43*dFdR
                        !        dF/dGammaaa
                        ! D1F(i,dGB) = D1F(i,dGB) + dGGAdG*Fsig
                        vsigma(3, i) = vsigma(3, i) + dGGAdG*Fsig
                        !        dF/dTaua
                        ! D1F(i,dTB) = D1F(i,dTB) + rho43*(Ax+E)*dFdTau
                        vtau(2, i) = vtau(2, i) + rho43*(Ax+E)*dFdTau
                  ENDIF
            enddo
      End Subroutine Um06x
end module m06exch
