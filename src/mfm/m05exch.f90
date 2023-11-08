module m05exch
      implicit none

contains
      
      Subroutine um05x(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy)
            ! ----------------------------------------------------------------------
            !  M06x evaluates the exchange part of the M06 suite of                
            !  functionals on the grid.                                            
            !  !!! Second derivatives are not available yet.                       
            !                                                                      
            !     *  Ref: (a) Zhao, Y., Schultz, N. E., and Truhlar, D. G. J.      
            !     *   Chem. Phys. 123, 161103 (2005).                              
            !     *       (b) Y. Zhao, Schultz, N. E., and D. G. Truhlar, J. Chem. 
            !     *   Theory Comput. 2, 364 (2006)                                  
            !     *      
            !    ijzy - 1 M05                                             
            !    ijzy - 2 M05-2X                                          
            !
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

            integer i
            DOUBLE PRECISION fac, tauN
            DOUBLE PRECISION at1, at2, at3, at4, at5, at6, at7, at8, at9
            DOUBLE PRECISION at10, at11, at0
            DOUBLE PRECISION rrho, rho43, rho13, rhoo, rho53
            DOUBLE PRECISION Gamma2, Gamma
            DOUBLE PRECISION TauUEG, Tsig, Wsig
            DOUBLE PRECISION Fsig
            DOUBLE PRECISION tauu
            DOUBLE PRECISION x, x2, En, Ed, E, dE, dEn, dEd
            DOUBLE PRECISION dFdW, dWdT, dTdR, dTdTau, dGGAdR, dFdR
            DOUBLE PRECISION dFdTau, dGGAdG
            
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

            double precision, parameter :: C1 = 3.36116D-03
            double precision, parameter :: C2 = 4.49267D-03
            double precision, parameter :: Ax = -F3o2*(F4o3*PI)**(-F1o3)
            
            double precision, parameter :: dtol = 1.d-10
            
            if (ijzy.eq.1) then
                  !     Parameters for M05
                  fac = 0.72d0 
                  at0=    1.0d0*fac
                  at1=    0.08151d0*fac
                  at2=    -0.43956d0*fac
                  at3=    -3.22422d0*fac
                  at4=    2.01819d0*fac
                  at5=    8.79431d0*fac
                  at6=    -0.00295d0*fac
                  at7=    9.82029d0*fac
                  at8=    -4.82351d0*fac
                  at9=    -48.17574d0*fac
                  at10=   3.64802d0*fac
                  at11=   34.02248d0*fac
            else
                  !     Parameters for M05-2X
                  fac=  0.44d0
                  at0=   1.0d0*fac
                  at1=    -0.56833d0*fac
                  at2=    -1.30057d0*fac
                  at3=    5.50070d0*fac
                  at4=    9.06402d0*fac
                  at5=    -32.21075d0*fac
                  at6=    -23.73298d0*fac
                  at7=    70.22996d0*fac
                  at8=    29.88614d0*fac
                  at9=    -60.25778d0*fac
                  at10=   -13.22205d0*fac
                  at11=   15.23694d0*fac
            endif

            DO i = 1,NGrid
                  IF ((rho(1, i).gt.DTol).and.(tau(1, i).gt.DTol)) THEN
                        rhoo = rho(1, i)
                        rho43 = rhoo**F4o3  
                        rrho = 1.0d0/rhoo  
                        rho13 = rho43*rrho
                        rho53 = rhoo**F53

                        tauN = tau(1, i)
                        tauu = tauN
                        TauUEG=F3o5*((Six*pi*pi)**F2o3)*rho53
                        Tsig =TauUEG/tauN
                        Wsig =(Tsig-One)/(Tsig+One)
                        Fsig=(at0 + Wsig*(at1 + Wsig*(at2 + Wsig*(at3 + Wsig*( &
                              at4 + Wsig*(at5 + Wsig*(at6 + Wsig*(at7 + Wsig*( &
                              at8 + Wsig*(at9 + Wsig*(at10+Wsig*at11)))))))))))
                        gamma2 = sigma(1, i)
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

                        vrho(1, i)   = vrho(1, i) + dGGAdR*Fsig + (Ax+E)*rho43*dFdR
                        vsigma(1, i) = vsigma(1, i) + dGGAdG*Fsig
                        vtau(1, i)   = vtau(1, i) + rho43*(Ax+E)*dFdTau
                  ENDIF
                  !
                  ! beta component
                  !
                  IF ((rho(2, i).gt.DTol).and.(tau(2, i).gt.DTol)) THEN
                        rhoo = rho(2, i)
                        rho43 = rhoo**F4o3
                        rrho = 1.0d0/rhoo
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

                        gamma2 = sigma(3, i)
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

                        vrho(2, i)   = vrho(2, i) + dGGAdR*Fsig+ (Ax+E)*rho43*dFdR
                        vsigma(3, i) = vsigma(3, i) + dGGAdG*Fsig
                        vtau(2, i)   = vtau(2, i) + rho43*(Ax+E)*dFdTau
                  ENDIF
            enddo
      End Subroutine Um05x


      Subroutine m05x(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy)
            ! ----------------------------------------------------------------------
            !  M06x evaluates the exchange part of the M06 suite of                
            !  functionals on the grid.                                            
            !  !!! Second derivatives are not available yet.                       
            !                                                                      
            !     *  Ref: (a) Zhao, Y., Schultz, N. E., and Truhlar, D. G. J.      
            !     *   Chem. Phys. 123, 161103 (2005).                              
            !     *       (b) Y. Zhao, Schultz, N. E., and D. G. Truhlar, J. Chem. 
            !     *   Theory Comput. 2, 364 (2006)                                  
            !     *      
            !    ijzy - 1 M05                                             
            !    ijzy - 2 M05-2X                                          
            !
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

            integer i
            DOUBLE PRECISION fac, tauN
            DOUBLE PRECISION at1, at2, at3, at4, at5, at6, at7, at8, at9
            DOUBLE PRECISION at10, at11, at0
            DOUBLE PRECISION rrho, rho43, rho13, rhoo, rho53
            DOUBLE PRECISION Gamma2, Gamma
            DOUBLE PRECISION TauUEG, Tsig, Wsig
            DOUBLE PRECISION Fsig
            DOUBLE PRECISION tauu
            DOUBLE PRECISION x, x2, En, Ed, E, dE, dEn, dEd
            DOUBLE PRECISION dFdW, dWdT, dTdR, dTdTau, dGGAdR, dFdR
            DOUBLE PRECISION dFdTau, dGGAdG
            
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

            double precision, parameter :: C1 = 3.36116D-03
            double precision, parameter :: C2 = 4.49267D-03
            double precision, parameter :: Ax = -F3o2*(F4o3*PI)**(-F1o3)
            
            double precision, parameter :: dtol = 2.d-10
            
            if (ijzy.eq.1) then
                  !     Parameters for M05
                  fac = 0.72d0 
                  at0=    1.0d0*fac
                  at1=    0.08151d0*fac
                  at2=    -0.43956d0*fac
                  at3=    -3.22422d0*fac
                  at4=    2.01819d0*fac
                  at5=    8.79431d0*fac
                  at6=    -0.00295d0*fac
                  at7=    9.82029d0*fac
                  at8=    -4.82351d0*fac
                  at9=    -48.17574d0*fac
                  at10=   3.64802d0*fac
                  at11=   34.02248d0*fac
            else
                  !     Parameters for M05-2X
                  fac=  0.44d0
                  at0=   1.0d0*fac
                  at1=    -0.56833d0*fac
                  at2=    -1.30057d0*fac
                  at3=    5.50070d0*fac
                  at4=    9.06402d0*fac
                  at5=    -32.21075d0*fac
                  at6=    -23.73298d0*fac
                  at7=    70.22996d0*fac
                  at8=    29.88614d0*fac
                  at9=    -60.25778d0*fac
                  at10=   -13.22205d0*fac
                  at11=   15.23694d0*fac
            endif

            DO i = 1,NGrid
                  IF ((rho(i).gt.DTol).and.(tau(i).gt.DTol)) THEN
                        rhoo = 0.5d+0 * rho(i)
                        rho43 = rhoo**F4o3  
                        rrho = 1.0d0/rhoo  
                        rho13 = rho43*rrho
                        rho53 = rhoo**F53

                        tauN = 0.5d+0 * tau(i)
                        tauu = tauN
                        TauUEG=F3o5*((Six*pi*pi)**F2o3)*rho53
                        Tsig =TauUEG/tauN
                        Wsig =(Tsig-One)/(Tsig+One)
                        Fsig=(at0 + Wsig*(at1 + Wsig*(at2 + Wsig*(at3 + Wsig*( &
                              at4 + Wsig*(at5 + Wsig*(at6 + Wsig*(at7 + Wsig*( &
                              at8 + Wsig*(at9 + Wsig*(at10+Wsig*at11)))))))))))
                        gamma2 = 0.25d+0 * sigma(i)
                        Gamma = dsqrt(Gamma2)
                        x = Gamma/rho43
                        x2 = x*x
                        En = C1*x2
                        Ed = One + C2*x2
                        E  = -En/Ed
                        F(i) = F(i) + 2.d+0 * (Ax+E)*Fsig*rho43
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

                        vrho(i)   = vrho(i) + dGGAdR*Fsig + (Ax+E)*rho43*dFdR
                        vsigma(i) = vsigma(i) + 0.5d+0 * dGGAdG*Fsig
                        vtau(i)   = vtau(i) + rho43*(Ax+E)*dFdTau
                  ENDIF
            enddo
      End Subroutine m05x
end module m05exch
