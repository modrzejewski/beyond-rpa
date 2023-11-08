module vs98exch
      implicit none

contains

      Subroutine uvs98x(ngrid, rho, sigma, tau, f, vrho, vsigma, vtau, ijzy, dtol)
            !
            !     VS98x evaluates the exchange part of VS98 and its            
            !     variants on a grid.                                          
            !     !!! Second derivatives are not available yet.                
            !                                                                  
            !     Ref:  T. V. Voorhis and G. E. Scuseria, J. Chem. Phys. 109,  
            !           400 (1998).                                            
            !          Y. Zhao and D. G. Truhlar, J. Chem. Phys. 125,          
            !           194101 (2006).                                         
            !                                                                  
            !          ijzy - 1 exchange functional in VS98                    
            !          ijzy - 2 exchange functional in M06-L                   
            !          ijzy - 3 exchange functional in M06-HF
            !          ijzy - 4 exchange functional in M06                     
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

            integer :: i
            double precision :: rrho, rho43, rho13, rhoo, rho53, rho83
            double precision ::  Gamma

            double precision :: tauu
            double precision :: gx, x, z, kx,xk,zk
            double precision :: r1, r2, r3, r4, r5, r6
            double precision :: dxdr, dxdg, dzdr, dzdt, dgdx, dgdz

            double precision, parameter :: cf = 9.115599720d0
            double precision, parameter :: Axlsda = -0.9305257363491d0
            double precision, parameter :: gg  = 0.00186726d0
            double precision, parameter :: f43=4.0d0/3.0d0
            double precision, parameter :: f53=5.0d0/3.0d0
            double precision, parameter :: f83=8.d0/3.0d0
            double precision, parameter :: One=1.0d0

            if (ijzy == 1) then
                  !
                  ! Parameters for VS98
                  !
                  r1=  -9.800683d-01
                  r2=  -3.556788d-03
                  r3=   6.250326d-03
                  r4=  -2.354518d-05
                  r5=  -1.282732d-04
                  r6=   3.574822d-04
            elseif (ijzy == 2) then
                  !
                  ! Parameters for M06-L
                  !
                  r1 =   6.012244D-01*Axlsda
                  r2 =   4.748822D-03*Axlsda
                  r3 =  -8.635108D-03*Axlsda
                  r4 =  -9.308062D-06*Axlsda
                  r5 =   4.482811D-05*Axlsda
                  r6 =   0.000000D+00
            elseif (ijzy == 3) then
                  !
                  ! Parameters for M06-HF
                  !
                  r1 =   -1.179732D-01*Axlsda
                  r2 =   -2.500000D-03*Axlsda
                  r3 =   -1.180065D-02*Axlsda
                  r4 =   0.000000D+00
                  r5 =   0.000000D+00
                  r6 =   0.000000D+00
            else
                  !
                  ! (ijzy == 4) then
                  !
                  ! Parameters for M06
                  !
                  r1 =   1.422057D-01*Axlsda
                  r2 =   7.370319D-04*Axlsda
                  r3 =   -1.601373D-02*Axlsda
                  r4 =   0.000000D+00
                  r5 =   0.000000D+00
                  r6 =   0.000000D+00
            endif


            DO i = 1,NGrid
                  IF ((rho(1, i) > DTol) .AND. (tau(1, i) > DTol)) THEN
                        rhoo = rho(1, i)
                        rho43 = rhoo**F43
                        rrho = 1d0/rhoo
                        rho13 = rho43*rrho
                        rho53 = rhoo**F53
                        rho83 = rho53*rhoo

                        tauu = tau(1, i)
                        Gamma =  sigma(1, i) 
                        x = gamma/rho83
                        dxdr = -f83*x*rrho
                        dxdg = One/rho83
                        z = tauu/rho53 - cf
                        dzdr = -f53 * tauu/rho83
                        dzdt = One/rho53
                        kx = One + gg*x + gg*z
                        xk = x/kx
                        zk = z/kx
                        call gvt4(gx,dgdx,dgdz,xk,zk,kx,gg,r1,r2,r3,r4,r5,r6)

                        F(i) = F(i) + rho43*gx

                        !     functional derivatives

                        ! D1F(i,dRA) = D1F(i,dRA) + f43*rho13*gx + &
                        !       rho43*(dgdx*dxdr + dgdz*dzdr)
                        vrho(1, i) = vrho(1, i) + f43*rho13*gx + &
                              rho43*(dgdx*dxdr + dgdz*dzdr)
                        ! D1F(i,dGA) = D1F(i,dGA) + rho43*(dgdx*dxdg)
                        vsigma(1, i) = vsigma(1, i) + rho43*(dgdx*dxdg)
                        ! D1F(i,dTA) = D1F(i,dTA) + rho43*(dgdz*dzdt)
                        vtau(1, i) = vtau(1, i) + rho43*(dgdz*dzdt)
                  ENDIF

                  IF ((rho(2, i) > DTol) .AND. (tau(2, i) > DTol)) THEN
                        rhoo = rho(2, i)
                        rho43 = rhoo**F43
                        rrho = 1d0/rhoo       ! reciprocal of rho
                        rho13 = rho43*rrho
                        rho53 = rhoo**F53
                        rho83 = rho53*rhoo

                        tauu = tau(2, i)
                        Gamma = sigma(3, i) ! D1RB(i,1)**2 + D1RB(i,2)**2 + D1RB(i,3)**2
                        x = gamma/rho83
                        dxdr = -f83*x*rrho
                        dxdg = One/rho83
                        z = tauu/rho53 - cf
                        dzdr = -f53 * tauu/rho83
                        dzdt = One/rho53
                        kx = One + gg*x + gg*z
                        xk = x/kx
                        zk = z/kx
                        call gvt4(gx,dgdx,dgdz,xk,zk,kx,gg,r1,r2,r3,r4,r5,r6)
                        F(i) = F(i) + rho43*gx

                        !     functional derivatives

                        ! D1F(i,dRB) = D1F(i,dRB) + f43*rho13*gx + &
                        !       rho43*(dgdx*dxdr + dgdz*dzdr)
                        vrho(2, i) = vrho(2, i) + f43*rho13*gx + &
                              rho43*(dgdx*dxdr + dgdz*dzdr)
                        ! D1F(i,dGB) = D1F(i,dGB) + rho43*(dgdx*dxdg)
                        vsigma(3, i) = vsigma(3, i) + rho43*(dgdx*dxdg)
                        ! D1F(i,dTB) = D1F(i,dTB) + rho43*(dgdz*dzdt)
                        vtau(2, i) = vtau(2, i) + rho43*(dgdz*dzdt)
                  ENDIF
            ENDDO
      end subroutine uvs98x


      Subroutine gvt4(gvt,dg_dx,dg_dz,xg,zg,gama,ct,a,b,c,d,e,f)
            !     Evaluate the GVT4 form in VS98

            double precision :: gvt,dg_dx,dg_dz,xg,zg,gama,ct,a,b,c,d,e,f
            double precision :: g,g2

            double precision, parameter :: F1 = 1.0d0
            double precision, parameter :: F2 = 2.0d0
            double precision, parameter :: F3 = 3.0d0

            g=gama
            g2=gama*gama

            gvt =(a + b*xg + c*zg + d*xg*xg + e*zg*xg + f*zg*zg)/g
            dg_dx =(-a*ct+b*(F1-F2*ct*xg)-F2*c*zg*ct+d*(F2*xg-F3*xg*xg*ct) &
                  +e*(zg -F3*zg*xg*ct)-F3*f*zg*zg*ct )/g2
            dg_dz =(-a*ct -F2*b*xg*ct +c*(F1-F2*zg*ct)-F3*d*xg*xg*ct &
                  +e*(xg-F3*xg*zg*ct)+f*(F2*zg-F3*zg*zg*ct))/g2
      end Subroutine gvt4
end module vs98exch
