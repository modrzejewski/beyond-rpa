module grad_hg
      
      implicit none

contains 
    
     subroutine grad_coefficent_generator(maxdeg, Kab, p, ae, mu, Xab, Xpa, Xpb, E, gE)
            integer, intent(in) :: maxdeg
            double precision, intent(in) :: Kab
            double precision, intent(in) :: p
            double precision, intent(in) :: ae
            double precision, intent(in) :: mu
            double precision, intent(in) :: Xab
            double precision, intent(in) :: Xpa
            double precision, intent(in) :: Xpb
            double precision, dimension(0:,0:,0:), intent(out) :: E
            double precision, dimension(0:,0:,0:), intent(out) :: gE
            double precision :: gKab
            integer :: deg
            
            gKab = - 2 * mu * Xab * Kab
            E = 0.d+0
            E(0, 0, 0)  = Kab
            gE(0, 0, 0) = gKab
            
            do deg = 1, maxdeg
                  call grad_level(deg, p, ae, Xpa, Xpb, E, gE)
            end do
            
      end subroutine grad_coefficent_generator

      subroutine grad_level(deg, p, ae, Xpa, Xpb, E, gE)
            integer, intent(in) :: deg
            double precision, intent(in) :: p
            double precision, intent(in) :: ae
            double precision, intent(in) :: Xpa
            double precision, intent(in) :: Xpb
            double precision, dimension(0:,0:,0:), intent(out) :: E
            double precision, dimension(0:,0:,0:), intent(out) :: gE
            integer :: t, i, j

            E(0, deg, 0) = Xpa * E(0, deg - 1, 0) + E(1, deg - 1, 0)
            gE(0, deg, 0) = (-(p-ae)/p) * E(0, deg - 1, 0) + Xpa * gE(0, deg - 1, 0) + gE(1, deg - 1, 0)
            
            E(0, 0, deg) = Xpb * E(0, 0, deg - 1) + E(1, 0, deg - 1)
            gE(0, 0, deg) = (ae/p) * E(0, 0, deg - 1) + Xpb * gE(0, 0, deg - 1) + gE(1, 0, deg - 1)
            
            do i = 1, deg - 1
                  j = deg - i
                  E(0, i, j) =  Xpa * E(0, i - 1, j) + E(1, i - 1, j)
                  
                  gE(0, i, j) = (-(p-ae)/p) * E(0, i - 1, j) + Xpa * gE(0, i - 1, j) + gE(1, i - 1, j)
            end do
            
            do t = 1, deg
                
                  E(t, 0, deg) =  deg * E(t - 1, 0, deg - 1)  / (2 * p * t)
                  gE(t, 0, deg) =  deg * gE(t - 1, 0, deg - 1)  / (2 * p * t)

                  E(t, deg, 0) =  deg * E(t - 1, deg - 1, 0)  / (2 * p * t)
                  gE(t, deg, 0) =  deg * gE(t - 1, deg - 1, 0)  / (2 * p * t)
                  do i = 1, deg - 1
                        j = deg - i
                      !  if(j.ge.1)then
                              E(t, i, j) = (i * E(t -1, i - 1, j) + j * E(t - 1, i, j - 1) ) / (2 * p * t)
                           
                        ! else
                        !       E(t, i, j) = (i * E(t -1, i - 1, j) )/ (2 * p * t)
                        ! end if

                    !    if(j.ge.1)then
                              gE(t, i, j) = (i * gE(t -1, i - 1, j) + j * gE(t - 1, i, j - 1) ) / (2 * p * t)
                        ! else
                        !       gE(t, i, j) = (i * gE(t -1, i - 1, j) ) / (2 * p * t)
                        ! end if
                  end do
            end do
            
      end subroutine grad_level

end module grad_hg
