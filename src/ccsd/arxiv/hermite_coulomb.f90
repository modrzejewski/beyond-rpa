module hermite_coulomb

      use oboys

contains
      
      subroutine R_tuv(mdeg, Xpc, Ypc, Zpc, R, alpha)
            integer, intent(in)                                      :: mdeg
            double precision, intent(in)                             :: alpha
            double precision, intent(in)                             :: Xpc
            double precision, intent(in)                             :: Ypc
            double precision, intent(in)                             :: Zpc
            double precision                                         :: x 
            double precision, dimension(-2:,-2:,-2:, 0:), intent(out) :: R

            double precision, allocatable :: F(:)      
            integer :: k,  i, l
            integer :: t, u, v 
            
            x = alpha * (Xpc**2 + Ypc**2 + Zpc**2)
 
            R = 0.d+0
            allocate(F(0:mdeg))
            call F_m(x, mdeg, F)
          
            do k = 0, mdeg
                  
                  do i = 0, k
                        R(0, 0, 0, i) = (-2 * alpha) ** i * F(i)
                  end do

                  l = k
                  do v = 1, k
                        l = l - 1
                        R(0, 0, v, l) = (v - 1) * R(0, 0, v - 2, l + 1) + Zpc * R(0, 0, v - 1, l + 1)
                  end do

                  do u = 1, k
                        l = k - u + 1
                        do v = 0, k - u
                              l = l - 1
                              R(0, u, v, l) = (u - 1) * R(0, u - 2, v , l + 1) + Ypc * R(0, u - 1, v, l + 1)
                        end do
                  end do

                  do t = 1, k
                        do u = 0, k - t
                              l = k - t - u + 1
                              do v = 0, k - u - t
                                    l = l - 1
                                    R(t, u, v, l) = (t - 1) * R(t - 2 , u, v, l + 1) + Xpc * R(t - 1 , u, v, l + 1)     
                              end do
                        end do
                  end do
                 
             end do
             deallocate(F)
      end subroutine R_tuv

end module hermite_coulomb
