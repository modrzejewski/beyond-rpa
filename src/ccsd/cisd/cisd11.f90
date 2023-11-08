module cisd11

      use ccsd_transformed_integrals
      use basis

      implicit none

contains

      function cisd11_aiaj(a, i, j) 
            double precision :: cisd11_aiaj   
            integer, intent(in) :: a, i, j 
            integer :: s  
            double precision, dimension(0:1) :: term 
            term = 0.d+0 
            term(0) = term(0) + vvoo(a, a, i, j)
            term(1) = term(1) + vovo(a, j, a, i)

            term(0) = -term(0) 
            term(1) = term(1) * 2.0d+0 


            cisd11_aiaj = 0.d+0
            do s = 0, 1
                  cisd11_aiaj = cisd11_aiaj + term(s)
            end do

      end function cisd11_aiaj
      function cisd11_aibi(a, i, b) 
            double precision :: cisd11_aibi   
            integer, intent(in) :: a, i, b 
            integer :: s  
            double precision, dimension(0:1) :: term 
            term = 0.d+0 
            term(0) = term(0) + vvoo(a, b, i, i)
            term(1) = term(1) + vovo(b, i, a, i)

            term(0) = -term(0) 
            term(1) = term(1) * 2.0d+0 


            cisd11_aibi = 0.d+0
            do s = 0, 1
                  cisd11_aibi = cisd11_aibi + term(s)
            end do

      end function cisd11_aibi
      function cisd11_aiai(eorb, nocc, a, i) 
            real(F64), dimension(:), intent(in) :: eorb
            double precision :: cisd11_aiai 
            integer, intent(in) :: nocc 
            integer, intent(in) :: a, i 
            integer :: s ,k,l 
            double precision, dimension(0:6) :: term 
            term = 0.d+0 
            term(0) = term(0) + eorb(i)
            term(1) = term(1) + vvoo(a, a, i, i)
            term(2) = term(2) + vovo(a, i, a, i)
            term(3) = term(3) + eorb(a)

            term(0) = -term(0) 
            term(1) = -term(1) 
            term(2) = term(2) * 2.0d+0 

            do k = 1, nocc 
                  do l = 1, nocc 
                        term(4) = term(4) + oooo(l, l, k, k)
                  end do
            end do

            term(4) = term(4) * (-2.0d+0) 

            do l = 1, nocc 
                  do k = 1, nocc 
                        term(5) = term(5) + oooo(k, l, k, l)
                  end do
            end do


            do k = 1, nocc 
                  term(6) = term(6) + eorb(k)
            end do

            term(6) = term(6) * 2.0d+0 


            cisd11_aiai = 0.d+0
            do s = 0, 6
                  cisd11_aiai = cisd11_aiai + term(s)
            end do


      end function cisd11_aiai
      function cisd11_aibj(a, i, b, j) 
            double precision :: cisd11_aibj   
            integer, intent(in) :: a, i, b, j 
            integer :: s  
            double precision, dimension(0:1) :: term 
            term = 0.d+0 
            term(0) = term(0) + vvoo(a, b, i, j)
            term(1) = term(1) + vovo(b, j, a, i)

            term(0) = -term(0) 
            term(1) = term(1) * 2.0d+0 


            cisd11_aibj = 0.d+0
            do s = 0, 1
                  cisd11_aibj = cisd11_aibj + term(s)
            end do

      end function cisd11_aibj
end module cisd11

