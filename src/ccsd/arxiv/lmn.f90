module lmn
      
      implicit none

contains

      subroutine ang_mom(k, s, l)
            integer, intent(in) :: k
            integer, intent(in) :: s
            integer, dimension(:,:), intent(out) :: l
            integer :: i, j, p
           
            p = 1
            do i =  k, 0, -1
                  do j =  k - i, 0, -1
                        l(1, p) = i
                        l(2, p) = j
                        l(3, p) = k - i - j
                        p = p + 1
                  end do
            end do
      end subroutine ang_mom

      function nfunco(k)
            integer :: nfunco
            integer, intent(in) :: k
            
            nfunco = (k + 1) * (k + 2) / 2

      end function nfunco

end module lmn
