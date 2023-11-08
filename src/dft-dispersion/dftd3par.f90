module dftd3par
      implicit none

contains
      
      subroutine copyc6(c6ab, maxci)
            double precision, dimension(:, :, :, :, :), intent(out) :: c6ab
            integer, dimension(:), intent(out)                      :: maxci

            integer :: nlines
            integer :: iat, jat, iadr, jadr, nn, kk
            include "pars.f90.include"

            c6ab=-1
            maxci=0
            ! read file
            kk=1
            do nn = 1, nlines
                  iat=int(pars(kk+1))
                  jat=int(pars(kk+2))
                  !       write(*,*)pars(kk+1),pars(kk+2)
                  !       write(*,*)iat,jat
                  call limit(iat,jat,iadr,jadr)
                  maxci(iat)=max(maxci(iat),iadr)
                  maxci(jat)=max(maxci(jat),jadr)

                  c6ab(iat,jat,iadr,jadr,1)=pars(kk)
                  c6ab(iat,jat,iadr,jadr,2)=pars(kk+3)
                  c6ab(iat,jat,iadr,jadr,3)=pars(kk+4)

                  c6ab(jat,iat,jadr,iadr,1)=pars(kk)
                  c6ab(jat,iat,jadr,iadr,2)=pars(kk+4)
                  c6ab(jat,iat,jadr,iadr,3)=pars(kk+3)
                  kk=(nn*5)+1
            enddo
      end subroutine copyc6


      subroutine limit(iat,jat,iadr,jadr)
            integer :: iat,jat
            integer :: iadr, jadr

            iadr=1
            jadr=1

            do while (iat > 100)
                  iat=iat-100
                  iadr=iadr+1
            end do

            do while (jat > 100)
                  jat=jat-100
                  jadr=jadr+1
            end do
      end subroutine limit
end module dftd3par
