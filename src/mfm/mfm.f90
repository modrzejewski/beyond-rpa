module mfm
      use m05corr
      use m05exch
      use m06exch
      use m06corr
      use m08m11exch
      use m08m11corr
      
      implicit none
      
contains
            
      subroutine m05_exx(c)
            double precision, intent(out) :: c
            c = 0.28d+0
      end subroutine m05_exx

      subroutine m052x_exx(c)
            double precision, intent(out) :: c
            c = 0.56d+0
      end subroutine m052x_exx

      subroutine m06_exx(c)
            double precision, intent(out) :: c
            c = 0.27d+0
      end subroutine m06_exx

      subroutine m062x_exx(c)
            double precision, intent(out) :: c
            c = 0.54d+0
      end subroutine m062x_exx

      subroutine m06l_exx(c)
            double precision, intent(out) :: c
            c = 0.d+0
      end subroutine m06l_exx

      subroutine m06hf_exx(c)
            double precision, intent(out) :: c
            c = 1.d+0
      end subroutine m06hf_exx

      
      subroutine m11_srexx(c)
            double precision, intent(out) :: c
            c = 0.428d+0
      end subroutine m11_srexx

      
      subroutine m11_omega(omega)
            double precision, intent(out) :: omega
            omega = 0.25d+0
      end subroutine m11_omega


      subroutine m11l_exx(c)
            double precision, intent(out) :: c
            c = 0.00d+0
      end subroutine m11l_exx

      
      subroutine m08hx_exx(c)
            double precision, intent(out) :: c
            c = 0.5223d+0
      end subroutine m08hx_exx

      
      subroutine m08so_exx(c)
            double precision, intent(out) :: c
            c = 0.5679d+0
      end subroutine m08so_exx


      subroutine um052x(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um05c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)
            call um05x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um052x


      subroutine um05(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um05c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)
            call um05x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um05


      subroutine um06(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0

            call um06c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 3)
            call um06x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 3)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um06


      subroutine um062x(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um06c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 4)
            call um06x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 4)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um062x


      subroutine um06hf(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um06c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)
            call um06x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um06hf


      subroutine um06l(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um06c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)
            call um06x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um06l


      subroutine m05(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:), intent(in)  :: rhovec
            double precision, dimension(:), intent(in)  :: sigmavec
            double precision, dimension(:), intent(in)  :: tauvec
            double precision, dimension(:), intent(out) :: epsvec
            double precision, dimension(:), intent(out) :: vrhovec
            double precision, dimension(:), intent(out) :: vsigmavec
            double precision, dimension(:), intent(out) :: vlaplvec
            double precision, dimension(:), intent(out) :: vtauvec
            integer, intent(in)                         :: npt                  

            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(1:npt) = 0.d+0
            vsigmavec(1:npt) = 0.d+0
            vlaplvec(1:npt) = 0.d+0
            vtauvec(1:npt) = 0.d+0
            call m05c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)
            call m05x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)

            do k = 1, npt
                  epsvec(k) = epsvec(k) / rhovec(k)
            end do
      end subroutine m05


      subroutine m05_check(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:), intent(in)  :: rhovec
            double precision, dimension(:), intent(in)  :: sigmavec
            double precision, dimension(:), intent(in)  :: tauvec
            double precision, dimension(:), intent(out) :: epsvec
            double precision, dimension(:), intent(out) :: vrhovec
            double precision, dimension(:), intent(out) :: vsigmavec
            double precision, dimension(:), intent(out) :: vlaplvec
            double precision, dimension(:), intent(out) :: vtauvec
            integer, intent(in)                         :: npt  

            integer :: k

            double precision, dimension(1)    :: ueps
            double precision, dimension(2, 1) :: urho, utau, uvrho, uvtau, uvlapl
            double precision, dimension(3, 1) :: usigma, uvsigma
            

            do k = 1, npt
                  urho(1, 1) = rhovec(k) * 0.5d+0
                  urho(2, 1) = rhovec(k) * 0.5d+0

                  usigma(1, 1) = sigmavec(k) * 0.25d+0
                  usigma(3, 1) = sigmavec(k) * 0.25d+0

                  utau(1, 1) = tauvec(k) * 0.5d+0
                  utau(2, 1) = tauvec(k) * 0.5d+0

                  call um052x(urho, usigma, utau, ueps, uvrho, uvsigma, &
                        uvlapl, uvtau, 1)

                  vrhovec(k) = 0.5d+0 * (uvrho(1, 1) + uvrho(2, 1))
                  vsigmavec(k) = 0.25d+0 * (uvsigma(1, 1) + uvsigma(3, 1))
                  vtauvec(k) = 0.5d+0 * (uvtau(1, 1) + uvtau(2, 1))
                  vlaplvec(k) = 0.d+0
                  epsvec(k) = ueps(1)
            end do
      end subroutine m05_check


      subroutine m052x(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:), intent(in)  :: rhovec
            double precision, dimension(:), intent(in)  :: sigmavec
            double precision, dimension(:), intent(in)  :: tauvec
            double precision, dimension(:), intent(out) :: epsvec
            double precision, dimension(:), intent(out) :: vrhovec
            double precision, dimension(:), intent(out) :: vsigmavec
            double precision, dimension(:), intent(out) :: vlaplvec
            double precision, dimension(:), intent(out) :: vtauvec
            integer, intent(in)                         :: npt                  

            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(1:npt) = 0.d+0
            vsigmavec(1:npt) = 0.d+0
            vlaplvec(1:npt) = 0.d+0
            vtauvec(1:npt) = 0.d+0
            call m05c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)
            call m05x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)

            do k = 1, npt
                  epsvec(k) = epsvec(k) / rhovec(k)
            end do
      end subroutine m052x


      subroutine um11(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um08m11c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 3)
            call um08m11x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 3)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um11


      subroutine um11l(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um08m11c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 4)
            call um08m11x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 4)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um11l


      subroutine um08hx(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um08m11c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)
            call um08m11x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um08hx


      subroutine um08so(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:, :), intent(in)  :: rhovec
            double precision, dimension(:, :), intent(in)  :: sigmavec
            double precision, dimension(:, :), intent(in)  :: tauvec
            double precision, dimension(:), intent(out)    :: epsvec
            double precision, dimension(:, :), intent(out) :: vrhovec
            double precision, dimension(:, :), intent(out) :: vsigmavec
            double precision, dimension(:, :), intent(out) :: vlaplvec
            double precision, dimension(:, :), intent(out) :: vtauvec
            integer, intent(in)                            :: npt                  

            double precision :: rho
            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(:, 1:npt) = 0.d+0
            vsigmavec(:, 1:npt) = 0.d+0
            vlaplvec(:, 1:npt) = 0.d+0
            vtauvec(:, 1:npt) = 0.d+0
            call um08m11c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)
            call um08m11x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)

            do k = 1, npt
                  rho = rhovec(1, k) + rhovec(2, k)
                  epsvec(k) = epsvec(k) / rho
            end do
      end subroutine um08so


      subroutine m11(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:), intent(in)  :: rhovec
            double precision, dimension(:), intent(in)  :: sigmavec
            double precision, dimension(:), intent(in)  :: tauvec
            double precision, dimension(:), intent(out) :: epsvec
            double precision, dimension(:), intent(out) :: vrhovec
            double precision, dimension(:), intent(out) :: vsigmavec
            double precision, dimension(:), intent(out) :: vlaplvec
            double precision, dimension(:), intent(out) :: vtauvec
            integer, intent(in)                         :: npt                  

            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(1:npt) = 0.d+0
            vsigmavec(1:npt) = 0.d+0
            vlaplvec(1:npt) = 0.d+0
            vtauvec(1:npt) = 0.d+0
            call m08m11c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 3)
            call m08m11x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 3)

            do k = 1, npt
                  epsvec(k) = epsvec(k) / rhovec(k)
            end do
      end subroutine m11


      subroutine m11l(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:), intent(in)  :: rhovec
            double precision, dimension(:), intent(in)  :: sigmavec
            double precision, dimension(:), intent(in)  :: tauvec
            double precision, dimension(:), intent(out) :: epsvec
            double precision, dimension(:), intent(out) :: vrhovec
            double precision, dimension(:), intent(out) :: vsigmavec
            double precision, dimension(:), intent(out) :: vlaplvec
            double precision, dimension(:), intent(out) :: vtauvec
            integer, intent(in)                         :: npt                  

            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(1:npt) = 0.d+0
            vsigmavec(1:npt) = 0.d+0
            vlaplvec(1:npt) = 0.d+0
            vtauvec(1:npt) = 0.d+0
            call m08m11c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 4)
            call m08m11x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 4)

            do k = 1, npt
                  epsvec(k) = epsvec(k) / rhovec(k)
            end do
      end subroutine m11l


      subroutine m08hx(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:), intent(in)  :: rhovec
            double precision, dimension(:), intent(in)  :: sigmavec
            double precision, dimension(:), intent(in)  :: tauvec
            double precision, dimension(:), intent(out) :: epsvec
            double precision, dimension(:), intent(out) :: vrhovec
            double precision, dimension(:), intent(out) :: vsigmavec
            double precision, dimension(:), intent(out) :: vlaplvec
            double precision, dimension(:), intent(out) :: vtauvec
            integer, intent(in)                         :: npt                  

            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(1:npt) = 0.d+0
            vsigmavec(1:npt) = 0.d+0
            vlaplvec(1:npt) = 0.d+0
            vtauvec(1:npt) = 0.d+0
            call m08m11c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)
            call m08m11x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)

            do k = 1, npt
                  epsvec(k) = epsvec(k) / rhovec(k)
            end do
      end subroutine m08hx


      subroutine m08so(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
            vlaplvec, vtauvec, npt)

            double precision, dimension(:), intent(in)  :: rhovec
            double precision, dimension(:), intent(in)  :: sigmavec
            double precision, dimension(:), intent(in)  :: tauvec
            double precision, dimension(:), intent(out) :: epsvec
            double precision, dimension(:), intent(out) :: vrhovec
            double precision, dimension(:), intent(out) :: vsigmavec
            double precision, dimension(:), intent(out) :: vlaplvec
            double precision, dimension(:), intent(out) :: vtauvec
            integer, intent(in)                         :: npt                  

            integer :: k

            epsvec(1:npt) = 0.d+0
            vrhovec(1:npt) = 0.d+0
            vsigmavec(1:npt) = 0.d+0
            vlaplvec(1:npt) = 0.d+0
            vtauvec(1:npt) = 0.d+0
            call m08m11c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)
            call m08m11x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 2)

            do k = 1, npt
                  epsvec(k) = epsvec(k) / rhovec(k)
            end do
      end subroutine m08so


     !  subroutine um05(rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, &
     !        vlaplvec, vtauvec, npt)

     !        double precision, dimension(:, :), intent(in)  :: rhovec
     !        double precision, dimension(:, :), intent(in)  :: sigmavec
     !        double precision, dimension(:, :), intent(in)  :: tauvec
     !        double precision, dimension(:), intent(out)    :: epsvec
     !        double precision, dimension(:, :), intent(out) :: vrhovec
     !        double precision, dimension(:, :), intent(out) :: vsigmavec
     !        double precision, dimension(:, :), intent(out) :: vlaplvec
     !        double precision, dimension(:, :), intent(out) :: vtauvec
     !        integer, intent(in)                            :: npt                  

     !        double precision :: f0, f1, delta   
     !        double precision :: rho, deriv
     !        double precision, dimension(2, 1) :: rho0, tau0
     !        double precision, dimension(3, 1) :: sigma0
     !        double precision, parameter :: dx = 1.d-5

     !        double precision :: deriv_num
     !        ! double precision, dimension(2) :: rho
     !        ! double precision, dimension(2) :: vrho, vtau
     !        ! double precision, dimension(3) :: vsigma
     !        integer :: k

     !        epsvec(1:npt) = 0.d+0
     !        vrhovec(:, 1:npt) = 0.d+0
     !        vsigmavec(:, 1:npt) = 0.d+0
     !        vlaplvec(:, 1:npt) = 0.d+0
     !        vtauvec(:, 1:npt) = 0.d+0
     !        call M05c(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)
     ! !       call M05x(npt, rhovec, sigmavec, tauvec, epsvec, vrhovec, vsigmavec, vtauvec, 1)

     !        do k = 1, npt
     !              f0 = epsvec(k)
     !              deriv = vrhovec(2, k)
     !              rho0(:, 1) = rhovec(:, k)
     !              rho0(2,1) = rho0(2,1) + dx

     !              epsvec(k) = 0.d+0
     !              vrhovec(:, k) = 0.d+0
     !              vsigmavec(:, k) = 0.d+0
     !              vtauvec(:, k) = 0.d+0

     !              call M05c(1, rho0, sigmavec(:, k:), tauvec(:, k:), &
     !                    epsvec(k:), vrhovec(:, k:), vsigmavec(:, k:), vtauvec(:, k:), 1)
     !   !           call M05x(1, rho0, sigmavec(:, k:), tauvec(:, k:), &
     !   !                 epsvec(k:), vrhovec(:, k:), vsigmavec(:, k:), vtauvec(:, k:), 1)
                  
     !              f1 = epsvec(k)
     !              deriv_num = (f1 - f0) / dx

     !              print *, "RHO ALPHA          = ", rhovec(1, k)
     !              print *, "RHO BETA           = ", rhovec(2, k)
     !              print *, "RHO DERIV ANALYTIC = ", DERIV
     !              PRINT *, "RHO DERIV NUMERICA = ", DERIV_NUM


     !              ! =========================================================================

     !              f0 = epsvec(k)
     !              deriv = vtauvec(2, k)

     !              epsvec(k) = 0.d+0
     !              vrhovec(:, k) = 0.d+0
     !              vsigmavec(:, k) = 0.d+0
     !              vtauvec(:, k) = 0.d+0

     !              tau0(:, 1) = tauvec(:, k)
     !              tau0(2, 1) = tau0(2, 1) + dx

     !              call M05c(1, rho0, sigmavec(:, k:), tau0, &
     !                    epsvec(k:), vrhovec(:, k:), vsigmavec(:, k:), vtauvec(:, k:), 1)
     !    !          call M05x(1, rho0, sigmavec(:, k:), tau0, &
     !    !                epsvec(k:), vrhovec(:, k:), vsigmavec(:, k:), vtauvec(:, k:), 1)
                  
     !              f1 = epsvec(k)
     !              deriv_num = (f1 - f0) / dx
     !              print *, "TAU ALPHA          = ", tauvec(1, k)
     !              print *, "TAU BETA           = ", tauvec(2, k)
     !              print *, "TAU DERIV ANALYTIC = ", DERIV
     !              PRINT *, "TAU DERIV NUMERICA = ", DERIV_NUM

     !              ! ===========================================================================

     !              f0 = epsvec(k)
     !              deriv = vsigmavec(3, k)

     !              epsvec(k) = 0.d+0
     !              vrhovec(:, k) = 0.d+0
     !              vsigmavec(:, k) = 0.d+0
     !              vtauvec(:, k) = 0.d+0

     !              sigma0(:, 1) = sigmavec(:, k)
     !              sigma0(3, 1) = sigma0(3, 1) + dx

     !              call M05c(1, rho0, sigma0, tau0, &
     !                    epsvec(k:), vrhovec(:, k:), vsigmavec(:, k:), vtauvec(:, k:), 1)
     !    !          call M05x(1, rho0, sigmavec(:, k:), tau0, &
     !    !                epsvec(k:), vrhovec(:, k:), vsigmavec(:, k:), vtauvec(:, k:), 1)
                  
     !              f1 = epsvec(k)
     !              deriv_num = (f1 - f0) / dx
     !              print *, "SIGMA ALPHA          = ", sigmavec(1, k)
     !              print *, "SIGMA BETA           = ", sigmavec(3, k)
     !              print *, "SIGMA DERIV ANALYTIC = ", DERIV
     !              PRINT *, "SIGMA DERIV NUMERICA = ", DERIV_NUM


     !              rho = rhovec(1, k) + rhovec(2, k)
     !              epsvec(k) = epsvec(k) / rho
     !             ! vsigmavec(1, k) = vsigmavec(1, k) / (2.d+0*sqrt(sigmavec(1, k)))
     !              ! vsigmavec(3, k) = vsigmavec(3, k) / (2.d+0*sqrt(sigmavec(3, k)))
     !        end do

     !  end subroutine um05


      
end module mfm
