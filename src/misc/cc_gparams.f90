module cc_gparams

      use arithmetic
      use gparam
      use linalg
      use sort
      use math_constants

      implicit none


      real(F64), dimension(:,:,:,:), allocatable :: toooo
      real(F64), dimension(:,:,:,:), allocatable :: tovoo
      real(F64), dimension(:,:,:,:), allocatable :: tvooo
      real(F64), dimension(:,:,:,:), allocatable :: tvovo
      real(F64), dimension(:,:,:,:), allocatable :: tovov
      real(F64), dimension(:,:,:,:), allocatable :: tvoov
      real(F64), dimension(:,:,:,:), allocatable :: tvvoo
      real(F64), dimension(:,:,:,:), allocatable :: tvvov
      real(F64), dimension(:,:,:,:), allocatable :: tvvvo
!      real(F64), dimension(:,:,:,:), allocatable :: tvvvv
      
      real(F64), dimension(:), allocatable :: ftvvvv

      real(F64), dimension(:,:), allocatable :: too
      real(F64), dimension(:,:), allocatable :: tvo
      real(F64), dimension(:,:), allocatable :: tov
      real(F64), dimension(:,:), allocatable :: tvv

      integer,private :: cc_nocc0, cc_nvirt0,  cc_nocc, cc_nvirt, cc_npair

      real(F64), dimension(:,:), allocatable :: jac_temp
      real(F64), dimension(:,:), allocatable :: jac_temp_rvec
      real(F64), dimension(:, :), allocatable :: jac_dun
      integer :: cc_it
      integer, dimension(9) :: cc_hh
      

contains

      subroutine jac_temp_init(n)
            integer, intent(in) :: n
            
            print*, 'alokuje jac_temp n*n', n*n
            allocate(jac_temp(n,n))
            allocate(jac_temp_rvec(n,n))
            allocate(jac_dun(n, 2))
            jac_temp = zero
            jac_dun = zero
      end subroutine jac_temp_init

      subroutine jac_temp_free()

            deallocate(jac_temp)
            deallocate(jac_temp_rvec)
            deallocate(jac_dun)

      end subroutine jac_temp_free

      subroutine jac_temp_print2(n)
            integer, intent(in) :: n
            real(F64), dimension(:), allocatable :: work
            integer :: d, i, j
            integer :: lwork
            real(F64), dimension(:), allocatable    :: wr, wi
            real(F64), dimension(:,:), allocatable    :: vl, vr
            integer, dimension(:), allocatable :: dy
            allocate(vl(1,1))
            allocate(vr(1,1))
            allocate(dy(n))
            call dgeevquery(n, lwork, 'N', 'N')
            
            d = max(1, lwork)
            allocate(work(1:d))
            allocate(wr(n))
            allocate(wi(n))

            call geevwrap(jac_temp, wr, wi, vl, vr, n, 'n', 'n', work)
      end subroutine jac_temp_print2

      subroutine jac_temp_print(n)
            integer, intent(in) :: n
            real(F64), dimension(:), allocatable    :: work
            integer :: d, i, j, k, n2
            integer :: lwork
            real(F64), dimension(:), allocatable    :: wr, wi
            real(F64), dimension(:,:), allocatable    :: vl, vr
            integer, dimension(:), allocatable :: dy
            real(f64), allocatable, dimension(:,:) :: jac_temp2, jac_temp3
            real(F64) :: x, sum
            logical :: gowno
            print*, 'DZIEJE SIE GOWNO WSZECHCZASOW'
            allocate(vl(1,1))
            allocate(vr(1,1))
            allocate(dy(n))


            ! do i = 1, 126
            !       write(*, '(126F12.8, A1)') jac_temp(i, :), ';'
            ! end do

            ! allocate(jac_temp2(119,119))
            ! allocate(jac_temp3(119,119))


            ! jac_temp2 = jac_temp(1:119, 1:119)

            ! do i = 1, 14
            !       do j = 1, 14
            !             sum = zero
            !             do k = 120, 679
            !                   sum = sum + jac_temp(i, k) * jac_temp(k, j)
            !             end do
            !             jac_temp3(i, j) = sum
            !       end do
            ! end do


            ! do i = 1, 14
            !       do j = 15, 119
            !             sum = zero
            !             do k = 120, 679
            !                   sum = sum+ jac_temp(i, k) * jac_temp(k, j)
            !             end do
            !             jac_temp3(i, j)= sum
            !       end do
            ! end do


            ! do i = 15, 119
            !       do j = 1, 14
            !             sum = zero
            !             do k = 120, 679
            !                   sum = sum+ jac_temp(i, k) * jac_temp(k, j)
            !             end do
            !             jac_temp3(i, j)= sum
            !       end do
            ! end do



            ! do i = 15, 119
            !       do j = 15, 119
            !             sum = zero
            !             do k = 120, 679
            !                   sum = sum+ jac_temp(i, k) * jac_temp(k, j)
            !             end do
            !             jac_temp3(i, j)= sum
            !       end do
            ! end do

            
            ! jac_temp2 = jac_temp2 + jac_temp3
            ! n = 119
            call dgeevquery(n, lwork, 'N', 'N')

            d = max(1, lwork)
            allocate(work(1:d))
            allocate(wr(n))
            allocate(wi(n))

            call geevwrap(jac_temp, wr, wi, vl, vr, n, 'n', 'n', work)

            ! jac_temp_rvec = vr
            
            do i = 1, n
                  dy(i) = i
            end do
            
            call dsort(wr, dy, n)

            
            do i = 1, n
                  !if (abs(wr(i)).gt.1.d-7)then
                  print*, i, wr(i), wi(dy(i))
                  !end if
            end do
      end subroutine jac_temp_print
      
      subroutine cc_gparams_init(nc, nv, nc0, nv0)
            integer, intent(in) :: nc, nv, nc0, nv0
            cc_nocc = nc
            cc_nvirt = nv
            cc_nocc0 = nc0
            cc_nvirt0 = nv0
            cc_npair = cc_nocc * cc_nvirt
      end subroutine cc_gparams_init

      function aibj_mem(a, i, b, j)
            integer :: aibj_mem
            integer, intent(in) :: a, i, b, j
            integer :: ai, bj

            ai = (a - cc_nvirt0) * cc_nocc + (i - cc_nocc0) + 1
            bj = (b - cc_nvirt0) * cc_nocc + (j - cc_nocc0) + 1


            if(ai.gt.bj)then
                  aibj_mem = ((2 * cc_npair - bj + 2) * (bj - 1)) / 2 + ai - bj + 1 + cc_npair
            else if (ai.lt.bj)then
                  aibj_mem = ((2 * cc_npair - ai + 2) * (ai - 1)) / 2 + bj - ai + 1 + cc_npair
            else if (ai.eq.bj)then
                  aibj_mem = ((2 * cc_npair - ai + 2) * (ai - 1)) / 2  + 1 + cc_npair
            end if
            

      end function aibj_mem

      function ai_mem(a, i)
            integer :: ai_mem
            integer, intent(in) :: a, i

            ai_mem = (a - cc_nvirt0) * cc_nocc + (i - cc_nocc0) + 1

      end function ai_mem



end module cc_gparams
