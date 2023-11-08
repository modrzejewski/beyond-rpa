module cc_diis

      use gparam
      use basis
      use ints
      use linalg

      implicit none

      double precision, allocatable :: delta_p(:,:)
      double precision, allocatable :: B(:,:), Bcopy(:,:)
      double precision, allocatable :: Binv(:,:)

contains

      subroutine diis_init(dmax, t1t2dim)
            integer, intent(in) :: dmax
            integer, intent(in) :: t1t2dim

            allocate(delta_p(t1t2dim, dmax+1))
            allocate(B(dmax+1,dmax+1))
            allocate(Bcopy(dmax+1,dmax+1))
            allocate(Binv(dmax+1,dmax+1))

      end subroutine diis_init
      
    
      subroutine diis(P, m, n, c, diis_idx)
      
      !======================
      ! DIIS
      !======================
      ! 
      ! subroutine to calculate best m-1, c coefficents
      ! in DIIS procedure
      !
      ! ARGUMENTS
      !
      integer, intent(in)                          :: m, n
      double precision, dimension(:), intent(out)  :: c
      double precision, dimension(:,:), intent(in) :: P
      integer, dimension(:), intent(in)            :: diis_idx
      
      integer          :: i, j
      double precision :: ddot
      double precision :: sum
      external :: ddot
      

      delta_p = 0.d+0

      do i = 1, m-1

         delta_p(:, i) = P(:, diis_idx(i+1)) - P(:, diis_idx(i))
      end do
      
      B = 0.d+0
      do i = 1, m-1
         do j = i, m-1
            B(j,i) = ddot(n, delta_p(:,j), 1,  delta_p(:,i), 1)
            B(i,j) = B(j,i)
         end do
      end do

      do i = 1, m
         B(m, i) = -1.d+0
         B(i, m) = -1.d+0
      end do
      
      B(m,m) = 0.d+0
      
      Bcopy = B
!      call geprn(B, m)
!      print*, 'b'
      ! condnum = issingular(Bcopy, m, 1.d-8)
      ! call geprn(B, m)
      ! !print*, condnum
      ! if(condnum)then
      !    print*, 'DIIS MATRIX SINGULAR'
      !    stop
      ! end if
      call sminv(Binv, B, m)
!      print*, ''
!      print*, 'binv'
!      call geprn(Binv, m)
!      print*, Binv(1,1)
      !            ac = matmul(Binv, bcopy)
      !            call geprn(ac)
      
      c(1:m-1) = -Binv(1:m-1,m)
      
      sum = 0
      do i = 1, m-1
         sum = sum + c(i)
      end do
      
!      deallocate(bcopy)
 !     deallocate(ac)
    end subroutine diis

      subroutine diis_free()
            
            deallocate(delta_p)
            deallocate(B)
            deallocate(Bcopy)
            deallocate(Binv)

      end subroutine diis_free

    end module cc_diis
