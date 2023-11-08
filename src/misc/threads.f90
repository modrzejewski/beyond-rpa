module threads
      use gparam
      use display
      use tiledmatrix
      !$ use omp_lib

      implicit none

      double precision, dimension(:, :, :), allocatable, save :: PRVMAT
      double precision, dimension(:, :), allocatable, save :: PRVTILE
      integer, dimension(:), allocatable, save :: PRVIDA
      integer, dimension(:), allocatable, save :: PRVIDB

contains

      subroutine threads_init()
            integer :: k

            !$omp parallel default(shared)
            !$omp master
            OMP_NTHREAD = 1
            !$ OMP_NTHREAD = omp_get_num_threads()
            !$omp end master
            !$omp end parallel
            
            if (BUILDSKS) then
                  !
                  ! If there is no need for separate storage of
                  ! exchange and Coulomb matrices (or exchange
                  ! and correlation), do not allocate separate
                  ! matrices
                  !
                  allocate(PRVIDA(OMP_NTHREAD))
                  allocate(PRVIDB(OMP_NTHREAD))
                  if (SEPKSCONTRIB) then
                        allocate(PRVMAT(NORB, NORB, 2 * OMP_NTHREAD))
                        allocate(PRVTILE(NORB**2, 2 * OMP_NTHREAD))
                        do k = 1, OMP_NTHREAD
                              PRVIDA(k) = k
                              PRVIDB(k) = OMP_NTHREAD + k
                        end do
                  else
                        allocate(PRVMAT(NORB, NORB, OMP_NTHREAD))
                        allocate(PRVTILE(NORB**2, OMP_NTHREAD))
                        do k = 1, OMP_NTHREAD
                              PRVIDA(k) = k
                              PRVIDB(k) = k
                        end do
                  end if
            end if
      end subroutine threads_init


      subroutine threads_free()
            if (BUILDSKS) then
                  deallocate(PRVMAT)
                  deallocate(PRVTILE)
                  deallocate(PRVIDA)
                  deallocate(PRVIDB)
            end if
      end subroutine threads_free


      subroutine thr_reset()
            if (BUILDSKS) then
                  PRVMAT = ZERO
                  PRVTILE = ZERO
            end if
      end subroutine thr_reset


      subroutine thr_reducea(m)
            double precision, dimension(:, :), intent(inout) :: m
            integer :: k

            do k = 1, OMP_NTHREAD
                  m = m + PRVMAT(:, :, PRVIDA(k))
            end do
      end subroutine thr_reducea


      subroutine thr_reduce_tilea(m)
            double precision, dimension(:, :), intent(inout) :: m
            integer :: k

            do k = 2, OMP_NTHREAD
                  PRVTILE(:, PRVIDA(1)) = PRVTILE(:, PRVIDA(1)) &
                        + PRVTILE(:, PRVIDA(k))
            end do
            call tile2matrix(m, PRVTILE(:, PRVIDA(1)))
      end subroutine thr_reduce_tilea

      
      subroutine thr_reduceb(m)
            double precision, dimension(:, :), intent(inout) :: m
            integer :: k

            do k = 1, omp_nthread
                  m = m + PRVMAT(:, :, PRVIDB(k))
            end do
      end subroutine thr_reduceb


      subroutine thr_reduce_tileb(m)
            double precision, dimension(:, :), intent(inout) :: m
            integer :: k

            do k = 2, OMP_NTHREAD
                  PRVTILE(:, PRVIDB(1)) = PRVTILE(:, PRVIDB(1)) &
                        + PRVTILE(:, PRVIDB(k))
            end do
            call tile2matrix(m, PRVTILE(:, PRVIDB(1)))
      end subroutine thr_reduce_tileb
end module threads
