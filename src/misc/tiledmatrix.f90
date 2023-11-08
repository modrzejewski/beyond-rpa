module tiledmatrix
      use gparam
      
      implicit none

contains

      pure function tilepos(a, b)
            integer             :: tilepos
            integer, intent(in) :: a
            integer, intent(in) :: b

            integer :: shpos_a, shpos_b, shpos_b_1

            shpos_a = SHPOS(a)
            shpos_b = SHPOS(b)
            shpos_b_1 = SHPOS(b+1)

            tilepos = NORB * (shpos_b - 1) + (shpos_a - 1) * (shpos_b_1 - shpos_b) + 1
      end function tilepos

      
      pure subroutine matrix2tile(m_tile, m)
            !
            ! Convert non-symmetric matrix into a tiled matrix.
            !
            double precision, dimension(:), intent(out)   :: m_tile
            double precision, dimension(:, :), intent(in) :: m

            integer :: k, p, q, a, b
            integer :: a0, a1, b0, b1

            k = 1
            do b = 1, NSHELL
                  b0 = SHPOS(b)
                  b1 = SHPOS(b+1) - 1
                  do a = 1, NSHELL
                        a0 = SHPOS(a)
                        a1 = SHPOS(a+1) - 1
                        do q = b0, b1
                              do p = a0, a1
                                    m_tile(k) = m(p, q)
                                    k = k + 1
                              end do
                        end do
                  end do
            end do
      end subroutine matrix2tile


      pure subroutine tile2matrix(m, m_tile)
            !
            ! Convert a tiled representation of a nonsymmetric matrix
            ! into a rank-2 array.
            ! WARNING! THE MATRIX M MUST BE INITIALIZED ON 
            ! ENTRY TO THIS SUBROUTINE BECAUSE ITS VALUES
            ! ARE UPDATED AND NOT SUBSTITUTED.
            !
            double precision, dimension(:, :), intent(inout) :: m
            double precision, dimension(:), intent(in)       :: m_tile

            integer :: k, p, q, a, b
            integer :: a0, a1, b0, b1

            k = 1
            do b = 1, NSHELL
                  b0 = SHPOS(b)
                  b1 = SHPOS(b+1) - 1
                  do a = 1, NSHELL
                        a0 = SHPOS(a)
                        a1 = SHPOS(a+1) - 1
                        do q = b0, b1
                              do p = a0, a1
                                    m(p, q) = m(p, q) + m_tile(k)
                                    k = k + 1
                              end do
                        end do
                  end do
            end do
      end subroutine tile2matrix
end module tiledmatrix
