! File generated automatically. 2010-12-08 13:08:26 UTC
module hermite_automatic 
implicit none

contains


    subroutine chints_new_level_1(oldlev, newlev, xpc, ypc, zpc)
            double precision, dimension(:, :, :), intent(in) :: oldlev
            double precision, dimension(:, :, :), intent(out) :: newlev
            double precision, intent(in) :: xpc, ypc, zpc
    
newlev(1, 1, 2) = zpc * oldlev(1, 1, 1)
newlev(1, 2, 1) = ypc * oldlev(1, 1, 1)
newlev(2, 1, 1) = xpc * oldlev(1, 1, 1)

      end subroutine chints_new_level_1
      

    subroutine chints_new_level_2(oldlev, newlev, xpc, ypc, zpc)
            double precision, dimension(:, :, :), intent(in) :: oldlev
            double precision, dimension(:, :, :), intent(out) :: newlev
            double precision, intent(in) :: xpc, ypc, zpc
    
newlev(1, 1, 2) = zpc * oldlev(1, 1, 1)
newlev(1, 2, 1) = ypc * oldlev(1, 1, 1)
newlev(2, 1, 1) = xpc * oldlev(1, 1, 1)
newlev(3, 1, 1) = 1.d+0 * oldlev(1, 1, 1) + xpc * oldlev(2, 1, 1)
newlev(1, 3, 1) = 1.d+0 * oldlev(1, 1, 1) + ypc * oldlev(1, 2, 1)
newlev(2, 2, 1) = xpc * oldlev(1, 2, 1)
newlev(1, 1, 3) = 1.d+0 * oldlev(1, 1, 1) + zpc * oldlev(1, 1, 2)
newlev(1, 2, 2) = ypc * oldlev(1, 1, 2)
newlev(2, 1, 2) = xpc * oldlev(1, 1, 2)

      end subroutine chints_new_level_2
      

    subroutine chints_new_level_3(oldlev, newlev, xpc, ypc, zpc)
            double precision, dimension(:, :, :), intent(in) :: oldlev
            double precision, dimension(:, :, :), intent(out) :: newlev
            double precision, intent(in) :: xpc, ypc, zpc
    
newlev(1, 1, 2) = zpc * oldlev(1, 1, 1)
newlev(1, 2, 1) = ypc * oldlev(1, 1, 1)
newlev(2, 1, 1) = xpc * oldlev(1, 1, 1)
newlev(3, 1, 1) = 1.d+0 * oldlev(1, 1, 1) + xpc * oldlev(2, 1, 1)
newlev(4, 1, 1) = 2.d+0 * oldlev(2, 1, 1) + xpc * oldlev(3, 1, 1)
newlev(1, 3, 1) = 1.d+0 * oldlev(1, 1, 1) + ypc * oldlev(1, 2, 1)
newlev(2, 2, 1) = xpc * oldlev(1, 2, 1)
newlev(3, 2, 1) = 1.d+0 * oldlev(1, 2, 1) + xpc * oldlev(2, 2, 1)
newlev(1, 4, 1) = 2.d+0 * oldlev(1, 2, 1) + ypc * oldlev(1, 3, 1)
newlev(2, 3, 1) = xpc * oldlev(1, 3, 1)
newlev(1, 1, 3) = 1.d+0 * oldlev(1, 1, 1) + zpc * oldlev(1, 1, 2)
newlev(1, 2, 2) = ypc * oldlev(1, 1, 2)
newlev(2, 1, 2) = xpc * oldlev(1, 1, 2)
newlev(3, 1, 2) = 1.d+0 * oldlev(1, 1, 2) + xpc * oldlev(2, 1, 2)
newlev(1, 3, 2) = 1.d+0 * oldlev(1, 1, 2) + ypc * oldlev(1, 2, 2)
newlev(2, 2, 2) = xpc * oldlev(1, 2, 2)
newlev(1, 1, 4) = 2.d+0 * oldlev(1, 1, 2) + zpc * oldlev(1, 1, 3)
newlev(1, 2, 3) = ypc * oldlev(1, 1, 3)
newlev(2, 1, 3) = xpc * oldlev(1, 1, 3)

      end subroutine chints_new_level_3
      

    subroutine chints_new_level_4(oldlev, newlev, xpc, ypc, zpc)
            double precision, dimension(:, :, :), intent(in) :: oldlev
            double precision, dimension(:, :, :), intent(out) :: newlev
            double precision, intent(in) :: xpc, ypc, zpc
    
newlev(1, 1, 2) = zpc * oldlev(1, 1, 1)
newlev(1, 2, 1) = ypc * oldlev(1, 1, 1)
newlev(2, 1, 1) = xpc * oldlev(1, 1, 1)
newlev(3, 1, 1) = 1.d+0 * oldlev(1, 1, 1) + xpc * oldlev(2, 1, 1)
newlev(4, 1, 1) = 2.d+0 * oldlev(2, 1, 1) + xpc * oldlev(3, 1, 1)
newlev(5, 1, 1) = 3.d+0 * oldlev(3, 1, 1) + xpc * oldlev(4, 1, 1)
newlev(1, 3, 1) = 1.d+0 * oldlev(1, 1, 1) + ypc * oldlev(1, 2, 1)
newlev(2, 2, 1) = xpc * oldlev(1, 2, 1)
newlev(3, 2, 1) = 1.d+0 * oldlev(1, 2, 1) + xpc * oldlev(2, 2, 1)
newlev(4, 2, 1) = 2.d+0 * oldlev(2, 2, 1) + xpc * oldlev(3, 2, 1)
newlev(1, 4, 1) = 2.d+0 * oldlev(1, 2, 1) + ypc * oldlev(1, 3, 1)
newlev(2, 3, 1) = xpc * oldlev(1, 3, 1)
newlev(3, 3, 1) = 1.d+0 * oldlev(1, 3, 1) + xpc * oldlev(2, 3, 1)
newlev(1, 5, 1) = 3.d+0 * oldlev(1, 3, 1) + ypc * oldlev(1, 4, 1)
newlev(2, 4, 1) = xpc * oldlev(1, 4, 1)
newlev(1, 1, 3) = 1.d+0 * oldlev(1, 1, 1) + zpc * oldlev(1, 1, 2)
newlev(1, 2, 2) = ypc * oldlev(1, 1, 2)
newlev(2, 1, 2) = xpc * oldlev(1, 1, 2)
newlev(3, 1, 2) = 1.d+0 * oldlev(1, 1, 2) + xpc * oldlev(2, 1, 2)
newlev(4, 1, 2) = 2.d+0 * oldlev(2, 1, 2) + xpc * oldlev(3, 1, 2)
newlev(1, 3, 2) = 1.d+0 * oldlev(1, 1, 2) + ypc * oldlev(1, 2, 2)
newlev(2, 2, 2) = xpc * oldlev(1, 2, 2)
newlev(3, 2, 2) = 1.d+0 * oldlev(1, 2, 2) + xpc * oldlev(2, 2, 2)
newlev(1, 4, 2) = 2.d+0 * oldlev(1, 2, 2) + ypc * oldlev(1, 3, 2)
newlev(2, 3, 2) = xpc * oldlev(1, 3, 2)
newlev(1, 1, 4) = 2.d+0 * oldlev(1, 1, 2) + zpc * oldlev(1, 1, 3)
newlev(1, 2, 3) = ypc * oldlev(1, 1, 3)
newlev(2, 1, 3) = xpc * oldlev(1, 1, 3)
newlev(3, 1, 3) = 1.d+0 * oldlev(1, 1, 3) + xpc * oldlev(2, 1, 3)
newlev(1, 3, 3) = 1.d+0 * oldlev(1, 1, 3) + ypc * oldlev(1, 2, 3)
newlev(2, 2, 3) = xpc * oldlev(1, 2, 3)
newlev(1, 1, 5) = 3.d+0 * oldlev(1, 1, 3) + zpc * oldlev(1, 1, 4)
newlev(1, 2, 4) = ypc * oldlev(1, 1, 4)
newlev(2, 1, 4) = xpc * oldlev(1, 1, 4)

      end subroutine chints_new_level_4
      

    subroutine chints_new_level_5(oldlev, newlev, xpc, ypc, zpc)
            double precision, dimension(:, :, :), intent(in) :: oldlev
            double precision, dimension(:, :, :), intent(out) :: newlev
            double precision, intent(in) :: xpc, ypc, zpc
    
newlev(1, 1, 2) = zpc * oldlev(1, 1, 1)
newlev(1, 2, 1) = ypc * oldlev(1, 1, 1)
newlev(2, 1, 1) = xpc * oldlev(1, 1, 1)
newlev(3, 1, 1) = 1.d+0 * oldlev(1, 1, 1) + xpc * oldlev(2, 1, 1)
newlev(4, 1, 1) = 2.d+0 * oldlev(2, 1, 1) + xpc * oldlev(3, 1, 1)
newlev(5, 1, 1) = 3.d+0 * oldlev(3, 1, 1) + xpc * oldlev(4, 1, 1)
newlev(6, 1, 1) = 4.d+0 * oldlev(4, 1, 1) + xpc * oldlev(5, 1, 1)
newlev(1, 3, 1) = 1.d+0 * oldlev(1, 1, 1) + ypc * oldlev(1, 2, 1)
newlev(2, 2, 1) = xpc * oldlev(1, 2, 1)
newlev(3, 2, 1) = 1.d+0 * oldlev(1, 2, 1) + xpc * oldlev(2, 2, 1)
newlev(4, 2, 1) = 2.d+0 * oldlev(2, 2, 1) + xpc * oldlev(3, 2, 1)
newlev(5, 2, 1) = 3.d+0 * oldlev(3, 2, 1) + xpc * oldlev(4, 2, 1)
newlev(1, 4, 1) = 2.d+0 * oldlev(1, 2, 1) + ypc * oldlev(1, 3, 1)
newlev(2, 3, 1) = xpc * oldlev(1, 3, 1)
newlev(3, 3, 1) = 1.d+0 * oldlev(1, 3, 1) + xpc * oldlev(2, 3, 1)
newlev(4, 3, 1) = 2.d+0 * oldlev(2, 3, 1) + xpc * oldlev(3, 3, 1)
newlev(1, 5, 1) = 3.d+0 * oldlev(1, 3, 1) + ypc * oldlev(1, 4, 1)
newlev(2, 4, 1) = xpc * oldlev(1, 4, 1)
newlev(3, 4, 1) = 1.d+0 * oldlev(1, 4, 1) + xpc * oldlev(2, 4, 1)
newlev(1, 6, 1) = 4.d+0 * oldlev(1, 4, 1) + ypc * oldlev(1, 5, 1)
newlev(2, 5, 1) = xpc * oldlev(1, 5, 1)
newlev(1, 1, 3) = 1.d+0 * oldlev(1, 1, 1) + zpc * oldlev(1, 1, 2)
newlev(1, 2, 2) = ypc * oldlev(1, 1, 2)
newlev(2, 1, 2) = xpc * oldlev(1, 1, 2)
newlev(3, 1, 2) = 1.d+0 * oldlev(1, 1, 2) + xpc * oldlev(2, 1, 2)
newlev(4, 1, 2) = 2.d+0 * oldlev(2, 1, 2) + xpc * oldlev(3, 1, 2)
newlev(5, 1, 2) = 3.d+0 * oldlev(3, 1, 2) + xpc * oldlev(4, 1, 2)
newlev(1, 3, 2) = 1.d+0 * oldlev(1, 1, 2) + ypc * oldlev(1, 2, 2)
newlev(2, 2, 2) = xpc * oldlev(1, 2, 2)
newlev(3, 2, 2) = 1.d+0 * oldlev(1, 2, 2) + xpc * oldlev(2, 2, 2)
newlev(4, 2, 2) = 2.d+0 * oldlev(2, 2, 2) + xpc * oldlev(3, 2, 2)
newlev(1, 4, 2) = 2.d+0 * oldlev(1, 2, 2) + ypc * oldlev(1, 3, 2)
newlev(2, 3, 2) = xpc * oldlev(1, 3, 2)
newlev(3, 3, 2) = 1.d+0 * oldlev(1, 3, 2) + xpc * oldlev(2, 3, 2)
newlev(1, 5, 2) = 3.d+0 * oldlev(1, 3, 2) + ypc * oldlev(1, 4, 2)
newlev(2, 4, 2) = xpc * oldlev(1, 4, 2)
newlev(1, 1, 4) = 2.d+0 * oldlev(1, 1, 2) + zpc * oldlev(1, 1, 3)
newlev(1, 2, 3) = ypc * oldlev(1, 1, 3)
newlev(2, 1, 3) = xpc * oldlev(1, 1, 3)
newlev(3, 1, 3) = 1.d+0 * oldlev(1, 1, 3) + xpc * oldlev(2, 1, 3)
newlev(4, 1, 3) = 2.d+0 * oldlev(2, 1, 3) + xpc * oldlev(3, 1, 3)
newlev(1, 3, 3) = 1.d+0 * oldlev(1, 1, 3) + ypc * oldlev(1, 2, 3)
newlev(2, 2, 3) = xpc * oldlev(1, 2, 3)
newlev(3, 2, 3) = 1.d+0 * oldlev(1, 2, 3) + xpc * oldlev(2, 2, 3)
newlev(1, 4, 3) = 2.d+0 * oldlev(1, 2, 3) + ypc * oldlev(1, 3, 3)
newlev(2, 3, 3) = xpc * oldlev(1, 3, 3)
newlev(1, 1, 5) = 3.d+0 * oldlev(1, 1, 3) + zpc * oldlev(1, 1, 4)
newlev(1, 2, 4) = ypc * oldlev(1, 1, 4)
newlev(2, 1, 4) = xpc * oldlev(1, 1, 4)
newlev(3, 1, 4) = 1.d+0 * oldlev(1, 1, 4) + xpc * oldlev(2, 1, 4)
newlev(1, 3, 4) = 1.d+0 * oldlev(1, 1, 4) + ypc * oldlev(1, 2, 4)
newlev(2, 2, 4) = xpc * oldlev(1, 2, 4)
newlev(1, 1, 6) = 4.d+0 * oldlev(1, 1, 4) + zpc * oldlev(1, 1, 5)
newlev(1, 2, 5) = ypc * oldlev(1, 1, 5)
newlev(2, 1, 5) = xpc * oldlev(1, 1, 5)

      end subroutine chints_new_level_5
      

    subroutine chints_new_level_6(oldlev, newlev, xpc, ypc, zpc)
            double precision, dimension(:, :, :), intent(in) :: oldlev
            double precision, dimension(:, :, :), intent(out) :: newlev
            double precision, intent(in) :: xpc, ypc, zpc
    
newlev(1, 1, 2) = zpc * oldlev(1, 1, 1)
newlev(1, 2, 1) = ypc * oldlev(1, 1, 1)
newlev(2, 1, 1) = xpc * oldlev(1, 1, 1)
newlev(3, 1, 1) = 1.d+0 * oldlev(1, 1, 1) + xpc * oldlev(2, 1, 1)
newlev(4, 1, 1) = 2.d+0 * oldlev(2, 1, 1) + xpc * oldlev(3, 1, 1)
newlev(5, 1, 1) = 3.d+0 * oldlev(3, 1, 1) + xpc * oldlev(4, 1, 1)
newlev(6, 1, 1) = 4.d+0 * oldlev(4, 1, 1) + xpc * oldlev(5, 1, 1)
newlev(7, 1, 1) = 5.d+0 * oldlev(5, 1, 1) + xpc * oldlev(6, 1, 1)
newlev(1, 3, 1) = 1.d+0 * oldlev(1, 1, 1) + ypc * oldlev(1, 2, 1)
newlev(2, 2, 1) = xpc * oldlev(1, 2, 1)
newlev(3, 2, 1) = 1.d+0 * oldlev(1, 2, 1) + xpc * oldlev(2, 2, 1)
newlev(4, 2, 1) = 2.d+0 * oldlev(2, 2, 1) + xpc * oldlev(3, 2, 1)
newlev(5, 2, 1) = 3.d+0 * oldlev(3, 2, 1) + xpc * oldlev(4, 2, 1)
newlev(6, 2, 1) = 4.d+0 * oldlev(4, 2, 1) + xpc * oldlev(5, 2, 1)
newlev(1, 4, 1) = 2.d+0 * oldlev(1, 2, 1) + ypc * oldlev(1, 3, 1)
newlev(2, 3, 1) = xpc * oldlev(1, 3, 1)
newlev(3, 3, 1) = 1.d+0 * oldlev(1, 3, 1) + xpc * oldlev(2, 3, 1)
newlev(4, 3, 1) = 2.d+0 * oldlev(2, 3, 1) + xpc * oldlev(3, 3, 1)
newlev(5, 3, 1) = 3.d+0 * oldlev(3, 3, 1) + xpc * oldlev(4, 3, 1)
newlev(1, 5, 1) = 3.d+0 * oldlev(1, 3, 1) + ypc * oldlev(1, 4, 1)
newlev(2, 4, 1) = xpc * oldlev(1, 4, 1)
newlev(3, 4, 1) = 1.d+0 * oldlev(1, 4, 1) + xpc * oldlev(2, 4, 1)
newlev(4, 4, 1) = 2.d+0 * oldlev(2, 4, 1) + xpc * oldlev(3, 4, 1)
newlev(1, 6, 1) = 4.d+0 * oldlev(1, 4, 1) + ypc * oldlev(1, 5, 1)
newlev(2, 5, 1) = xpc * oldlev(1, 5, 1)
newlev(3, 5, 1) = 1.d+0 * oldlev(1, 5, 1) + xpc * oldlev(2, 5, 1)
newlev(1, 7, 1) = 5.d+0 * oldlev(1, 5, 1) + ypc * oldlev(1, 6, 1)
newlev(2, 6, 1) = xpc * oldlev(1, 6, 1)
newlev(1, 1, 3) = 1.d+0 * oldlev(1, 1, 1) + zpc * oldlev(1, 1, 2)
newlev(1, 2, 2) = ypc * oldlev(1, 1, 2)
newlev(2, 1, 2) = xpc * oldlev(1, 1, 2)
newlev(3, 1, 2) = 1.d+0 * oldlev(1, 1, 2) + xpc * oldlev(2, 1, 2)
newlev(4, 1, 2) = 2.d+0 * oldlev(2, 1, 2) + xpc * oldlev(3, 1, 2)
newlev(5, 1, 2) = 3.d+0 * oldlev(3, 1, 2) + xpc * oldlev(4, 1, 2)
newlev(6, 1, 2) = 4.d+0 * oldlev(4, 1, 2) + xpc * oldlev(5, 1, 2)
newlev(1, 3, 2) = 1.d+0 * oldlev(1, 1, 2) + ypc * oldlev(1, 2, 2)
newlev(2, 2, 2) = xpc * oldlev(1, 2, 2)
newlev(3, 2, 2) = 1.d+0 * oldlev(1, 2, 2) + xpc * oldlev(2, 2, 2)
newlev(4, 2, 2) = 2.d+0 * oldlev(2, 2, 2) + xpc * oldlev(3, 2, 2)
newlev(5, 2, 2) = 3.d+0 * oldlev(3, 2, 2) + xpc * oldlev(4, 2, 2)
newlev(1, 4, 2) = 2.d+0 * oldlev(1, 2, 2) + ypc * oldlev(1, 3, 2)
newlev(2, 3, 2) = xpc * oldlev(1, 3, 2)
newlev(3, 3, 2) = 1.d+0 * oldlev(1, 3, 2) + xpc * oldlev(2, 3, 2)
newlev(4, 3, 2) = 2.d+0 * oldlev(2, 3, 2) + xpc * oldlev(3, 3, 2)
newlev(1, 5, 2) = 3.d+0 * oldlev(1, 3, 2) + ypc * oldlev(1, 4, 2)
newlev(2, 4, 2) = xpc * oldlev(1, 4, 2)
newlev(3, 4, 2) = 1.d+0 * oldlev(1, 4, 2) + xpc * oldlev(2, 4, 2)
newlev(1, 6, 2) = 4.d+0 * oldlev(1, 4, 2) + ypc * oldlev(1, 5, 2)
newlev(2, 5, 2) = xpc * oldlev(1, 5, 2)
newlev(1, 1, 4) = 2.d+0 * oldlev(1, 1, 2) + zpc * oldlev(1, 1, 3)
newlev(1, 2, 3) = ypc * oldlev(1, 1, 3)
newlev(2, 1, 3) = xpc * oldlev(1, 1, 3)
newlev(3, 1, 3) = 1.d+0 * oldlev(1, 1, 3) + xpc * oldlev(2, 1, 3)
newlev(4, 1, 3) = 2.d+0 * oldlev(2, 1, 3) + xpc * oldlev(3, 1, 3)
newlev(5, 1, 3) = 3.d+0 * oldlev(3, 1, 3) + xpc * oldlev(4, 1, 3)
newlev(1, 3, 3) = 1.d+0 * oldlev(1, 1, 3) + ypc * oldlev(1, 2, 3)
newlev(2, 2, 3) = xpc * oldlev(1, 2, 3)
newlev(3, 2, 3) = 1.d+0 * oldlev(1, 2, 3) + xpc * oldlev(2, 2, 3)
newlev(4, 2, 3) = 2.d+0 * oldlev(2, 2, 3) + xpc * oldlev(3, 2, 3)
newlev(1, 4, 3) = 2.d+0 * oldlev(1, 2, 3) + ypc * oldlev(1, 3, 3)
newlev(2, 3, 3) = xpc * oldlev(1, 3, 3)
newlev(3, 3, 3) = 1.d+0 * oldlev(1, 3, 3) + xpc * oldlev(2, 3, 3)
newlev(1, 5, 3) = 3.d+0 * oldlev(1, 3, 3) + ypc * oldlev(1, 4, 3)
newlev(2, 4, 3) = xpc * oldlev(1, 4, 3)
newlev(1, 1, 5) = 3.d+0 * oldlev(1, 1, 3) + zpc * oldlev(1, 1, 4)
newlev(1, 2, 4) = ypc * oldlev(1, 1, 4)
newlev(2, 1, 4) = xpc * oldlev(1, 1, 4)
newlev(3, 1, 4) = 1.d+0 * oldlev(1, 1, 4) + xpc * oldlev(2, 1, 4)
newlev(4, 1, 4) = 2.d+0 * oldlev(2, 1, 4) + xpc * oldlev(3, 1, 4)
newlev(1, 3, 4) = 1.d+0 * oldlev(1, 1, 4) + ypc * oldlev(1, 2, 4)
newlev(2, 2, 4) = xpc * oldlev(1, 2, 4)
newlev(3, 2, 4) = 1.d+0 * oldlev(1, 2, 4) + xpc * oldlev(2, 2, 4)
newlev(1, 4, 4) = 2.d+0 * oldlev(1, 2, 4) + ypc * oldlev(1, 3, 4)
newlev(2, 3, 4) = xpc * oldlev(1, 3, 4)
newlev(1, 1, 6) = 4.d+0 * oldlev(1, 1, 4) + zpc * oldlev(1, 1, 5)
newlev(1, 2, 5) = ypc * oldlev(1, 1, 5)
newlev(2, 1, 5) = xpc * oldlev(1, 1, 5)
newlev(3, 1, 5) = 1.d+0 * oldlev(1, 1, 5) + xpc * oldlev(2, 1, 5)
newlev(1, 3, 5) = 1.d+0 * oldlev(1, 1, 5) + ypc * oldlev(1, 2, 5)
newlev(2, 2, 5) = xpc * oldlev(1, 2, 5)
newlev(1, 1, 7) = 5.d+0 * oldlev(1, 1, 5) + zpc * oldlev(1, 1, 6)
newlev(1, 2, 6) = ypc * oldlev(1, 1, 6)
newlev(2, 1, 6) = xpc * oldlev(1, 1, 6)

      end subroutine chints_new_level_6
      

    subroutine chints_1(fm, rtuv, xpc, ypc, zpc, p)
            double precision, dimension(:), intent(in) :: fm
            double precision, dimension(2, 2, 2), intent(out) :: rtuv
            double precision, intent(in) :: xpc, ypc, zpc, p

            double precision, dimension(2, 2, 2) :: work1
            double precision :: mtwop, const1
    
mtwop = -2.d+0 * p
const1 = mtwop
work1(1, 1, 1) = const1 * fm(2)
call chints_new_level_1(work1, rtuv, xpc, ypc, zpc)
rtuv(1, 1, 1) = fm(1)

    end subroutine chints_1
    

    subroutine chints_2(fm, rtuv, xpc, ypc, zpc, p)
            double precision, dimension(:), intent(in) :: fm
            double precision, dimension(3, 3, 3), intent(out) :: rtuv
            double precision, intent(in) :: xpc, ypc, zpc, p

            double precision, dimension(3, 3, 3) :: work1, work2
            double precision :: mtwop, const1
    
mtwop = -2.d+0 * p
const1 = mtwop**2
work1(1, 1, 1) = const1 * fm(3)
call chints_new_level_1(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(2)
call chints_new_level_2(work2, rtuv, xpc, ypc, zpc)
rtuv(1, 1, 1) = fm(1)

    end subroutine chints_2
    

    subroutine chints_3(fm, rtuv, xpc, ypc, zpc, p)
            double precision, dimension(:), intent(in) :: fm
            double precision, dimension(4, 4, 4), intent(out) :: rtuv
            double precision, intent(in) :: xpc, ypc, zpc, p

            double precision, dimension(4, 4, 4) :: work1, work2
            double precision :: mtwop, const1
    
mtwop = -2.d+0 * p
const1 = mtwop**3
work1(1, 1, 1) = const1 * fm(4)
call chints_new_level_1(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(3)
call chints_new_level_2(work2, work1, xpc, ypc, zpc)
const1 = const1 / mtwop
work1(1, 1, 1) = const1 *  fm(2)
call chints_new_level_3(work1, rtuv, xpc, ypc, zpc)
rtuv(1, 1, 1) = fm(1)

    end subroutine chints_3
    

    subroutine chints_4(fm, rtuv, xpc, ypc, zpc, p)
            double precision, dimension(:), intent(in) :: fm
            double precision, dimension(5, 5, 5), intent(out) :: rtuv
            double precision, intent(in) :: xpc, ypc, zpc, p

            double precision, dimension(5, 5, 5) :: work1, work2
            double precision :: mtwop, const1
    
mtwop = -2.d+0 * p
const1 = mtwop**4
work1(1, 1, 1) = const1 * fm(5)
call chints_new_level_1(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(4)
call chints_new_level_2(work2, work1, xpc, ypc, zpc)
const1 = const1 / mtwop
work1(1, 1, 1) = const1 *  fm(3)
call chints_new_level_3(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(2)
call chints_new_level_4(work2, rtuv, xpc, ypc, zpc)
rtuv(1, 1, 1) = fm(1)

    end subroutine chints_4
    

    subroutine chints_5(fm, rtuv, xpc, ypc, zpc, p)
            double precision, dimension(:), intent(in) :: fm
            double precision, dimension(6, 6, 6), intent(out) :: rtuv
            double precision, intent(in) :: xpc, ypc, zpc, p

            double precision, dimension(6, 6, 6) :: work1, work2
            double precision :: mtwop, const1
    
mtwop = -2.d+0 * p
const1 = mtwop**5
work1(1, 1, 1) = const1 * fm(6)
call chints_new_level_1(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(5)
call chints_new_level_2(work2, work1, xpc, ypc, zpc)
const1 = const1 / mtwop
work1(1, 1, 1) = const1 *  fm(4)
call chints_new_level_3(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(3)
call chints_new_level_4(work2, work1, xpc, ypc, zpc)
const1 = const1 / mtwop
work1(1, 1, 1) = const1 *  fm(2)
call chints_new_level_5(work1, rtuv, xpc, ypc, zpc)
rtuv(1, 1, 1) = fm(1)

    end subroutine chints_5
    

    subroutine chints_6(fm, rtuv, xpc, ypc, zpc, p)
            double precision, dimension(:), intent(in) :: fm
            double precision, dimension(7, 7, 7), intent(out) :: rtuv
            double precision, intent(in) :: xpc, ypc, zpc, p

            double precision, dimension(7, 7, 7) :: work1, work2
            double precision :: mtwop, const1
    
mtwop = -2.d+0 * p
const1 = mtwop**6
work1(1, 1, 1) = const1 * fm(7)
call chints_new_level_1(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(6)
call chints_new_level_2(work2, work1, xpc, ypc, zpc)
const1 = const1 / mtwop
work1(1, 1, 1) = const1 *  fm(5)
call chints_new_level_3(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(4)
call chints_new_level_4(work2, work1, xpc, ypc, zpc)
const1 = const1 / mtwop
work1(1, 1, 1) = const1 *  fm(3)
call chints_new_level_5(work1, work2, xpc, ypc, zpc)
const1 = const1 / mtwop
work2(1, 1, 1) = const1 *  fm(2)
call chints_new_level_6(work2, rtuv, xpc, ypc, zpc)
rtuv(1, 1, 1) = fm(1)

    end subroutine chints_6
    
end module hermite_automatic
