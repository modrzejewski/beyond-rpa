module ccjac_block_31
use ccjac_block_31_part1
use ccjac_block_31_part2
use cc_gparams 
 implicit none
!
! File generated automatically on 2012-05-19 23:36:33 UTC.
!
contains
 
subroutine ccjac_31(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
double precision, dimension(:,:), intent(out)       :: jac
double precision, dimension(:), intent(in)          :: eorb
double precision, dimension(:, :), intent(in)       :: t1
double precision, dimension(:, :, :, :), intent(in) :: t2
integer, intent(in)                                 :: nocc0, nocc1
integer, intent(in)                                 :: nvirt0, nvirt1
integer, intent(in)                                 :: bra0, ket0
!
! Generating full CC Jacobian
!
call ccjac_31_part1(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
call ccjac_31_part2(jac, eorb, t1, t2, nocc0, nocc1, nvirt0, nvirt1, bra0, ket0)
end subroutine ccjac_31
end module ccjac_block_31
