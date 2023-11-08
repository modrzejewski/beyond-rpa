module density_exc_exc_functions_so_pt0123_sym

    use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    implicit none
    !
    ! File generated automatically on 2017-10-31 15:04:26
    !
    real(F64), dimension(:, :), allocatable :: wm_interm_0_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_3_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_10_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_11_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_12_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_16_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_17_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_sym_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_20_so_sym_pt1


    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_10_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_16_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_20_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_21_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_22_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_23_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_24_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_25_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_27_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_28_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_29_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_30_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_35_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_37_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_38_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_39_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_40_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_41_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_44_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_45_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_46_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_47_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_48_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_49_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_50_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_51_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_52_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_53_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_54_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_55_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_56_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_57_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_58_so_sym_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_59_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_so_sym_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_62_so_sym_pt2 


   real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_2_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_3_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_10_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_16_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_17_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_18_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_20_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_21_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_22_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_24_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_27_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_28_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_29_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_30_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_31_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_32_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_33_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_35_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_37_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_38_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_39_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_40_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_41_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_44_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_45_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_47_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_48_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_49_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_50_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_51_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_52_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_54_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_55_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_56_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_57_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_58_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_61_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_62_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_63_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_64_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_65_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_66_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_67_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_68_so_sym_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_69_so_sym_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_70_so_sym_pt3 

contains

   subroutine wm_so_sym_intermediates_ccsd_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    
    end subroutine wm_so_sym_intermediates_ccsd_init_pt0
    
    subroutine wm_so_sym_intermediates_ccsd_free_pt0
    
    end subroutine wm_so_sym_intermediates_ccsd_free_pt0
    
    subroutine wm_so_sym_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

        end subroutine wm_so_sym_intermediates_ccsd_pt0

    subroutine wm_so_sym_intermediates_ccsd_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_sym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_so_sym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_so_sym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_3_so_sym_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_4_so_sym_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_5_so_sym_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_so_sym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_7_so_sym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_8_so_sym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_9_so_sym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_so_sym_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_11_so_sym_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_12_so_sym_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_13_so_sym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_14_so_sym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_15_so_sym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_16_so_sym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_17_so_sym_pt1(1: nocc, 1: nocc))
allocate(wm_interm_18_so_sym_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_19_so_sym_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_20_so_sym_pt1(nocc+1: nactive, 1: nocc))
wm_interm_0_so_sym_pt1 = zero 
wm_interm_1_so_sym_pt1 = zero 
wm_interm_2_so_sym_pt1 = zero 
wm_interm_3_so_sym_pt1 = zero 
wm_interm_4_so_sym_pt1 = zero 
wm_interm_5_so_sym_pt1 = zero 
wm_interm_6_so_sym_pt1 = zero 
wm_interm_7_so_sym_pt1 = zero 
wm_interm_8_so_sym_pt1 = zero 
wm_interm_9_so_sym_pt1 = zero 
wm_interm_10_so_sym_pt1 = zero 
wm_interm_11_so_sym_pt1 = zero 
wm_interm_12_so_sym_pt1 = zero 
wm_interm_13_so_sym_pt1 = zero 
wm_interm_14_so_sym_pt1 = zero 
wm_interm_15_so_sym_pt1 = zero 
wm_interm_16_so_sym_pt1 = zero 
wm_interm_17_so_sym_pt1 = zero 
wm_interm_18_so_sym_pt1 = zero 
wm_interm_19_so_sym_pt1 = zero 
wm_interm_20_so_sym_pt1 = zero 

    end subroutine wm_so_sym_intermediates_ccsd_init_pt1
    
    subroutine wm_so_sym_intermediates_ccsd_free_pt1
    deallocate(wm_interm_0_so_sym_pt1)
deallocate(wm_interm_1_so_sym_pt1)
deallocate(wm_interm_2_so_sym_pt1)
deallocate(wm_interm_3_so_sym_pt1)
deallocate(wm_interm_4_so_sym_pt1)
deallocate(wm_interm_5_so_sym_pt1)
deallocate(wm_interm_6_so_sym_pt1)
deallocate(wm_interm_7_so_sym_pt1)
deallocate(wm_interm_8_so_sym_pt1)
deallocate(wm_interm_9_so_sym_pt1)
deallocate(wm_interm_10_so_sym_pt1)
deallocate(wm_interm_11_so_sym_pt1)
deallocate(wm_interm_12_so_sym_pt1)
deallocate(wm_interm_13_so_sym_pt1)
deallocate(wm_interm_14_so_sym_pt1)
deallocate(wm_interm_15_so_sym_pt1)
deallocate(wm_interm_16_so_sym_pt1)
deallocate(wm_interm_17_so_sym_pt1)
deallocate(wm_interm_18_so_sym_pt1)
deallocate(wm_interm_19_so_sym_pt1)
deallocate(wm_interm_20_so_sym_pt1)

    end subroutine wm_so_sym_intermediates_ccsd_free_pt1
    
    subroutine wm_so_sym_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, b, j, c, k 

    !$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
wm_interm_0_so_sym_pt1(b, j) = wm_interm_0_so_sym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
wm_interm_1_so_sym_pt1(b, j) = wm_interm_1_so_sym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_2_so_sym_pt1(b, j) = wm_interm_2_so_sym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_3_so_sym_pt1(b, c) = wm_interm_3_so_sym_pt1(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_4_so_sym_pt1(b, c) = wm_interm_4_so_sym_pt1(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_5_so_sym_pt1(b, c) = wm_interm_5_so_sym_pt1(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_6_so_sym_pt1(j, k) = wm_interm_6_so_sym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_7_so_sym_pt1(j, k) = wm_interm_7_so_sym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_8_so_sym_pt1(j, k) = wm_interm_8_so_sym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_9_so_sym_pt1(b, j) = wm_interm_9_so_sym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_10_so_sym_pt1(b, j) = wm_interm_10_so_sym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_11_so_sym_pt1(b, c) = wm_interm_11_so_sym_pt1(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_12_so_sym_pt1(b, c) = wm_interm_12_so_sym_pt1(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_13_so_sym_pt1(j, k) = wm_interm_13_so_sym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_14_so_sym_pt1(j, k) = wm_interm_14_so_sym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_15_so_sym_pt1(j, k) = wm_interm_15_so_sym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,i,k)
end do 
end do 
end do 
wm_interm_16_so_sym_pt1(j, k) = wm_interm_16_so_sym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_17_so_sym_pt1(j, k) = wm_interm_17_so_sym_pt1(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,j)
end do 
end do 
end do 
wm_interm_18_so_sym_pt1(b, c) = wm_interm_18_so_sym_pt1(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_19_so_sym_pt1(b, c) = wm_interm_19_so_sym_pt1(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_20_so_sym_pt1(b, j) = wm_interm_20_so_sym_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



   end subroutine wm_so_sym_intermediates_ccsd_pt1


    subroutine wm_so_sym_intermediates_ccsd_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_5_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_6_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_7_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_10_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_11_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_so_sym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_14_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_15_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_16_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_17_so_sym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_19_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_20_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_21_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_23_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_24_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_25_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_26_so_sym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_28_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_29_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_30_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_31_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_33_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_34_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_35_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_36_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_37_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_38_so_sym_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_39_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_40_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_41_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_42_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_43_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_44_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_45_so_sym_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_46_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_48_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_50_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_51_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_so_sym_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_53_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_54_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_55_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_56_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_57_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_58_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_59_so_sym_pt2(1: nocc, 1: nocc))
allocate(wm_interm_60_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_so_sym_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_so_sym_pt2 = zero 
wm_interm_1_so_sym_pt2 = zero 
wm_interm_2_so_sym_pt2 = zero 
wm_interm_3_so_sym_pt2 = zero 
wm_interm_4_so_sym_pt2 = zero 
wm_interm_5_so_sym_pt2 = zero 
wm_interm_6_so_sym_pt2 = zero 
wm_interm_7_so_sym_pt2 = zero 
wm_interm_8_so_sym_pt2 = zero 
wm_interm_9_so_sym_pt2 = zero 
wm_interm_10_so_sym_pt2 = zero 
wm_interm_11_so_sym_pt2 = zero 
wm_interm_12_so_sym_pt2 = zero 
wm_interm_13_so_sym_pt2 = zero 
wm_interm_14_so_sym_pt2 = zero 
wm_interm_15_so_sym_pt2 = zero 
wm_interm_16_so_sym_pt2 = zero 
wm_interm_17_so_sym_pt2 = zero 
wm_interm_18_so_sym_pt2 = zero 
wm_interm_19_so_sym_pt2 = zero 
wm_interm_20_so_sym_pt2 = zero 
wm_interm_21_so_sym_pt2 = zero 
wm_interm_22_so_sym_pt2 = zero 
wm_interm_23_so_sym_pt2 = zero 
wm_interm_24_so_sym_pt2 = zero 
wm_interm_25_so_sym_pt2 = zero 
wm_interm_26_so_sym_pt2 = zero 
wm_interm_27_so_sym_pt2 = zero 
wm_interm_28_so_sym_pt2 = zero 
wm_interm_29_so_sym_pt2 = zero 
wm_interm_30_so_sym_pt2 = zero 
wm_interm_31_so_sym_pt2 = zero 
wm_interm_32_so_sym_pt2 = zero 
wm_interm_33_so_sym_pt2 = zero 
wm_interm_34_so_sym_pt2 = zero 
wm_interm_35_so_sym_pt2 = zero 
wm_interm_36_so_sym_pt2 = zero 
wm_interm_37_so_sym_pt2 = zero 
wm_interm_38_so_sym_pt2 = zero 
wm_interm_39_so_sym_pt2 = zero 
wm_interm_40_so_sym_pt2 = zero 
wm_interm_41_so_sym_pt2 = zero 
wm_interm_42_so_sym_pt2 = zero 
wm_interm_43_so_sym_pt2 = zero 
wm_interm_44_so_sym_pt2 = zero 
wm_interm_45_so_sym_pt2 = zero 
wm_interm_46_so_sym_pt2 = zero 
wm_interm_47_so_sym_pt2 = zero 
wm_interm_48_so_sym_pt2 = zero 
wm_interm_49_so_sym_pt2 = zero 
wm_interm_50_so_sym_pt2 = zero 
wm_interm_51_so_sym_pt2 = zero 
wm_interm_52_so_sym_pt2 = zero 
wm_interm_53_so_sym_pt2 = zero 
wm_interm_54_so_sym_pt2 = zero 
wm_interm_55_so_sym_pt2 = zero 
wm_interm_56_so_sym_pt2 = zero 
wm_interm_57_so_sym_pt2 = zero 
wm_interm_58_so_sym_pt2 = zero 
wm_interm_59_so_sym_pt2 = zero 
wm_interm_60_so_sym_pt2 = zero 
wm_interm_61_so_sym_pt2 = zero 
wm_interm_62_so_sym_pt2 = zero 

    end subroutine wm_so_sym_intermediates_ccsd_init_pt2
    
    subroutine wm_so_sym_intermediates_ccsd_free_pt2
    deallocate(wm_interm_0_so_sym_pt2)
deallocate(wm_interm_1_so_sym_pt2)
deallocate(wm_interm_2_so_sym_pt2)
deallocate(wm_interm_3_so_sym_pt2)
deallocate(wm_interm_4_so_sym_pt2)
deallocate(wm_interm_5_so_sym_pt2)
deallocate(wm_interm_6_so_sym_pt2)
deallocate(wm_interm_7_so_sym_pt2)
deallocate(wm_interm_8_so_sym_pt2)
deallocate(wm_interm_9_so_sym_pt2)
deallocate(wm_interm_10_so_sym_pt2)
deallocate(wm_interm_11_so_sym_pt2)
deallocate(wm_interm_12_so_sym_pt2)
deallocate(wm_interm_13_so_sym_pt2)
deallocate(wm_interm_14_so_sym_pt2)
deallocate(wm_interm_15_so_sym_pt2)
deallocate(wm_interm_16_so_sym_pt2)
deallocate(wm_interm_17_so_sym_pt2)
deallocate(wm_interm_18_so_sym_pt2)
deallocate(wm_interm_19_so_sym_pt2)
deallocate(wm_interm_20_so_sym_pt2)
deallocate(wm_interm_21_so_sym_pt2)
deallocate(wm_interm_22_so_sym_pt2)
deallocate(wm_interm_23_so_sym_pt2)
deallocate(wm_interm_24_so_sym_pt2)
deallocate(wm_interm_25_so_sym_pt2)
deallocate(wm_interm_26_so_sym_pt2)
deallocate(wm_interm_27_so_sym_pt2)
deallocate(wm_interm_28_so_sym_pt2)
deallocate(wm_interm_29_so_sym_pt2)
deallocate(wm_interm_30_so_sym_pt2)
deallocate(wm_interm_31_so_sym_pt2)
deallocate(wm_interm_32_so_sym_pt2)
deallocate(wm_interm_33_so_sym_pt2)
deallocate(wm_interm_34_so_sym_pt2)
deallocate(wm_interm_35_so_sym_pt2)
deallocate(wm_interm_36_so_sym_pt2)
deallocate(wm_interm_37_so_sym_pt2)
deallocate(wm_interm_38_so_sym_pt2)
deallocate(wm_interm_39_so_sym_pt2)
deallocate(wm_interm_40_so_sym_pt2)
deallocate(wm_interm_41_so_sym_pt2)
deallocate(wm_interm_42_so_sym_pt2)
deallocate(wm_interm_43_so_sym_pt2)
deallocate(wm_interm_44_so_sym_pt2)
deallocate(wm_interm_45_so_sym_pt2)
deallocate(wm_interm_46_so_sym_pt2)
deallocate(wm_interm_47_so_sym_pt2)
deallocate(wm_interm_48_so_sym_pt2)
deallocate(wm_interm_49_so_sym_pt2)
deallocate(wm_interm_50_so_sym_pt2)
deallocate(wm_interm_51_so_sym_pt2)
deallocate(wm_interm_52_so_sym_pt2)
deallocate(wm_interm_53_so_sym_pt2)
deallocate(wm_interm_54_so_sym_pt2)
deallocate(wm_interm_55_so_sym_pt2)
deallocate(wm_interm_56_so_sym_pt2)
deallocate(wm_interm_57_so_sym_pt2)
deallocate(wm_interm_58_so_sym_pt2)
deallocate(wm_interm_59_so_sym_pt2)
deallocate(wm_interm_60_so_sym_pt2)
deallocate(wm_interm_61_so_sym_pt2)
deallocate(wm_interm_62_so_sym_pt2)

    end subroutine wm_so_sym_intermediates_ccsd_free_pt2
    
    subroutine wm_so_sym_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j, k, c, l 

    !$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,k)
end do 
wm_interm_0_so_sym_pt2(b, i, j, k) = wm_interm_0_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_1_so_sym_pt2(j, k) = wm_interm_1_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_2_so_sym_pt2(j, k) = wm_interm_2_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,k)
end do 
wm_interm_3_so_sym_pt2(b, i, j, k) = wm_interm_3_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_4_so_sym_pt2(b, c) = wm_interm_4_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_5_so_sym_pt2(b, j) = wm_interm_5_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_6_so_sym_pt2(b, c) = wm_interm_6_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_7_so_sym_pt2(b, j) = wm_interm_7_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,k)
end do 
wm_interm_8_so_sym_pt2(b, i, j, k) = wm_interm_8_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
wm_interm_9_so_sym_pt2(b, j) = wm_interm_9_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
wm_interm_10_so_sym_pt2(b, j) = wm_interm_10_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
end do 
wm_interm_11_so_sym_pt2(b, i, j, k) = wm_interm_11_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,i,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_12_so_sym_pt2(i, j, k, l) = wm_interm_12_so_sym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_13_so_sym_pt2(b, c) = wm_interm_13_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_14_so_sym_pt2(b, c) = wm_interm_14_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_15_so_sym_pt2(b, j) = wm_interm_15_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_16_so_sym_pt2(b, c) = wm_interm_16_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_17_so_sym_pt2(i, j, k, l) = wm_interm_17_so_sym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_18_so_sym_pt2(j, k) = wm_interm_18_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_19_so_sym_pt2(j, k) = wm_interm_19_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_20_so_sym_pt2(j, k) = wm_interm_20_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_21_so_sym_pt2(b, j) = wm_interm_21_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_22_so_sym_pt2(b, j) = wm_interm_22_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_23_so_sym_pt2(b, c) = wm_interm_23_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_24_so_sym_pt2(b, c) = wm_interm_24_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_25_so_sym_pt2(b, c) = wm_interm_25_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_26_so_sym_pt2(i, j, k, l) = wm_interm_26_so_sym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_27_so_sym_pt2(j, k) = wm_interm_27_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_28_so_sym_pt2(j, k) = wm_interm_28_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_29_so_sym_pt2(j, k) = wm_interm_29_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_30_so_sym_pt2(b, j) = wm_interm_30_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_31_so_sym_pt2(b, i, j, k) = wm_interm_31_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
end do 
wm_interm_32_so_sym_pt2(b, i, j, k) = wm_interm_32_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_33_so_sym_pt2(b, j, i, k) = wm_interm_33_so_sym_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,i)
end do 
end do 
wm_interm_34_so_sym_pt2(b, j) = wm_interm_34_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_35_so_sym_pt2(b, c) = wm_interm_35_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,j)
end do 
end do 
end do 
wm_interm_36_so_sym_pt2(b, c) = wm_interm_36_so_sym_pt2(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,i,k)
end do 
end do 
end do 
wm_interm_37_so_sym_pt2(j, k) = wm_interm_37_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_38_so_sym_pt2(i, j, k, l) = wm_interm_38_so_sym_pt2(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_39_so_sym_pt2(j, k) = wm_interm_39_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_40_so_sym_pt2(i, j) = wm_interm_40_so_sym_pt2(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i)
end do 
wm_interm_41_so_sym_pt2(a, b) = wm_interm_41_so_sym_pt2(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
end do 
wm_interm_42_so_sym_pt2(a, b) = wm_interm_42_so_sym_pt2(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_43_so_sym_pt2(i, j) = wm_interm_43_so_sym_pt2(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t1(a,j)
end do 
wm_interm_44_so_sym_pt2(i, j) = wm_interm_44_so_sym_pt2(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
end do 
wm_interm_45_so_sym_pt2(a, b) = wm_interm_45_so_sym_pt2(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s1(a,i)
end do 
end do 
wm_interm_46_so_sym_pt2(b, j) = wm_interm_46_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,i)
end do 
end do 
wm_interm_47_so_sym_pt2(b, j) = wm_interm_47_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_48_so_sym_pt2(b, j) = wm_interm_48_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_49_so_sym_pt2(b, j) = wm_interm_49_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_50_so_sym_pt2(b, j) = wm_interm_50_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_51_so_sym_pt2(b, j) = wm_interm_51_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_52_so_sym_pt2(b, j) = wm_interm_52_so_sym_pt2(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_53_so_sym_pt2(j, k) = wm_interm_53_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_54_so_sym_pt2(j, k) = wm_interm_54_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, b,k,a,i)
end do 
end do 
end do 
wm_interm_55_so_sym_pt2(j, k) = wm_interm_55_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_56_so_sym_pt2(b, i, j, k) = wm_interm_56_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,k)
end do 
wm_interm_57_so_sym_pt2(b, i, j, k) = wm_interm_57_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_58_so_sym_pt2(j, k) = wm_interm_58_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_59_so_sym_pt2(j, k) = wm_interm_59_so_sym_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_60_so_sym_pt2(b, j, i, k) = wm_interm_60_so_sym_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_61_so_sym_pt2(b, i, j, k) = wm_interm_61_so_sym_pt2(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_62_so_sym_pt2(b, j, i, k) = wm_interm_62_so_sym_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_sym_intermediates_ccsd_pt2

    subroutine wm_so_sym_intermediates_ccsd_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_sym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_so_sym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_3_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_4_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_5_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_6_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_7_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_8_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_9_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_10_so_sym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_11_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_so_sym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_14_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_15_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_16_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_17_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_18_so_sym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_19_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_20_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_22_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_23_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_25_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_28_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_29_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_30_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_31_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_32_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_33_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_34_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_35_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_36_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_37_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_38_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_39_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_40_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_43_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_44_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_45_so_sym_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_46_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_48_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_49_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_50_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_51_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_52_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_53_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_54_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_55_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_56_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_57_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_58_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_59_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_62_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_63_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_64_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_65_so_sym_pt3(1: nocc, 1: nocc))
allocate(wm_interm_66_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_67_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_68_so_sym_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_69_so_sym_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_70_so_sym_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_so_sym_pt3 = zero 
wm_interm_1_so_sym_pt3 = zero 
wm_interm_2_so_sym_pt3 = zero 
wm_interm_3_so_sym_pt3 = zero 
wm_interm_4_so_sym_pt3 = zero 
wm_interm_5_so_sym_pt3 = zero 
wm_interm_6_so_sym_pt3 = zero 
wm_interm_7_so_sym_pt3 = zero 
wm_interm_8_so_sym_pt3 = zero 
wm_interm_9_so_sym_pt3 = zero 
wm_interm_10_so_sym_pt3 = zero 
wm_interm_11_so_sym_pt3 = zero 
wm_interm_12_so_sym_pt3 = zero 
wm_interm_13_so_sym_pt3 = zero 
wm_interm_14_so_sym_pt3 = zero 
wm_interm_15_so_sym_pt3 = zero 
wm_interm_16_so_sym_pt3 = zero 
wm_interm_17_so_sym_pt3 = zero 
wm_interm_18_so_sym_pt3 = zero 
wm_interm_19_so_sym_pt3 = zero 
wm_interm_20_so_sym_pt3 = zero 
wm_interm_21_so_sym_pt3 = zero 
wm_interm_22_so_sym_pt3 = zero 
wm_interm_23_so_sym_pt3 = zero 
wm_interm_24_so_sym_pt3 = zero 
wm_interm_25_so_sym_pt3 = zero 
wm_interm_26_so_sym_pt3 = zero 
wm_interm_27_so_sym_pt3 = zero 
wm_interm_28_so_sym_pt3 = zero 
wm_interm_29_so_sym_pt3 = zero 
wm_interm_30_so_sym_pt3 = zero 
wm_interm_31_so_sym_pt3 = zero 
wm_interm_32_so_sym_pt3 = zero 
wm_interm_33_so_sym_pt3 = zero 
wm_interm_34_so_sym_pt3 = zero 
wm_interm_35_so_sym_pt3 = zero 
wm_interm_36_so_sym_pt3 = zero 
wm_interm_37_so_sym_pt3 = zero 
wm_interm_38_so_sym_pt3 = zero 
wm_interm_39_so_sym_pt3 = zero 
wm_interm_40_so_sym_pt3 = zero 
wm_interm_41_so_sym_pt3 = zero 
wm_interm_42_so_sym_pt3 = zero 
wm_interm_43_so_sym_pt3 = zero 
wm_interm_44_so_sym_pt3 = zero 
wm_interm_45_so_sym_pt3 = zero 
wm_interm_46_so_sym_pt3 = zero 
wm_interm_47_so_sym_pt3 = zero 
wm_interm_48_so_sym_pt3 = zero 
wm_interm_49_so_sym_pt3 = zero 
wm_interm_50_so_sym_pt3 = zero 
wm_interm_51_so_sym_pt3 = zero 
wm_interm_52_so_sym_pt3 = zero 
wm_interm_53_so_sym_pt3 = zero 
wm_interm_54_so_sym_pt3 = zero 
wm_interm_55_so_sym_pt3 = zero 
wm_interm_56_so_sym_pt3 = zero 
wm_interm_57_so_sym_pt3 = zero 
wm_interm_58_so_sym_pt3 = zero 
wm_interm_59_so_sym_pt3 = zero 
wm_interm_60_so_sym_pt3 = zero 
wm_interm_61_so_sym_pt3 = zero 
wm_interm_62_so_sym_pt3 = zero 
wm_interm_63_so_sym_pt3 = zero 
wm_interm_64_so_sym_pt3 = zero 
wm_interm_65_so_sym_pt3 = zero 
wm_interm_66_so_sym_pt3 = zero 
wm_interm_67_so_sym_pt3 = zero 
wm_interm_68_so_sym_pt3 = zero 
wm_interm_69_so_sym_pt3 = zero 
wm_interm_70_so_sym_pt3 = zero 

    end subroutine wm_so_sym_intermediates_ccsd_init_pt3
    
    subroutine wm_so_sym_intermediates_ccsd_free_pt3
    deallocate(wm_interm_0_so_sym_pt3)
deallocate(wm_interm_1_so_sym_pt3)
deallocate(wm_interm_2_so_sym_pt3)
deallocate(wm_interm_3_so_sym_pt3)
deallocate(wm_interm_4_so_sym_pt3)
deallocate(wm_interm_5_so_sym_pt3)
deallocate(wm_interm_6_so_sym_pt3)
deallocate(wm_interm_7_so_sym_pt3)
deallocate(wm_interm_8_so_sym_pt3)
deallocate(wm_interm_9_so_sym_pt3)
deallocate(wm_interm_10_so_sym_pt3)
deallocate(wm_interm_11_so_sym_pt3)
deallocate(wm_interm_12_so_sym_pt3)
deallocate(wm_interm_13_so_sym_pt3)
deallocate(wm_interm_14_so_sym_pt3)
deallocate(wm_interm_15_so_sym_pt3)
deallocate(wm_interm_16_so_sym_pt3)
deallocate(wm_interm_17_so_sym_pt3)
deallocate(wm_interm_18_so_sym_pt3)
deallocate(wm_interm_19_so_sym_pt3)
deallocate(wm_interm_20_so_sym_pt3)
deallocate(wm_interm_21_so_sym_pt3)
deallocate(wm_interm_22_so_sym_pt3)
deallocate(wm_interm_23_so_sym_pt3)
deallocate(wm_interm_24_so_sym_pt3)
deallocate(wm_interm_25_so_sym_pt3)
deallocate(wm_interm_26_so_sym_pt3)
deallocate(wm_interm_27_so_sym_pt3)
deallocate(wm_interm_28_so_sym_pt3)
deallocate(wm_interm_29_so_sym_pt3)
deallocate(wm_interm_30_so_sym_pt3)
deallocate(wm_interm_31_so_sym_pt3)
deallocate(wm_interm_32_so_sym_pt3)
deallocate(wm_interm_33_so_sym_pt3)
deallocate(wm_interm_34_so_sym_pt3)
deallocate(wm_interm_35_so_sym_pt3)
deallocate(wm_interm_36_so_sym_pt3)
deallocate(wm_interm_37_so_sym_pt3)
deallocate(wm_interm_38_so_sym_pt3)
deallocate(wm_interm_39_so_sym_pt3)
deallocate(wm_interm_40_so_sym_pt3)
deallocate(wm_interm_41_so_sym_pt3)
deallocate(wm_interm_42_so_sym_pt3)
deallocate(wm_interm_43_so_sym_pt3)
deallocate(wm_interm_44_so_sym_pt3)
deallocate(wm_interm_45_so_sym_pt3)
deallocate(wm_interm_46_so_sym_pt3)
deallocate(wm_interm_47_so_sym_pt3)
deallocate(wm_interm_48_so_sym_pt3)
deallocate(wm_interm_49_so_sym_pt3)
deallocate(wm_interm_50_so_sym_pt3)
deallocate(wm_interm_51_so_sym_pt3)
deallocate(wm_interm_52_so_sym_pt3)
deallocate(wm_interm_53_so_sym_pt3)
deallocate(wm_interm_54_so_sym_pt3)
deallocate(wm_interm_55_so_sym_pt3)
deallocate(wm_interm_56_so_sym_pt3)
deallocate(wm_interm_57_so_sym_pt3)
deallocate(wm_interm_58_so_sym_pt3)
deallocate(wm_interm_59_so_sym_pt3)
deallocate(wm_interm_60_so_sym_pt3)
deallocate(wm_interm_61_so_sym_pt3)
deallocate(wm_interm_62_so_sym_pt3)
deallocate(wm_interm_63_so_sym_pt3)
deallocate(wm_interm_64_so_sym_pt3)
deallocate(wm_interm_65_so_sym_pt3)
deallocate(wm_interm_66_so_sym_pt3)
deallocate(wm_interm_67_so_sym_pt3)
deallocate(wm_interm_68_so_sym_pt3)
deallocate(wm_interm_69_so_sym_pt3)
deallocate(wm_interm_70_so_sym_pt3)

    end subroutine wm_so_sym_intermediates_ccsd_free_pt3
    
    subroutine wm_so_sym_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, j, i, k, l, c 

    !$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + s2(a,b,j,i) * t1(a,k)
end do 
wm_interm_0_so_sym_pt3(b, j, i, k) = wm_interm_0_so_sym_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_1_so_sym_pt3(i, j, k, l) = wm_interm_1_so_sym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,a,l)
end do 
end do 
wm_interm_2_so_sym_pt3(i, j, k, l) = wm_interm_2_so_sym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, b,k,a,i)
end do 
end do 
end do 
wm_interm_3_so_sym_pt3(j, k) = wm_interm_3_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_4_so_sym_pt3(j, k) = wm_interm_4_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_5_so_sym_pt3(j, k) = wm_interm_5_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_6_so_sym_pt3(b, j, i, k) = wm_interm_6_so_sym_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_7_so_sym_pt3(j, k) = wm_interm_7_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_8_so_sym_pt3(j, k) = wm_interm_8_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_9_so_sym_pt3(j, k) = wm_interm_9_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_10_so_sym_pt3(i, j, k, l) = wm_interm_10_so_sym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_11_so_sym_pt3(b, i, j, k) = wm_interm_11_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_12_so_sym_pt3(i, j, k, l) = wm_interm_12_so_sym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_13_so_sym_pt3(j, k) = wm_interm_13_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_14_so_sym_pt3(j, k) = wm_interm_14_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_15_so_sym_pt3(j, k) = wm_interm_15_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_16_so_sym_pt3(j, k) = wm_interm_16_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_17_so_sym_pt3(j, k) = wm_interm_17_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_18_so_sym_pt3(i, j, k, l) = wm_interm_18_so_sym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_19_so_sym_pt3(b, j) = wm_interm_19_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_20_so_sym_pt3(b, j) = wm_interm_20_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, k, j, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,k,j)
end do 
wm_interm_21_so_sym_pt3(b, i, k, j) = wm_interm_21_so_sym_pt3(b, i, k, j) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,i,k)
end do 
end do 
end do 
wm_interm_22_so_sym_pt3(j, k) = wm_interm_22_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,k)
end do 
wm_interm_23_so_sym_pt3(b, i, j, k) = wm_interm_23_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_24_so_sym_pt3(j, k) = wm_interm_24_so_sym_pt3(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_25_so_sym_pt3(b, i, j, k) = wm_interm_25_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_26_so_sym_pt3(b, j, i, k) = wm_interm_26_so_sym_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_27_so_sym_pt3(b, i, j, k) = wm_interm_27_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_28_so_sym_pt3(b, j) = wm_interm_28_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,i,b,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_29_so_sym_pt3(b, c) = wm_interm_29_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_30_so_sym_pt3(b, c) = wm_interm_30_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_31_so_sym_pt3(b, c) = wm_interm_31_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_32_so_sym_pt3(b, j) = wm_interm_32_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_33_so_sym_pt3(b, c) = wm_interm_33_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_34_so_sym_pt3(b, c) = wm_interm_34_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, b,j,a,i) * s1(a,i)
end do 
end do 
wm_interm_35_so_sym_pt3(b, j) = wm_interm_35_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s1(a,i)
end do 
end do 
wm_interm_36_so_sym_pt3(b, j) = wm_interm_36_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_37_so_sym_pt3(b, j) = wm_interm_37_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,i,j)
end do 
end do 
end do 
wm_interm_38_so_sym_pt3(b, c) = wm_interm_38_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_39_so_sym_pt3(b, c) = wm_interm_39_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_40_so_sym_pt3(b, j) = wm_interm_40_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_41_so_sym_pt3(b, j) = wm_interm_41_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,j,a,i)
end do 
end do 
end do 
wm_interm_42_so_sym_pt3(b, c) = wm_interm_42_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, c,i,a,j)
end do 
end do 
end do 
wm_interm_43_so_sym_pt3(b, c) = wm_interm_43_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_44_so_sym_pt3(b, c) = wm_interm_44_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, l, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
do l = 1, nocc 
sum = zero 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_45_so_sym_pt3(i, j, k, l) = wm_interm_45_so_sym_pt3(i, j, k, l) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + s1(a,i) * t2(a,b,j,k)
end do 
wm_interm_46_so_sym_pt3(b, i, j, k) = wm_interm_46_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,c,j)
end do 
end do 
end do 
wm_interm_47_so_sym_pt3(b, c) = wm_interm_47_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, b, c, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do c = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_48_so_sym_pt3(b, c) = wm_interm_48_so_sym_pt3(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,k)
end do 
wm_interm_49_so_sym_pt3(b, i, j, k) = wm_interm_49_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,k)
end do 
wm_interm_50_so_sym_pt3(b, i, j, k) = wm_interm_50_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * t1(b,i)
end do 
wm_interm_51_so_sym_pt3(a, b) = wm_interm_51_so_sym_pt3(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t1(a,j)
end do 
wm_interm_52_so_sym_pt3(i, j) = wm_interm_52_so_sym_pt3(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,k)
end do 
wm_interm_53_so_sym_pt3(b, i, j, k) = wm_interm_53_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,i,a,j)
end do 
end do 
wm_interm_54_so_sym_pt3(b, j) = wm_interm_54_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
wm_interm_55_so_sym_pt3(b, j) = wm_interm_55_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_56_so_sym_pt3(b, j) = wm_interm_56_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_57_so_sym_pt3(b, j) = wm_interm_57_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
wm_interm_58_so_sym_pt3(b, j) = wm_interm_58_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_59_so_sym_pt3(b, i, j, k) = wm_interm_59_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, j, i, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_60_so_sym_pt3(b, j, i, k) = wm_interm_60_so_sym_pt3(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
end do 
wm_interm_61_so_sym_pt3(a, b) = wm_interm_61_so_sym_pt3(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s1(a,j)
end do 
wm_interm_62_so_sym_pt3(i, j) = wm_interm_62_so_sym_pt3(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_63_so_sym_pt3(b, j) = wm_interm_63_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,i)
end do 
end do 
wm_interm_64_so_sym_pt3(b, j) = wm_interm_64_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do i = 1, nocc 
do j = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,j)
end do 
wm_interm_65_so_sym_pt3(i, j) = wm_interm_65_so_sym_pt3(i, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,j,k)
end do 
wm_interm_66_so_sym_pt3(b, i, j, k) = wm_interm_66_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,i)
end do 
end do 
wm_interm_67_so_sym_pt3(b, j) = wm_interm_67_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, b,i)
end do 
wm_interm_68_so_sym_pt3(a, b) = wm_interm_68_so_sym_pt3(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, i, b, j, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do b = nocc + 1, nactive 
do j = 1, nocc 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,i,j)
end do 
end do 
wm_interm_69_so_sym_pt3(b, j) = wm_interm_69_so_sym_pt3(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(4)
do b = nocc + 1, nactive 
do i = 1, nocc 
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
end do 
wm_interm_70_so_sym_pt3(b, i, j, k) = wm_interm_70_so_sym_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_sym_intermediates_ccsd_pt3


    function calc_D_oo_wm_so_sym_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_sym_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, j 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,i,b,q)
term(1) = term(1) + r2(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
term(2) = term(2) + r2(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, b,q,a,i)
term(3) = term(3) + r2(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
term(4) = term(4) + r2(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
end do 
end do 
end do 

term(1) = term(1) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(5) = term(5) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
end do 

term(5) = term(5) * (-2.0d+0) 


    calc_D_oo_wm_so_sym_pt0 = zero
    do s = 0, 5
    calc_D_oo_wm_so_sym_pt0 = calc_D_oo_wm_so_sym_pt0 + term(s)
    end do

    end function calc_D_oo_wm_so_sym_pt0
    
    function calc_D_ov_wm_so_sym_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_sym_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    calc_D_ov_wm_so_sym_pt0 = zero

    end function calc_D_ov_wm_so_sym_pt0
    
    function calc_D_vo_wm_so_sym_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_sym_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,a,q)
end do 
end do 

term(0) = term(0) * (-1.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(1) = term(1) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,a,i)
term(2) = term(2) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, a,q,p,i)
term(3) = term(3) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,q,p,i)
term(4) = term(4) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,p,q)
term(5) = term(5) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
end do 
end do 

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 


    calc_D_vo_wm_so_sym_pt0 = zero
    do s = 0, 5
    calc_D_vo_wm_so_sym_pt0 = calc_D_vo_wm_so_sym_pt0 + term(s)
    end do

    end function calc_D_vo_wm_so_sym_pt0
    
    function calc_D_vv_wm_so_sym_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_sym_pt0

    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, j, b 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(0) = term(0) + r2(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, q,j,a,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1) = term(1) + r2(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, q,i,a,j)
term(2) = term(2) + r2(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, a,i,q,j)
term(3) = term(3) + r2(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,i,q,j)
end do 
end do 
end do 

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + r2(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,j,q,i)
end do 
end do 
end do 

term(4) = term(4) * (4.0d+0) 

do i = 1, nocc 
term(5) = term(5) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
end do 

term(5) = term(5) * (2.0d+0) 


    calc_D_vv_wm_so_sym_pt0 = zero
    do s = 0, 5
    calc_D_vv_wm_so_sym_pt0 = calc_D_vv_wm_so_sym_pt0 + term(s)
    end do

    end function calc_D_vv_wm_so_sym_pt0

    function calc_D_oo_wm_so_sym_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_sym_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_oo_wm_so_sym_pt1 = zero

    end function calc_D_oo_wm_so_sym_pt1
    
    function calc_D_ov_wm_so_sym_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_sym_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:6) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, a,p) * wm_interm_3_so_sym_pt1(a,q)
term(1) = term(1) + r1(vrdav_Rl, a,p) * wm_interm_4_so_sym_pt1(a,q)
term(2) = term(2) + r1(vrdav_Rl, a,p) * wm_interm_5_so_sym_pt1(a,q)
term(3) = term(3) + r1(vrdav_Rl, a,p) * wm_interm_11_so_sym_pt1(a,q)
term(4) = term(4) + r1(vrdav_Rl, a,p) * wm_interm_12_so_sym_pt1(a,q)
term(5) = term(5) + r1(vrdav_Rr, a,p) * wm_interm_18_so_sym_pt1(a,q)
term(6) = term(6) + r1(vrdav_Rr, a,p) * wm_interm_19_so_sym_pt1(a,q)
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 


    calc_D_ov_wm_so_sym_pt1 = zero
    do s = 0, 6
    calc_D_ov_wm_so_sym_pt1 = calc_D_ov_wm_so_sym_pt1 + term(s)
    end do

    end function calc_D_ov_wm_so_sym_pt1
    
    function calc_D_vo_wm_so_sym_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_sym_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, b, j 
    real(F64), dimension(0:13) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, p,i) * wm_interm_6_so_sym_pt1(i,q)
term(1) = term(1) + r1(vrdav_Rl, p,i) * wm_interm_7_so_sym_pt1(i,q)
term(2) = term(2) + r1(vrdav_Rl, p,i) * wm_interm_8_so_sym_pt1(i,q)
term(3) = term(3) + r1(vrdav_Rl, p,i) * wm_interm_13_so_sym_pt1(i,q)
term(4) = term(4) + r1(vrdav_Rl, p,i) * wm_interm_14_so_sym_pt1(i,q)
term(5) = term(5) + r1(vrdav_Rl, p,i) * wm_interm_15_so_sym_pt1(i,q)
term(6) = term(6) + r1(vrdav_Rr, p,i) * wm_interm_16_so_sym_pt1(i,q)
term(7) = term(7) + r1(vrdav_Rr, p,i) * wm_interm_17_so_sym_pt1(i,q)
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (4.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(8) = term(8) + s2(a,p,q,i) * wm_interm_0_so_sym_pt1(a,i)
term(9) = term(9) + s2(a,p,q,i) * wm_interm_1_so_sym_pt1(a,i)
term(10) = term(10) + s2(a,p,q,i) * wm_interm_2_so_sym_pt1(a,i)
term(11) = term(11) + s2(a,p,q,i) * wm_interm_9_so_sym_pt1(a,i)
term(12) = term(12) + s2(a,p,q,i) * wm_interm_10_so_sym_pt1(a,i)
term(13) = term(13) + t2(a,p,q,i) * wm_interm_20_so_sym_pt1(a,i)
end do 
end do 

term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (-2.0d+0) 


    calc_D_vo_wm_so_sym_pt1 = zero
    do s = 0, 13
    calc_D_vo_wm_so_sym_pt1 = calc_D_vo_wm_so_sym_pt1 + term(s)
    end do

    end function calc_D_vo_wm_so_sym_pt1
    
    function calc_D_vv_wm_so_sym_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_sym_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:0) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vv_wm_so_sym_pt1 = zero

    end function calc_D_vv_wm_so_sym_pt1

    function calc_D_oo_wm_so_sym_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_sym_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b 
    real(F64), dimension(0:29) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_11_so_sym_pt2(a,q,i,j) * wm_interm_32_so_sym_pt2(a,p,j,i)
term(1) = term(1) + wm_interm_11_so_sym_pt2(a,q,i,j) * wm_interm_32_so_sym_pt2(a,p,i,j)
term(2) = term(2) + wm_interm_11_so_sym_pt2(a,i,q,j) * wm_interm_32_so_sym_pt2(a,i,j,p)
term(3) = term(3) + wm_interm_11_so_sym_pt2(a,i,q,j) * wm_interm_32_so_sym_pt2(a,i,p,j)
term(4) = term(4) + wm_interm_11_so_sym_pt2(a,i,j,q) * wm_interm_32_so_sym_pt2(a,i,p,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(5) = term(5) + wm_interm_34_so_sym_pt2(a,p) * wm_interm_7_so_sym_pt2(a,q)
term(6) = term(6) + wm_interm_34_so_sym_pt2(a,p) * wm_interm_5_so_sym_pt2(a,q)
term(7) = term(7) + r1(vrdav_Rl, a,q) * wm_interm_46_so_sym_pt2(a,p)
term(8) = term(8) + r1(vrdav_Rl, a,q) * wm_interm_47_so_sym_pt2(a,p)
term(9) = term(9) + r1(vrdav_Rl, a,q) * wm_interm_48_so_sym_pt2(a,p)
term(10) = term(10) + r1(vrdav_Rl, a,q) * wm_interm_49_so_sym_pt2(a,p)
term(11) = term(11) + r1(vrdav_Rl, a,q) * wm_interm_50_so_sym_pt2(a,p)
term(12) = term(12) + s1(a,q) * wm_interm_9_so_sym_pt2(a,p)
term(13) = term(13) + s1(a,q) * wm_interm_10_so_sym_pt2(a,p)
term(14) = term(14) + s1(a,q) * wm_interm_15_so_sym_pt2(a,p)
term(15) = term(15) + s1(a,q) * wm_interm_21_so_sym_pt2(a,p)
term(16) = term(16) + s1(a,q) * wm_interm_22_so_sym_pt2(a,p)
term(17) = term(17) + r1(vrdav_Rr, a,p) * wm_interm_51_so_sym_pt2(a,q)
term(18) = term(18) + r1(vrdav_Rr, a,p) * wm_interm_52_so_sym_pt2(a,q)
term(19) = term(19) + t1(a,q) * wm_interm_30_so_sym_pt2(a,p)
end do 

term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-2.0d+0) 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-1.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (4.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(20) = term(20) + wm_interm_12_so_sym_pt2(p,i,j,q) * wm_interm_40_so_sym_pt2(j,i)
end do 
end do 

term(20) = term(20) * (-2.0d+0) 

do i = 1, nocc 
term(21) = term(21) + wm_interm_1_so_sym_pt2(q,i) * wm_interm_40_so_sym_pt2(i,p)
term(22) = term(22) + wm_interm_2_so_sym_pt2(q,i) * wm_interm_40_so_sym_pt2(i,p)
term(23) = term(23) + wm_interm_1_so_sym_pt2(i,q) * wm_interm_40_so_sym_pt2(p,i)
term(24) = term(24) + wm_interm_2_so_sym_pt2(i,q) * wm_interm_40_so_sym_pt2(p,i)
end do 

term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(25) = term(25) + wm_interm_32_so_sym_pt2(a,p,i,q) * wm_interm_5_so_sym_pt2(a,i)
term(26) = term(26) + wm_interm_32_so_sym_pt2(a,p,q,i) * wm_interm_5_so_sym_pt2(a,i)
term(27) = term(27) + wm_interm_32_so_sym_pt2(a,p,i,q) * wm_interm_7_so_sym_pt2(a,i)
term(28) = term(28) + wm_interm_32_so_sym_pt2(a,p,q,i) * wm_interm_7_so_sym_pt2(a,i)
term(29) = term(29) + wm_interm_11_so_sym_pt2(a,p,i,q) * wm_interm_34_so_sym_pt2(a,i)
end do 
end do 

term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-2.0d+0) 


    calc_D_oo_wm_so_sym_pt2 = zero
    do s = 0, 29
    calc_D_oo_wm_so_sym_pt2 = calc_D_oo_wm_so_sym_pt2 + term(s)
    end do

    end function calc_D_oo_wm_so_sym_pt2
    
    function calc_D_ov_wm_so_sym_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_sym_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:72) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_11_so_sym_pt2(q,i,j,k) * wm_interm_17_so_sym_pt2(p,i,j,k)
term(1) = term(1) + wm_interm_11_so_sym_pt2(q,i,j,k) * wm_interm_17_so_sym_pt2(i,p,k,j)
term(2) = term(2) + wm_interm_11_so_sym_pt2(q,i,j,k) * wm_interm_17_so_sym_pt2(p,i,k,j)
term(3) = term(3) + wm_interm_11_so_sym_pt2(q,i,j,k) * wm_interm_26_so_sym_pt2(p,i,j,k)
term(4) = term(4) + wm_interm_11_so_sym_pt2(q,i,j,k) * wm_interm_26_so_sym_pt2(i,p,k,j)
term(5) = term(5) + wm_interm_11_so_sym_pt2(q,i,j,k) * wm_interm_26_so_sym_pt2(p,i,k,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(6) = term(6) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_0_so_sym_pt2(a,k,j,p)
term(7) = term(7) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_0_so_sym_pt2(a,k,p,j)
term(8) = term(8) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_3_so_sym_pt2(a,k,p,j)
term(9) = term(9) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_0_so_sym_pt2(a,k,p,j)
term(10) = term(10) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_3_so_sym_pt2(a,k,p,j)
term(11) = term(11) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_8_so_sym_pt2(a,k,p,j)
term(12) = term(12) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_8_so_sym_pt2(a,k,j,p)
term(13) = term(13) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_8_so_sym_pt2(a,k,p,j)
term(14) = term(14) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_32_so_sym_pt2(b,i,k,p)
end do 
end do 
end do 
end do 
end do 

term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-2.0d+0) 

do i = 1, nocc 
term(15) = term(15) + wm_interm_18_so_sym_pt2(p,i) * wm_interm_7_so_sym_pt2(q,i)
term(16) = term(16) + wm_interm_19_so_sym_pt2(p,i) * wm_interm_7_so_sym_pt2(q,i)
term(17) = term(17) + wm_interm_20_so_sym_pt2(p,i) * wm_interm_7_so_sym_pt2(q,i)
term(18) = term(18) + wm_interm_18_so_sym_pt2(p,i) * wm_interm_5_so_sym_pt2(q,i)
term(19) = term(19) + wm_interm_19_so_sym_pt2(p,i) * wm_interm_5_so_sym_pt2(q,i)
term(20) = term(20) + wm_interm_20_so_sym_pt2(p,i) * wm_interm_5_so_sym_pt2(q,i)
term(21) = term(21) + wm_interm_27_so_sym_pt2(p,i) * wm_interm_7_so_sym_pt2(q,i)
term(22) = term(22) + wm_interm_28_so_sym_pt2(p,i) * wm_interm_7_so_sym_pt2(q,i)
term(23) = term(23) + wm_interm_29_so_sym_pt2(p,i) * wm_interm_7_so_sym_pt2(q,i)
term(24) = term(24) + wm_interm_27_so_sym_pt2(p,i) * wm_interm_5_so_sym_pt2(q,i)
term(25) = term(25) + wm_interm_28_so_sym_pt2(p,i) * wm_interm_5_so_sym_pt2(q,i)
term(26) = term(26) + wm_interm_29_so_sym_pt2(p,i) * wm_interm_5_so_sym_pt2(q,i)
term(27) = term(27) + wm_interm_1_so_sym_pt2(p,i) * wm_interm_30_so_sym_pt2(q,i)
term(28) = term(28) + wm_interm_2_so_sym_pt2(p,i) * wm_interm_30_so_sym_pt2(q,i)
term(29) = term(29) + s1(q,i) * wm_interm_53_so_sym_pt2(p,i)
term(30) = term(30) + s1(q,i) * wm_interm_54_so_sym_pt2(p,i)
term(31) = term(31) + s1(q,i) * wm_interm_55_so_sym_pt2(p,i)
term(32) = term(32) + s1(q,i) * wm_interm_58_so_sym_pt2(p,i)
term(33) = term(33) + s1(q,i) * wm_interm_59_so_sym_pt2(p,i)
term(34) = term(34) + t1(q,i) * wm_interm_53_so_sym_pt2(i,p)
term(35) = term(35) + t1(q,i) * wm_interm_54_so_sym_pt2(i,p)
term(36) = term(36) + t1(q,i) * wm_interm_55_so_sym_pt2(i,p)
term(37) = term(37) + t1(q,i) * wm_interm_58_so_sym_pt2(i,p)
term(38) = term(38) + t1(q,i) * wm_interm_59_so_sym_pt2(i,p)
end do 

term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-1.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (4.0d+0) 
term(25) = term(25) * (-8.0d+0) 
term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-1.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (4.0d+0) 
term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (2.0d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (-4.0d+0) 
term(38) = term(38) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(39) = term(39) + wm_interm_12_so_sym_pt2(i,p,j,k) * wm_interm_33_so_sym_pt2(q,j,k,i)
term(40) = term(40) + wm_interm_12_so_sym_pt2(p,i,j,k) * wm_interm_31_so_sym_pt2(q,j,k,i)
end do 
end do 
end do 

term(39) = term(39) * (-1.0d+0) 
term(40) = term(40) * (-1.0d+0) 

do a = nocc + 1, nactive 
term(41) = term(41) + wm_interm_4_so_sym_pt2(a,q) * wm_interm_9_so_sym_pt2(a,p)
term(42) = term(42) + wm_interm_10_so_sym_pt2(a,p) * wm_interm_4_so_sym_pt2(a,q)
term(43) = term(43) + wm_interm_15_so_sym_pt2(a,p) * wm_interm_4_so_sym_pt2(a,q)
term(44) = term(44) + wm_interm_6_so_sym_pt2(a,q) * wm_interm_9_so_sym_pt2(a,p)
term(45) = term(45) + wm_interm_10_so_sym_pt2(a,p) * wm_interm_6_so_sym_pt2(a,q)
term(46) = term(46) + wm_interm_15_so_sym_pt2(a,p) * wm_interm_6_so_sym_pt2(a,q)
term(47) = term(47) + wm_interm_21_so_sym_pt2(a,p) * wm_interm_4_so_sym_pt2(a,q)
term(48) = term(48) + wm_interm_22_so_sym_pt2(a,p) * wm_interm_4_so_sym_pt2(a,q)
term(49) = term(49) + wm_interm_21_so_sym_pt2(a,p) * wm_interm_6_so_sym_pt2(a,q)
term(50) = term(50) + wm_interm_22_so_sym_pt2(a,p) * wm_interm_6_so_sym_pt2(a,q)
term(51) = term(51) + wm_interm_34_so_sym_pt2(a,p) * wm_interm_35_so_sym_pt2(q,a)
term(52) = term(52) + wm_interm_34_so_sym_pt2(a,p) * wm_interm_36_so_sym_pt2(q,a)
term(53) = term(53) + r1(vrdav_Rl, a,p) * wm_interm_42_so_sym_pt2(a,q)
term(54) = term(54) + r1(vrdav_Rr, a,p) * wm_interm_45_so_sym_pt2(a,q)
end do 

term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-1.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (8.0d+0) 
term(50) = term(50) * (-8.0d+0) 
term(51) = term(51) * (4.0d+0) 
term(52) = term(52) * (-2.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(55) = term(55) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_3_so_sym_pt2(a,k,j,p)
term(56) = term(56) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_8_so_sym_pt2(a,k,j,p)
end do 
end do 
end do 
end do 
end do 

term(55) = term(55) * (-1.0d+0) 
term(56) = term(56) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(57) = term(57) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_0_so_sym_pt2(a,i,p,k)
term(58) = term(58) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_3_so_sym_pt2(a,i,k,p)
term(59) = term(59) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_3_so_sym_pt2(a,i,p,k)
term(60) = term(60) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_3_so_sym_pt2(a,i,j,p)
term(61) = term(61) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_8_so_sym_pt2(a,i,k,p)
term(62) = term(62) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_8_so_sym_pt2(a,i,p,k)
term(63) = term(63) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_8_so_sym_pt2(a,i,j,p)
term(64) = term(64) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_32_so_sym_pt2(b,i,k,p)
term(65) = term(65) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_32_so_sym_pt2(b,i,p,k)
term(66) = term(66) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_32_so_sym_pt2(b,j,k,p)
term(67) = term(67) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_32_so_sym_pt2(b,j,p,k)
term(68) = term(68) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_32_so_sym_pt2(b,j,p,k)
end do 
end do 
end do 
end do 
end do 

term(57) = term(57) * (-1.0d+0) 
term(58) = term(58) * (-1.0d+0) 
term(59) = term(59) * (2.0d+0) 
term(60) = term(60) * (2.0d+0) 
term(61) = term(61) * (-4.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (8.0d+0) 
term(64) = term(64) * (4.0d+0) 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * (-2.0d+0) 
term(67) = term(67) * (4.0d+0) 
term(68) = term(68) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(69) = term(69) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_0_so_sym_pt2(a,i,p,j)
term(70) = term(70) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_3_so_sym_pt2(a,i,p,j)
term(71) = term(71) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_8_so_sym_pt2(a,i,p,j)
term(72) = term(72) + r2(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_32_so_sym_pt2(b,i,p,k)
end do 
end do 
end do 
end do 
end do 

term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * (4.0d+0) 


    calc_D_ov_wm_so_sym_pt2 = zero
    do s = 0, 72
    calc_D_ov_wm_so_sym_pt2 = calc_D_ov_wm_so_sym_pt2 + term(s)
    end do

    end function calc_D_ov_wm_so_sym_pt2
    
    function calc_D_vo_wm_so_sym_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_sym_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:170) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_0_so_sym_pt2(p,i,j,k) * wm_interm_12_so_sym_pt2(j,k,i,q)
term(1) = term(1) + wm_interm_0_so_sym_pt2(p,i,j,k) * wm_interm_12_so_sym_pt2(k,j,i,q)
term(2) = term(2) + wm_interm_32_so_sym_pt2(p,i,j,k) * wm_interm_38_so_sym_pt2(q,i,j,k)
term(3) = term(3) + wm_interm_32_so_sym_pt2(p,i,j,k) * wm_interm_38_so_sym_pt2(i,q,k,j)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 

do a = nocc + 1, nactive 
term(4) = term(4) + wm_interm_13_so_sym_pt2(p,a) * wm_interm_7_so_sym_pt2(a,q)
term(5) = term(5) + wm_interm_14_so_sym_pt2(p,a) * wm_interm_7_so_sym_pt2(a,q)
term(6) = term(6) + wm_interm_16_so_sym_pt2(p,a) * wm_interm_7_so_sym_pt2(a,q)
term(7) = term(7) + wm_interm_13_so_sym_pt2(p,a) * wm_interm_5_so_sym_pt2(a,q)
term(8) = term(8) + wm_interm_14_so_sym_pt2(p,a) * wm_interm_5_so_sym_pt2(a,q)
term(9) = term(9) + wm_interm_16_so_sym_pt2(p,a) * wm_interm_5_so_sym_pt2(a,q)
term(10) = term(10) + wm_interm_23_so_sym_pt2(p,a) * wm_interm_7_so_sym_pt2(a,q)
term(11) = term(11) + wm_interm_24_so_sym_pt2(p,a) * wm_interm_7_so_sym_pt2(a,q)
term(12) = term(12) + wm_interm_25_so_sym_pt2(p,a) * wm_interm_7_so_sym_pt2(a,q)
term(13) = term(13) + wm_interm_23_so_sym_pt2(p,a) * wm_interm_5_so_sym_pt2(a,q)
term(14) = term(14) + wm_interm_24_so_sym_pt2(p,a) * wm_interm_5_so_sym_pt2(a,q)
term(15) = term(15) + wm_interm_25_so_sym_pt2(p,a) * wm_interm_5_so_sym_pt2(a,q)
term(16) = term(16) + wm_interm_30_so_sym_pt2(a,q) * wm_interm_4_so_sym_pt2(p,a)
term(17) = term(17) + wm_interm_30_so_sym_pt2(a,q) * wm_interm_6_so_sym_pt2(p,a)
end do 

term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (4.0d+0) 
term(14) = term(14) * (-8.0d+0) 
term(15) = term(15) * (4.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (4.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(18) = term(18) + wm_interm_12_so_sym_pt2(i,j,k,q) * wm_interm_3_so_sym_pt2(p,k,j,i)
term(19) = term(19) + wm_interm_12_so_sym_pt2(i,j,k,q) * wm_interm_8_so_sym_pt2(p,k,j,i)
end do 
end do 
end do 

term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_11_so_sym_pt2(b,j,k,q)
end do 
end do 
end do 
end do 
end do 

term(20) = term(20) * (4.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(21) = term(21) + wm_interm_12_so_sym_pt2(i,j,k,q) * wm_interm_8_so_sym_pt2(p,k,i,j)
end do 
end do 
end do 

term(21) = term(21) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(22) = term(22) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,i) * wm_interm_11_so_sym_pt2(b,j,q,k)
term(23) = term(23) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_11_so_sym_pt2(b,i,q,k)
term(24) = term(24) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_31_so_sym_pt2(b,q,k,j)
term(25) = term(25) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_31_so_sym_pt2(b,k,q,j)
term(26) = term(26) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_33_so_sym_pt2(b,q,k,j)
term(27) = term(27) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_33_so_sym_pt2(b,k,q,j)
end do 
end do 
end do 
end do 
end do 

term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (-1.0d+0) 
term(27) = term(27) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + r2(vrdav_Rl, a,j,p,i) * wm_interm_56_so_sym_pt2(a,j,i,q)
term(29) = term(29) + r2(vrdav_Rl, a,j,p,i) * wm_interm_56_so_sym_pt2(a,i,j,q)
term(30) = term(30) + r2(vrdav_Rl, a,j,p,i) * wm_interm_57_so_sym_pt2(a,i,j,q)
term(31) = term(31) + r2(vrdav_Rl, a,j,p,i) * wm_interm_60_so_sym_pt2(a,i,j,q)
term(32) = term(32) + r2(vrdav_Rl, a,j,p,i) * wm_interm_60_so_sym_pt2(a,j,i,q)
end do 
end do 
end do 

term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-1.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(33) = term(33) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_61_so_sym_pt2(a,j,i,q)
term(34) = term(34) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_61_so_sym_pt2(a,i,j,q)
term(35) = term(35) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_62_so_sym_pt2(a,i,j,q)
term(36) = term(36) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_62_so_sym_pt2(a,j,i,q)
end do 
end do 
end do 

term(33) = term(33) * (-0.5d+0) 
term(35) = term(35) * (-0.5d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(37) = term(37) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_51_so_sym_pt2(a,i)
term(38) = term(38) + r2p(vrdav_Rr, p,i,a,q) * wm_interm_52_so_sym_pt2(a,i)
end do 
end do 

term(37) = term(37) * (2.0d+0) 
term(38) = term(38) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(39) = term(39) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_62_so_sym_pt2(a,j,i,q)
term(40) = term(40) + r2p(vrdav_Rr, a,i,p,j) * wm_interm_61_so_sym_pt2(a,i,j,q)
term(41) = term(41) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_61_so_sym_pt2(a,i,j,q)
term(42) = term(42) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_61_so_sym_pt2(a,j,i,q)
term(43) = term(43) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_62_so_sym_pt2(a,j,i,q)
term(44) = term(44) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_62_so_sym_pt2(a,i,j,q)
end do 
end do 
end do 

term(39) = term(39) * (-0.5d+0) 
term(40) = term(40) * (-0.5d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (2.0d+0) 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(45) = term(45) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,b,q) * wm_interm_4_so_sym_pt2(b,a)
term(46) = term(46) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,i,b,q) * wm_interm_6_so_sym_pt2(b,a)
end do 
end do 
end do 

term(45) = term(45) * (-1.0d+0) 
term(46) = term(46) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(47) = term(47) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,j,i) * wm_interm_5_so_sym_pt2(b,j)
term(48) = term(48) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,j,i) * wm_interm_5_so_sym_pt2(b,j)
term(49) = term(49) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,j,i) * wm_interm_7_so_sym_pt2(b,j)
term(50) = term(50) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,j,i) * wm_interm_7_so_sym_pt2(b,j)
end do 
end do 
end do 
end do 

term(47) = term(47) * (2.0d+0) 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(51) = term(51) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,j) * wm_interm_11_so_sym_pt2(b,i,q,k)
term(52) = term(52) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,i,k) * wm_interm_11_so_sym_pt2(b,j,q,k)
term(53) = term(53) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,j,k) * wm_interm_11_so_sym_pt2(b,i,q,k)
term(54) = term(54) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,i,k) * wm_interm_11_so_sym_pt2(b,j,k,q)
term(55) = term(55) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,j,k) * wm_interm_11_so_sym_pt2(b,i,k,q)
end do 
end do 
end do 
end do 
end do 

term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (-1.0d+0) 
term(55) = term(55) * (2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(56) = term(56) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,i,j) * wm_interm_5_so_sym_pt2(b,j)
term(57) = term(57) + r2p(vrdav_Rr, p,i,a,q) * s2(a,b,i,j) * wm_interm_7_so_sym_pt2(b,j)
end do 
end do 
end do 
end do 

term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(58) = term(58) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,i,j) * wm_interm_5_so_sym_pt2(b,j)
term(59) = term(59) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_5_so_sym_pt2(b,j)
term(60) = term(60) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,i,j) * wm_interm_7_so_sym_pt2(b,j)
term(61) = term(61) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_7_so_sym_pt2(b,j)
term(62) = term(62) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_5_so_sym_pt2(b,j)
term(63) = term(63) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,i,j) * wm_interm_7_so_sym_pt2(b,j)
end do 
end do 
end do 
end do 

term(58) = term(58) * (8.0d+0) 
term(59) = term(59) * (-4.0d+0) 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * (2.0d+0) 
term(62) = term(62) * (-16.0d+0) 
term(63) = term(63) * (8.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(64) = term(64) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_5_so_sym_pt2(b,j)
term(65) = term(65) + r2p(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_7_so_sym_pt2(b,j)
term(66) = term(66) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_5_so_sym_pt2(b,j)
term(67) = term(67) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_5_so_sym_pt2(b,j)
term(68) = term(68) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_7_so_sym_pt2(b,j)
term(69) = term(69) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_7_so_sym_pt2(b,j)
term(70) = term(70) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_9_so_sym_pt2(b,j)
term(71) = term(71) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_10_so_sym_pt2(b,j)
term(72) = term(72) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_15_so_sym_pt2(b,j)
term(73) = term(73) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_21_so_sym_pt2(b,j)
term(74) = term(74) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_22_so_sym_pt2(b,j)
term(75) = term(75) + r2(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_34_so_sym_pt2(b,j)
term(76) = term(76) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_30_so_sym_pt2(b,j)
end do 
end do 
end do 
end do 

term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (-1.0d+0) 
term(66) = term(66) * (8.0d+0) 
term(67) = term(67) * (-8.0d+0) 
term(68) = term(68) * (-4.0d+0) 
term(69) = term(69) * (4.0d+0) 
term(70) = term(70) * (-1.0d+0) 
term(71) = term(71) * (2.0d+0) 
term(72) = term(72) * (-1.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (4.0d+0) 
term(75) = term(75) * (-2.0d+0) 
term(76) = term(76) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(77) = term(77) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,q,p,i) * wm_interm_4_so_sym_pt2(b,a)
term(78) = term(78) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,q,p,i) * wm_interm_6_so_sym_pt2(b,a)
term(79) = term(79) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_4_so_sym_pt2(b,a)
term(80) = term(80) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_6_so_sym_pt2(b,a)
term(81) = term(81) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_16_so_sym_pt2(b,a)
term(82) = term(82) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_13_so_sym_pt2(b,a)
term(83) = term(83) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_14_so_sym_pt2(b,a)
term(84) = term(84) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_23_so_sym_pt2(b,a)
term(85) = term(85) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_24_so_sym_pt2(b,a)
term(86) = term(86) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_4_so_sym_pt2(b,a)
term(87) = term(87) + r2(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_6_so_sym_pt2(b,a)
end do 
end do 
end do 

term(77) = term(77) * (-1.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-4.0d+0) 
term(80) = term(80) * (8.0d+0) 
term(81) = term(81) * (-1.0d+0) 
term(82) = term(82) * (-1.0d+0) 
term(83) = term(83) * (2.0d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (-2.0d+0) 
term(87) = term(87) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(88) = term(88) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,k,j) * wm_interm_11_so_sym_pt2(b,i,q,k)
term(89) = term(89) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_11_so_sym_pt2(b,i,q,k)
term(90) = term(90) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_11_so_sym_pt2(b,i,k,q)
term(91) = term(91) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,j) * wm_interm_11_so_sym_pt2(b,i,q,k)
term(92) = term(92) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_11_so_sym_pt2(b,i,q,k)
term(93) = term(93) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_11_so_sym_pt2(b,i,k,q)
end do 
end do 
end do 
end do 
end do 

term(88) = term(88) * (-1.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (-1.0d+0) 
term(91) = term(91) * (-2.0d+0) 
term(92) = term(92) * (4.0d+0) 
term(93) = term(93) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(94) = term(94) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,j) * wm_interm_11_so_sym_pt2(b,i,k,q)
end do 
end do 
end do 
end do 
end do 

term(94) = term(94) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(95) = term(95) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,i) * wm_interm_11_so_sym_pt2(b,j,k,q)
end do 
end do 
end do 
end do 
end do 

term(95) = term(95) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(96) = term(96) + r2p(vrdav_Rr, a,i,p,j) * s2(a,b,k,i) * wm_interm_11_so_sym_pt2(b,j,k,q)
term(97) = term(97) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_11_so_sym_pt2(b,i,k,q)
term(98) = term(98) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,i) * wm_interm_11_so_sym_pt2(b,j,k,q)
term(99) = term(99) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_33_so_sym_pt2(b,q,k,i)
term(100) = term(100) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_31_so_sym_pt2(b,q,k,j)
term(101) = term(101) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_31_so_sym_pt2(b,q,k,i)
term(102) = term(102) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_31_so_sym_pt2(b,k,q,i)
term(103) = term(103) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_33_so_sym_pt2(b,k,q,j)
term(104) = term(104) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_33_so_sym_pt2(b,k,q,i)
end do 
end do 
end do 
end do 
end do 

term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (-2.0d+0) 
term(98) = term(98) * (-2.0d+0) 
term(99) = term(99) * (-1.0d+0) 
term(100) = term(100) * (-1.0d+0) 
term(101) = term(101) * (2.0d+0) 
term(102) = term(102) * (-1.0d+0) 
term(103) = term(103) * (-1.0d+0) 
term(104) = term(104) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(105) = term(105) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_11_so_sym_pt2(b,j,q,k)
term(106) = term(106) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_11_so_sym_pt2(b,j,q,k)
term(107) = term(107) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_11_so_sym_pt2(b,i,q,k)
term(108) = term(108) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_11_so_sym_pt2(b,j,k,q)
term(109) = term(109) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_11_so_sym_pt2(b,i,k,q)
term(110) = term(110) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_31_so_sym_pt2(b,q,k,i)
term(111) = term(111) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_31_so_sym_pt2(b,k,q,i)
term(112) = term(112) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_33_so_sym_pt2(b,q,k,i)
term(113) = term(113) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_33_so_sym_pt2(b,k,q,i)
end do 
end do 
end do 
end do 
end do 

term(105) = term(105) * (-2.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (-8.0d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * (4.0d+0) 
term(110) = term(110) * (-4.0d+0) 
term(111) = term(111) * (2.0d+0) 
term(112) = term(112) * (2.0d+0) 
term(113) = term(113) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(114) = term(114) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,i,j) * wm_interm_5_so_sym_pt2(b,j)
term(115) = term(115) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,i,j) * wm_interm_7_so_sym_pt2(b,j)
end do 
end do 
end do 
end do 

term(114) = term(114) * (16.0d+0) 
term(115) = term(115) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(116) = term(116) + r2(vrdav_Rl, a,q,p,i) * wm_interm_46_so_sym_pt2(a,i)
term(117) = term(117) + r2(vrdav_Rl, a,q,p,i) * wm_interm_47_so_sym_pt2(a,i)
term(118) = term(118) + r2(vrdav_Rl, a,q,p,i) * wm_interm_48_so_sym_pt2(a,i)
term(119) = term(119) + r2(vrdav_Rl, a,q,p,i) * wm_interm_49_so_sym_pt2(a,i)
term(120) = term(120) + r2(vrdav_Rl, a,q,p,i) * wm_interm_50_so_sym_pt2(a,i)
term(121) = term(121) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_51_so_sym_pt2(a,i)
term(122) = term(122) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_51_so_sym_pt2(a,i)
term(123) = term(123) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_52_so_sym_pt2(a,i)
term(124) = term(124) + r2p(vrdav_Rr, a,q,p,i) * wm_interm_52_so_sym_pt2(a,i)
term(125) = term(125) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_51_so_sym_pt2(a,i)
term(126) = term(126) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_51_so_sym_pt2(a,i)
term(127) = term(127) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_52_so_sym_pt2(a,i)
term(128) = term(128) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_52_so_sym_pt2(a,i)
end do 
end do 

term(116) = term(116) * (-2.0d+0) 
term(119) = term(119) * (-4.0d+0) 
term(120) = term(120) * (4.0d+0) 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (2.0d+0) 
term(123) = term(123) * (2.0d+0) 
term(124) = term(124) * (-1.0d+0) 
term(125) = term(125) * (8.0d+0) 
term(126) = term(126) * (-8.0d+0) 
term(127) = term(127) * (-4.0d+0) 
term(128) = term(128) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(129) = term(129) + wm_interm_0_so_sym_pt2(p,i,j,q) * wm_interm_1_so_sym_pt2(j,i)
term(130) = term(130) + wm_interm_0_so_sym_pt2(p,i,q,j) * wm_interm_1_so_sym_pt2(j,i)
term(131) = term(131) + wm_interm_0_so_sym_pt2(p,i,j,q) * wm_interm_2_so_sym_pt2(j,i)
term(132) = term(132) + wm_interm_0_so_sym_pt2(p,i,q,j) * wm_interm_2_so_sym_pt2(j,i)
term(133) = term(133) + wm_interm_11_so_sym_pt2(p,i,q,j) * wm_interm_18_so_sym_pt2(i,j)
term(134) = term(134) + wm_interm_11_so_sym_pt2(p,i,q,j) * wm_interm_19_so_sym_pt2(i,j)
term(135) = term(135) + wm_interm_11_so_sym_pt2(p,i,q,j) * wm_interm_20_so_sym_pt2(i,j)
term(136) = term(136) + wm_interm_11_so_sym_pt2(p,i,q,j) * wm_interm_27_so_sym_pt2(i,j)
term(137) = term(137) + wm_interm_11_so_sym_pt2(p,i,q,j) * wm_interm_28_so_sym_pt2(i,j)
term(138) = term(138) + wm_interm_11_so_sym_pt2(p,i,q,j) * wm_interm_29_so_sym_pt2(i,j)
term(139) = term(139) + wm_interm_32_so_sym_pt2(p,i,q,j) * wm_interm_37_so_sym_pt2(i,j)
term(140) = term(140) + wm_interm_32_so_sym_pt2(p,i,q,j) * wm_interm_39_so_sym_pt2(i,j)
end do 
end do 

term(129) = term(129) * (-1.0d+0) 
term(130) = term(130) * (2.0d+0) 
term(131) = term(131) * (2.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (-1.0d+0) 
term(134) = term(134) * (2.0d+0) 
term(135) = term(135) * (-1.0d+0) 
term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * (4.0d+0) 
term(138) = term(138) * (-2.0d+0) 
term(139) = term(139) * (-2.0d+0) 
term(140) = term(140) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(141) = term(141) + wm_interm_1_so_sym_pt2(i,j) * wm_interm_3_so_sym_pt2(p,j,q,i)
term(142) = term(142) + wm_interm_2_so_sym_pt2(i,j) * wm_interm_3_so_sym_pt2(p,j,q,i)
term(143) = term(143) + wm_interm_1_so_sym_pt2(i,j) * wm_interm_8_so_sym_pt2(p,j,q,i)
term(144) = term(144) + wm_interm_1_so_sym_pt2(i,j) * wm_interm_8_so_sym_pt2(p,j,i,q)
term(145) = term(145) + wm_interm_2_so_sym_pt2(i,j) * wm_interm_8_so_sym_pt2(p,j,q,i)
term(146) = term(146) + wm_interm_2_so_sym_pt2(i,j) * wm_interm_8_so_sym_pt2(p,j,i,q)
term(147) = term(147) + wm_interm_1_so_sym_pt2(i,j) * wm_interm_31_so_sym_pt2(p,q,j,i)
term(148) = term(148) + wm_interm_2_so_sym_pt2(i,j) * wm_interm_31_so_sym_pt2(p,q,j,i)
term(149) = term(149) + wm_interm_1_so_sym_pt2(i,j) * wm_interm_33_so_sym_pt2(p,j,q,i)
term(150) = term(150) + wm_interm_2_so_sym_pt2(i,j) * wm_interm_33_so_sym_pt2(p,j,q,i)
end do 
end do 

term(141) = term(141) * (-1.0d+0) 
term(142) = term(142) * (2.0d+0) 
term(143) = term(143) * (-4.0d+0) 
term(144) = term(144) * (4.0d+0) 
term(145) = term(145) * (8.0d+0) 
term(146) = term(146) * (-8.0d+0) 
term(147) = term(147) * (-1.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-1.0d+0) 
term(150) = term(150) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(151) = term(151) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_4_so_sym_pt2(b,a)
term(152) = term(152) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_6_so_sym_pt2(b,a)
term(153) = term(153) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_35_so_sym_pt2(a,b)
term(154) = term(154) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_36_so_sym_pt2(a,b)
end do 
end do 
end do 

term(151) = term(151) * (2.0d+0) 
term(152) = term(152) * (-4.0d+0) 
term(153) = term(153) * (4.0d+0) 
term(154) = term(154) * (-2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(155) = term(155) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_4_so_sym_pt2(b,a)
term(156) = term(156) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_6_so_sym_pt2(b,a)
end do 
end do 
end do 

term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-8.0d+0) 

do i = 1, nocc 
term(157) = term(157) + wm_interm_1_so_sym_pt2(i,q) * wm_interm_9_so_sym_pt2(p,i)
term(158) = term(158) + wm_interm_10_so_sym_pt2(p,i) * wm_interm_1_so_sym_pt2(i,q)
term(159) = term(159) + wm_interm_2_so_sym_pt2(i,q) * wm_interm_9_so_sym_pt2(p,i)
term(160) = term(160) + wm_interm_10_so_sym_pt2(p,i) * wm_interm_2_so_sym_pt2(i,q)
term(161) = term(161) + wm_interm_15_so_sym_pt2(p,i) * wm_interm_1_so_sym_pt2(i,q)
term(162) = term(162) + wm_interm_15_so_sym_pt2(p,i) * wm_interm_2_so_sym_pt2(i,q)
term(163) = term(163) + wm_interm_1_so_sym_pt2(i,q) * wm_interm_21_so_sym_pt2(p,i)
term(164) = term(164) + wm_interm_1_so_sym_pt2(i,q) * wm_interm_22_so_sym_pt2(p,i)
term(165) = term(165) + wm_interm_21_so_sym_pt2(p,i) * wm_interm_2_so_sym_pt2(i,q)
term(166) = term(166) + wm_interm_22_so_sym_pt2(p,i) * wm_interm_2_so_sym_pt2(i,q)
term(167) = term(167) + wm_interm_34_so_sym_pt2(p,i) * wm_interm_37_so_sym_pt2(q,i)
term(168) = term(168) + wm_interm_34_so_sym_pt2(p,i) * wm_interm_39_so_sym_pt2(q,i)
term(169) = term(169) + r1(vrdav_Rl, p,i) * wm_interm_43_so_sym_pt2(i,q)
term(170) = term(170) + r1(vrdav_Rr, p,i) * wm_interm_44_so_sym_pt2(i,q)
end do 

term(157) = term(157) * (-1.0d+0) 
term(158) = term(158) * (2.0d+0) 
term(159) = term(159) * (2.0d+0) 
term(160) = term(160) * (-4.0d+0) 
term(161) = term(161) * (-1.0d+0) 
term(162) = term(162) * (2.0d+0) 
term(163) = term(163) * (-4.0d+0) 
term(164) = term(164) * (4.0d+0) 
term(165) = term(165) * (8.0d+0) 
term(166) = term(166) * (-8.0d+0) 
term(167) = term(167) * (-2.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (2.0d+0) 
term(170) = term(170) * (2.0d+0) 


    calc_D_vo_wm_so_sym_pt2 = zero
    do s = 0, 170
    calc_D_vo_wm_so_sym_pt2 = calc_D_vo_wm_so_sym_pt2 + term(s)
    end do

    end function calc_D_vo_wm_so_sym_pt2
    
    function calc_D_vv_wm_so_sym_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_sym_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:29) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_11_so_sym_pt2(q,i,j,k) * wm_interm_32_so_sym_pt2(p,i,k,j)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 

do a = nocc + 1, nactive 
term(1) = term(1) + wm_interm_41_so_sym_pt2(a,p) * wm_interm_4_so_sym_pt2(q,a)
term(2) = term(2) + wm_interm_41_so_sym_pt2(a,p) * wm_interm_6_so_sym_pt2(q,a)
term(3) = term(3) + wm_interm_41_so_sym_pt2(p,a) * wm_interm_4_so_sym_pt2(a,q)
term(4) = term(4) + wm_interm_41_so_sym_pt2(p,a) * wm_interm_6_so_sym_pt2(a,q)
end do 

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(5) = term(5) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_1_so_sym_pt2(i,j)
term(6) = term(6) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_2_so_sym_pt2(i,j)
end do 
end do 

term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-4.0d+0) 

do i = 1, nocc 
term(7) = term(7) + wm_interm_34_so_sym_pt2(p,i) * wm_interm_7_so_sym_pt2(q,i)
term(8) = term(8) + wm_interm_34_so_sym_pt2(p,i) * wm_interm_5_so_sym_pt2(q,i)
term(9) = term(9) + r1(vrdav_Rl, q,i) * wm_interm_46_so_sym_pt2(p,i)
term(10) = term(10) + r1(vrdav_Rl, q,i) * wm_interm_47_so_sym_pt2(p,i)
term(11) = term(11) + r1(vrdav_Rl, q,i) * wm_interm_48_so_sym_pt2(p,i)
term(12) = term(12) + r1(vrdav_Rl, q,i) * wm_interm_49_so_sym_pt2(p,i)
term(13) = term(13) + r1(vrdav_Rl, q,i) * wm_interm_50_so_sym_pt2(p,i)
term(14) = term(14) + s1(q,i) * wm_interm_9_so_sym_pt2(p,i)
term(15) = term(15) + s1(q,i) * wm_interm_10_so_sym_pt2(p,i)
term(16) = term(16) + s1(q,i) * wm_interm_15_so_sym_pt2(p,i)
term(17) = term(17) + s1(q,i) * wm_interm_21_so_sym_pt2(p,i)
term(18) = term(18) + s1(q,i) * wm_interm_22_so_sym_pt2(p,i)
term(19) = term(19) + r1(vrdav_Rr, p,i) * wm_interm_51_so_sym_pt2(q,i)
term(20) = term(20) + r1(vrdav_Rr, p,i) * wm_interm_52_so_sym_pt2(q,i)
term(21) = term(21) + t1(q,i) * wm_interm_30_so_sym_pt2(p,i)
end do 

term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (-1.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(15) = term(15) * (-2.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(22) = term(22) + s2(a,p,k,j) * t2(a,q,j,i) * wm_interm_40_so_sym_pt2(i,k)
term(23) = term(23) + s2(a,p,i,j) * t2(a,q,k,i) * wm_interm_40_so_sym_pt2(k,j)
term(24) = term(24) + s2(a,p,k,j) * t2(a,q,k,i) * wm_interm_40_so_sym_pt2(i,j)
end do 
end do 
end do 
end do 

term(22) = term(22) * (2.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(25) = term(25) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_5_so_sym_pt2(a,j)
term(26) = term(26) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_7_so_sym_pt2(a,j)
term(27) = term(27) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_34_so_sym_pt2(a,j)
end do 
end do 
end do 

term(25) = term(25) * (-4.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_5_so_sym_pt2(a,j)
term(29) = term(29) + r1(vrdav_Rr, p,i) * s2(a,q,j,i) * wm_interm_7_so_sym_pt2(a,j)
end do 
end do 
end do 

term(28) = term(28) * (8.0d+0) 
term(29) = term(29) * (-4.0d+0) 


    calc_D_vv_wm_so_sym_pt2 = zero
    do s = 0, 29
    calc_D_vv_wm_so_sym_pt2 = calc_D_vv_wm_so_sym_pt2 + term(s)
    end do

    end function calc_D_vv_wm_so_sym_pt2

  function calc_D_oo_wm_so_sym_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_sym_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b 
    real(F64), dimension(0:64) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_so_sym_pt3(a,i,q,j) * wm_interm_49_so_sym_pt3(a,j,i,p)
term(1) = term(1) + wm_interm_0_so_sym_pt3(a,i,q,j) * wm_interm_49_so_sym_pt3(a,j,p,i)
term(2) = term(2) + wm_interm_0_so_sym_pt3(a,i,q,j) * wm_interm_50_so_sym_pt3(a,j,p,i)
term(3) = term(3) + wm_interm_0_so_sym_pt3(a,q,i,j) * wm_interm_49_so_sym_pt3(a,j,p,i)
term(4) = term(4) + wm_interm_0_so_sym_pt3(a,q,i,j) * wm_interm_50_so_sym_pt3(a,j,i,p)
term(5) = term(5) + wm_interm_0_so_sym_pt3(a,q,i,j) * wm_interm_50_so_sym_pt3(a,j,p,i)
term(6) = term(6) + wm_interm_0_so_sym_pt3(a,i,q,j) * wm_interm_53_so_sym_pt3(a,j,i,p)
term(7) = term(7) + wm_interm_0_so_sym_pt3(a,i,q,j) * wm_interm_53_so_sym_pt3(a,j,p,i)
term(8) = term(8) + wm_interm_0_so_sym_pt3(a,q,i,j) * wm_interm_53_so_sym_pt3(a,j,p,i)
term(9) = term(9) + wm_interm_0_so_sym_pt3(a,q,i,j) * wm_interm_53_so_sym_pt3(a,j,i,p)
term(10) = term(10) + wm_interm_0_so_sym_pt3(a,i,j,q) * wm_interm_49_so_sym_pt3(a,p,i,j)
term(11) = term(11) + wm_interm_0_so_sym_pt3(a,i,j,q) * wm_interm_53_so_sym_pt3(a,p,i,j)
term(12) = term(12) + wm_interm_21_so_sym_pt3(a,q,i,j) * wm_interm_60_so_sym_pt3(a,i,j,p)
term(13) = term(13) + wm_interm_21_so_sym_pt3(a,q,i,j) * wm_interm_59_so_sym_pt3(a,i,j,p)
term(14) = term(14) + wm_interm_21_so_sym_pt3(a,q,i,j) * wm_interm_59_so_sym_pt3(a,j,i,p)
term(15) = term(15) + wm_interm_21_so_sym_pt3(a,q,i,j) * wm_interm_60_so_sym_pt3(a,j,i,p)
term(16) = term(16) + wm_interm_21_so_sym_pt3(a,i,q,j) * wm_interm_60_so_sym_pt3(a,j,p,i)
term(17) = term(17) + wm_interm_21_so_sym_pt3(a,i,q,j) * wm_interm_59_so_sym_pt3(a,p,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * (4.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (-1.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(18) = term(18) + wm_interm_0_so_sym_pt3(a,p,i,q) * wm_interm_54_so_sym_pt3(a,i)
term(19) = term(19) + wm_interm_0_so_sym_pt3(a,p,i,q) * wm_interm_55_so_sym_pt3(a,i)
term(20) = term(20) + wm_interm_0_so_sym_pt3(a,p,i,q) * wm_interm_56_so_sym_pt3(a,i)
term(21) = term(21) + wm_interm_0_so_sym_pt3(a,p,i,q) * wm_interm_57_so_sym_pt3(a,i)
term(22) = term(22) + wm_interm_0_so_sym_pt3(a,p,i,q) * wm_interm_58_so_sym_pt3(a,i)
term(23) = term(23) + wm_interm_19_so_sym_pt3(a,i) * wm_interm_59_so_sym_pt3(a,q,i,p)
term(24) = term(24) + wm_interm_19_so_sym_pt3(a,i) * wm_interm_59_so_sym_pt3(a,i,q,p)
term(25) = term(25) + wm_interm_19_so_sym_pt3(a,i) * wm_interm_60_so_sym_pt3(a,q,i,p)
term(26) = term(26) + wm_interm_19_so_sym_pt3(a,i) * wm_interm_60_so_sym_pt3(a,i,q,p)
term(27) = term(27) + wm_interm_20_so_sym_pt3(a,i) * wm_interm_60_so_sym_pt3(a,q,i,p)
term(28) = term(28) + wm_interm_20_so_sym_pt3(a,i) * wm_interm_59_so_sym_pt3(a,q,i,p)
term(29) = term(29) + wm_interm_20_so_sym_pt3(a,i) * wm_interm_59_so_sym_pt3(a,i,q,p)
term(30) = term(30) + wm_interm_20_so_sym_pt3(a,i) * wm_interm_60_so_sym_pt3(a,i,q,p)
term(31) = term(31) + wm_interm_21_so_sym_pt3(a,p,q,i) * wm_interm_63_so_sym_pt3(a,i)
end do 
end do 

term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (-1.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(27) = term(27) * (-1.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (2.0d+0) 
term(31) = term(31) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(32) = term(32) + wm_interm_19_so_sym_pt3(a,q) * wm_interm_63_so_sym_pt3(a,p)
term(33) = term(33) + wm_interm_20_so_sym_pt3(a,q) * wm_interm_63_so_sym_pt3(a,p)
end do 

term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-2.0d+0) 

do i = 1, nocc 
term(34) = term(34) + wm_interm_52_so_sym_pt3(q,i) * wm_interm_7_so_sym_pt3(p,i)
term(35) = term(35) + wm_interm_52_so_sym_pt3(q,i) * wm_interm_8_so_sym_pt3(p,i)
term(36) = term(36) + wm_interm_52_so_sym_pt3(q,i) * wm_interm_9_so_sym_pt3(p,i)
term(37) = term(37) + wm_interm_15_so_sym_pt3(p,i) * wm_interm_52_so_sym_pt3(q,i)
term(38) = term(38) + wm_interm_16_so_sym_pt3(p,i) * wm_interm_52_so_sym_pt3(q,i)
term(39) = term(39) + wm_interm_17_so_sym_pt3(p,i) * wm_interm_52_so_sym_pt3(q,i)
term(40) = term(40) + wm_interm_52_so_sym_pt3(i,q) * wm_interm_7_so_sym_pt3(i,p)
term(41) = term(41) + wm_interm_52_so_sym_pt3(i,q) * wm_interm_8_so_sym_pt3(i,p)
term(42) = term(42) + wm_interm_52_so_sym_pt3(i,q) * wm_interm_9_so_sym_pt3(i,p)
term(43) = term(43) + wm_interm_15_so_sym_pt3(i,p) * wm_interm_52_so_sym_pt3(i,q)
term(44) = term(44) + wm_interm_16_so_sym_pt3(i,p) * wm_interm_52_so_sym_pt3(i,q)
term(45) = term(45) + wm_interm_17_so_sym_pt3(i,p) * wm_interm_52_so_sym_pt3(i,q)
term(46) = term(46) + wm_interm_22_so_sym_pt3(q,i) * wm_interm_62_so_sym_pt3(p,i)
term(47) = term(47) + wm_interm_24_so_sym_pt3(q,i) * wm_interm_62_so_sym_pt3(p,i)
term(48) = term(48) + wm_interm_22_so_sym_pt3(i,q) * wm_interm_62_so_sym_pt3(i,p)
term(49) = term(49) + wm_interm_24_so_sym_pt3(i,q) * wm_interm_62_so_sym_pt3(i,p)
end do 

term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (2.0d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (-1.0d+0) 
term(41) = term(41) * (2.0d+0) 
term(42) = term(42) * (-1.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (4.0d+0) 
term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * (-2.0d+0) 
term(47) = term(47) * (4.0d+0) 
term(48) = term(48) * (-2.0d+0) 
term(49) = term(49) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(50) = term(50) + wm_interm_0_so_sym_pt3(a,i,j,q) * wm_interm_50_so_sym_pt3(a,p,j,i)
term(51) = term(51) + wm_interm_0_so_sym_pt3(a,i,j,q) * wm_interm_49_so_sym_pt3(a,p,j,i)
term(52) = term(52) + wm_interm_0_so_sym_pt3(a,i,j,q) * wm_interm_53_so_sym_pt3(a,p,j,i)
term(53) = term(53) + wm_interm_21_so_sym_pt3(a,i,j,q) * wm_interm_60_so_sym_pt3(a,p,j,i)
term(54) = term(54) + wm_interm_21_so_sym_pt3(a,i,j,q) * wm_interm_60_so_sym_pt3(a,j,p,i)
term(55) = term(55) + wm_interm_21_so_sym_pt3(a,i,j,q) * wm_interm_59_so_sym_pt3(a,j,p,i)
term(56) = term(56) + wm_interm_21_so_sym_pt3(a,i,j,q) * wm_interm_59_so_sym_pt3(a,p,j,i)
end do 
end do 
end do 

term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (-1.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-1.0d+0) 
term(56) = term(56) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(57) = term(57) + wm_interm_10_so_sym_pt3(p,i,j,q) * wm_interm_52_so_sym_pt3(i,j)
term(58) = term(58) + wm_interm_10_so_sym_pt3(i,p,q,j) * wm_interm_52_so_sym_pt3(i,j)
term(59) = term(59) + wm_interm_10_so_sym_pt3(p,i,q,j) * wm_interm_52_so_sym_pt3(i,j)
term(60) = term(60) + wm_interm_18_so_sym_pt3(p,i,j,q) * wm_interm_52_so_sym_pt3(i,j)
term(61) = term(61) + wm_interm_18_so_sym_pt3(i,p,q,j) * wm_interm_52_so_sym_pt3(i,j)
term(62) = term(62) + wm_interm_18_so_sym_pt3(p,i,q,j) * wm_interm_52_so_sym_pt3(i,j)
term(63) = term(63) + wm_interm_45_so_sym_pt3(i,p,q,j) * wm_interm_62_so_sym_pt3(i,j)
term(64) = term(64) + wm_interm_45_so_sym_pt3(p,i,j,q) * wm_interm_62_so_sym_pt3(i,j)
end do 
end do 

term(57) = term(57) * (-1.0d+0) 
term(58) = term(58) * (-1.0d+0) 
term(59) = term(59) * (2.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (-2.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (-1.0d+0) 


    calc_D_oo_wm_so_sym_pt3 = zero
    do s = 0, 64
    calc_D_oo_wm_so_sym_pt3 = calc_D_oo_wm_so_sym_pt3 + term(s)
    end do

    end function calc_D_oo_wm_so_sym_pt3
    
    function calc_D_ov_wm_so_sym_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_sym_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:113) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_10_so_sym_pt3(i,j,k,p) * wm_interm_6_so_sym_pt3(q,i,j,k)
term(1) = term(1) + wm_interm_10_so_sym_pt3(i,j,p,k) * wm_interm_6_so_sym_pt3(q,i,j,k)
term(2) = term(2) + wm_interm_10_so_sym_pt3(i,j,p,k) * wm_interm_11_so_sym_pt3(q,i,j,k)
term(3) = term(3) + wm_interm_18_so_sym_pt3(i,j,k,p) * wm_interm_6_so_sym_pt3(q,i,j,k)
term(4) = term(4) + wm_interm_18_so_sym_pt3(i,j,p,k) * wm_interm_6_so_sym_pt3(q,i,j,k)
term(5) = term(5) + wm_interm_11_so_sym_pt3(q,i,j,k) * wm_interm_18_so_sym_pt3(j,i,k,p)
term(6) = term(6) + wm_interm_11_so_sym_pt3(q,i,j,k) * wm_interm_18_so_sym_pt3(i,j,p,k)
term(7) = term(7) + wm_interm_11_so_sym_pt3(q,i,j,k) * wm_interm_18_so_sym_pt3(j,i,p,k)
term(8) = term(8) + wm_interm_1_so_sym_pt3(i,j,p,k) * wm_interm_46_so_sym_pt3(q,k,i,j)
term(9) = term(9) + wm_interm_2_so_sym_pt3(i,j,p,k) * wm_interm_46_so_sym_pt3(q,k,i,j)
term(10) = term(10) + wm_interm_12_so_sym_pt3(i,j,p,k) * wm_interm_46_so_sym_pt3(q,k,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-0.5d+0) 
term(2) = term(2) * (-0.5d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-1.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-0.5d+0) 
term(10) = term(10) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(11) = term(11) + wm_interm_10_so_sym_pt3(i,j,p,k) * wm_interm_6_so_sym_pt3(q,j,i,k)
term(12) = term(12) + wm_interm_10_so_sym_pt3(i,j,k,p) * wm_interm_11_so_sym_pt3(q,j,i,k)
term(13) = term(13) + wm_interm_10_so_sym_pt3(i,j,p,k) * wm_interm_11_so_sym_pt3(q,j,i,k)
term(14) = term(14) + wm_interm_18_so_sym_pt3(i,j,p,k) * wm_interm_6_so_sym_pt3(q,j,i,k)
term(15) = term(15) + wm_interm_1_so_sym_pt3(i,j,p,k) * wm_interm_46_so_sym_pt3(q,k,j,i)
term(16) = term(16) + wm_interm_2_so_sym_pt3(i,j,p,k) * wm_interm_46_so_sym_pt3(q,k,j,i)
term(17) = term(17) + wm_interm_12_so_sym_pt3(i,j,p,k) * wm_interm_46_so_sym_pt3(q,k,j,i)
end do 
end do 
end do 

term(11) = term(11) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(14) = term(14) * (-1.0d+0) 
term(16) = term(16) * (-0.5d+0) 
term(17) = term(17) * (2.0d+0) 

do a = nocc + 1, nactive 
term(18) = term(18) + wm_interm_35_so_sym_pt3(a,p) * wm_interm_38_so_sym_pt3(a,q)
term(19) = term(19) + wm_interm_35_so_sym_pt3(a,p) * wm_interm_39_so_sym_pt3(a,q)
term(20) = term(20) + wm_interm_36_so_sym_pt3(a,p) * wm_interm_38_so_sym_pt3(a,q)
term(21) = term(21) + wm_interm_36_so_sym_pt3(a,p) * wm_interm_39_so_sym_pt3(a,q)
term(22) = term(22) + wm_interm_37_so_sym_pt3(a,p) * wm_interm_38_so_sym_pt3(a,q)
term(23) = term(23) + wm_interm_37_so_sym_pt3(a,p) * wm_interm_39_so_sym_pt3(a,q)
term(24) = term(24) + wm_interm_38_so_sym_pt3(a,q) * wm_interm_40_so_sym_pt3(a,p)
term(25) = term(25) + wm_interm_39_so_sym_pt3(a,q) * wm_interm_40_so_sym_pt3(a,p)
term(26) = term(26) + wm_interm_38_so_sym_pt3(a,q) * wm_interm_41_so_sym_pt3(a,p)
term(27) = term(27) + wm_interm_39_so_sym_pt3(a,q) * wm_interm_41_so_sym_pt3(a,p)
term(28) = term(28) + wm_interm_51_so_sym_pt3(q,a) * wm_interm_64_so_sym_pt3(a,p)
end do 

term(18) = term(18) * (-2.0d+0) 
term(19) = term(19) * (4.0d+0) 
term(21) = term(21) * (-2.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * (8.0d+0) 
term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * (2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(29) = term(29) + wm_interm_1_so_sym_pt3(i,j,k,p) * wm_interm_46_so_sym_pt3(q,k,j,i)
term(30) = term(30) + wm_interm_12_so_sym_pt3(i,j,k,p) * wm_interm_46_so_sym_pt3(q,k,j,i)
end do 
end do 
end do 

term(29) = term(29) * (-0.5d+0) 
term(30) = term(30) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(31) = term(31) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_so_sym_pt3(b,p,k,i)
term(32) = term(32) + r2(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_25_so_sym_pt3(b,p,j,k)
term(33) = term(33) + r2(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_25_so_sym_pt3(a,j,p,k)
term(34) = term(34) + r2(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_25_so_sym_pt3(a,p,j,k)
term(35) = term(35) + r2(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_23_so_sym_pt3(a,p,j,k)
term(36) = term(36) + r2(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_23_so_sym_pt3(b,j,p,k)
term(37) = term(37) + r2(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_23_so_sym_pt3(b,p,j,k)
term(38) = term(38) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_25_so_sym_pt3(b,j,p,i)
term(39) = term(39) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_25_so_sym_pt3(b,p,j,i)
term(40) = term(40) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_25_so_sym_pt3(b,k,p,i)
term(41) = term(41) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_25_so_sym_pt3(b,p,k,i)
term(42) = term(42) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_25_so_sym_pt3(a,k,p,i)
term(43) = term(43) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_25_so_sym_pt3(a,p,k,i)
term(44) = term(44) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_25_so_sym_pt3(a,j,p,i)
term(45) = term(45) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_25_so_sym_pt3(a,p,j,i)
term(46) = term(46) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_23_so_sym_pt3(b,p,j,i)
term(47) = term(47) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_23_so_sym_pt3(b,p,k,i)
term(48) = term(48) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_23_so_sym_pt3(a,p,k,i)
term(49) = term(49) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_23_so_sym_pt3(a,p,j,i)
term(50) = term(50) + r2(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_26_so_sym_pt3(b,j,p,k)
term(51) = term(51) + r2(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_26_so_sym_pt3(a,p,j,k)
term(52) = term(52) + r2(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_26_so_sym_pt3(a,j,p,k)
term(53) = term(53) + r2(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_26_so_sym_pt3(b,p,j,k)
term(54) = term(54) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_26_so_sym_pt3(b,p,j,i)
term(55) = term(55) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_26_so_sym_pt3(b,j,p,i)
term(56) = term(56) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_26_so_sym_pt3(b,p,k,i)
term(57) = term(57) + r2(vrdav_Rl, a,j,b,k) * t2(a,q,j,i) * wm_interm_26_so_sym_pt3(b,k,p,i)
term(58) = term(58) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_26_so_sym_pt3(a,p,k,i)
term(59) = term(59) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_26_so_sym_pt3(a,k,p,i)
term(60) = term(60) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_26_so_sym_pt3(a,p,j,i)
term(61) = term(61) + r2(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_26_so_sym_pt3(a,j,p,i)
end do 
end do 
end do 
end do 
end do 

term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-0.5d+0) 
term(33) = term(33) * (-0.5d+0) 
term(35) = term(35) * (-0.5d+0) 
term(36) = term(36) * (-0.5d+0) 
term(38) = term(38) * (-0.5d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (-0.5d+0) 
term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * (-0.5d+0) 
term(48) = term(48) * (-0.5d+0) 
term(50) = term(50) * (-2.0d+0) 
term(51) = term(51) * (-2.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (-2.0d+0) 
term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (4.0d+0) 
term(57) = term(57) * (-4.0d+0) 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (2.0d+0) 
term(60) = term(60) * (4.0d+0) 
term(61) = term(61) * (-4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(62) = term(62) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_0_so_sym_pt3(b,p,k,i)
end do 
end do 
end do 
end do 
end do 

term(62) = term(62) * (-1.0d+0) 

do j = 1, nocc 
do k = 1, nocc 
do i = 1, nocc 
term(63) = term(63) + wm_interm_2_so_sym_pt3(i,j,k,p) * wm_interm_46_so_sym_pt3(q,k,i,j)
term(64) = term(64) + wm_interm_12_so_sym_pt3(i,j,k,p) * wm_interm_46_so_sym_pt3(q,k,i,j)
end do 
end do 
end do 

term(63) = term(63) * (-0.5d+0) 
term(64) = term(64) * (2.0d+0) 

do i = 1, nocc 
term(65) = term(65) + wm_interm_28_so_sym_pt3(q,i) * wm_interm_7_so_sym_pt3(i,p)
term(66) = term(66) + wm_interm_28_so_sym_pt3(q,i) * wm_interm_8_so_sym_pt3(i,p)
term(67) = term(67) + wm_interm_28_so_sym_pt3(q,i) * wm_interm_9_so_sym_pt3(i,p)
term(68) = term(68) + wm_interm_32_so_sym_pt3(q,i) * wm_interm_7_so_sym_pt3(i,p)
term(69) = term(69) + wm_interm_32_so_sym_pt3(q,i) * wm_interm_8_so_sym_pt3(i,p)
term(70) = term(70) + wm_interm_32_so_sym_pt3(q,i) * wm_interm_9_so_sym_pt3(i,p)
term(71) = term(71) + wm_interm_15_so_sym_pt3(i,p) * wm_interm_28_so_sym_pt3(q,i)
term(72) = term(72) + wm_interm_16_so_sym_pt3(i,p) * wm_interm_28_so_sym_pt3(q,i)
term(73) = term(73) + wm_interm_17_so_sym_pt3(i,p) * wm_interm_28_so_sym_pt3(q,i)
term(74) = term(74) + wm_interm_15_so_sym_pt3(i,p) * wm_interm_32_so_sym_pt3(q,i)
term(75) = term(75) + wm_interm_16_so_sym_pt3(i,p) * wm_interm_32_so_sym_pt3(q,i)
term(76) = term(76) + wm_interm_17_so_sym_pt3(i,p) * wm_interm_32_so_sym_pt3(q,i)
term(77) = term(77) + wm_interm_20_so_sym_pt3(q,i) * wm_interm_4_so_sym_pt3(i,p)
term(78) = term(78) + wm_interm_20_so_sym_pt3(q,i) * wm_interm_5_so_sym_pt3(i,p)
term(79) = term(79) + wm_interm_20_so_sym_pt3(q,i) * wm_interm_3_so_sym_pt3(i,p)
term(80) = term(80) + wm_interm_19_so_sym_pt3(q,i) * wm_interm_4_so_sym_pt3(i,p)
term(81) = term(81) + wm_interm_19_so_sym_pt3(q,i) * wm_interm_5_so_sym_pt3(i,p)
term(82) = term(82) + wm_interm_19_so_sym_pt3(q,i) * wm_interm_3_so_sym_pt3(i,p)
term(83) = term(83) + wm_interm_13_so_sym_pt3(i,p) * wm_interm_20_so_sym_pt3(q,i)
term(84) = term(84) + wm_interm_14_so_sym_pt3(i,p) * wm_interm_20_so_sym_pt3(q,i)
term(85) = term(85) + wm_interm_13_so_sym_pt3(i,p) * wm_interm_19_so_sym_pt3(q,i)
term(86) = term(86) + wm_interm_14_so_sym_pt3(i,p) * wm_interm_19_so_sym_pt3(q,i)
term(87) = term(87) + wm_interm_20_so_sym_pt3(q,i) * wm_interm_65_so_sym_pt3(i,p)
term(88) = term(88) + wm_interm_19_so_sym_pt3(q,i) * wm_interm_65_so_sym_pt3(i,p)
term(89) = term(89) + wm_interm_62_so_sym_pt3(p,i) * wm_interm_67_so_sym_pt3(q,i)
term(90) = term(90) + wm_interm_62_so_sym_pt3(p,i) * wm_interm_69_so_sym_pt3(q,i)
end do 

term(65) = term(65) * (2.0d+0) 
term(66) = term(66) * (-4.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-1.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (-1.0d+0) 
term(71) = term(71) * (4.0d+0) 
term(72) = term(72) * (-8.0d+0) 
term(73) = term(73) * (4.0d+0) 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * (4.0d+0) 
term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (-1.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-1.0d+0) 
term(80) = term(80) * (2.0d+0) 
term(81) = term(81) * (-4.0d+0) 
term(82) = term(82) * (2.0d+0) 
term(83) = term(83) * (-4.0d+0) 
term(84) = term(84) * (4.0d+0) 
term(85) = term(85) * (8.0d+0) 
term(86) = term(86) * (-8.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (2.0d+0) 
term(90) = term(90) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(91) = term(91) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,b,i) * wm_interm_0_so_sym_pt3(b,k,p,j)
term(92) = term(92) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,k,b,j) * wm_interm_0_so_sym_pt3(b,p,k,i)
term(93) = term(93) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_0_so_sym_pt3(b,k,p,j)
term(94) = term(94) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,i) * wm_interm_0_so_sym_pt3(b,p,k,j)
term(95) = term(95) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,k,b,j) * wm_interm_0_so_sym_pt3(b,p,k,i)
end do 
end do 
end do 
end do 
end do 

term(91) = term(91) * (-1.0d+0) 
term(92) = term(92) * (-1.0d+0) 
term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(96) = term(96) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,i,b,k) * wm_interm_0_so_sym_pt3(b,k,p,j)
term(97) = term(97) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, a,i,b,k) * wm_interm_0_so_sym_pt3(b,p,k,j)
term(98) = term(98) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_0_so_sym_pt3(b,k,p,j)
term(99) = term(99) + r2(vrdav_Rl, a,j,q,i) * r2m(vrdav_Rr, a,i,b,k) * wm_interm_0_so_sym_pt3(b,p,k,j)
term(100) = term(100) + r2(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_25_so_sym_pt3(b,j,p,k)
term(101) = term(101) + r2(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_25_so_sym_pt3(b,p,j,k)
term(102) = term(102) + r2(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_25_so_sym_pt3(a,p,j,k)
term(103) = term(103) + r2(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_23_so_sym_pt3(a,j,p,k)
term(104) = term(104) + r2(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_23_so_sym_pt3(a,p,j,k)
term(105) = term(105) + r2(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_23_so_sym_pt3(b,p,j,k)
term(106) = term(106) + r2(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_26_so_sym_pt3(b,p,j,k)
term(107) = term(107) + r2(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_26_so_sym_pt3(b,j,p,k)
term(108) = term(108) + r2(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_26_so_sym_pt3(a,j,p,k)
term(109) = term(109) + r2(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_26_so_sym_pt3(a,p,j,k)
end do 
end do 
end do 
end do 
end do 

term(96) = term(96) * (2.0d+0) 
term(97) = term(97) * (-1.0d+0) 
term(98) = term(98) * (4.0d+0) 
term(99) = term(99) * (-4.0d+0) 
term(100) = term(100) * (-0.5d+0) 
term(102) = term(102) * (-0.5d+0) 
term(103) = term(103) * (-0.5d+0) 
term(105) = term(105) * (-0.5d+0) 
term(106) = term(106) * (-2.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (-2.0d+0) 
term(109) = term(109) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(110) = term(110) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,i,a,k) * wm_interm_0_so_sym_pt3(b,k,p,j)
term(111) = term(111) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,i,a,k) * wm_interm_0_so_sym_pt3(b,p,k,j)
end do 
end do 
end do 
end do 
end do 

term(110) = term(110) * (-1.0d+0) 
term(111) = term(111) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do b = nocc + 1, nactive 
term(112) = term(112) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,i) * wm_interm_0_so_sym_pt3(b,p,k,j)
term(113) = term(113) + r2(vrdav_Rl, a,j,q,i) * r2p(vrdav_Rr, b,k,a,j) * wm_interm_0_so_sym_pt3(b,p,k,i)
end do 
end do 
end do 
end do 
end do 

term(112) = term(112) * (-1.0d+0) 
term(113) = term(113) * (2.0d+0) 


    calc_D_ov_wm_so_sym_pt3 = zero
    do s = 0, 113
    calc_D_ov_wm_so_sym_pt3 = calc_D_ov_wm_so_sym_pt3 + term(s)
    end do

    end function calc_D_ov_wm_so_sym_pt3
    
    function calc_D_vo_wm_so_sym_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_sym_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, b, a 
    real(F64), dimension(0:264) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_1_so_sym_pt3(q,k,j,i)
term(1) = term(1) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_2_so_sym_pt3(k,q,j,i)
term(2) = term(2) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_12_so_sym_pt3(q,k,j,i)
term(3) = term(3) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_12_so_sym_pt3(k,q,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-0.5d+0) 
term(1) = term(1) * (-0.5d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(4) = term(4) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_1_so_sym_pt3(q,k,i,j)
term(5) = term(5) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_2_so_sym_pt3(q,k,i,j)
term(6) = term(6) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_1_so_sym_pt3(k,q,i,j)
term(7) = term(7) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_2_so_sym_pt3(k,q,i,j)
term(8) = term(8) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_12_so_sym_pt3(q,k,i,j)
term(9) = term(9) + wm_interm_0_so_sym_pt3(p,i,j,k) * wm_interm_12_so_sym_pt3(k,q,i,j)
term(10) = term(10) + wm_interm_23_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(j,i,q,k)
term(11) = term(11) + wm_interm_23_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(i,j,q,k)
term(12) = term(12) + wm_interm_23_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(i,j,k,q)
term(13) = term(13) + wm_interm_23_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(j,i,k,q)
term(14) = term(14) + wm_interm_25_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(j,i,k,q)
term(15) = term(15) + wm_interm_25_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(i,j,q,k)
term(16) = term(16) + wm_interm_26_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(j,i,q,k)
term(17) = term(17) + wm_interm_26_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(i,j,q,k)
term(18) = term(18) + wm_interm_26_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(i,j,k,q)
term(19) = term(19) + wm_interm_26_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(j,i,k,q)
term(20) = term(20) + wm_interm_27_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(j,i,k,q)
term(21) = term(21) + wm_interm_27_so_sym_pt3(p,i,j,k) * wm_interm_45_so_sym_pt3(i,j,q,k)
end do 
end do 
end do 

term(5) = term(5) * (-0.5d+0) 
term(6) = term(6) * (-0.5d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (-0.5d+0) 
term(12) = term(12) * (-0.5d+0) 
term(14) = term(14) * (-0.5d+0) 
term(15) = term(15) * (-0.5d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 
term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (-1.0d+0) 
term(21) = term(21) * (-1.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
term(22) = term(22) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,q) * wm_interm_21_so_sym_pt3(b,k,i,j)
term(23) = term(23) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_21_so_sym_pt3(b,k,i,q)
end do 
end do 
end do 
end do 
end do 

term(22) = term(22) * (-0.5d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(24) = term(24) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_21_so_sym_pt3(b,k,i,j)
term(25) = term(25) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,i,a,k) * wm_interm_21_so_sym_pt3(b,k,q,j)
term(26) = term(26) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_21_so_sym_pt3(b,k,i,q)
term(27) = term(27) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,i,a,k) * wm_interm_21_so_sym_pt3(b,k,j,q)
end do 
end do 
end do 
end do 
end do 

term(25) = term(25) * (-0.5d+0) 
term(26) = term(26) * (-0.5d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(28) = term(28) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,i,a,q) * wm_interm_19_so_sym_pt3(b,j)
term(29) = term(29) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,j,a,q) * wm_interm_19_so_sym_pt3(b,i)
term(30) = term(30) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,j) * wm_interm_19_so_sym_pt3(b,i)
term(31) = term(31) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,i,a,q) * wm_interm_20_so_sym_pt3(b,j)
term(32) = term(32) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,j,a,q) * wm_interm_20_so_sym_pt3(b,i)
term(33) = term(33) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,j) * wm_interm_20_so_sym_pt3(b,i)
end do 
end do 
end do 
end do 

term(28) = term(28) * (-2.0d+0) 
term(30) = term(30) * (-2.0d+0) 
term(32) = term(32) * (-0.5d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(34) = term(34) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,q) * wm_interm_21_so_sym_pt3(b,k,j,i)
term(35) = term(35) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,j) * wm_interm_21_so_sym_pt3(b,k,q,i)
term(36) = term(36) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_21_so_sym_pt3(b,k,q,j)
term(37) = term(37) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,a,i) * wm_interm_21_so_sym_pt3(b,k,j,q)
end do 
end do 
end do 
end do 
end do 

term(35) = term(35) * (-0.5d+0) 
term(37) = term(37) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(38) = term(38) + r2(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_35_so_sym_pt3(b,j)
term(39) = term(39) + r2(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_36_so_sym_pt3(b,j)
term(40) = term(40) + r2(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_37_so_sym_pt3(b,j)
term(41) = term(41) + r2(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_40_so_sym_pt3(b,j)
term(42) = term(42) + r2(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_41_so_sym_pt3(b,j)
end do 
end do 
end do 
end do 

term(38) = term(38) * (-2.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(43) = term(43) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,q) * wm_interm_21_so_sym_pt3(a,k,j,i)
term(44) = term(44) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_21_so_sym_pt3(b,k,j,i)
term(45) = term(45) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_21_so_sym_pt3(a,k,j,i)
term(46) = term(46) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_21_so_sym_pt3(a,k,q,i)
term(47) = term(47) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_21_so_sym_pt3(a,k,q,j)
term(48) = term(48) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,p,j) * wm_interm_21_so_sym_pt3(a,k,q,i)
term(49) = term(49) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,p,i) * wm_interm_21_so_sym_pt3(b,k,q,j)
term(50) = term(50) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,i) * wm_interm_21_so_sym_pt3(a,k,j,q)
term(51) = term(51) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,p,i) * wm_interm_21_so_sym_pt3(a,k,j,q)
term(52) = term(52) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,p,i) * wm_interm_21_so_sym_pt3(b,k,j,q)
term(53) = term(53) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_25_so_sym_pt3(b,j,k,i)
term(54) = term(54) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_25_so_sym_pt3(b,k,j,i)
term(55) = term(55) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_23_so_sym_pt3(b,k,j,i)
term(56) = term(56) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_21_so_sym_pt3(a,k,q,i)
term(57) = term(57) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_21_so_sym_pt3(b,k,q,i)
term(58) = term(58) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_21_so_sym_pt3(a,k,q,j)
term(59) = term(59) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_21_so_sym_pt3(b,k,q,j)
term(60) = term(60) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,i) * wm_interm_21_so_sym_pt3(a,k,j,q)
term(61) = term(61) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,i) * wm_interm_21_so_sym_pt3(b,k,j,q)
term(62) = term(62) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_26_so_sym_pt3(b,k,j,i)
term(63) = term(63) + r2(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_26_so_sym_pt3(b,j,k,i)
end do 
end do 
end do 
end do 
end do 

term(43) = term(43) * (-0.5d+0) 
term(44) = term(44) * (-4.0d+0) 
term(45) = term(45) * (2.0d+0) 
term(47) = term(47) * (-0.5d+0) 
term(48) = term(48) * (-0.5d+0) 
term(49) = term(49) * (-0.5d+0) 
term(51) = term(51) * (-0.5d+0) 
term(53) = term(53) * (-0.5d+0) 
term(55) = term(55) * (-0.5d+0) 
term(56) = term(56) * (-2.0d+0) 
term(57) = term(57) * (2.0d+0) 
term(58) = term(58) * (2.0d+0) 
term(59) = term(59) * (-2.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (4.0d+0) 
term(62) = term(62) * (-2.0d+0) 
term(63) = term(63) * (2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(64) = term(64) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,q) * wm_interm_21_so_sym_pt3(a,k,i,j)
term(65) = term(65) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,q) * wm_interm_21_so_sym_pt3(a,k,i,j)
term(66) = term(66) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,q) * wm_interm_21_so_sym_pt3(b,k,i,j)
term(67) = term(67) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,k,b,j) * wm_interm_21_so_sym_pt3(a,k,i,q)
term(68) = term(68) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,k,p,j) * wm_interm_21_so_sym_pt3(a,k,i,q)
term(69) = term(69) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,p,j) * wm_interm_21_so_sym_pt3(b,k,i,q)
term(70) = term(70) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,k,p,j) * wm_interm_21_so_sym_pt3(a,k,i,q)
term(71) = term(71) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,p,j) * wm_interm_21_so_sym_pt3(b,k,i,q)
end do 
end do 
end do 
end do 
end do 

term(65) = term(65) * (-4.0d+0) 
term(66) = term(66) * (2.0d+0) 
term(67) = term(67) * (-2.0d+0) 
term(69) = term(69) * (-0.5d+0) 
term(70) = term(70) * (4.0d+0) 
term(71) = term(71) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(72) = term(72) + wm_interm_3_so_sym_pt3(i,j) * wm_interm_46_so_sym_pt3(p,j,q,i)
term(73) = term(73) + wm_interm_13_so_sym_pt3(i,j) * wm_interm_46_so_sym_pt3(p,j,q,i)
term(74) = term(74) + wm_interm_14_so_sym_pt3(i,j) * wm_interm_46_so_sym_pt3(p,j,q,i)
end do 
end do 

term(72) = term(72) * (-1.0d+0) 
term(73) = term(73) * (-4.0d+0) 
term(74) = term(74) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(75) = term(75) + wm_interm_0_so_sym_pt3(p,i,q,j) * wm_interm_3_so_sym_pt3(j,i)
term(76) = term(76) + wm_interm_0_so_sym_pt3(p,i,q,j) * wm_interm_4_so_sym_pt3(j,i)
term(77) = term(77) + wm_interm_0_so_sym_pt3(p,i,q,j) * wm_interm_5_so_sym_pt3(j,i)
term(78) = term(78) + wm_interm_6_so_sym_pt3(p,i,q,j) * wm_interm_7_so_sym_pt3(i,j)
term(79) = term(79) + wm_interm_6_so_sym_pt3(p,i,q,j) * wm_interm_8_so_sym_pt3(i,j)
term(80) = term(80) + wm_interm_6_so_sym_pt3(p,i,q,j) * wm_interm_9_so_sym_pt3(i,j)
term(81) = term(81) + wm_interm_11_so_sym_pt3(p,q,i,j) * wm_interm_7_so_sym_pt3(i,j)
term(82) = term(82) + wm_interm_11_so_sym_pt3(p,q,i,j) * wm_interm_8_so_sym_pt3(i,j)
term(83) = term(83) + wm_interm_11_so_sym_pt3(p,q,i,j) * wm_interm_9_so_sym_pt3(i,j)
term(84) = term(84) + wm_interm_0_so_sym_pt3(p,i,q,j) * wm_interm_13_so_sym_pt3(j,i)
term(85) = term(85) + wm_interm_0_so_sym_pt3(p,i,q,j) * wm_interm_14_so_sym_pt3(j,i)
term(86) = term(86) + wm_interm_15_so_sym_pt3(i,j) * wm_interm_6_so_sym_pt3(p,i,q,j)
term(87) = term(87) + wm_interm_16_so_sym_pt3(i,j) * wm_interm_6_so_sym_pt3(p,i,q,j)
term(88) = term(88) + wm_interm_17_so_sym_pt3(i,j) * wm_interm_6_so_sym_pt3(p,i,q,j)
term(89) = term(89) + wm_interm_11_so_sym_pt3(p,q,i,j) * wm_interm_15_so_sym_pt3(i,j)
term(90) = term(90) + wm_interm_11_so_sym_pt3(p,q,i,j) * wm_interm_16_so_sym_pt3(i,j)
term(91) = term(91) + wm_interm_11_so_sym_pt3(p,q,i,j) * wm_interm_17_so_sym_pt3(i,j)
term(92) = term(92) + wm_interm_22_so_sym_pt3(i,j) * wm_interm_23_so_sym_pt3(p,i,q,j)
term(93) = term(93) + wm_interm_22_so_sym_pt3(i,j) * wm_interm_23_so_sym_pt3(p,q,i,j)
term(94) = term(94) + wm_interm_23_so_sym_pt3(p,i,q,j) * wm_interm_24_so_sym_pt3(i,j)
term(95) = term(95) + wm_interm_23_so_sym_pt3(p,q,i,j) * wm_interm_24_so_sym_pt3(i,j)
term(96) = term(96) + wm_interm_22_so_sym_pt3(i,j) * wm_interm_25_so_sym_pt3(p,q,i,j)
term(97) = term(97) + wm_interm_24_so_sym_pt3(i,j) * wm_interm_25_so_sym_pt3(p,q,i,j)
term(98) = term(98) + wm_interm_22_so_sym_pt3(i,j) * wm_interm_26_so_sym_pt3(p,i,q,j)
term(99) = term(99) + wm_interm_22_so_sym_pt3(i,j) * wm_interm_26_so_sym_pt3(p,q,i,j)
term(100) = term(100) + wm_interm_24_so_sym_pt3(i,j) * wm_interm_26_so_sym_pt3(p,i,q,j)
term(101) = term(101) + wm_interm_24_so_sym_pt3(i,j) * wm_interm_26_so_sym_pt3(p,q,i,j)
term(102) = term(102) + wm_interm_22_so_sym_pt3(i,j) * wm_interm_27_so_sym_pt3(p,q,i,j)
term(103) = term(103) + wm_interm_24_so_sym_pt3(i,j) * wm_interm_27_so_sym_pt3(p,q,i,j)
term(104) = term(104) + wm_interm_46_so_sym_pt3(p,i,q,j) * wm_interm_4_so_sym_pt3(j,i)
term(105) = term(105) + wm_interm_46_so_sym_pt3(p,i,q,j) * wm_interm_5_so_sym_pt3(j,i)
term(106) = term(106) + wm_interm_0_so_sym_pt3(p,i,q,j) * wm_interm_65_so_sym_pt3(j,i)
term(107) = term(107) + wm_interm_52_so_sym_pt3(i,j) * wm_interm_66_so_sym_pt3(p,i,q,j)
term(108) = term(108) + wm_interm_46_so_sym_pt3(p,i,q,j) * wm_interm_65_so_sym_pt3(j,i)
term(109) = term(109) + wm_interm_62_so_sym_pt3(i,j) * wm_interm_70_so_sym_pt3(p,i,q,j)
end do 
end do 

term(75) = term(75) * (-1.0d+0) 
term(76) = term(76) * (-1.0d+0) 
term(77) = term(77) * (2.0d+0) 
term(78) = term(78) * (-0.5d+0) 
term(80) = term(80) * (-0.5d+0) 
term(81) = term(81) * (-0.5d+0) 
term(83) = term(83) * (-0.5d+0) 
term(84) = term(84) * (-4.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (-1.0d+0) 
term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-1.0d+0) 
term(89) = term(89) * (-1.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-1.0d+0) 
term(92) = term(92) * (-1.0d+0) 
term(93) = term(93) * (2.0d+0) 
term(94) = term(94) * (2.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (-2.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (4.0d+0) 
term(101) = term(101) * (-8.0d+0) 
term(102) = term(102) * (-2.0d+0) 
term(103) = term(103) * (4.0d+0) 
term(104) = term(104) * (-1.0d+0) 
term(105) = term(105) * (2.0d+0) 
term(106) = term(106) * (2.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (2.0d+0) 
term(109) = term(109) * (2.0d+0) 

do i = 1, nocc 
term(110) = term(110) + wm_interm_22_so_sym_pt3(i,q) * wm_interm_35_so_sym_pt3(p,i)
term(111) = term(111) + wm_interm_24_so_sym_pt3(i,q) * wm_interm_35_so_sym_pt3(p,i)
term(112) = term(112) + wm_interm_22_so_sym_pt3(i,q) * wm_interm_36_so_sym_pt3(p,i)
term(113) = term(113) + wm_interm_24_so_sym_pt3(i,q) * wm_interm_36_so_sym_pt3(p,i)
term(114) = term(114) + wm_interm_22_so_sym_pt3(i,q) * wm_interm_37_so_sym_pt3(p,i)
term(115) = term(115) + wm_interm_24_so_sym_pt3(i,q) * wm_interm_37_so_sym_pt3(p,i)
term(116) = term(116) + wm_interm_22_so_sym_pt3(i,q) * wm_interm_40_so_sym_pt3(p,i)
term(117) = term(117) + wm_interm_24_so_sym_pt3(i,q) * wm_interm_40_so_sym_pt3(p,i)
term(118) = term(118) + wm_interm_22_so_sym_pt3(i,q) * wm_interm_41_so_sym_pt3(p,i)
term(119) = term(119) + wm_interm_24_so_sym_pt3(i,q) * wm_interm_41_so_sym_pt3(p,i)
term(120) = term(120) + wm_interm_52_so_sym_pt3(q,i) * wm_interm_64_so_sym_pt3(p,i)
end do 

term(110) = term(110) * (-2.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(113) = term(113) * (-2.0d+0) 
term(115) = term(115) * (-2.0d+0) 
term(116) = term(116) * (-4.0d+0) 
term(117) = term(117) * (8.0d+0) 
term(118) = term(118) * (4.0d+0) 
term(119) = term(119) * (-8.0d+0) 
term(120) = term(120) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(121) = term(121) + r2(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, a,j,b,k) * wm_interm_0_so_sym_pt3(b,j,k,i)
term(122) = term(122) + r2(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, a,j,b,k) * wm_interm_0_so_sym_pt3(b,k,j,i)
term(123) = term(123) + r2(vrdav_Rl, a,q,p,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_so_sym_pt3(b,j,k,i)
term(124) = term(124) + r2(vrdav_Rl, a,q,p,i) * r2m(vrdav_Rr, a,j,b,k) * wm_interm_0_so_sym_pt3(b,k,j,i)
term(125) = term(125) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_25_so_sym_pt3(b,k,j,i)
term(126) = term(126) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_25_so_sym_pt3(b,j,k,i)
term(127) = term(127) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_23_so_sym_pt3(b,j,k,i)
term(128) = term(128) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_26_so_sym_pt3(b,j,k,i)
term(129) = term(129) + r2(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_26_so_sym_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(121) = term(121) * (-1.0d+0) 
term(122) = term(122) * (2.0d+0) 
term(123) = term(123) * (-4.0d+0) 
term(124) = term(124) * (4.0d+0) 
term(125) = term(125) * (-0.5d+0) 
term(127) = term(127) * (-0.5d+0) 
term(128) = term(128) * (-2.0d+0) 
term(129) = term(129) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(130) = term(130) + r2(vrdav_Rl, a,q,p,i) * r2p(vrdav_Rr, b,j,a,k) * wm_interm_0_so_sym_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(130) = term(130) * (-1.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(131) = term(131) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_6_so_sym_pt3(a,q,j,k)
term(132) = term(132) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_6_so_sym_pt3(a,q,i,k)
term(133) = term(133) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_6_so_sym_pt3(a,q,i,k)
term(134) = term(134) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,j,k) * wm_interm_6_so_sym_pt3(b,q,i,k)
term(135) = term(135) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_6_so_sym_pt3(a,j,i,k)
term(136) = term(136) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_6_so_sym_pt3(b,i,j,k)
term(137) = term(137) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,j) * wm_interm_6_so_sym_pt3(b,i,q,k)
term(138) = term(138) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_6_so_sym_pt3(b,j,i,k)
term(139) = term(139) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_11_so_sym_pt3(a,j,q,k)
term(140) = term(140) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_11_so_sym_pt3(a,i,q,k)
term(141) = term(141) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,j,k) * wm_interm_11_so_sym_pt3(b,i,q,k)
term(142) = term(142) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_11_so_sym_pt3(a,i,j,k)
term(143) = term(143) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_11_so_sym_pt3(b,j,i,k)
term(144) = term(144) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,k) * wm_interm_11_so_sym_pt3(b,i,j,k)
term(145) = term(145) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,j) * wm_interm_11_so_sym_pt3(b,q,i,k)
term(146) = term(146) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_11_so_sym_pt3(a,i,q,k)
term(147) = term(147) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_6_so_sym_pt3(a,q,j,k)
term(148) = term(148) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_6_so_sym_pt3(a,q,i,k)
term(149) = term(149) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_6_so_sym_pt3(a,q,i,k)
term(150) = term(150) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_6_so_sym_pt3(a,j,i,k)
term(151) = term(151) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_6_so_sym_pt3(a,i,j,k)
term(152) = term(152) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_6_so_sym_pt3(a,i,q,k)
term(153) = term(153) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_11_so_sym_pt3(a,j,q,k)
term(154) = term(154) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_11_so_sym_pt3(a,i,q,k)
term(155) = term(155) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_11_so_sym_pt3(a,i,j,k)
term(156) = term(156) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_11_so_sym_pt3(a,j,i,k)
term(157) = term(157) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_11_so_sym_pt3(a,q,i,k)
term(158) = term(158) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_11_so_sym_pt3(a,i,q,k)
term(159) = term(159) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_21_so_sym_pt3(a,k,i,j)
term(160) = term(160) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,k) * wm_interm_21_so_sym_pt3(a,k,i,j)
term(161) = term(161) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,k) * wm_interm_21_so_sym_pt3(b,k,i,j)
term(162) = term(162) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_21_so_sym_pt3(a,k,i,j)
term(163) = term(163) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_21_so_sym_pt3(b,k,i,j)
term(164) = term(164) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,i,b,k) * wm_interm_21_so_sym_pt3(a,k,q,j)
term(165) = term(165) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,i,p,k) * wm_interm_21_so_sym_pt3(a,k,q,j)
term(166) = term(166) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,i,b,k) * wm_interm_21_so_sym_pt3(a,k,j,q)
term(167) = term(167) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,j,b,k) * wm_interm_21_so_sym_pt3(a,k,i,q)
term(168) = term(168) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_21_so_sym_pt3(a,k,q,j)
term(169) = term(169) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_21_so_sym_pt3(b,k,q,j)
term(170) = term(170) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,k) * wm_interm_21_so_sym_pt3(a,k,j,q)
term(171) = term(171) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_21_so_sym_pt3(a,k,i,q)
term(172) = term(172) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_21_so_sym_pt3(b,k,i,q)
term(173) = term(173) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,k) * wm_interm_21_so_sym_pt3(b,k,j,q)
end do 
end do 
end do 
end do 
end do 

term(131) = term(131) * (-0.5d+0) 
term(133) = term(133) * (-0.5d+0) 
term(134) = term(134) * (-0.5d+0) 
term(135) = term(135) * (-0.5d+0) 
term(136) = term(136) * (-0.5d+0) 
term(137) = term(137) * (-0.5d+0) 
term(139) = term(139) * (-0.5d+0) 
term(141) = term(141) * (-0.5d+0) 
term(142) = term(142) * (-0.5d+0) 
term(143) = term(143) * (-0.5d+0) 
term(145) = term(145) * (-0.5d+0) 
term(146) = term(146) * (-0.5d+0) 
term(147) = term(147) * (-2.0d+0) 
term(148) = term(148) * (2.0d+0) 
term(149) = term(149) * (-2.0d+0) 
term(150) = term(150) * (-2.0d+0) 
term(151) = term(151) * (2.0d+0) 
term(152) = term(152) * (2.0d+0) 
term(153) = term(153) * (-2.0d+0) 
term(154) = term(154) * (2.0d+0) 
term(155) = term(155) * (-2.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (2.0d+0) 
term(158) = term(158) * (-2.0d+0) 
term(159) = term(159) * (-2.0d+0) 
term(161) = term(161) * (-0.5d+0) 
term(162) = term(162) * (4.0d+0) 
term(163) = term(163) * (-2.0d+0) 
term(165) = term(165) * (-0.5d+0) 
term(166) = term(166) * (-0.5d+0) 
term(168) = term(168) * (-2.0d+0) 
term(169) = term(169) * (2.0d+0) 
term(170) = term(170) * (2.0d+0) 
term(171) = term(171) * (-4.0d+0) 
term(172) = term(172) * (2.0d+0) 
term(173) = term(173) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(174) = term(174) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,i) * wm_interm_19_so_sym_pt3(b,j)
term(175) = term(175) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,i) * wm_interm_19_so_sym_pt3(b,j)
term(176) = term(176) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_19_so_sym_pt3(a,j)
term(177) = term(177) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,i) * wm_interm_19_so_sym_pt3(a,j)
term(178) = term(178) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_20_so_sym_pt3(a,j)
term(179) = term(179) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,i) * wm_interm_20_so_sym_pt3(b,j)
term(180) = term(180) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,i) * wm_interm_20_so_sym_pt3(a,j)
term(181) = term(181) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,i) * wm_interm_20_so_sym_pt3(b,j)
term(182) = term(182) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,i) * wm_interm_19_so_sym_pt3(b,j)
term(183) = term(183) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_19_so_sym_pt3(a,j)
term(184) = term(184) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_20_so_sym_pt3(a,j)
term(185) = term(185) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,i) * wm_interm_20_so_sym_pt3(b,j)
term(186) = term(186) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,i) * wm_interm_28_so_sym_pt3(b,j)
term(187) = term(187) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_28_so_sym_pt3(a,j)
term(188) = term(188) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,i) * wm_interm_32_so_sym_pt3(b,j)
term(189) = term(189) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_32_so_sym_pt3(a,j)
term(190) = term(190) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_28_so_sym_pt3(a,j)
term(191) = term(191) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_32_so_sym_pt3(a,j)
end do 
end do 
end do 
end do 

term(174) = term(174) * (4.0d+0) 
term(175) = term(175) * (-2.0d+0) 
term(176) = term(176) * (-2.0d+0) 
term(179) = term(179) * (-2.0d+0) 
term(180) = term(180) * (-0.5d+0) 
term(182) = term(182) * (-8.0d+0) 
term(183) = term(183) * (4.0d+0) 
term(184) = term(184) * (-2.0d+0) 
term(185) = term(185) * (4.0d+0) 
term(186) = term(186) * (-4.0d+0) 
term(187) = term(187) * (2.0d+0) 
term(188) = term(188) * (2.0d+0) 
term(189) = term(189) * (-1.0d+0) 
term(190) = term(190) * (8.0d+0) 
term(191) = term(191) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(192) = term(192) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_6_so_sym_pt3(a,q,j,k)
term(193) = term(193) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,i) * wm_interm_6_so_sym_pt3(b,q,j,k)
term(194) = term(194) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_6_so_sym_pt3(a,j,q,k)
term(195) = term(195) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,i) * wm_interm_6_so_sym_pt3(b,j,q,k)
term(196) = term(196) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,i) * wm_interm_11_so_sym_pt3(b,q,j,k)
term(197) = term(197) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_11_so_sym_pt3(a,q,j,k)
term(198) = term(198) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,k,i) * wm_interm_11_so_sym_pt3(b,j,q,k)
term(199) = term(199) + r2p(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_11_so_sym_pt3(a,j,q,k)
term(200) = term(200) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_6_so_sym_pt3(a,q,j,k)
term(201) = term(201) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_6_so_sym_pt3(a,j,q,k)
term(202) = term(202) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_11_so_sym_pt3(a,q,j,k)
term(203) = term(203) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_11_so_sym_pt3(a,j,q,k)
term(204) = term(204) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,a,k) * wm_interm_21_so_sym_pt3(b,k,j,i)
term(205) = term(205) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,k) * wm_interm_21_so_sym_pt3(b,k,j,i)
term(206) = term(206) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,k) * wm_interm_21_so_sym_pt3(a,k,j,i)
term(207) = term(207) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,k) * wm_interm_21_so_sym_pt3(a,k,j,i)
term(208) = term(208) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,k) * wm_interm_21_so_sym_pt3(b,k,j,i)
term(209) = term(209) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,k) * wm_interm_21_so_sym_pt3(a,k,j,i)
term(210) = term(210) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,j,b,k) * wm_interm_21_so_sym_pt3(a,k,q,i)
term(211) = term(211) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,j,a,k) * wm_interm_21_so_sym_pt3(b,k,q,i)
term(212) = term(212) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,j,p,k) * wm_interm_21_so_sym_pt3(b,k,q,i)
term(213) = term(213) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,k) * wm_interm_21_so_sym_pt3(a,k,q,i)
term(214) = term(214) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,k) * wm_interm_21_so_sym_pt3(b,k,q,i)
end do 
end do 
end do 
end do 
end do 

term(193) = term(193) * (-0.5d+0) 
term(194) = term(194) * (-0.5d+0) 
term(197) = term(197) * (-0.5d+0) 
term(198) = term(198) * (-0.5d+0) 
term(200) = term(200) * (2.0d+0) 
term(201) = term(201) * (-2.0d+0) 
term(202) = term(202) * (-2.0d+0) 
term(203) = term(203) * (2.0d+0) 
term(204) = term(204) * (-2.0d+0) 
term(207) = term(207) * (-0.5d+0) 
term(208) = term(208) * (4.0d+0) 
term(209) = term(209) * (-2.0d+0) 
term(210) = term(210) * (-0.5d+0) 
term(212) = term(212) * (-0.5d+0) 
term(213) = term(213) * (2.0d+0) 
term(214) = term(214) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(215) = term(215) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,j) * wm_interm_19_so_sym_pt3(b,i)
term(216) = term(216) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,i,b,q) * wm_interm_19_so_sym_pt3(a,j)
term(217) = term(217) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,j,b,q) * wm_interm_19_so_sym_pt3(a,i)
term(218) = term(218) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_19_so_sym_pt3(a,i)
term(219) = term(219) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,j) * wm_interm_19_so_sym_pt3(a,i)
term(220) = term(220) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,i,b,q) * wm_interm_20_so_sym_pt3(a,j)
term(221) = term(221) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,q,p,j) * wm_interm_20_so_sym_pt3(b,i)
term(222) = term(222) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,j,b,q) * wm_interm_20_so_sym_pt3(a,i)
term(223) = term(223) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, p,q,b,j) * wm_interm_20_so_sym_pt3(a,i)
term(224) = term(224) + r2(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, b,q,p,j) * wm_interm_20_so_sym_pt3(a,i)
term(225) = term(225) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,q) * wm_interm_19_so_sym_pt3(b,j)
term(226) = term(226) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,j) * wm_interm_19_so_sym_pt3(b,i)
term(227) = term(227) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,q) * wm_interm_19_so_sym_pt3(b,i)
term(228) = term(228) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_19_so_sym_pt3(a,j)
term(229) = term(229) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_19_so_sym_pt3(a,i)
term(230) = term(230) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_19_so_sym_pt3(a,i)
term(231) = term(231) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_20_so_sym_pt3(a,j)
term(232) = term(232) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,p,q) * wm_interm_20_so_sym_pt3(b,j)
term(233) = term(233) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,q,p,j) * wm_interm_20_so_sym_pt3(b,i)
term(234) = term(234) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,p,q) * wm_interm_20_so_sym_pt3(b,i)
term(235) = term(235) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,q,p,j) * wm_interm_20_so_sym_pt3(a,i)
term(236) = term(236) + r2(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, b,j,p,q) * wm_interm_20_so_sym_pt3(a,i)
term(237) = term(237) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,j) * wm_interm_28_so_sym_pt3(b,i)
term(238) = term(238) + r2p(vrdav_Rr, a,i,b,j) * s2(a,p,q,j) * wm_interm_32_so_sym_pt3(b,i)
term(239) = term(239) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_28_so_sym_pt3(a,i)
term(240) = term(240) + r2m(vrdav_Rr, a,i,b,j) * s2(b,p,q,j) * wm_interm_32_so_sym_pt3(a,i)
end do 
end do 
end do 
end do 

term(217) = term(217) * (-2.0d+0) 
term(218) = term(218) * (4.0d+0) 
term(219) = term(219) * (-2.0d+0) 
term(220) = term(220) * (-0.5d+0) 
term(221) = term(221) * (-0.5d+0) 
term(223) = term(223) * (-2.0d+0) 
term(225) = term(225) * (8.0d+0) 
term(226) = term(226) * (4.0d+0) 
term(227) = term(227) * (-4.0d+0) 
term(228) = term(228) * (-4.0d+0) 
term(229) = term(229) * (-8.0d+0) 
term(230) = term(230) * (8.0d+0) 
term(231) = term(231) * (2.0d+0) 
term(232) = term(232) * (-4.0d+0) 
term(233) = term(233) * (-2.0d+0) 
term(234) = term(234) * (2.0d+0) 
term(235) = term(235) * (4.0d+0) 
term(236) = term(236) * (-4.0d+0) 
term(237) = term(237) * (2.0d+0) 
term(238) = term(238) * (-1.0d+0) 
term(239) = term(239) * (-8.0d+0) 
term(240) = term(240) * (4.0d+0) 

do a = nocc + 1, nactive 
term(241) = term(241) + wm_interm_28_so_sym_pt3(a,q) * wm_interm_29_so_sym_pt3(a,p)
term(242) = term(242) + wm_interm_28_so_sym_pt3(a,q) * wm_interm_30_so_sym_pt3(a,p)
term(243) = term(243) + wm_interm_28_so_sym_pt3(a,q) * wm_interm_31_so_sym_pt3(a,p)
term(244) = term(244) + wm_interm_29_so_sym_pt3(a,p) * wm_interm_32_so_sym_pt3(a,q)
term(245) = term(245) + wm_interm_30_so_sym_pt3(a,p) * wm_interm_32_so_sym_pt3(a,q)
term(246) = term(246) + wm_interm_31_so_sym_pt3(a,p) * wm_interm_32_so_sym_pt3(a,q)
term(247) = term(247) + wm_interm_28_so_sym_pt3(a,q) * wm_interm_33_so_sym_pt3(a,p)
term(248) = term(248) + wm_interm_28_so_sym_pt3(a,q) * wm_interm_34_so_sym_pt3(a,p)
term(249) = term(249) + wm_interm_32_so_sym_pt3(a,q) * wm_interm_33_so_sym_pt3(a,p)
term(250) = term(250) + wm_interm_32_so_sym_pt3(a,q) * wm_interm_34_so_sym_pt3(a,p)
term(251) = term(251) + wm_interm_19_so_sym_pt3(a,q) * wm_interm_42_so_sym_pt3(a,p)
term(252) = term(252) + wm_interm_19_so_sym_pt3(a,q) * wm_interm_43_so_sym_pt3(a,p)
term(253) = term(253) + wm_interm_19_so_sym_pt3(a,q) * wm_interm_44_so_sym_pt3(a,p)
term(254) = term(254) + wm_interm_20_so_sym_pt3(a,q) * wm_interm_42_so_sym_pt3(a,p)
term(255) = term(255) + wm_interm_20_so_sym_pt3(a,q) * wm_interm_43_so_sym_pt3(a,p)
term(256) = term(256) + wm_interm_20_so_sym_pt3(a,q) * wm_interm_44_so_sym_pt3(a,p)
term(257) = term(257) + wm_interm_19_so_sym_pt3(a,q) * wm_interm_47_so_sym_pt3(a,p)
term(258) = term(258) + wm_interm_19_so_sym_pt3(a,q) * wm_interm_48_so_sym_pt3(a,p)
term(259) = term(259) + wm_interm_20_so_sym_pt3(a,q) * wm_interm_47_so_sym_pt3(a,p)
term(260) = term(260) + wm_interm_20_so_sym_pt3(a,q) * wm_interm_48_so_sym_pt3(a,p)
term(261) = term(261) + wm_interm_61_so_sym_pt3(p,a) * wm_interm_67_so_sym_pt3(a,q)
term(262) = term(262) + wm_interm_19_so_sym_pt3(a,q) * wm_interm_68_so_sym_pt3(a,p)
term(263) = term(263) + wm_interm_20_so_sym_pt3(a,q) * wm_interm_68_so_sym_pt3(a,p)
term(264) = term(264) + wm_interm_61_so_sym_pt3(p,a) * wm_interm_69_so_sym_pt3(a,q)
end do 

term(241) = term(241) * (2.0d+0) 
term(242) = term(242) * (2.0d+0) 
term(243) = term(243) * (-4.0d+0) 
term(244) = term(244) * (-1.0d+0) 
term(245) = term(245) * (-1.0d+0) 
term(246) = term(246) * (2.0d+0) 
term(247) = term(247) * (8.0d+0) 
term(248) = term(248) * (-8.0d+0) 
term(249) = term(249) * (-4.0d+0) 
term(250) = term(250) * (4.0d+0) 
term(251) = term(251) * (2.0d+0) 
term(252) = term(252) * (-4.0d+0) 
term(253) = term(253) * (2.0d+0) 
term(254) = term(254) * (-1.0d+0) 
term(255) = term(255) * (2.0d+0) 
term(256) = term(256) * (-1.0d+0) 
term(257) = term(257) * (8.0d+0) 
term(258) = term(258) * (-8.0d+0) 
term(259) = term(259) * (-4.0d+0) 
term(260) = term(260) * (4.0d+0) 
term(261) = term(261) * (2.0d+0) 
term(262) = term(262) * (-4.0d+0) 
term(263) = term(263) * (2.0d+0) 
term(264) = term(264) * (-4.0d+0) 


    calc_D_vo_wm_so_sym_pt3 = zero
    do s = 0, 264
    calc_D_vo_wm_so_sym_pt3 = calc_D_vo_wm_so_sym_pt3 + term(s)
    end do

    end function calc_D_vo_wm_so_sym_pt3
    
    function calc_D_vv_wm_so_sym_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_sym_pt3
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, i, j, a, b 
    real(F64), dimension(0:55) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_0_so_sym_pt3(q,i,j,k) * wm_interm_50_so_sym_pt3(p,k,j,i)
term(1) = term(1) + wm_interm_0_so_sym_pt3(q,i,j,k) * wm_interm_53_so_sym_pt3(p,k,j,i)
term(2) = term(2) + wm_interm_46_so_sym_pt3(q,i,j,k) * wm_interm_60_so_sym_pt3(p,j,k,i)
term(3) = term(3) + wm_interm_46_so_sym_pt3(q,i,j,k) * wm_interm_59_so_sym_pt3(p,k,j,i)
end do 
end do 
end do 

term(1) = term(1) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(4) = term(4) + wm_interm_0_so_sym_pt3(q,i,j,k) * wm_interm_50_so_sym_pt3(p,k,i,j)
term(5) = term(5) + wm_interm_0_so_sym_pt3(q,i,j,k) * wm_interm_49_so_sym_pt3(p,k,i,j)
term(6) = term(6) + wm_interm_0_so_sym_pt3(q,i,j,k) * wm_interm_53_so_sym_pt3(p,k,i,j)
end do 
end do 
end do 

term(4) = term(4) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(7) = term(7) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,k,i) * wm_interm_52_so_sym_pt3(j,k)
end do 
end do 
end do 
end do 

term(7) = term(7) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(8) = term(8) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,k,i) * wm_interm_52_so_sym_pt3(j,k)
term(9) = term(9) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,k,j) * wm_interm_52_so_sym_pt3(i,k)
term(10) = term(10) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,k,i) * wm_interm_52_so_sym_pt3(j,k)
term(11) = term(11) + r2(vrdav_Rl, a,j,p,i) * t2(a,q,k,j) * wm_interm_62_so_sym_pt3(i,k)
term(12) = term(12) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,i,j) * wm_interm_62_so_sym_pt3(k,j)
term(13) = term(13) + r2(vrdav_Rl, a,k,p,i) * t2(a,q,k,j) * wm_interm_62_so_sym_pt3(i,j)
end do 
end do 
end do 
end do 

term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (2.0d+0) 
term(13) = term(13) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(14) = term(14) + r2p(vrdav_Rr, a,i,p,j) * s2(a,q,j,k) * wm_interm_52_so_sym_pt3(i,k)
term(15) = term(15) + r2m(vrdav_Rr, a,i,p,j) * s2(a,q,j,k) * wm_interm_52_so_sym_pt3(i,k)
end do 
end do 
end do 
end do 

term(15) = term(15) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,i,k) * wm_interm_52_so_sym_pt3(j,k)
term(17) = term(17) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,j,k) * wm_interm_52_so_sym_pt3(i,k)
end do 
end do 
end do 
end do 

term(16) = term(16) * (2.0d+0) 
term(17) = term(17) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(18) = term(18) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_19_so_sym_pt3(a,j)
term(19) = term(19) + r2(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_20_so_sym_pt3(a,j)
end do 
end do 
end do 

term(18) = term(18) * (8.0d+0) 
term(19) = term(19) * (-4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(20) = term(20) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,k,j) * wm_interm_52_so_sym_pt3(i,k)
end do 
end do 
end do 
end do 


do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(21) = term(21) + r2m(vrdav_Rr, a,j,p,i) * s2(a,q,k,i) * wm_interm_52_so_sym_pt3(j,k)
end do 
end do 
end do 
end do 

term(21) = term(21) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(22) = term(22) + wm_interm_30_so_sym_pt3(p,a) * wm_interm_51_so_sym_pt3(q,a)
term(23) = term(23) + wm_interm_31_so_sym_pt3(p,a) * wm_interm_51_so_sym_pt3(q,a)
term(24) = term(24) + wm_interm_29_so_sym_pt3(p,a) * wm_interm_51_so_sym_pt3(q,a)
term(25) = term(25) + wm_interm_33_so_sym_pt3(p,a) * wm_interm_51_so_sym_pt3(q,a)
term(26) = term(26) + wm_interm_34_so_sym_pt3(p,a) * wm_interm_51_so_sym_pt3(q,a)
term(27) = term(27) + wm_interm_29_so_sym_pt3(a,p) * wm_interm_51_so_sym_pt3(a,q)
term(28) = term(28) + wm_interm_30_so_sym_pt3(a,p) * wm_interm_51_so_sym_pt3(a,q)
term(29) = term(29) + wm_interm_31_so_sym_pt3(a,p) * wm_interm_51_so_sym_pt3(a,q)
term(30) = term(30) + wm_interm_33_so_sym_pt3(a,p) * wm_interm_51_so_sym_pt3(a,q)
term(31) = term(31) + wm_interm_34_so_sym_pt3(a,p) * wm_interm_51_so_sym_pt3(a,q)
term(32) = term(32) + wm_interm_39_so_sym_pt3(q,a) * wm_interm_61_so_sym_pt3(p,a)
term(33) = term(33) + wm_interm_38_so_sym_pt3(q,a) * wm_interm_61_so_sym_pt3(p,a)
term(34) = term(34) + wm_interm_38_so_sym_pt3(a,q) * wm_interm_61_so_sym_pt3(a,p)
term(35) = term(35) + wm_interm_39_so_sym_pt3(a,q) * wm_interm_61_so_sym_pt3(a,p)
end do 

term(23) = term(23) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-4.0d+0) 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (4.0d+0) 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (2.0d+0) 
term(35) = term(35) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(36) = term(36) + s2(a,p,i,j) * t1(q,i) * wm_interm_54_so_sym_pt3(a,j)
term(37) = term(37) + s2(a,p,i,j) * t1(q,i) * wm_interm_55_so_sym_pt3(a,j)
term(38) = term(38) + s2(a,p,i,j) * t1(q,i) * wm_interm_56_so_sym_pt3(a,j)
term(39) = term(39) + s2(a,p,i,j) * t1(q,i) * wm_interm_57_so_sym_pt3(a,j)
term(40) = term(40) + s2(a,p,i,j) * t1(q,i) * wm_interm_58_so_sym_pt3(a,j)
term(41) = term(41) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_19_so_sym_pt3(a,j)
term(42) = term(42) + r2(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_20_so_sym_pt3(a,j)
term(43) = term(43) + s1(p,i) * t2(a,q,i,j) * wm_interm_63_so_sym_pt3(a,j)
end do 
end do 
end do 

term(37) = term(37) * (-2.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (-4.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (2.0d+0) 

do i = 1, nocc 
term(44) = term(44) + wm_interm_20_so_sym_pt3(q,i) * wm_interm_63_so_sym_pt3(p,i)
term(45) = term(45) + wm_interm_19_so_sym_pt3(q,i) * wm_interm_63_so_sym_pt3(p,i)
end do 

term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(46) = term(46) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_7_so_sym_pt3(i,j)
term(47) = term(47) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_8_so_sym_pt3(i,j)
term(48) = term(48) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_9_so_sym_pt3(i,j)
term(49) = term(49) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_15_so_sym_pt3(i,j)
term(50) = term(50) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_16_so_sym_pt3(i,j)
term(51) = term(51) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_17_so_sym_pt3(i,j)
term(52) = term(52) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_22_so_sym_pt3(i,j)
term(53) = term(53) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_24_so_sym_pt3(i,j)
end do 
end do 

term(47) = term(47) * (-2.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(54) = term(54) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,i,k) * wm_interm_52_so_sym_pt3(j,k)
term(55) = term(55) + r2p(vrdav_Rr, p,i,a,j) * s2(a,q,j,k) * wm_interm_52_so_sym_pt3(i,k)
end do 
end do 
end do 
end do 

term(55) = term(55) * (-2.0d+0) 


    calc_D_vv_wm_so_sym_pt3 = zero
    do s = 0, 55
    calc_D_vv_wm_so_sym_pt3 = calc_D_vv_wm_so_sym_pt3 + term(s)
    end do

    end function calc_D_vv_wm_so_sym_pt3

end module density_exc_exc_functions_so_pt0123_sym
    
