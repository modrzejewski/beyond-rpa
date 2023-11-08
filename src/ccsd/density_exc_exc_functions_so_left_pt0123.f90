module density_exc_exc_functions_so_left_pt0123

    use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

implicit none

   !
    ! File generated automatically on 2017-11-02 15:44:19
!
   real(F64), dimension(:, :), allocatable :: wm_interm_0_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_3_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_10_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_11_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_12_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_16_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_17_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_left_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_left_pt1

   real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_3_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_6_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_7_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_9_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_10_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_11_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_12_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_14_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_15_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_16_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_17_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_20_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_21_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_22_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_23_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_24_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_25_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_26_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_27_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_28_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_29_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_32_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_33_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_37_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_38_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_39_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_40_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_41_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_44_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_45_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_46_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_47_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_48_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_49_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_50_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_51_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_53_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_54_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_55_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_56_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_57_so_left_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_58_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_so_left_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_so_left_pt2


    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_1_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_2_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_3_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_4_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_5_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_7_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_8_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_10_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_11_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_12_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_13_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_15_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_16_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_17_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_18_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_19_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_20_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_21_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_22_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_23_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_24_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_25_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_26_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_27_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_28_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_29_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_30_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_32_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_33_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_34_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_35_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_36_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_37_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_38_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_39_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_40_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_41_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_42_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_43_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_44_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_45_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_46_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_47_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_48_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_49_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_50_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_51_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_52_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_53_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_54_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_55_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_56_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_57_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_58_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_59_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_60_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_61_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_62_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_63_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_64_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_65_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_66_so_left_pt3 
real(F64), dimension(:, :), allocatable :: wm_interm_67_so_left_pt3 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_68_so_left_pt3 




contains
    subroutine wm_so_left_intermediates_ccsd_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    
    end subroutine wm_so_left_intermediates_ccsd_init_pt0
    
    subroutine wm_so_left_intermediates_ccsd_free_pt0
    
    end subroutine wm_so_left_intermediates_ccsd_free_pt0
    
    subroutine wm_so_left_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    real(F64), dimension(:), intent(in) :: vrdav_Rl
    real(F64), dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum

    end subroutine wm_so_left_intermediates_ccsd_pt0

   subroutine wm_so_left_intermediates_ccsd_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_left_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_1_so_left_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_2_so_left_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_3_so_left_pt1(1: nocc, 1: nocc))
allocate(wm_interm_4_so_left_pt1(1: nocc, 1: nocc))
allocate(wm_interm_5_so_left_pt1(1: nocc, 1: nocc))
allocate(wm_interm_6_so_left_pt1(1: nocc, 1: nocc))
allocate(wm_interm_7_so_left_pt1(1: nocc, 1: nocc))
allocate(wm_interm_8_so_left_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_9_so_left_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_10_so_left_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_11_so_left_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_12_so_left_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_13_so_left_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_14_so_left_pt1(1: nocc, 1: nocc))
allocate(wm_interm_15_so_left_pt1(1: nocc, 1: nocc))
allocate(wm_interm_16_so_left_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_17_so_left_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_18_so_left_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_19_so_left_pt1(nocc+1: nactive, 1: nocc))
wm_interm_0_so_left_pt1 = zero 
wm_interm_1_so_left_pt1 = zero 
wm_interm_2_so_left_pt1 = zero 
wm_interm_3_so_left_pt1 = zero 
wm_interm_4_so_left_pt1 = zero 
wm_interm_5_so_left_pt1 = zero 
wm_interm_6_so_left_pt1 = zero 
wm_interm_7_so_left_pt1 = zero 
wm_interm_8_so_left_pt1 = zero 
wm_interm_9_so_left_pt1 = zero 
wm_interm_10_so_left_pt1 = zero 
wm_interm_11_so_left_pt1 = zero 
wm_interm_12_so_left_pt1 = zero 
wm_interm_13_so_left_pt1 = zero 
wm_interm_14_so_left_pt1 = zero 
wm_interm_15_so_left_pt1 = zero 
wm_interm_16_so_left_pt1 = zero 
wm_interm_17_so_left_pt1 = zero 
wm_interm_18_so_left_pt1 = zero 
wm_interm_19_so_left_pt1 = zero 

    end subroutine wm_so_left_intermediates_ccsd_init_pt1
    
    subroutine wm_so_left_intermediates_ccsd_free_pt1
    deallocate(wm_interm_0_so_left_pt1)
deallocate(wm_interm_1_so_left_pt1)
deallocate(wm_interm_2_so_left_pt1)
deallocate(wm_interm_3_so_left_pt1)
deallocate(wm_interm_4_so_left_pt1)
deallocate(wm_interm_5_so_left_pt1)
deallocate(wm_interm_6_so_left_pt1)
deallocate(wm_interm_7_so_left_pt1)
deallocate(wm_interm_8_so_left_pt1)
deallocate(wm_interm_9_so_left_pt1)
deallocate(wm_interm_10_so_left_pt1)
deallocate(wm_interm_11_so_left_pt1)
deallocate(wm_interm_12_so_left_pt1)
deallocate(wm_interm_13_so_left_pt1)
deallocate(wm_interm_14_so_left_pt1)
deallocate(wm_interm_15_so_left_pt1)
deallocate(wm_interm_16_so_left_pt1)
deallocate(wm_interm_17_so_left_pt1)
deallocate(wm_interm_18_so_left_pt1)
deallocate(wm_interm_19_so_left_pt1)

    end subroutine wm_so_left_intermediates_ccsd_free_pt1
    
    subroutine wm_so_left_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_0_so_left_pt1(b, j) = wm_interm_0_so_left_pt1(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_1_so_left_pt1(b, c) = wm_interm_1_so_left_pt1(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_2_so_left_pt1(b, c) = wm_interm_2_so_left_pt1(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_3_so_left_pt1(j, k) = wm_interm_3_so_left_pt1(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_4_so_left_pt1(j, k) = wm_interm_4_so_left_pt1(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_5_so_left_pt1(j, k) = wm_interm_5_so_left_pt1(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_6_so_left_pt1(j, k) = wm_interm_6_so_left_pt1(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_7_so_left_pt1(j, k) = wm_interm_7_so_left_pt1(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_8_so_left_pt1(b, c) = wm_interm_8_so_left_pt1(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_9_so_left_pt1(b, c) = wm_interm_9_so_left_pt1(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_10_so_left_pt1(b, c) = wm_interm_10_so_left_pt1(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_11_so_left_pt1(b, j) = wm_interm_11_so_left_pt1(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_12_so_left_pt1(b, j) = wm_interm_12_so_left_pt1(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_13_so_left_pt1(b, j) = wm_interm_13_so_left_pt1(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_14_so_left_pt1(j, k) = wm_interm_14_so_left_pt1(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_15_so_left_pt1(j, k) = wm_interm_15_so_left_pt1(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_16_so_left_pt1(b, c) = wm_interm_16_so_left_pt1(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_17_so_left_pt1(b, c) = wm_interm_17_so_left_pt1(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_18_so_left_pt1(b, j) = wm_interm_18_so_left_pt1(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_19_so_left_pt1(b, j) = wm_interm_19_so_left_pt1(b, j) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



end subroutine wm_so_left_intermediates_ccsd_pt1


   subroutine wm_so_left_intermediates_ccsd_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_4_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_5_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_6_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_8_so_left_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_10_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_11_so_left_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_12_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_13_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_14_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_15_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_16_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_17_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_18_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_19_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_20_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_23_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_24_so_left_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_25_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_26_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_27_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_28_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_29_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_30_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_33_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_34_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_35_so_left_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_37_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_38_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_39_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_40_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_41_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_42_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_43_so_left_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_44_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_45_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_46_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_47_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_48_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_49_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_50_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_51_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_52_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_53_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_54_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_55_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_56_so_left_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_57_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_58_so_left_pt2(1: nocc, 1: nocc))
allocate(wm_interm_59_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_so_left_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_so_left_pt2 = zero 
wm_interm_1_so_left_pt2 = zero 
wm_interm_2_so_left_pt2 = zero 
wm_interm_3_so_left_pt2 = zero 
wm_interm_4_so_left_pt2 = zero 
wm_interm_5_so_left_pt2 = zero 
wm_interm_6_so_left_pt2 = zero 
wm_interm_7_so_left_pt2 = zero 
wm_interm_8_so_left_pt2 = zero 
wm_interm_9_so_left_pt2 = zero 
wm_interm_10_so_left_pt2 = zero 
wm_interm_11_so_left_pt2 = zero 
wm_interm_12_so_left_pt2 = zero 
wm_interm_13_so_left_pt2 = zero 
wm_interm_14_so_left_pt2 = zero 
wm_interm_15_so_left_pt2 = zero 
wm_interm_16_so_left_pt2 = zero 
wm_interm_17_so_left_pt2 = zero 
wm_interm_18_so_left_pt2 = zero 
wm_interm_19_so_left_pt2 = zero 
wm_interm_20_so_left_pt2 = zero 
wm_interm_21_so_left_pt2 = zero 
wm_interm_22_so_left_pt2 = zero 
wm_interm_23_so_left_pt2 = zero 
wm_interm_24_so_left_pt2 = zero 
wm_interm_25_so_left_pt2 = zero 
wm_interm_26_so_left_pt2 = zero 
wm_interm_27_so_left_pt2 = zero 
wm_interm_28_so_left_pt2 = zero 
wm_interm_29_so_left_pt2 = zero 
wm_interm_30_so_left_pt2 = zero 
wm_interm_31_so_left_pt2 = zero 
wm_interm_32_so_left_pt2 = zero 
wm_interm_33_so_left_pt2 = zero 
wm_interm_34_so_left_pt2 = zero 
wm_interm_35_so_left_pt2 = zero 
wm_interm_36_so_left_pt2 = zero 
wm_interm_37_so_left_pt2 = zero 
wm_interm_38_so_left_pt2 = zero 
wm_interm_39_so_left_pt2 = zero 
wm_interm_40_so_left_pt2 = zero 
wm_interm_41_so_left_pt2 = zero 
wm_interm_42_so_left_pt2 = zero 
wm_interm_43_so_left_pt2 = zero 
wm_interm_44_so_left_pt2 = zero 
wm_interm_45_so_left_pt2 = zero 
wm_interm_46_so_left_pt2 = zero 
wm_interm_47_so_left_pt2 = zero 
wm_interm_48_so_left_pt2 = zero 
wm_interm_49_so_left_pt2 = zero 
wm_interm_50_so_left_pt2 = zero 
wm_interm_51_so_left_pt2 = zero 
wm_interm_52_so_left_pt2 = zero 
wm_interm_53_so_left_pt2 = zero 
wm_interm_54_so_left_pt2 = zero 
wm_interm_55_so_left_pt2 = zero 
wm_interm_56_so_left_pt2 = zero 
wm_interm_57_so_left_pt2 = zero 
wm_interm_58_so_left_pt2 = zero 
wm_interm_59_so_left_pt2 = zero 
wm_interm_60_so_left_pt2 = zero 

    end subroutine wm_so_left_intermediates_ccsd_init_pt2
    
    subroutine wm_so_left_intermediates_ccsd_free_pt2
    deallocate(wm_interm_0_so_left_pt2)
deallocate(wm_interm_1_so_left_pt2)
deallocate(wm_interm_2_so_left_pt2)
deallocate(wm_interm_3_so_left_pt2)
deallocate(wm_interm_4_so_left_pt2)
deallocate(wm_interm_5_so_left_pt2)
deallocate(wm_interm_6_so_left_pt2)
deallocate(wm_interm_7_so_left_pt2)
deallocate(wm_interm_8_so_left_pt2)
deallocate(wm_interm_9_so_left_pt2)
deallocate(wm_interm_10_so_left_pt2)
deallocate(wm_interm_11_so_left_pt2)
deallocate(wm_interm_12_so_left_pt2)
deallocate(wm_interm_13_so_left_pt2)
deallocate(wm_interm_14_so_left_pt2)
deallocate(wm_interm_15_so_left_pt2)
deallocate(wm_interm_16_so_left_pt2)
deallocate(wm_interm_17_so_left_pt2)
deallocate(wm_interm_18_so_left_pt2)
deallocate(wm_interm_19_so_left_pt2)
deallocate(wm_interm_20_so_left_pt2)
deallocate(wm_interm_21_so_left_pt2)
deallocate(wm_interm_22_so_left_pt2)
deallocate(wm_interm_23_so_left_pt2)
deallocate(wm_interm_24_so_left_pt2)
deallocate(wm_interm_25_so_left_pt2)
deallocate(wm_interm_26_so_left_pt2)
deallocate(wm_interm_27_so_left_pt2)
deallocate(wm_interm_28_so_left_pt2)
deallocate(wm_interm_29_so_left_pt2)
deallocate(wm_interm_30_so_left_pt2)
deallocate(wm_interm_31_so_left_pt2)
deallocate(wm_interm_32_so_left_pt2)
deallocate(wm_interm_33_so_left_pt2)
deallocate(wm_interm_34_so_left_pt2)
deallocate(wm_interm_35_so_left_pt2)
deallocate(wm_interm_36_so_left_pt2)
deallocate(wm_interm_37_so_left_pt2)
deallocate(wm_interm_38_so_left_pt2)
deallocate(wm_interm_39_so_left_pt2)
deallocate(wm_interm_40_so_left_pt2)
deallocate(wm_interm_41_so_left_pt2)
deallocate(wm_interm_42_so_left_pt2)
deallocate(wm_interm_43_so_left_pt2)
deallocate(wm_interm_44_so_left_pt2)
deallocate(wm_interm_45_so_left_pt2)
deallocate(wm_interm_46_so_left_pt2)
deallocate(wm_interm_47_so_left_pt2)
deallocate(wm_interm_48_so_left_pt2)
deallocate(wm_interm_49_so_left_pt2)
deallocate(wm_interm_50_so_left_pt2)
deallocate(wm_interm_51_so_left_pt2)
deallocate(wm_interm_52_so_left_pt2)
deallocate(wm_interm_53_so_left_pt2)
deallocate(wm_interm_54_so_left_pt2)
deallocate(wm_interm_55_so_left_pt2)
deallocate(wm_interm_56_so_left_pt2)
deallocate(wm_interm_57_so_left_pt2)
deallocate(wm_interm_58_so_left_pt2)
deallocate(wm_interm_59_so_left_pt2)
deallocate(wm_interm_60_so_left_pt2)

    end subroutine wm_so_left_intermediates_ccsd_free_pt2
    
    subroutine wm_so_left_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_0_so_left_pt2(b, i, j, k) = wm_interm_0_so_left_pt2(b, i, j, k) + sum 
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
wm_interm_1_so_left_pt2(j, k) = wm_interm_1_so_left_pt2(j, k) + sum 
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
wm_interm_2_so_left_pt2(j, k) = wm_interm_2_so_left_pt2(j, k) + sum 
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
wm_interm_3_so_left_pt2(b, c) = wm_interm_3_so_left_pt2(b, c) + sum 
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
wm_interm_4_so_left_pt2(b, c) = wm_interm_4_so_left_pt2(b, c) + sum 
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
wm_interm_5_so_left_pt2(b, j) = wm_interm_5_so_left_pt2(b, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_6_so_left_pt2(b, j) = wm_interm_6_so_left_pt2(b, j) + sum 
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
wm_interm_7_so_left_pt2(b, i, j, k) = wm_interm_7_so_left_pt2(b, i, j, k) + sum 
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
wm_interm_8_so_left_pt2(i, j, k, l) = wm_interm_8_so_left_pt2(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_9_so_left_pt2(b, c) = wm_interm_9_so_left_pt2(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_10_so_left_pt2(b, c) = wm_interm_10_so_left_pt2(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_11_so_left_pt2(i, j, k, l) = wm_interm_11_so_left_pt2(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_12_so_left_pt2(j, k) = wm_interm_12_so_left_pt2(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_13_so_left_pt2(j, k) = wm_interm_13_so_left_pt2(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_14_so_left_pt2(b, j) = wm_interm_14_so_left_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_15_so_left_pt2(b, i, j, k) = wm_interm_15_so_left_pt2(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_16_so_left_pt2(b, i, j, k) = wm_interm_16_so_left_pt2(b, i, j, k) + sum 
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
wm_interm_17_so_left_pt2(b, i, j, k) = wm_interm_17_so_left_pt2(b, i, j, k) + sum 
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
wm_interm_18_so_left_pt2(b, j) = wm_interm_18_so_left_pt2(b, j) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,i,j)
end do 
end do 
wm_interm_19_so_left_pt2(b, j) = wm_interm_19_so_left_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_20_so_left_pt2(b, j) = wm_interm_20_so_left_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_21_so_left_pt2(b, j) = wm_interm_21_so_left_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_22_so_left_pt2(b, c) = wm_interm_22_so_left_pt2(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_23_so_left_pt2(j, k) = wm_interm_23_so_left_pt2(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_24_so_left_pt2(i, j, k, l) = wm_interm_24_so_left_pt2(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_25_so_left_pt2(b, c) = wm_interm_25_so_left_pt2(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_26_so_left_pt2(j, k) = wm_interm_26_so_left_pt2(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_27_so_left_pt2(b, c) = wm_interm_27_so_left_pt2(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_28_so_left_pt2(j, k) = wm_interm_28_so_left_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_29_so_left_pt2(b, j) = wm_interm_29_so_left_pt2(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_30_so_left_pt2(b, j, i, k) = wm_interm_30_so_left_pt2(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_31_so_left_pt2(b, i, j, k) = wm_interm_31_so_left_pt2(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_32_so_left_pt2(b, j) = wm_interm_32_so_left_pt2(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_33_so_left_pt2(b, c) = wm_interm_33_so_left_pt2(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_34_so_left_pt2(j, k) = wm_interm_34_so_left_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_35_so_left_pt2(i, j, k, l) = wm_interm_35_so_left_pt2(i, j, k, l) + sum 
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
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_36_so_left_pt2(b, c) = wm_interm_36_so_left_pt2(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_37_so_left_pt2(j, k) = wm_interm_37_so_left_pt2(j, k) + sum 
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
wm_interm_38_so_left_pt2(i, j) = wm_interm_38_so_left_pt2(i, j) + sum 
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
wm_interm_39_so_left_pt2(a, b) = wm_interm_39_so_left_pt2(a, b) + sum 
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
wm_interm_40_so_left_pt2(a, b) = wm_interm_40_so_left_pt2(a, b) + sum 
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
wm_interm_41_so_left_pt2(i, j) = wm_interm_41_so_left_pt2(i, j) + sum 
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
wm_interm_42_so_left_pt2(i, j) = wm_interm_42_so_left_pt2(i, j) + sum 
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
wm_interm_43_so_left_pt2(a, b) = wm_interm_43_so_left_pt2(a, b) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_44_so_left_pt2(b, j) = wm_interm_44_so_left_pt2(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_45_so_left_pt2(b, j) = wm_interm_45_so_left_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t1(a,i)
end do 
end do 
wm_interm_46_so_left_pt2(b, j) = wm_interm_46_so_left_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t1(a,i)
end do 
end do 
wm_interm_47_so_left_pt2(b, j) = wm_interm_47_so_left_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_48_so_left_pt2(b, j) = wm_interm_48_so_left_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_49_so_left_pt2(j, k) = wm_interm_49_so_left_pt2(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_50_so_left_pt2(j, k) = wm_interm_50_so_left_pt2(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_51_so_left_pt2(j, k) = wm_interm_51_so_left_pt2(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_52_so_left_pt2(b, j, i, k) = wm_interm_52_so_left_pt2(b, j, i, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_53_so_left_pt2(b, i, j, k) = wm_interm_53_so_left_pt2(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t1(a,k)
end do 
wm_interm_54_so_left_pt2(b, i, j, k) = wm_interm_54_so_left_pt2(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_55_so_left_pt2(b, j) = wm_interm_55_so_left_pt2(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_56_so_left_pt2(b, j) = wm_interm_56_so_left_pt2(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_57_so_left_pt2(j, k) = wm_interm_57_so_left_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_58_so_left_pt2(j, k) = wm_interm_58_so_left_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_59_so_left_pt2(b, i, j, k) = wm_interm_59_so_left_pt2(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_60_so_left_pt2(b, j, i, k) = wm_interm_60_so_left_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_so_left_intermediates_ccsd_pt2


    subroutine wm_so_left_intermediates_ccsd_init_pt3(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_so_left_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_2_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_3_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_4_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_5_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_6_so_left_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_7_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_8_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_9_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_11_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_12_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_13_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_14_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_16_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_17_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_18_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_19_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_20_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_21_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_22_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_23_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_24_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_25_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_26_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_27_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_28_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_29_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_30_so_left_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_31_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_33_so_left_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_34_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_35_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_36_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_37_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_38_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_39_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_40_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_41_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_42_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_43_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_44_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_45_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_46_so_left_pt3(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_47_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_48_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_49_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_50_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_51_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_52_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_53_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_54_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_55_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_56_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_57_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_58_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_59_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_60_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_61_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_62_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_63_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_64_so_left_pt3(1: nocc, 1: nocc))
allocate(wm_interm_65_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_66_so_left_pt3(nocc+1: nactive, 1: nocc))
allocate(wm_interm_67_so_left_pt3(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_68_so_left_pt3(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_so_left_pt3 = zero 
wm_interm_1_so_left_pt3 = zero 
wm_interm_2_so_left_pt3 = zero 
wm_interm_3_so_left_pt3 = zero 
wm_interm_4_so_left_pt3 = zero 
wm_interm_5_so_left_pt3 = zero 
wm_interm_6_so_left_pt3 = zero 
wm_interm_7_so_left_pt3 = zero 
wm_interm_8_so_left_pt3 = zero 
wm_interm_9_so_left_pt3 = zero 
wm_interm_10_so_left_pt3 = zero 
wm_interm_11_so_left_pt3 = zero 
wm_interm_12_so_left_pt3 = zero 
wm_interm_13_so_left_pt3 = zero 
wm_interm_14_so_left_pt3 = zero 
wm_interm_15_so_left_pt3 = zero 
wm_interm_16_so_left_pt3 = zero 
wm_interm_17_so_left_pt3 = zero 
wm_interm_18_so_left_pt3 = zero 
wm_interm_19_so_left_pt3 = zero 
wm_interm_20_so_left_pt3 = zero 
wm_interm_21_so_left_pt3 = zero 
wm_interm_22_so_left_pt3 = zero 
wm_interm_23_so_left_pt3 = zero 
wm_interm_24_so_left_pt3 = zero 
wm_interm_25_so_left_pt3 = zero 
wm_interm_26_so_left_pt3 = zero 
wm_interm_27_so_left_pt3 = zero 
wm_interm_28_so_left_pt3 = zero 
wm_interm_29_so_left_pt3 = zero 
wm_interm_30_so_left_pt3 = zero 
wm_interm_31_so_left_pt3 = zero 
wm_interm_32_so_left_pt3 = zero 
wm_interm_33_so_left_pt3 = zero 
wm_interm_34_so_left_pt3 = zero 
wm_interm_35_so_left_pt3 = zero 
wm_interm_36_so_left_pt3 = zero 
wm_interm_37_so_left_pt3 = zero 
wm_interm_38_so_left_pt3 = zero 
wm_interm_39_so_left_pt3 = zero 
wm_interm_40_so_left_pt3 = zero 
wm_interm_41_so_left_pt3 = zero 
wm_interm_42_so_left_pt3 = zero 
wm_interm_43_so_left_pt3 = zero 
wm_interm_44_so_left_pt3 = zero 
wm_interm_45_so_left_pt3 = zero 
wm_interm_46_so_left_pt3 = zero 
wm_interm_47_so_left_pt3 = zero 
wm_interm_48_so_left_pt3 = zero 
wm_interm_49_so_left_pt3 = zero 
wm_interm_50_so_left_pt3 = zero 
wm_interm_51_so_left_pt3 = zero 
wm_interm_52_so_left_pt3 = zero 
wm_interm_53_so_left_pt3 = zero 
wm_interm_54_so_left_pt3 = zero 
wm_interm_55_so_left_pt3 = zero 
wm_interm_56_so_left_pt3 = zero 
wm_interm_57_so_left_pt3 = zero 
wm_interm_58_so_left_pt3 = zero 
wm_interm_59_so_left_pt3 = zero 
wm_interm_60_so_left_pt3 = zero 
wm_interm_61_so_left_pt3 = zero 
wm_interm_62_so_left_pt3 = zero 
wm_interm_63_so_left_pt3 = zero 
wm_interm_64_so_left_pt3 = zero 
wm_interm_65_so_left_pt3 = zero 
wm_interm_66_so_left_pt3 = zero 
wm_interm_67_so_left_pt3 = zero 
wm_interm_68_so_left_pt3 = zero 

    end subroutine wm_so_left_intermediates_ccsd_init_pt3
    
    subroutine wm_so_left_intermediates_ccsd_free_pt3
    deallocate(wm_interm_0_so_left_pt3)
deallocate(wm_interm_1_so_left_pt3)
deallocate(wm_interm_2_so_left_pt3)
deallocate(wm_interm_3_so_left_pt3)
deallocate(wm_interm_4_so_left_pt3)
deallocate(wm_interm_5_so_left_pt3)
deallocate(wm_interm_6_so_left_pt3)
deallocate(wm_interm_7_so_left_pt3)
deallocate(wm_interm_8_so_left_pt3)
deallocate(wm_interm_9_so_left_pt3)
deallocate(wm_interm_10_so_left_pt3)
deallocate(wm_interm_11_so_left_pt3)
deallocate(wm_interm_12_so_left_pt3)
deallocate(wm_interm_13_so_left_pt3)
deallocate(wm_interm_14_so_left_pt3)
deallocate(wm_interm_15_so_left_pt3)
deallocate(wm_interm_16_so_left_pt3)
deallocate(wm_interm_17_so_left_pt3)
deallocate(wm_interm_18_so_left_pt3)
deallocate(wm_interm_19_so_left_pt3)
deallocate(wm_interm_20_so_left_pt3)
deallocate(wm_interm_21_so_left_pt3)
deallocate(wm_interm_22_so_left_pt3)
deallocate(wm_interm_23_so_left_pt3)
deallocate(wm_interm_24_so_left_pt3)
deallocate(wm_interm_25_so_left_pt3)
deallocate(wm_interm_26_so_left_pt3)
deallocate(wm_interm_27_so_left_pt3)
deallocate(wm_interm_28_so_left_pt3)
deallocate(wm_interm_29_so_left_pt3)
deallocate(wm_interm_30_so_left_pt3)
deallocate(wm_interm_31_so_left_pt3)
deallocate(wm_interm_32_so_left_pt3)
deallocate(wm_interm_33_so_left_pt3)
deallocate(wm_interm_34_so_left_pt3)
deallocate(wm_interm_35_so_left_pt3)
deallocate(wm_interm_36_so_left_pt3)
deallocate(wm_interm_37_so_left_pt3)
deallocate(wm_interm_38_so_left_pt3)
deallocate(wm_interm_39_so_left_pt3)
deallocate(wm_interm_40_so_left_pt3)
deallocate(wm_interm_41_so_left_pt3)
deallocate(wm_interm_42_so_left_pt3)
deallocate(wm_interm_43_so_left_pt3)
deallocate(wm_interm_44_so_left_pt3)
deallocate(wm_interm_45_so_left_pt3)
deallocate(wm_interm_46_so_left_pt3)
deallocate(wm_interm_47_so_left_pt3)
deallocate(wm_interm_48_so_left_pt3)
deallocate(wm_interm_49_so_left_pt3)
deallocate(wm_interm_50_so_left_pt3)
deallocate(wm_interm_51_so_left_pt3)
deallocate(wm_interm_52_so_left_pt3)
deallocate(wm_interm_53_so_left_pt3)
deallocate(wm_interm_54_so_left_pt3)
deallocate(wm_interm_55_so_left_pt3)
deallocate(wm_interm_56_so_left_pt3)
deallocate(wm_interm_57_so_left_pt3)
deallocate(wm_interm_58_so_left_pt3)
deallocate(wm_interm_59_so_left_pt3)
deallocate(wm_interm_60_so_left_pt3)
deallocate(wm_interm_61_so_left_pt3)
deallocate(wm_interm_62_so_left_pt3)
deallocate(wm_interm_63_so_left_pt3)
deallocate(wm_interm_64_so_left_pt3)
deallocate(wm_interm_65_so_left_pt3)
deallocate(wm_interm_66_so_left_pt3)
deallocate(wm_interm_67_so_left_pt3)
deallocate(wm_interm_68_so_left_pt3)

    end subroutine wm_so_left_intermediates_ccsd_free_pt3
    
    subroutine wm_so_left_intermediates_ccsd_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
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
wm_interm_0_so_left_pt3(b, j, i, k) = wm_interm_0_so_left_pt3(b, j, i, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_1_so_left_pt3(i, j, k, l) = wm_interm_1_so_left_pt3(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_2_so_left_pt3(j, k) = wm_interm_2_so_left_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t1(a,k)
end do 
wm_interm_3_so_left_pt3(b, i, j, k) = wm_interm_3_so_left_pt3(b, i, j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,i,k)
end do 
end do 
end do 
wm_interm_4_so_left_pt3(j, k) = wm_interm_4_so_left_pt3(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,b,k,i)
end do 
end do 
end do 
wm_interm_5_so_left_pt3(j, k) = wm_interm_5_so_left_pt3(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s2(a,b,k,l)
end do 
end do 
wm_interm_6_so_left_pt3(i, j, k, l) = wm_interm_6_so_left_pt3(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_7_so_left_pt3(j, k) = wm_interm_7_so_left_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_8_so_left_pt3(j, k) = wm_interm_8_so_left_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_9_so_left_pt3(b, i, j, k) = wm_interm_9_so_left_pt3(b, i, j, k) + sum 
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
wm_interm_10_so_left_pt3(b, j) = wm_interm_10_so_left_pt3(b, j) + sum 
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
wm_interm_11_so_left_pt3(b, j) = wm_interm_11_so_left_pt3(b, j) + sum 
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
wm_interm_12_so_left_pt3(b, i, k, j) = wm_interm_12_so_left_pt3(b, i, k, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_13_so_left_pt3(j, k) = wm_interm_13_so_left_pt3(j, k) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,k)
end do 
wm_interm_14_so_left_pt3(b, j, i, k) = wm_interm_14_so_left_pt3(b, j, i, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_15_so_left_pt3(j, k) = wm_interm_15_so_left_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_16_so_left_pt3(j, k) = wm_interm_16_so_left_pt3(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t1(a,i)
end do 
end do 
wm_interm_17_so_left_pt3(b, j) = wm_interm_17_so_left_pt3(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,i,j)
end do 
end do 
end do 
wm_interm_18_so_left_pt3(b, c) = wm_interm_18_so_left_pt3(b, c) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_19_so_left_pt3(b, c) = wm_interm_19_so_left_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t1(a,i)
end do 
end do 
wm_interm_20_so_left_pt3(b, j) = wm_interm_20_so_left_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_21_so_left_pt3(b, j) = wm_interm_21_so_left_pt3(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,i)
end do 
end do 
wm_interm_22_so_left_pt3(b, j) = wm_interm_22_so_left_pt3(b, j) + sum 
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
sum = sum + r2(vrdav_Rr, a,j,b,i) * s1(a,i)
end do 
end do 
wm_interm_23_so_left_pt3(b, j) = wm_interm_23_so_left_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_24_so_left_pt3(b, c) = wm_interm_24_so_left_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_25_so_left_pt3(b, c) = wm_interm_25_so_left_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_26_so_left_pt3(b, c) = wm_interm_26_so_left_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r2(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_27_so_left_pt3(b, c) = wm_interm_27_so_left_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r2(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_28_so_left_pt3(b, c) = wm_interm_28_so_left_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_29_so_left_pt3(b, c) = wm_interm_29_so_left_pt3(b, c) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_30_so_left_pt3(i, j, k, l) = wm_interm_30_so_left_pt3(i, j, k, l) + sum 
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
sum = sum + r2(vrdav_Rr, a,i,b,j) * s1(a,k)
end do 
wm_interm_31_so_left_pt3(b, i, j, k) = wm_interm_31_so_left_pt3(b, i, j, k) + sum 
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
wm_interm_32_so_left_pt3(b, i, j, k) = wm_interm_32_so_left_pt3(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,l)
end do 
end do 
wm_interm_33_so_left_pt3(i, j, k, l) = wm_interm_33_so_left_pt3(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_34_so_left_pt3(j, k) = wm_interm_34_so_left_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,k)
end do 
wm_interm_35_so_left_pt3(b, j, i, k) = wm_interm_35_so_left_pt3(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_36_so_left_pt3(j, k) = wm_interm_36_so_left_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,k)
end do 
wm_interm_37_so_left_pt3(b, i, j, k) = wm_interm_37_so_left_pt3(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_38_so_left_pt3(j, k) = wm_interm_38_so_left_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_39_so_left_pt3(j, k) = wm_interm_39_so_left_pt3(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t1(a,i)
end do 
end do 
wm_interm_40_so_left_pt3(b, j) = wm_interm_40_so_left_pt3(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t1(a,i)
end do 
end do 
wm_interm_41_so_left_pt3(b, j) = wm_interm_41_so_left_pt3(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_42_so_left_pt3(b, c) = wm_interm_42_so_left_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_43_so_left_pt3(b, c) = wm_interm_43_so_left_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_44_so_left_pt3(b, c) = wm_interm_44_so_left_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,j,c,i)
end do 
end do 
end do 
wm_interm_45_so_left_pt3(b, c) = wm_interm_45_so_left_pt3(b, c) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,b,k,l)
end do 
end do 
wm_interm_46_so_left_pt3(i, j, k, l) = wm_interm_46_so_left_pt3(i, j, k, l) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,k)
end do 
wm_interm_47_so_left_pt3(b, i, j, k) = wm_interm_47_so_left_pt3(b, i, j, k) + sum 
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
wm_interm_48_so_left_pt3(a, b) = wm_interm_48_so_left_pt3(a, b) + sum 
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
wm_interm_49_so_left_pt3(i, j) = wm_interm_49_so_left_pt3(i, j) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_50_so_left_pt3(b, j) = wm_interm_50_so_left_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_51_so_left_pt3(b, i, j, k) = wm_interm_51_so_left_pt3(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_52_so_left_pt3(b, i, j, k) = wm_interm_52_so_left_pt3(b, i, j, k) + sum 
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
wm_interm_53_so_left_pt3(a, b) = wm_interm_53_so_left_pt3(a, b) + sum 
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
wm_interm_54_so_left_pt3(i, j) = wm_interm_54_so_left_pt3(i, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_55_so_left_pt3(b, j) = wm_interm_55_so_left_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_56_so_left_pt3(b, j) = wm_interm_56_so_left_pt3(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, b,i,a,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_57_so_left_pt3(b, j) = wm_interm_57_so_left_pt3(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,k)
end do 
wm_interm_58_so_left_pt3(b, i, j, k) = wm_interm_58_so_left_pt3(b, i, j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,k)
end do 
wm_interm_59_so_left_pt3(b, j, i, k) = wm_interm_59_so_left_pt3(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_60_so_left_pt3(b, j) = wm_interm_60_so_left_pt3(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_61_so_left_pt3(b, j) = wm_interm_61_so_left_pt3(b, j) + sum 
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
wm_interm_62_so_left_pt3(b, j) = wm_interm_62_so_left_pt3(b, j) + sum 
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
sum = sum + r1(vrdav_Rr, a,i) * s2(a,b,i,j)
end do 
end do 
wm_interm_63_so_left_pt3(b, j) = wm_interm_63_so_left_pt3(b, j) + sum 
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
wm_interm_64_so_left_pt3(i, j) = wm_interm_64_so_left_pt3(i, j) + sum 
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
wm_interm_65_so_left_pt3(b, i, j, k) = wm_interm_65_so_left_pt3(b, i, j, k) + sum 
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
wm_interm_66_so_left_pt3(b, j) = wm_interm_66_so_left_pt3(b, j) + sum 
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
wm_interm_67_so_left_pt3(a, b) = wm_interm_67_so_left_pt3(a, b) + sum 
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
wm_interm_68_so_left_pt3(b, i, j, k) = wm_interm_68_so_left_pt3(b, i, j, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



end subroutine wm_so_left_intermediates_ccsd_pt3

   function calc_D_oo_wm_so_left_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_left_pt0
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
term(0) = term(0) + r2p(vrdav_Rl, b,p,a,i) * r2(vrdav_Rr, a,q,b,i)
term(1) = term(1) + r2p(vrdav_Rl, a,p,b,i) * r2(vrdav_Rr, a,q,b,i)
term(2) = term(2) + r2p(vrdav_Rl, a,i,b,p) * r2(vrdav_Rr, a,q,b,i)
term(3) = term(3) + r2m(vrdav_Rl, a,i,b,p) * r2(vrdav_Rr, a,q,b,i)
term(4) = term(4) + r2m(vrdav_Rl, a,p,b,i) * r2(vrdav_Rr, a,q,b,i)
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


    calc_D_oo_wm_so_left_pt0 = zero
    do s = 0, 5
    calc_D_oo_wm_so_left_pt0 = calc_D_oo_wm_so_left_pt0 + term(s)
    end do

    end function calc_D_oo_wm_so_left_pt0
    
    function calc_D_ov_wm_so_left_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_left_pt0
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
    
    calc_D_ov_wm_so_left_pt0 = zero

    end function calc_D_ov_wm_so_left_pt0
    
    function calc_D_vo_wm_so_left_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_left_pt0
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
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, a,i) * r2(vrdav_Rr, a,q,p,i)
term(1) = term(1) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, a,i)
term(2) = term(2) + r2p(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(3) = term(3) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(4) = term(4) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * (4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(5) = term(5) + r2p(vrdav_Rl, p,i,a,q) * r1(vrdav_Rr, a,i)
end do 
end do 

term(5) = term(5) * (-1.0d+0) 


    calc_D_vo_wm_so_left_pt0 = zero
    do s = 0, 5
    calc_D_vo_wm_so_left_pt0 = calc_D_vo_wm_so_left_pt0 + term(s)
    end do

    end function calc_D_vo_wm_so_left_pt0
    
    function calc_D_vv_wm_so_left_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_left_pt0
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
term(0) = term(0) + r2p(vrdav_Rl, p,j,a,i) * r2(vrdav_Rr, a,j,q,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(1) = term(1) + r2p(vrdav_Rl, p,i,a,j) * r2(vrdav_Rr, a,j,q,i)
term(2) = term(2) + r2p(vrdav_Rl, a,i,p,j) * r2(vrdav_Rr, a,j,q,i)
term(3) = term(3) + r2m(vrdav_Rl, a,i,p,j) * r2(vrdav_Rr, a,j,q,i)
end do 
end do 
end do 

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + r2m(vrdav_Rl, a,j,p,i) * r2(vrdav_Rr, a,j,q,i)
end do 
end do 
end do 

term(4) = term(4) * (4.0d+0) 

do i = 1, nocc 
term(5) = term(5) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
end do 

term(5) = term(5) * (2.0d+0) 


    calc_D_vv_wm_so_left_pt0 = zero
    do s = 0, 5
    calc_D_vv_wm_so_left_pt0 = calc_D_vv_wm_so_left_pt0 + term(s)
    end do

    end function calc_D_vv_wm_so_left_pt0	

    function calc_D_oo_wm_so_left_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_left_pt1
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
    
    calc_D_oo_wm_so_left_pt1 = zero

    end function calc_D_oo_wm_so_left_pt1
    
    function calc_D_ov_wm_so_left_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_left_pt1
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
term(0) = term(0) + r1(vrdav_Rl, a,p) * wm_interm_1_so_left_pt1(a,q)
term(1) = term(1) + r1(vrdav_Rl, a,p) * wm_interm_2_so_left_pt1(a,q)
term(2) = term(2) + r1(vrdav_Rr, a,p) * wm_interm_8_so_left_pt1(a,q)
term(3) = term(3) + r1(vrdav_Rr, a,p) * wm_interm_9_so_left_pt1(a,q)
term(4) = term(4) + r1(vrdav_Rr, a,p) * wm_interm_10_so_left_pt1(a,q)
term(5) = term(5) + r1(vrdav_Rr, a,p) * wm_interm_16_so_left_pt1(a,q)
term(6) = term(6) + r1(vrdav_Rr, a,p) * wm_interm_17_so_left_pt1(a,q)
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (4.0d+0) 


    calc_D_ov_wm_so_left_pt1 = zero
    do s = 0, 6
    calc_D_ov_wm_so_left_pt1 = calc_D_ov_wm_so_left_pt1 + term(s)
    end do

    end function calc_D_ov_wm_so_left_pt1
    
    function calc_D_vo_wm_so_left_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_left_pt1
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
    real(F64), dimension(0:12) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, p,i) * wm_interm_3_so_left_pt1(i,q)
term(1) = term(1) + r1(vrdav_Rl, p,i) * wm_interm_4_so_left_pt1(i,q)
term(2) = term(2) + r1(vrdav_Rr, p,i) * wm_interm_5_so_left_pt1(i,q)
term(3) = term(3) + r1(vrdav_Rr, p,i) * wm_interm_6_so_left_pt1(i,q)
term(4) = term(4) + r1(vrdav_Rr, p,i) * wm_interm_7_so_left_pt1(i,q)
term(5) = term(5) + r1(vrdav_Rr, p,i) * wm_interm_14_so_left_pt1(i,q)
term(6) = term(6) + r1(vrdav_Rr, p,i) * wm_interm_15_so_left_pt1(i,q)
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(7) = term(7) + s2(a,p,q,i) * wm_interm_0_so_left_pt1(a,i)
term(8) = term(8) + t2(a,p,q,i) * wm_interm_11_so_left_pt1(a,i)
term(9) = term(9) + t2(a,p,q,i) * wm_interm_12_so_left_pt1(a,i)
term(10) = term(10) + t2(a,p,q,i) * wm_interm_13_so_left_pt1(a,i)
term(11) = term(11) + t2(a,p,q,i) * wm_interm_18_so_left_pt1(a,i)
term(12) = term(12) + t2(a,p,q,i) * wm_interm_19_so_left_pt1(a,i)
end do 
end do 

term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-1.0d+0) 
term(9) = term(9) * (2.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (-4.0d+0) 
term(12) = term(12) * (4.0d+0) 


    calc_D_vo_wm_so_left_pt1 = zero
    do s = 0, 12
    calc_D_vo_wm_so_left_pt1 = calc_D_vo_wm_so_left_pt1 + term(s)
    end do

    end function calc_D_vo_wm_so_left_pt1
    
    function calc_D_vv_wm_so_left_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_left_pt1
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
    
    calc_D_vv_wm_so_left_pt1 = zero

    end function calc_D_vv_wm_so_left_pt1

    function calc_D_oo_wm_so_left_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_left_pt2
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
term(0) = term(0) + wm_interm_17_so_left_pt2(a,p,i,j) * wm_interm_7_so_left_pt2(a,q,j,i)
term(1) = term(1) + wm_interm_17_so_left_pt2(a,p,i,j) * wm_interm_7_so_left_pt2(a,q,i,j)
term(2) = term(2) + wm_interm_17_so_left_pt2(a,i,j,p) * wm_interm_7_so_left_pt2(a,i,q,j)
term(3) = term(3) + wm_interm_17_so_left_pt2(a,i,p,j) * wm_interm_7_so_left_pt2(a,i,q,j)
term(4) = term(4) + wm_interm_17_so_left_pt2(a,i,p,j) * wm_interm_7_so_left_pt2(a,i,j,q)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (4.0d+0) 
term(4) = term(4) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(5) = term(5) + wm_interm_18_so_left_pt2(a,p) * wm_interm_5_so_left_pt2(a,q)
term(6) = term(6) + wm_interm_19_so_left_pt2(a,p) * wm_interm_5_so_left_pt2(a,q)
term(7) = term(7) + r1(vrdav_Rl, a,q) * wm_interm_44_so_left_pt2(a,p)
term(8) = term(8) + r1(vrdav_Rl, a,q) * wm_interm_45_so_left_pt2(a,p)
term(9) = term(9) + s1(a,q) * wm_interm_6_so_left_pt2(a,p)
term(10) = term(10) + r1(vrdav_Rr, a,p) * wm_interm_46_so_left_pt2(a,q)
term(11) = term(11) + r1(vrdav_Rr, a,p) * wm_interm_47_so_left_pt2(a,q)
term(12) = term(12) + r1(vrdav_Rr, a,p) * wm_interm_48_so_left_pt2(a,q)
term(13) = term(13) + t1(a,q) * wm_interm_14_so_left_pt2(a,p)
term(14) = term(14) + t1(a,q) * wm_interm_21_so_left_pt2(a,p)
term(15) = term(15) + t1(a,q) * wm_interm_20_so_left_pt2(a,p)
term(16) = term(16) + r1(vrdav_Rr, a,p) * wm_interm_55_so_left_pt2(a,q)
term(17) = term(17) + r1(vrdav_Rr, a,p) * wm_interm_56_so_left_pt2(a,q)
term(18) = term(18) + t1(a,q) * wm_interm_29_so_left_pt2(a,p)
term(19) = term(19) + t1(a,q) * wm_interm_32_so_left_pt2(a,p)
end do 

term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (-1.0d+0) 
term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-1.0d+0) 
term(16) = term(16) * (4.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(20) = term(20) + wm_interm_38_so_left_pt2(i,j) * wm_interm_8_so_left_pt2(p,j,i,q)
end do 
end do 

term(20) = term(20) * (-2.0d+0) 

do i = 1, nocc 
term(21) = term(21) + wm_interm_1_so_left_pt2(q,i) * wm_interm_38_so_left_pt2(i,p)
term(22) = term(22) + wm_interm_2_so_left_pt2(q,i) * wm_interm_38_so_left_pt2(i,p)
term(23) = term(23) + wm_interm_1_so_left_pt2(i,q) * wm_interm_38_so_left_pt2(p,i)
term(24) = term(24) + wm_interm_2_so_left_pt2(i,q) * wm_interm_38_so_left_pt2(p,i)
end do 

term(21) = term(21) * (-2.0d+0) 
term(22) = term(22) * (4.0d+0) 
term(23) = term(23) * (-2.0d+0) 
term(24) = term(24) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(25) = term(25) + wm_interm_17_so_left_pt2(a,p,i,q) * wm_interm_5_so_left_pt2(a,i)
term(26) = term(26) + wm_interm_18_so_left_pt2(a,i) * wm_interm_7_so_left_pt2(a,p,q,i)
term(27) = term(27) + wm_interm_19_so_left_pt2(a,i) * wm_interm_7_so_left_pt2(a,p,q,i)
term(28) = term(28) + wm_interm_19_so_left_pt2(a,i) * wm_interm_7_so_left_pt2(a,p,i,q)
term(29) = term(29) + wm_interm_18_so_left_pt2(a,i) * wm_interm_7_so_left_pt2(a,p,i,q)
end do 
end do 

term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (-8.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-2.0d+0) 


    calc_D_oo_wm_so_left_pt2 = zero
    do s = 0, 29
    calc_D_oo_wm_so_left_pt2 = calc_D_oo_wm_so_left_pt2 + term(s)
    end do

    end function calc_D_oo_wm_so_left_pt2
    
    function calc_D_ov_wm_so_left_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_left_pt2
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
    real(F64), dimension(0:73) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_11_so_left_pt2(i,p,j,k) * wm_interm_7_so_left_pt2(q,i,k,j)
term(1) = term(1) + wm_interm_15_so_left_pt2(q,i,j,k) * wm_interm_8_so_left_pt2(k,p,i,j)
term(2) = term(2) + wm_interm_15_so_left_pt2(q,i,j,k) * wm_interm_8_so_left_pt2(p,k,i,j)
term(3) = term(3) + wm_interm_16_so_left_pt2(q,i,j,k) * wm_interm_8_so_left_pt2(p,k,i,j)
term(4) = term(4) + wm_interm_30_so_left_pt2(q,i,j,k) * wm_interm_8_so_left_pt2(k,p,i,j)
term(5) = term(5) + wm_interm_30_so_left_pt2(q,i,j,k) * wm_interm_8_so_left_pt2(p,k,i,j)
term(6) = term(6) + wm_interm_31_so_left_pt2(q,i,j,k) * wm_interm_8_so_left_pt2(p,k,i,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(7) = term(7) + wm_interm_3_so_left_pt2(a,q) * wm_interm_6_so_left_pt2(a,p)
term(8) = term(8) + wm_interm_4_so_left_pt2(a,q) * wm_interm_6_so_left_pt2(a,p)
term(9) = term(9) + wm_interm_18_so_left_pt2(a,p) * wm_interm_22_so_left_pt2(q,a)
term(10) = term(10) + wm_interm_19_so_left_pt2(a,p) * wm_interm_22_so_left_pt2(q,a)
term(11) = term(11) + wm_interm_18_so_left_pt2(a,p) * wm_interm_25_so_left_pt2(q,a)
term(12) = term(12) + wm_interm_19_so_left_pt2(a,p) * wm_interm_25_so_left_pt2(q,a)
term(13) = term(13) + wm_interm_18_so_left_pt2(a,p) * wm_interm_27_so_left_pt2(q,a)
term(14) = term(14) + wm_interm_19_so_left_pt2(a,p) * wm_interm_27_so_left_pt2(q,a)
term(15) = term(15) + wm_interm_18_so_left_pt2(a,p) * wm_interm_33_so_left_pt2(q,a)
term(16) = term(16) + wm_interm_19_so_left_pt2(a,p) * wm_interm_33_so_left_pt2(q,a)
term(17) = term(17) + wm_interm_18_so_left_pt2(a,p) * wm_interm_36_so_left_pt2(q,a)
term(18) = term(18) + wm_interm_19_so_left_pt2(a,p) * wm_interm_36_so_left_pt2(q,a)
term(19) = term(19) + r1(vrdav_Rl, a,p) * wm_interm_40_so_left_pt2(a,q)
term(20) = term(20) + r1(vrdav_Rr, a,p) * wm_interm_43_so_left_pt2(a,q)
end do 

term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (4.0d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (2.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * (-1.0d+0) 
term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (8.0d+0) 
term(17) = term(17) * (4.0d+0) 
term(18) = term(18) * (-8.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(21) = term(21) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_17_so_left_pt2(b,j,k,p)
end do 
end do 
end do 
end do 
end do 

term(21) = term(21) * (4.0d+0) 

do i = 1, nocc 
term(22) = term(22) + wm_interm_12_so_left_pt2(p,i) * wm_interm_5_so_left_pt2(q,i)
term(23) = term(23) + wm_interm_13_so_left_pt2(p,i) * wm_interm_5_so_left_pt2(q,i)
term(24) = term(24) + wm_interm_14_so_left_pt2(q,i) * wm_interm_1_so_left_pt2(p,i)
term(25) = term(25) + wm_interm_14_so_left_pt2(q,i) * wm_interm_2_so_left_pt2(p,i)
term(26) = term(26) + wm_interm_1_so_left_pt2(p,i) * wm_interm_21_so_left_pt2(q,i)
term(27) = term(27) + wm_interm_21_so_left_pt2(q,i) * wm_interm_2_so_left_pt2(p,i)
term(28) = term(28) + wm_interm_1_so_left_pt2(p,i) * wm_interm_20_so_left_pt2(q,i)
term(29) = term(29) + wm_interm_20_so_left_pt2(q,i) * wm_interm_2_so_left_pt2(p,i)
term(30) = term(30) + wm_interm_1_so_left_pt2(p,i) * wm_interm_29_so_left_pt2(q,i)
term(31) = term(31) + wm_interm_29_so_left_pt2(q,i) * wm_interm_2_so_left_pt2(p,i)
term(32) = term(32) + wm_interm_1_so_left_pt2(p,i) * wm_interm_32_so_left_pt2(q,i)
term(33) = term(33) + wm_interm_2_so_left_pt2(p,i) * wm_interm_32_so_left_pt2(q,i)
term(34) = term(34) + s1(q,i) * wm_interm_49_so_left_pt2(p,i)
term(35) = term(35) + s1(q,i) * wm_interm_50_so_left_pt2(p,i)
term(36) = term(36) + s1(q,i) * wm_interm_51_so_left_pt2(p,i)
term(37) = term(37) + t1(q,i) * wm_interm_49_so_left_pt2(i,p)
term(38) = term(38) + t1(q,i) * wm_interm_50_so_left_pt2(i,p)
term(39) = term(39) + t1(q,i) * wm_interm_51_so_left_pt2(i,p)
term(40) = term(40) + s1(q,i) * wm_interm_57_so_left_pt2(p,i)
term(41) = term(41) + s1(q,i) * wm_interm_58_so_left_pt2(p,i)
term(42) = term(42) + t1(q,i) * wm_interm_57_so_left_pt2(i,p)
term(43) = term(43) + t1(q,i) * wm_interm_58_so_left_pt2(i,p)
end do 

term(22) = term(22) * (-2.0d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * (-1.0d+0) 
term(29) = term(29) * (2.0d+0) 
term(30) = term(30) * (-4.0d+0) 
term(31) = term(31) * (8.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (2.0d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (-1.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (-1.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(44) = term(44) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_0_so_left_pt2(a,i,k,p)
term(45) = term(45) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_0_so_left_pt2(a,i,p,k)
term(46) = term(46) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_0_so_left_pt2(a,i,j,p)
term(47) = term(47) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_17_so_left_pt2(b,i,k,p)
term(48) = term(48) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_17_so_left_pt2(b,i,p,k)
term(49) = term(49) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_17_so_left_pt2(b,j,k,p)
term(50) = term(50) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_17_so_left_pt2(b,j,p,k)
term(51) = term(51) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_17_so_left_pt2(b,j,p,k)
end do 
end do 
end do 
end do 
end do 

term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (4.0d+0) 
term(46) = term(46) * (4.0d+0) 
term(47) = term(47) * (4.0d+0) 
term(48) = term(48) * (-8.0d+0) 
term(49) = term(49) * (-2.0d+0) 
term(50) = term(50) * (4.0d+0) 
term(51) = term(51) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(52) = term(52) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_0_so_left_pt2(a,k,p,j)
term(53) = term(53) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_0_so_left_pt2(a,k,p,j)
term(54) = term(54) + r2p(vrdav_Rl, a,i,q,j) * t2(a,b,k,i) * wm_interm_17_so_left_pt2(b,j,k,p)
term(55) = term(55) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_17_so_left_pt2(b,i,k,p)
term(56) = term(56) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,i) * wm_interm_17_so_left_pt2(b,j,k,p)
end do 
end do 
end do 
end do 
end do 

term(52) = term(52) * (-2.0d+0) 
term(53) = term(53) * (4.0d+0) 
term(54) = term(54) * (-1.0d+0) 
term(55) = term(55) * (-2.0d+0) 
term(56) = term(56) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(57) = term(57) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_0_so_left_pt2(a,k,j,p)
end do 
end do 
end do 
end do 
end do 

term(57) = term(57) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(58) = term(58) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_0_so_left_pt2(a,i,p,j)
term(59) = term(59) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,i) * wm_interm_17_so_left_pt2(b,j,p,k)
term(60) = term(60) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_17_so_left_pt2(b,i,p,k)
end do 
end do 
end do 
end do 
end do 

term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * (-1.0d+0) 
term(60) = term(60) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(61) = term(61) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,j,k) * wm_interm_17_so_left_pt2(b,i,k,p)
term(62) = term(62) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,j,k) * wm_interm_17_so_left_pt2(b,i,p,k)
term(63) = term(63) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,i,k) * wm_interm_17_so_left_pt2(b,j,k,p)
term(64) = term(64) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,i,k) * wm_interm_17_so_left_pt2(b,j,p,k)
term(65) = term(65) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,j) * wm_interm_17_so_left_pt2(b,i,p,k)
end do 
end do 
end do 
end do 
end do 

term(61) = term(61) * (2.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(66) = term(66) + r2p(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_17_so_left_pt2(b,i,k,p)
term(67) = term(67) + r2p(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_17_so_left_pt2(b,i,p,k)
term(68) = term(68) + r2p(vrdav_Rl, a,i,q,j) * t2(a,b,k,j) * wm_interm_17_so_left_pt2(b,i,p,k)
term(69) = term(69) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_17_so_left_pt2(b,i,k,p)
term(70) = term(70) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_17_so_left_pt2(b,i,p,k)
term(71) = term(71) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,j) * wm_interm_17_so_left_pt2(b,i,p,k)
end do 
end do 
end do 
end do 
end do 

term(66) = term(66) * (-1.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-1.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (4.0d+0) 
term(71) = term(71) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(72) = term(72) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,j) * wm_interm_17_so_left_pt2(b,i,k,p)
end do 
end do 
end do 
end do 
end do 

term(72) = term(72) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(73) = term(73) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,i) * wm_interm_17_so_left_pt2(b,j,k,p)
end do 
end do 
end do 
end do 
end do 

term(73) = term(73) * (2.0d+0) 


    calc_D_ov_wm_so_left_pt2 = zero
    do s = 0, 73
    calc_D_ov_wm_so_left_pt2 = calc_D_ov_wm_so_left_pt2 + term(s)
    end do

    end function calc_D_ov_wm_so_left_pt2
    
    function calc_D_vo_wm_so_left_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_left_pt2
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
    real(F64), dimension(0:157) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_0_so_left_pt2(p,i,j,k) * wm_interm_8_so_left_pt2(k,j,i,q)
term(1) = term(1) + wm_interm_17_so_left_pt2(p,i,j,k) * wm_interm_24_so_left_pt2(q,i,j,k)
term(2) = term(2) + wm_interm_17_so_left_pt2(p,i,j,k) * wm_interm_24_so_left_pt2(i,q,k,j)
term(3) = term(3) + wm_interm_17_so_left_pt2(p,i,j,k) * wm_interm_24_so_left_pt2(q,i,k,j)
term(4) = term(4) + wm_interm_17_so_left_pt2(p,i,j,k) * wm_interm_35_so_left_pt2(q,i,j,k)
term(5) = term(5) + wm_interm_17_so_left_pt2(p,i,j,k) * wm_interm_35_so_left_pt2(i,q,k,j)
term(6) = term(6) + wm_interm_17_so_left_pt2(p,i,j,k) * wm_interm_35_so_left_pt2(q,i,k,j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (-1.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 
term(6) = term(6) * (4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(7) = term(7) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_7_so_left_pt2(b,i,q,k)
term(8) = term(8) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_16_so_left_pt2(b,q,k,j)
term(9) = term(9) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_16_so_left_pt2(b,k,q,j)
term(10) = term(10) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_15_so_left_pt2(b,q,k,j)
term(11) = term(11) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_31_so_left_pt2(b,q,k,j)
term(12) = term(12) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_31_so_left_pt2(b,k,q,j)
term(13) = term(13) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_30_so_left_pt2(b,q,k,j)
end do 
end do 
end do 
end do 
end do 

term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (-1.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
term(14) = term(14) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_44_so_left_pt2(a,i)
term(15) = term(15) + r2p(vrdav_Rl, p,i,a,q) * wm_interm_45_so_left_pt2(a,i)
end do 
end do 

term(14) = term(14) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,i,j) * wm_interm_18_so_left_pt2(b,j)
term(17) = term(17) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,i,j) * wm_interm_19_so_left_pt2(b,j)
end do 
end do 
end do 
end do 

term(16) = term(16) * (-8.0d+0) 
term(17) = term(17) * (16.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(18) = term(18) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_52_so_left_pt2(a,i,j,q)
term(19) = term(19) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_52_so_left_pt2(a,j,i,q)
end do 
end do 
end do 

term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(20) = term(20) + r2p(vrdav_Rl, a,i,p,j) * wm_interm_52_so_left_pt2(a,j,i,q)
term(21) = term(21) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_52_so_left_pt2(a,j,i,q)
end do 
end do 
end do 

term(20) = term(20) * (-1.0d+0) 
term(21) = term(21) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(22) = term(22) + r2(vrdav_Rr, a,j,p,i) * wm_interm_53_so_left_pt2(a,j,i,q)
term(23) = term(23) + r2(vrdav_Rr, a,j,p,i) * wm_interm_53_so_left_pt2(a,i,j,q)
term(24) = term(24) + r2(vrdav_Rr, a,j,p,i) * wm_interm_54_so_left_pt2(a,i,j,q)
term(25) = term(25) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_52_so_left_pt2(a,i,j,q)
term(26) = term(26) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_52_so_left_pt2(a,j,i,q)
term(27) = term(27) + r2(vrdav_Rr, a,j,p,i) * wm_interm_59_so_left_pt2(a,j,i,q)
term(28) = term(28) + r2(vrdav_Rr, a,j,p,i) * wm_interm_59_so_left_pt2(a,i,j,q)
term(29) = term(29) + r2(vrdav_Rr, a,j,p,i) * wm_interm_60_so_left_pt2(a,i,j,q)
end do 
end do 
end do 

term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (-2.0d+0) 
term(26) = term(26) * (4.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(30) = term(30) + r1(vrdav_Rl, b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_3_so_left_pt2(a,b)
term(31) = term(31) + r1(vrdav_Rl, b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_4_so_left_pt2(a,b)
term(32) = term(32) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_22_so_left_pt2(a,b)
term(33) = term(33) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_25_so_left_pt2(a,b)
term(34) = term(34) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_27_so_left_pt2(a,b)
term(35) = term(35) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_33_so_left_pt2(a,b)
term(36) = term(36) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_36_so_left_pt2(a,b)
end do 
end do 
end do 

term(30) = term(30) * (-2.0d+0) 
term(31) = term(31) * (4.0d+0) 
term(32) = term(32) * (-1.0d+0) 
term(33) = term(33) * (2.0d+0) 
term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(37) = term(37) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_9_so_left_pt2(b,a)
term(38) = term(38) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_10_so_left_pt2(b,a)
term(39) = term(39) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, b,i) * wm_interm_3_so_left_pt2(b,a)
term(40) = term(40) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, b,i) * wm_interm_4_so_left_pt2(b,a)
term(41) = term(41) + r2p(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_3_so_left_pt2(b,a)
term(42) = term(42) + r2p(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_4_so_left_pt2(b,a)
term(43) = term(43) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_3_so_left_pt2(b,a)
term(44) = term(44) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_4_so_left_pt2(b,a)
end do 
end do 
end do 

term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (2.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (-1.0d+0) 
term(42) = term(42) * (2.0d+0) 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * (8.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(45) = term(45) + wm_interm_0_so_left_pt2(p,i,q,j) * wm_interm_1_so_left_pt2(j,i)
term(46) = term(46) + wm_interm_0_so_left_pt2(p,i,q,j) * wm_interm_2_so_left_pt2(j,i)
term(47) = term(47) + wm_interm_12_so_left_pt2(i,j) * wm_interm_7_so_left_pt2(p,i,q,j)
term(48) = term(48) + wm_interm_13_so_left_pt2(i,j) * wm_interm_7_so_left_pt2(p,i,q,j)
term(49) = term(49) + wm_interm_15_so_left_pt2(p,q,i,j) * wm_interm_1_so_left_pt2(j,i)
term(50) = term(50) + wm_interm_15_so_left_pt2(p,q,i,j) * wm_interm_2_so_left_pt2(j,i)
term(51) = term(51) + wm_interm_16_so_left_pt2(p,q,i,j) * wm_interm_1_so_left_pt2(j,i)
term(52) = term(52) + wm_interm_16_so_left_pt2(p,q,i,j) * wm_interm_2_so_left_pt2(j,i)
term(53) = term(53) + wm_interm_15_so_left_pt2(p,i,q,j) * wm_interm_1_so_left_pt2(j,i)
term(54) = term(54) + wm_interm_15_so_left_pt2(p,i,q,j) * wm_interm_2_so_left_pt2(j,i)
term(55) = term(55) + wm_interm_17_so_left_pt2(p,i,q,j) * wm_interm_23_so_left_pt2(i,j)
term(56) = term(56) + wm_interm_17_so_left_pt2(p,i,q,j) * wm_interm_26_so_left_pt2(i,j)
term(57) = term(57) + wm_interm_17_so_left_pt2(p,i,q,j) * wm_interm_28_so_left_pt2(i,j)
term(58) = term(58) + wm_interm_17_so_left_pt2(p,i,q,j) * wm_interm_34_so_left_pt2(i,j)
term(59) = term(59) + wm_interm_17_so_left_pt2(p,i,q,j) * wm_interm_37_so_left_pt2(i,j)
end do 
end do 

term(45) = term(45) * (-2.0d+0) 
term(46) = term(46) * (4.0d+0) 
term(47) = term(47) * (-2.0d+0) 
term(48) = term(48) * (4.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * (-1.0d+0) 
term(52) = term(52) * (2.0d+0) 
term(53) = term(53) * (-1.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-1.0d+0) 
term(56) = term(56) * (2.0d+0) 
term(57) = term(57) * (-1.0d+0) 
term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * (4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(60) = term(60) + r2p(vrdav_Rl, p,i,a,q) * r1(vrdav_Rr, b,i) * wm_interm_3_so_left_pt2(b,a)
term(61) = term(61) + r2p(vrdav_Rl, p,i,a,q) * r1(vrdav_Rr, b,i) * wm_interm_4_so_left_pt2(b,a)
term(62) = term(62) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_3_so_left_pt2(b,a)
term(63) = term(63) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_4_so_left_pt2(b,a)
end do 
end do 
end do 

term(60) = term(60) * (-1.0d+0) 
term(61) = term(61) * (2.0d+0) 
term(62) = term(62) * (4.0d+0) 
term(63) = term(63) * (-8.0d+0) 

do a = nocc + 1, nactive 
term(64) = term(64) + wm_interm_5_so_left_pt2(a,q) * wm_interm_9_so_left_pt2(p,a)
term(65) = term(65) + wm_interm_10_so_left_pt2(p,a) * wm_interm_5_so_left_pt2(a,q)
term(66) = term(66) + wm_interm_20_so_left_pt2(a,q) * wm_interm_3_so_left_pt2(p,a)
term(67) = term(67) + wm_interm_20_so_left_pt2(a,q) * wm_interm_4_so_left_pt2(p,a)
term(68) = term(68) + wm_interm_14_so_left_pt2(a,q) * wm_interm_3_so_left_pt2(p,a)
term(69) = term(69) + wm_interm_14_so_left_pt2(a,q) * wm_interm_4_so_left_pt2(p,a)
term(70) = term(70) + wm_interm_21_so_left_pt2(a,q) * wm_interm_3_so_left_pt2(p,a)
term(71) = term(71) + wm_interm_21_so_left_pt2(a,q) * wm_interm_4_so_left_pt2(p,a)
term(72) = term(72) + wm_interm_29_so_left_pt2(a,q) * wm_interm_3_so_left_pt2(p,a)
term(73) = term(73) + wm_interm_29_so_left_pt2(a,q) * wm_interm_4_so_left_pt2(p,a)
term(74) = term(74) + wm_interm_32_so_left_pt2(a,q) * wm_interm_3_so_left_pt2(p,a)
term(75) = term(75) + wm_interm_32_so_left_pt2(a,q) * wm_interm_4_so_left_pt2(p,a)
end do 

term(64) = term(64) * (-2.0d+0) 
term(65) = term(65) * (4.0d+0) 
term(66) = term(66) * (-1.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-1.0d+0) 
term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (-4.0d+0) 
term(72) = term(72) * (-4.0d+0) 
term(73) = term(73) * (8.0d+0) 
term(74) = term(74) * (4.0d+0) 
term(75) = term(75) * (-8.0d+0) 

do i = 1, nocc 
term(76) = term(76) + wm_interm_1_so_left_pt2(i,q) * wm_interm_6_so_left_pt2(p,i)
term(77) = term(77) + wm_interm_2_so_left_pt2(i,q) * wm_interm_6_so_left_pt2(p,i)
term(78) = term(78) + wm_interm_19_so_left_pt2(p,i) * wm_interm_23_so_left_pt2(q,i)
term(79) = term(79) + wm_interm_18_so_left_pt2(p,i) * wm_interm_23_so_left_pt2(q,i)
term(80) = term(80) + wm_interm_19_so_left_pt2(p,i) * wm_interm_26_so_left_pt2(q,i)
term(81) = term(81) + wm_interm_18_so_left_pt2(p,i) * wm_interm_26_so_left_pt2(q,i)
term(82) = term(82) + wm_interm_19_so_left_pt2(p,i) * wm_interm_28_so_left_pt2(q,i)
term(83) = term(83) + wm_interm_18_so_left_pt2(p,i) * wm_interm_28_so_left_pt2(q,i)
term(84) = term(84) + wm_interm_19_so_left_pt2(p,i) * wm_interm_34_so_left_pt2(q,i)
term(85) = term(85) + wm_interm_18_so_left_pt2(p,i) * wm_interm_34_so_left_pt2(q,i)
term(86) = term(86) + wm_interm_19_so_left_pt2(p,i) * wm_interm_37_so_left_pt2(q,i)
term(87) = term(87) + wm_interm_18_so_left_pt2(p,i) * wm_interm_37_so_left_pt2(q,i)
term(88) = term(88) + r1(vrdav_Rl, p,i) * wm_interm_41_so_left_pt2(i,q)
term(89) = term(89) + r1(vrdav_Rr, p,i) * wm_interm_42_so_left_pt2(i,q)
end do 

term(76) = term(76) * (-2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(78) = term(78) * (2.0d+0) 
term(79) = term(79) * (-1.0d+0) 
term(80) = term(80) * (-4.0d+0) 
term(81) = term(81) * (2.0d+0) 
term(82) = term(82) * (2.0d+0) 
term(83) = term(83) * (-1.0d+0) 
term(84) = term(84) * (8.0d+0) 
term(85) = term(85) * (-4.0d+0) 
term(86) = term(86) * (-8.0d+0) 
term(87) = term(87) * (4.0d+0) 
term(88) = term(88) * (2.0d+0) 
term(89) = term(89) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(90) = term(90) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_7_so_left_pt2(b,j,q,k)
term(91) = term(91) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_7_so_left_pt2(b,j,q,k)
term(92) = term(92) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_7_so_left_pt2(b,i,q,k)
term(93) = term(93) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_7_so_left_pt2(b,j,k,q)
term(94) = term(94) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_7_so_left_pt2(b,i,k,q)
term(95) = term(95) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_16_so_left_pt2(b,q,k,i)
term(96) = term(96) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_16_so_left_pt2(b,k,q,i)
term(97) = term(97) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_15_so_left_pt2(b,q,k,i)
term(98) = term(98) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_31_so_left_pt2(b,q,k,i)
term(99) = term(99) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_31_so_left_pt2(b,k,q,i)
term(100) = term(100) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_30_so_left_pt2(b,q,k,i)
end do 
end do 
end do 
end do 
end do 

term(90) = term(90) * (-2.0d+0) 
term(91) = term(91) * (4.0d+0) 
term(92) = term(92) * (-8.0d+0) 
term(93) = term(93) * (-2.0d+0) 
term(94) = term(94) * (4.0d+0) 
term(95) = term(95) * (-4.0d+0) 
term(96) = term(96) * (2.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(101) = term(101) + r2(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_5_so_left_pt2(b,j)
term(102) = term(102) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_6_so_left_pt2(b,j)
term(103) = term(103) + r2p(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_19_so_left_pt2(b,j)
term(104) = term(104) + r2p(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_18_so_left_pt2(b,j)
term(105) = term(105) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_14_so_left_pt2(b,j)
term(106) = term(106) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_20_so_left_pt2(b,j)
term(107) = term(107) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_21_so_left_pt2(b,j)
term(108) = term(108) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_19_so_left_pt2(b,j)
term(109) = term(109) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_18_so_left_pt2(b,j)
term(110) = term(110) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_29_so_left_pt2(b,j)
term(111) = term(111) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_18_so_left_pt2(b,j)
term(112) = term(112) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_19_so_left_pt2(b,j)
term(113) = term(113) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_32_so_left_pt2(b,j)
end do 
end do 
end do 
end do 

term(101) = term(101) * (-2.0d+0) 
term(102) = term(102) * (-2.0d+0) 
term(103) = term(103) * (2.0d+0) 
term(104) = term(104) * (-1.0d+0) 
term(105) = term(105) * (-1.0d+0) 
term(106) = term(106) * (-1.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (8.0d+0) 
term(109) = term(109) * (-4.0d+0) 
term(110) = term(110) * (-4.0d+0) 
term(111) = term(111) * (4.0d+0) 
term(112) = term(112) * (-8.0d+0) 
term(113) = term(113) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(114) = term(114) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,i,j) * wm_interm_18_so_left_pt2(b,j)
term(115) = term(115) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,i,j) * wm_interm_19_so_left_pt2(b,j)
term(116) = term(116) + r2p(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_18_so_left_pt2(b,j)
term(117) = term(117) + r2p(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_19_so_left_pt2(b,j)
term(118) = term(118) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_18_so_left_pt2(b,j)
term(119) = term(119) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,i,j) * wm_interm_19_so_left_pt2(b,j)
end do 
end do 
end do 
end do 

term(114) = term(114) * (-4.0d+0) 
term(115) = term(115) * (8.0d+0) 
term(116) = term(116) * (2.0d+0) 
term(117) = term(117) * (-4.0d+0) 
term(118) = term(118) * (8.0d+0) 
term(119) = term(119) * (-16.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(120) = term(120) + r2p(vrdav_Rl, p,i,a,q) * t2(a,b,i,j) * wm_interm_18_so_left_pt2(b,j)
term(121) = term(121) + r2p(vrdav_Rl, p,i,a,q) * t2(a,b,i,j) * wm_interm_19_so_left_pt2(b,j)
end do 
end do 
end do 
end do 

term(120) = term(120) * (2.0d+0) 
term(121) = term(121) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(122) = term(122) + r2(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_7_so_left_pt2(b,i,k,q)
term(123) = term(123) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_15_so_left_pt2(b,q,k,i)
term(124) = term(124) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_15_so_left_pt2(b,q,k,j)
term(125) = term(125) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_16_so_left_pt2(b,q,k,j)
term(126) = term(126) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_16_so_left_pt2(b,q,k,i)
term(127) = term(127) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_16_so_left_pt2(b,k,q,i)
term(128) = term(128) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_15_so_left_pt2(b,k,q,j)
term(129) = term(129) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_30_so_left_pt2(b,q,k,i)
term(130) = term(130) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_30_so_left_pt2(b,q,k,j)
term(131) = term(131) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_31_so_left_pt2(b,q,k,j)
term(132) = term(132) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_31_so_left_pt2(b,q,k,i)
term(133) = term(133) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_31_so_left_pt2(b,k,q,i)
term(134) = term(134) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_30_so_left_pt2(b,k,q,j)
end do 
end do 
end do 
end do 
end do 

term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * (-1.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-1.0d+0) 
term(126) = term(126) * (2.0d+0) 
term(127) = term(127) * (-1.0d+0) 
term(128) = term(128) * (-1.0d+0) 
term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (-2.0d+0) 
term(132) = term(132) * (4.0d+0) 
term(133) = term(133) * (-2.0d+0) 
term(134) = term(134) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(135) = term(135) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_44_so_left_pt2(a,i)
term(136) = term(136) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_44_so_left_pt2(a,i)
term(137) = term(137) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_45_so_left_pt2(a,i)
term(138) = term(138) + r2p(vrdav_Rl, a,q,p,i) * wm_interm_45_so_left_pt2(a,i)
term(139) = term(139) + r2(vrdav_Rr, a,q,p,i) * wm_interm_46_so_left_pt2(a,i)
term(140) = term(140) + r2(vrdav_Rr, a,q,p,i) * wm_interm_47_so_left_pt2(a,i)
term(141) = term(141) + r2(vrdav_Rr, a,q,p,i) * wm_interm_48_so_left_pt2(a,i)
term(142) = term(142) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_44_so_left_pt2(a,i)
term(143) = term(143) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_44_so_left_pt2(a,i)
term(144) = term(144) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_45_so_left_pt2(a,i)
term(145) = term(145) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_45_so_left_pt2(a,i)
term(146) = term(146) + r2(vrdav_Rr, a,q,p,i) * wm_interm_55_so_left_pt2(a,i)
term(147) = term(147) + r2(vrdav_Rr, a,q,p,i) * wm_interm_56_so_left_pt2(a,i)
end do 
end do 

term(135) = term(135) * (4.0d+0) 
term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * (-2.0d+0) 
term(139) = term(139) * (2.0d+0) 
term(140) = term(140) * (-1.0d+0) 
term(141) = term(141) * (-1.0d+0) 
term(142) = term(142) * (-8.0d+0) 
term(143) = term(143) * (8.0d+0) 
term(144) = term(144) * (4.0d+0) 
term(145) = term(145) * (-4.0d+0) 
term(146) = term(146) * (4.0d+0) 
term(147) = term(147) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(148) = term(148) + wm_interm_1_so_left_pt2(i,j) * wm_interm_30_so_left_pt2(p,q,j,i)
term(149) = term(149) + wm_interm_2_so_left_pt2(i,j) * wm_interm_30_so_left_pt2(p,q,j,i)
term(150) = term(150) + wm_interm_1_so_left_pt2(i,j) * wm_interm_31_so_left_pt2(p,q,j,i)
term(151) = term(151) + wm_interm_2_so_left_pt2(i,j) * wm_interm_31_so_left_pt2(p,q,j,i)
term(152) = term(152) + wm_interm_1_so_left_pt2(i,j) * wm_interm_30_so_left_pt2(p,j,q,i)
term(153) = term(153) + wm_interm_2_so_left_pt2(i,j) * wm_interm_30_so_left_pt2(p,j,q,i)
end do 
end do 

term(148) = term(148) * (4.0d+0) 
term(149) = term(149) * (-8.0d+0) 
term(150) = term(150) * (-2.0d+0) 
term(151) = term(151) * (4.0d+0) 
term(152) = term(152) * (-2.0d+0) 
term(153) = term(153) * (4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(154) = term(154) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,j,i) * wm_interm_18_so_left_pt2(b,j)
term(155) = term(155) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,j,i) * wm_interm_19_so_left_pt2(b,j)
term(156) = term(156) + r2p(vrdav_Rl, p,i,a,q) * t2(a,b,j,i) * wm_interm_19_so_left_pt2(b,j)
term(157) = term(157) + r2p(vrdav_Rl, p,i,a,q) * t2(a,b,j,i) * wm_interm_18_so_left_pt2(b,j)
end do 
end do 
end do 
end do 

term(154) = term(154) * (2.0d+0) 
term(155) = term(155) * (-4.0d+0) 
term(156) = term(156) * (2.0d+0) 
term(157) = term(157) * (-1.0d+0) 


    calc_D_vo_wm_so_left_pt2 = zero
    do s = 0, 157
    calc_D_vo_wm_so_left_pt2 = calc_D_vo_wm_so_left_pt2 + term(s)
    end do

    end function calc_D_vo_wm_so_left_pt2
    
    function calc_D_vv_wm_so_left_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_left_pt2
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
term(0) = term(0) + wm_interm_17_so_left_pt2(p,i,j,k) * wm_interm_7_so_left_pt2(q,i,k,j)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 

do a = nocc + 1, nactive 
term(1) = term(1) + wm_interm_39_so_left_pt2(a,p) * wm_interm_3_so_left_pt2(q,a)
term(2) = term(2) + wm_interm_39_so_left_pt2(a,p) * wm_interm_4_so_left_pt2(q,a)
term(3) = term(3) + wm_interm_39_so_left_pt2(p,a) * wm_interm_3_so_left_pt2(a,q)
term(4) = term(4) + wm_interm_39_so_left_pt2(p,a) * wm_interm_4_so_left_pt2(a,q)
end do 

term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (2.0d+0) 
term(4) = term(4) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(5) = term(5) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_1_so_left_pt2(i,j)
term(6) = term(6) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_2_so_left_pt2(i,j)
end do 
end do 

term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-4.0d+0) 

do i = 1, nocc 
term(7) = term(7) + wm_interm_19_so_left_pt2(p,i) * wm_interm_5_so_left_pt2(q,i)
term(8) = term(8) + wm_interm_18_so_left_pt2(p,i) * wm_interm_5_so_left_pt2(q,i)
term(9) = term(9) + r1(vrdav_Rl, q,i) * wm_interm_44_so_left_pt2(p,i)
term(10) = term(10) + r1(vrdav_Rl, q,i) * wm_interm_45_so_left_pt2(p,i)
term(11) = term(11) + s1(q,i) * wm_interm_6_so_left_pt2(p,i)
term(12) = term(12) + r1(vrdav_Rr, p,i) * wm_interm_46_so_left_pt2(q,i)
term(13) = term(13) + r1(vrdav_Rr, p,i) * wm_interm_47_so_left_pt2(q,i)
term(14) = term(14) + r1(vrdav_Rr, p,i) * wm_interm_48_so_left_pt2(q,i)
term(15) = term(15) + t1(q,i) * wm_interm_14_so_left_pt2(p,i)
term(16) = term(16) + t1(q,i) * wm_interm_21_so_left_pt2(p,i)
term(17) = term(17) + t1(q,i) * wm_interm_20_so_left_pt2(p,i)
term(18) = term(18) + r1(vrdav_Rr, p,i) * wm_interm_55_so_left_pt2(q,i)
term(19) = term(19) + r1(vrdav_Rr, p,i) * wm_interm_56_so_left_pt2(q,i)
term(20) = term(20) + t1(q,i) * wm_interm_29_so_left_pt2(p,i)
term(21) = term(21) + t1(q,i) * wm_interm_32_so_left_pt2(p,i)
end do 

term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (4.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * (4.0d+0) 
term(20) = term(20) * (4.0d+0) 
term(21) = term(21) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(22) = term(22) + s2(a,p,k,j) * t2(a,q,j,i) * wm_interm_38_so_left_pt2(i,k)
term(23) = term(23) + s2(a,p,i,j) * t2(a,q,k,i) * wm_interm_38_so_left_pt2(k,j)
term(24) = term(24) + s2(a,p,k,j) * t2(a,q,k,i) * wm_interm_38_so_left_pt2(i,j)
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
term(25) = term(25) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_5_so_left_pt2(a,j)
term(26) = term(26) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_18_so_left_pt2(a,j)
term(27) = term(27) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_19_so_left_pt2(a,j)
end do 
end do 
end do 

term(25) = term(25) * (2.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_18_so_left_pt2(a,j)
term(29) = term(29) + r1(vrdav_Rl, p,i) * t2(a,q,j,i) * wm_interm_19_so_left_pt2(a,j)
end do 
end do 
end do 

term(28) = term(28) * (-4.0d+0) 
term(29) = term(29) * (8.0d+0) 


    calc_D_vv_wm_so_left_pt2 = zero
    do s = 0, 29
    calc_D_vv_wm_so_left_pt2 = calc_D_vv_wm_so_left_pt2 + term(s)
    end do

    end function calc_D_vv_wm_so_left_pt2


    function calc_D_oo_wm_so_left_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_so_left_pt3
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
    real(F64), dimension(0:71) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_0_so_left_pt3(a,i,q,j) * wm_interm_47_so_left_pt3(a,j,i,p)
term(1) = term(1) + wm_interm_0_so_left_pt3(a,i,q,j) * wm_interm_47_so_left_pt3(a,j,p,i)
term(2) = term(2) + wm_interm_0_so_left_pt3(a,q,i,j) * wm_interm_47_so_left_pt3(a,j,p,i)
term(3) = term(3) + wm_interm_0_so_left_pt3(a,i,j,q) * wm_interm_47_so_left_pt3(a,p,i,j)
term(4) = term(4) + wm_interm_12_so_left_pt3(a,q,i,j) * wm_interm_51_so_left_pt3(a,i,j,p)
term(5) = term(5) + wm_interm_12_so_left_pt3(a,q,i,j) * wm_interm_51_so_left_pt3(a,j,i,p)
term(6) = term(6) + wm_interm_12_so_left_pt3(a,q,i,j) * wm_interm_52_so_left_pt3(a,j,i,p)
term(7) = term(7) + wm_interm_12_so_left_pt3(a,i,q,j) * wm_interm_52_so_left_pt3(a,j,p,i)
term(8) = term(8) + wm_interm_12_so_left_pt3(a,i,q,j) * wm_interm_52_so_left_pt3(a,p,j,i)
term(9) = term(9) + wm_interm_12_so_left_pt3(a,i,q,j) * wm_interm_51_so_left_pt3(a,p,j,i)
term(10) = term(10) + wm_interm_12_so_left_pt3(a,q,i,j) * wm_interm_58_so_left_pt3(a,i,j,p)
term(11) = term(11) + wm_interm_12_so_left_pt3(a,q,i,j) * wm_interm_58_so_left_pt3(a,j,i,p)
term(12) = term(12) + wm_interm_12_so_left_pt3(a,q,i,j) * wm_interm_59_so_left_pt3(a,j,i,p)
term(13) = term(13) + wm_interm_12_so_left_pt3(a,i,q,j) * wm_interm_59_so_left_pt3(a,j,p,i)
term(14) = term(14) + wm_interm_12_so_left_pt3(a,i,q,j) * wm_interm_59_so_left_pt3(a,p,j,i)
term(15) = term(15) + wm_interm_12_so_left_pt3(a,i,q,j) * wm_interm_58_so_left_pt3(a,p,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * (4.0d+0) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (-1.0d+0) 
term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-1.0d+0) 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (4.0d+0) 
term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * (4.0d+0) 
term(15) = term(15) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_0_so_left_pt3(a,p,i,q) * wm_interm_50_so_left_pt3(a,i)
term(17) = term(17) + wm_interm_10_so_left_pt3(a,i) * wm_interm_51_so_left_pt3(a,q,i,p)
term(18) = term(18) + wm_interm_10_so_left_pt3(a,i) * wm_interm_51_so_left_pt3(a,i,q,p)
term(19) = term(19) + wm_interm_10_so_left_pt3(a,i) * wm_interm_52_so_left_pt3(a,q,i,p)
term(20) = term(20) + wm_interm_11_so_left_pt3(a,i) * wm_interm_52_so_left_pt3(a,q,i,p)
term(21) = term(21) + wm_interm_11_so_left_pt3(a,i) * wm_interm_51_so_left_pt3(a,q,i,p)
term(22) = term(22) + wm_interm_11_so_left_pt3(a,i) * wm_interm_51_so_left_pt3(a,i,q,p)
term(23) = term(23) + wm_interm_12_so_left_pt3(a,p,q,i) * wm_interm_55_so_left_pt3(a,i)
term(24) = term(24) + wm_interm_12_so_left_pt3(a,p,q,i) * wm_interm_56_so_left_pt3(a,i)
term(25) = term(25) + wm_interm_12_so_left_pt3(a,p,q,i) * wm_interm_57_so_left_pt3(a,i)
term(26) = term(26) + wm_interm_10_so_left_pt3(a,i) * wm_interm_58_so_left_pt3(a,q,i,p)
term(27) = term(27) + wm_interm_10_so_left_pt3(a,i) * wm_interm_58_so_left_pt3(a,i,q,p)
term(28) = term(28) + wm_interm_10_so_left_pt3(a,i) * wm_interm_59_so_left_pt3(a,q,i,p)
term(29) = term(29) + wm_interm_11_so_left_pt3(a,i) * wm_interm_59_so_left_pt3(a,q,i,p)
term(30) = term(30) + wm_interm_11_so_left_pt3(a,i) * wm_interm_58_so_left_pt3(a,q,i,p)
term(31) = term(31) + wm_interm_11_so_left_pt3(a,i) * wm_interm_58_so_left_pt3(a,i,q,p)
term(32) = term(32) + wm_interm_12_so_left_pt3(a,p,q,i) * wm_interm_60_so_left_pt3(a,i)
term(33) = term(33) + wm_interm_12_so_left_pt3(a,p,q,i) * wm_interm_61_so_left_pt3(a,i)
end do 
end do 

term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * (2.0d+0) 
term(19) = term(19) * (2.0d+0) 
term(20) = term(20) * (-1.0d+0) 
term(21) = term(21) * (2.0d+0) 
term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (2.0d+0) 
term(24) = term(24) * (-1.0d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (-8.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * (4.0d+0) 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * (4.0d+0) 
term(33) = term(33) * (-4.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + wm_interm_0_so_left_pt3(a,i,j,q) * wm_interm_47_so_left_pt3(a,p,j,i)
term(35) = term(35) + wm_interm_12_so_left_pt3(a,i,j,q) * wm_interm_52_so_left_pt3(a,p,j,i)
term(36) = term(36) + wm_interm_12_so_left_pt3(a,i,j,q) * wm_interm_51_so_left_pt3(a,j,p,i)
term(37) = term(37) + wm_interm_12_so_left_pt3(a,i,j,q) * wm_interm_51_so_left_pt3(a,p,j,i)
term(38) = term(38) + wm_interm_12_so_left_pt3(a,i,j,q) * wm_interm_59_so_left_pt3(a,p,j,i)
term(39) = term(39) + wm_interm_12_so_left_pt3(a,i,j,q) * wm_interm_58_so_left_pt3(a,j,p,i)
term(40) = term(40) + wm_interm_12_so_left_pt3(a,i,j,q) * wm_interm_58_so_left_pt3(a,p,j,i)
end do 
end do 
end do 

term(34) = term(34) * (4.0d+0) 
term(35) = term(35) * (-1.0d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (2.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (4.0d+0) 

do i = 1, nocc 
term(41) = term(41) + wm_interm_49_so_left_pt3(q,i) * wm_interm_4_so_left_pt3(p,i)
term(42) = term(42) + wm_interm_49_so_left_pt3(q,i) * wm_interm_5_so_left_pt3(p,i)
term(43) = term(43) + wm_interm_49_so_left_pt3(i,q) * wm_interm_4_so_left_pt3(i,p)
term(44) = term(44) + wm_interm_49_so_left_pt3(i,q) * wm_interm_5_so_left_pt3(i,p)
term(45) = term(45) + wm_interm_13_so_left_pt3(q,i) * wm_interm_54_so_left_pt3(p,i)
term(46) = term(46) + wm_interm_15_so_left_pt3(q,i) * wm_interm_54_so_left_pt3(p,i)
term(47) = term(47) + wm_interm_16_so_left_pt3(q,i) * wm_interm_54_so_left_pt3(p,i)
term(48) = term(48) + wm_interm_13_so_left_pt3(i,q) * wm_interm_54_so_left_pt3(i,p)
term(49) = term(49) + wm_interm_16_so_left_pt3(i,q) * wm_interm_54_so_left_pt3(i,p)
term(50) = term(50) + wm_interm_15_so_left_pt3(i,q) * wm_interm_54_so_left_pt3(i,p)
term(51) = term(51) + wm_interm_38_so_left_pt3(q,i) * wm_interm_54_so_left_pt3(p,i)
term(52) = term(52) + wm_interm_39_so_left_pt3(q,i) * wm_interm_54_so_left_pt3(p,i)
term(53) = term(53) + wm_interm_38_so_left_pt3(i,q) * wm_interm_54_so_left_pt3(i,p)
term(54) = term(54) + wm_interm_39_so_left_pt3(i,q) * wm_interm_54_so_left_pt3(i,p)
end do 

term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (4.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (4.0d+0) 
term(45) = term(45) * (-1.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-1.0d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (-1.0d+0) 
term(50) = term(50) * (2.0d+0) 
term(51) = term(51) * (-4.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (-4.0d+0) 
term(54) = term(54) * (4.0d+0) 

do a = nocc + 1, nactive 
term(55) = term(55) + wm_interm_10_so_left_pt3(a,q) * wm_interm_57_so_left_pt3(a,p)
term(56) = term(56) + wm_interm_10_so_left_pt3(a,q) * wm_interm_55_so_left_pt3(a,p)
term(57) = term(57) + wm_interm_10_so_left_pt3(a,q) * wm_interm_56_so_left_pt3(a,p)
term(58) = term(58) + wm_interm_11_so_left_pt3(a,q) * wm_interm_57_so_left_pt3(a,p)
term(59) = term(59) + wm_interm_11_so_left_pt3(a,q) * wm_interm_55_so_left_pt3(a,p)
term(60) = term(60) + wm_interm_11_so_left_pt3(a,q) * wm_interm_56_so_left_pt3(a,p)
term(61) = term(61) + wm_interm_10_so_left_pt3(a,q) * wm_interm_61_so_left_pt3(a,p)
term(62) = term(62) + wm_interm_10_so_left_pt3(a,q) * wm_interm_60_so_left_pt3(a,p)
term(63) = term(63) + wm_interm_11_so_left_pt3(a,q) * wm_interm_61_so_left_pt3(a,p)
term(64) = term(64) + wm_interm_11_so_left_pt3(a,q) * wm_interm_60_so_left_pt3(a,p)
end do 

term(55) = term(55) * (2.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (2.0d+0) 
term(58) = term(58) * (-1.0d+0) 
term(59) = term(59) * (2.0d+0) 
term(60) = term(60) * (-1.0d+0) 
term(61) = term(61) * (8.0d+0) 
term(62) = term(62) * (-8.0d+0) 
term(63) = term(63) * (-4.0d+0) 
term(64) = term(64) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(65) = term(65) + wm_interm_49_so_left_pt3(i,j) * wm_interm_6_so_left_pt3(i,p,q,j)
term(66) = term(66) + wm_interm_30_so_left_pt3(i,p,q,j) * wm_interm_54_so_left_pt3(i,j)
term(67) = term(67) + wm_interm_30_so_left_pt3(p,i,q,j) * wm_interm_54_so_left_pt3(i,j)
term(68) = term(68) + wm_interm_30_so_left_pt3(p,i,j,q) * wm_interm_54_so_left_pt3(i,j)
term(69) = term(69) + wm_interm_46_so_left_pt3(i,p,q,j) * wm_interm_54_so_left_pt3(i,j)
term(70) = term(70) + wm_interm_46_so_left_pt3(p,i,q,j) * wm_interm_54_so_left_pt3(i,j)
term(71) = term(71) + wm_interm_46_so_left_pt3(p,i,j,q) * wm_interm_54_so_left_pt3(i,j)
end do 
end do 

term(65) = term(65) * (-2.0d+0) 
term(66) = term(66) * (-1.0d+0) 
term(67) = term(67) * (2.0d+0) 
term(68) = term(68) * (-1.0d+0) 
term(69) = term(69) * (-2.0d+0) 
term(70) = term(70) * (4.0d+0) 
term(71) = term(71) * (-2.0d+0) 


    calc_D_oo_wm_so_left_pt3 = zero
    do s = 0, 71
    calc_D_oo_wm_so_left_pt3 = calc_D_oo_wm_so_left_pt3 + term(s)
    end do

    end function calc_D_oo_wm_so_left_pt3
    
    function calc_D_ov_wm_so_left_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_so_left_pt3
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
    real(F64), dimension(0:88) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_3_so_left_pt3(q,i,j,k) * wm_interm_6_so_left_pt3(i,j,k,p)
term(1) = term(1) + wm_interm_3_so_left_pt3(q,i,j,k) * wm_interm_6_so_left_pt3(i,j,p,k)
term(2) = term(2) + wm_interm_6_so_left_pt3(i,j,p,k) * wm_interm_9_so_left_pt3(q,i,j,k)
term(3) = term(3) + wm_interm_1_so_left_pt3(i,j,p,k) * wm_interm_32_so_left_pt3(q,k,i,j)
term(4) = term(4) + wm_interm_35_so_left_pt3(q,i,j,k) * wm_interm_6_so_left_pt3(i,j,k,p)
term(5) = term(5) + wm_interm_35_so_left_pt3(q,i,j,k) * wm_interm_6_so_left_pt3(i,j,p,k)
term(6) = term(6) + wm_interm_37_so_left_pt3(q,i,j,k) * wm_interm_6_so_left_pt3(i,j,p,k)
term(7) = term(7) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_33_so_left_pt3(k,j,i,p)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (2.0d+0) 
term(2) = term(2) * (-1.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (4.0d+0) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(8) = term(8) + r2p(vrdav_Rl, q,i,a,j) * r2(vrdav_Rr, a,i,b,k) * wm_interm_0_so_left_pt3(b,k,p,j)
term(9) = term(9) + r2p(vrdav_Rl, q,i,a,j) * r2(vrdav_Rr, a,i,b,k) * wm_interm_0_so_left_pt3(b,p,k,j)
end do 
end do 
end do 
end do 
end do 

term(8) = term(8) * (2.0d+0) 
term(9) = term(9) * (-1.0d+0) 

do a = nocc + 1, nactive 
term(10) = term(10) + wm_interm_22_so_left_pt3(a,p) * wm_interm_24_so_left_pt3(a,q)
term(11) = term(11) + wm_interm_22_so_left_pt3(a,p) * wm_interm_25_so_left_pt3(a,q)
term(12) = term(12) + wm_interm_22_so_left_pt3(a,p) * wm_interm_26_so_left_pt3(a,q)
term(13) = term(13) + wm_interm_23_so_left_pt3(a,p) * wm_interm_24_so_left_pt3(a,q)
term(14) = term(14) + wm_interm_23_so_left_pt3(a,p) * wm_interm_25_so_left_pt3(a,q)
term(15) = term(15) + wm_interm_23_so_left_pt3(a,p) * wm_interm_26_so_left_pt3(a,q)
term(16) = term(16) + wm_interm_22_so_left_pt3(a,p) * wm_interm_42_so_left_pt3(a,q)
term(17) = term(17) + wm_interm_22_so_left_pt3(a,p) * wm_interm_43_so_left_pt3(a,q)
term(18) = term(18) + wm_interm_23_so_left_pt3(a,p) * wm_interm_42_so_left_pt3(a,q)
term(19) = term(19) + wm_interm_23_so_left_pt3(a,p) * wm_interm_43_so_left_pt3(a,q)
term(20) = term(20) + wm_interm_48_so_left_pt3(q,a) * wm_interm_62_so_left_pt3(a,p)
term(21) = term(21) + wm_interm_48_so_left_pt3(q,a) * wm_interm_63_so_left_pt3(a,p)
end do 

term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * (-2.0d+0) 
term(12) = term(12) * (4.0d+0) 
term(15) = term(15) * (-2.0d+0) 
term(16) = term(16) * (-8.0d+0) 
term(17) = term(17) * (8.0d+0) 
term(18) = term(18) * (4.0d+0) 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(22) = term(22) + r2p(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,k,b,i) * wm_interm_0_so_left_pt3(b,p,k,j)
term(23) = term(23) + r2p(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,k,b,j) * wm_interm_0_so_left_pt3(b,k,p,i)
term(24) = term(24) + r2m(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,i) * wm_interm_0_so_left_pt3(b,k,p,j)
term(25) = term(25) + r2m(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,i) * wm_interm_0_so_left_pt3(b,p,k,j)
term(26) = term(26) + r2m(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,k,b,i) * wm_interm_0_so_left_pt3(b,p,k,j)
term(27) = term(27) + r2m(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,j) * wm_interm_0_so_left_pt3(b,p,k,i)
term(28) = term(28) + r2m(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,k,b,j) * wm_interm_0_so_left_pt3(b,k,p,i)
term(29) = term(29) + r2m(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,k,b,j) * wm_interm_0_so_left_pt3(b,k,p,i)
end do 
end do 
end do 
end do 
end do 

term(22) = term(22) * (-1.0d+0) 
term(23) = term(23) * (-1.0d+0) 
term(24) = term(24) * (-2.0d+0) 
term(25) = term(25) * (4.0d+0) 
term(26) = term(26) * (-2.0d+0) 
term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * (4.0d+0) 
term(29) = term(29) * (-2.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
term(30) = term(30) + wm_interm_1_so_left_pt3(i,j,k,p) * wm_interm_32_so_left_pt3(q,k,j,i)
end do 
end do 
end do 

term(30) = term(30) * (-1.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(31) = term(31) + wm_interm_1_so_left_pt3(i,j,p,k) * wm_interm_32_so_left_pt3(q,k,j,i)
term(32) = term(32) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_33_so_left_pt3(j,k,p,i)
term(33) = term(33) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_33_so_left_pt3(k,j,p,i)
end do 
end do 
end do 

term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(34) = term(34) + r2p(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_14_so_left_pt3(b,j,p,k)
term(35) = term(35) + r2p(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_14_so_left_pt3(a,p,j,k)
term(36) = term(36) + r2p(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_14_so_left_pt3(b,p,j,i)
term(37) = term(37) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_14_so_left_pt3(a,p,k,i)
term(38) = term(38) + r2p(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_14_so_left_pt3(a,p,j,i)
term(39) = term(39) + r2m(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,p,k,i)
term(40) = term(40) + r2m(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,p,i)
term(41) = term(41) + r2m(vrdav_Rl, a,j,b,i) * t2(a,q,k,i) * wm_interm_14_so_left_pt3(b,j,p,k)
term(42) = term(42) + r2m(vrdav_Rl, a,j,b,i) * t2(b,q,k,i) * wm_interm_14_so_left_pt3(a,p,j,k)
term(43) = term(43) + r2m(vrdav_Rl, a,j,b,k) * t2(a,q,k,i) * wm_interm_14_so_left_pt3(b,p,j,i)
term(44) = term(44) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,j,i) * wm_interm_14_so_left_pt3(a,p,k,i)
term(45) = term(45) + r2m(vrdav_Rl, a,j,b,k) * t2(b,q,k,i) * wm_interm_14_so_left_pt3(a,p,j,i)
end do 
end do 
end do 
end do 
end do 

term(34) = term(34) * (-1.0d+0) 
term(35) = term(35) * (-1.0d+0) 
term(36) = term(36) * (-1.0d+0) 
term(37) = term(37) * (-1.0d+0) 
term(38) = term(38) * (2.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-8.0d+0) 
term(41) = term(41) * (-2.0d+0) 
term(42) = term(42) * (-2.0d+0) 
term(43) = term(43) * (-2.0d+0) 
term(44) = term(44) * (-2.0d+0) 
term(45) = term(45) * (4.0d+0) 

do i = 1, nocc 
term(46) = term(46) + wm_interm_17_so_left_pt3(q,i) * wm_interm_4_so_left_pt3(i,p)
term(47) = term(47) + wm_interm_17_so_left_pt3(q,i) * wm_interm_5_so_left_pt3(i,p)
term(48) = term(48) + wm_interm_20_so_left_pt3(q,i) * wm_interm_4_so_left_pt3(i,p)
term(49) = term(49) + wm_interm_20_so_left_pt3(q,i) * wm_interm_5_so_left_pt3(i,p)
term(50) = term(50) + wm_interm_21_so_left_pt3(q,i) * wm_interm_4_so_left_pt3(i,p)
term(51) = term(51) + wm_interm_21_so_left_pt3(q,i) * wm_interm_5_so_left_pt3(i,p)
term(52) = term(52) + wm_interm_11_so_left_pt3(q,i) * wm_interm_7_so_left_pt3(i,p)
term(53) = term(53) + wm_interm_11_so_left_pt3(q,i) * wm_interm_8_so_left_pt3(i,p)
term(54) = term(54) + wm_interm_10_so_left_pt3(q,i) * wm_interm_7_so_left_pt3(i,p)
term(55) = term(55) + wm_interm_10_so_left_pt3(q,i) * wm_interm_8_so_left_pt3(i,p)
term(56) = term(56) + wm_interm_11_so_left_pt3(q,i) * wm_interm_2_so_left_pt3(i,p)
term(57) = term(57) + wm_interm_10_so_left_pt3(q,i) * wm_interm_2_so_left_pt3(i,p)
term(58) = term(58) + wm_interm_40_so_left_pt3(q,i) * wm_interm_4_so_left_pt3(i,p)
term(59) = term(59) + wm_interm_40_so_left_pt3(q,i) * wm_interm_5_so_left_pt3(i,p)
term(60) = term(60) + wm_interm_41_so_left_pt3(q,i) * wm_interm_4_so_left_pt3(i,p)
term(61) = term(61) + wm_interm_41_so_left_pt3(q,i) * wm_interm_5_so_left_pt3(i,p)
term(62) = term(62) + wm_interm_11_so_left_pt3(q,i) * wm_interm_34_so_left_pt3(i,p)
term(63) = term(63) + wm_interm_11_so_left_pt3(q,i) * wm_interm_36_so_left_pt3(i,p)
term(64) = term(64) + wm_interm_10_so_left_pt3(q,i) * wm_interm_34_so_left_pt3(i,p)
term(65) = term(65) + wm_interm_10_so_left_pt3(q,i) * wm_interm_36_so_left_pt3(i,p)
term(66) = term(66) + wm_interm_11_so_left_pt3(q,i) * wm_interm_64_so_left_pt3(i,p)
term(67) = term(67) + wm_interm_10_so_left_pt3(q,i) * wm_interm_64_so_left_pt3(i,p)
term(68) = term(68) + wm_interm_54_so_left_pt3(p,i) * wm_interm_66_so_left_pt3(q,i)
end do 

term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (-4.0d+0) 
term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (2.0d+0) 
term(52) = term(52) * (-1.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (2.0d+0) 
term(55) = term(55) * (-4.0d+0) 
term(56) = term(56) * (-1.0d+0) 
term(57) = term(57) * (2.0d+0) 
term(58) = term(58) * (4.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * (8.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(63) = term(63) * (4.0d+0) 
term(64) = term(64) * (8.0d+0) 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * (2.0d+0) 
term(67) = term(67) * (-4.0d+0) 
term(68) = term(68) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(69) = term(69) + r2p(vrdav_Rl, q,i,a,j) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,p,k,i)
term(70) = term(70) + r2p(vrdav_Rl, q,i,a,j) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,p,i)
term(71) = term(71) + r2p(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,p,i)
term(72) = term(72) + r2p(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,p,k,i)
term(73) = term(73) + r2p(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_14_so_left_pt3(b,p,j,k)
term(74) = term(74) + r2p(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_14_so_left_pt3(b,j,p,k)
term(75) = term(75) + r2p(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_14_so_left_pt3(a,j,p,k)
term(76) = term(76) + r2p(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_14_so_left_pt3(a,p,j,k)
term(77) = term(77) + r2m(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,i,b,k) * wm_interm_0_so_left_pt3(b,k,p,j)
term(78) = term(78) + r2m(vrdav_Rl, a,j,q,i) * r2(vrdav_Rr, a,i,b,k) * wm_interm_0_so_left_pt3(b,p,k,j)
term(79) = term(79) + r2m(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,p,i)
term(80) = term(80) + r2m(vrdav_Rl, a,i,q,j) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,p,k,i)
term(81) = term(81) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_14_so_left_pt3(b,p,j,k)
term(82) = term(82) + r2m(vrdav_Rl, a,i,b,j) * t2(a,q,k,i) * wm_interm_14_so_left_pt3(b,j,p,k)
term(83) = term(83) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_14_so_left_pt3(a,j,p,k)
term(84) = term(84) + r2m(vrdav_Rl, a,i,b,j) * t2(b,q,k,i) * wm_interm_14_so_left_pt3(a,p,j,k)
end do 
end do 
end do 
end do 
end do 

term(69) = term(69) * (2.0d+0) 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * (2.0d+0) 
term(72) = term(72) * (-1.0d+0) 
term(73) = term(73) * (-1.0d+0) 
term(74) = term(74) * (2.0d+0) 
term(75) = term(75) * (-1.0d+0) 
term(76) = term(76) * (2.0d+0) 
term(77) = term(77) * (4.0d+0) 
term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (4.0d+0) 
term(80) = term(80) * (-2.0d+0) 
term(81) = term(81) * (-2.0d+0) 
term(82) = term(82) * (4.0d+0) 
term(83) = term(83) * (-2.0d+0) 
term(84) = term(84) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(85) = term(85) + r2p(vrdav_Rl, q,i,a,j) * r2(vrdav_Rr, a,k,b,i) * wm_interm_0_so_left_pt3(b,k,p,j)
term(86) = term(86) + r2p(vrdav_Rl, q,i,a,j) * r2(vrdav_Rr, a,k,b,i) * wm_interm_0_so_left_pt3(b,p,k,j)
term(87) = term(87) + r2p(vrdav_Rl, q,i,a,j) * r2(vrdav_Rr, a,k,b,j) * wm_interm_0_so_left_pt3(b,p,k,i)
term(88) = term(88) + r2p(vrdav_Rl, q,i,a,j) * r2(vrdav_Rr, a,k,b,j) * wm_interm_0_so_left_pt3(b,k,p,i)
end do 
end do 
end do 
end do 
end do 

term(85) = term(85) * (-1.0d+0) 
term(86) = term(86) * (2.0d+0) 
term(87) = term(87) * (-1.0d+0) 
term(88) = term(88) * (2.0d+0) 


    calc_D_ov_wm_so_left_pt3 = zero
    do s = 0, 88
    calc_D_ov_wm_so_left_pt3 = calc_D_ov_wm_so_left_pt3 + term(s)
    end do

    end function calc_D_ov_wm_so_left_pt3
    
    function calc_D_vo_wm_so_left_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_so_left_pt3
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
    real(F64), dimension(0:172) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(0) = term(0) + wm_interm_0_so_left_pt3(p,i,j,k) * wm_interm_1_so_left_pt3(q,k,j,i)
term(1) = term(1) + wm_interm_0_so_left_pt3(p,i,j,k) * wm_interm_33_so_left_pt3(q,k,j,i)
end do 
end do 
end do 

term(0) = term(0) * (-1.0d+0) 
term(1) = term(1) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(2) = term(2) + wm_interm_0_so_left_pt3(p,i,j,k) * wm_interm_1_so_left_pt3(q,k,i,j)
term(3) = term(3) + wm_interm_0_so_left_pt3(p,i,j,k) * wm_interm_1_so_left_pt3(k,q,i,j)
term(4) = term(4) + wm_interm_30_so_left_pt3(i,j,q,k) * wm_interm_31_so_left_pt3(p,i,j,k)
term(5) = term(5) + wm_interm_14_so_left_pt3(p,i,j,k) * wm_interm_30_so_left_pt3(i,j,q,k)
term(6) = term(6) + wm_interm_14_so_left_pt3(p,i,j,k) * wm_interm_30_so_left_pt3(i,j,k,q)
term(7) = term(7) + wm_interm_0_so_left_pt3(p,i,j,k) * wm_interm_33_so_left_pt3(q,k,i,j)
term(8) = term(8) + wm_interm_0_so_left_pt3(p,i,j,k) * wm_interm_33_so_left_pt3(k,q,i,j)
term(9) = term(9) + wm_interm_31_so_left_pt3(p,i,j,k) * wm_interm_46_so_left_pt3(i,j,q,k)
term(10) = term(10) + wm_interm_14_so_left_pt3(p,i,j,k) * wm_interm_46_so_left_pt3(i,j,q,k)
term(11) = term(11) + wm_interm_14_so_left_pt3(p,i,j,k) * wm_interm_46_so_left_pt3(i,j,k,q)
end do 
end do 
end do 

term(2) = term(2) * (2.0d+0) 
term(3) = term(3) * (-1.0d+0) 
term(4) = term(4) * (-1.0d+0) 
term(5) = term(5) * (2.0d+0) 
term(6) = term(6) * (-1.0d+0) 
term(7) = term(7) * (4.0d+0) 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * (-2.0d+0) 
term(10) = term(10) * (4.0d+0) 
term(11) = term(11) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(12) = term(12) + r2p(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_12_so_left_pt3(b,k,j,q)
term(13) = term(13) + r2m(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,k,p,i) * wm_interm_12_so_left_pt3(b,k,j,q)
end do 
end do 
end do 
end do 
end do 

term(12) = term(12) * (-1.0d+0) 
term(13) = term(13) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(14) = term(14) + r2p(vrdav_Rl, p,q,a,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,j,k,i)
term(15) = term(15) + r2p(vrdav_Rl, p,q,a,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,j,i)
term(16) = term(16) + r2p(vrdav_Rl, p,i,a,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,j,k,i)
term(17) = term(17) + r2p(vrdav_Rl, p,i,a,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(14) = term(14) * (2.0d+0) 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (-1.0d+0) 
term(17) = term(17) * (2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(18) = term(18) + wm_interm_0_so_left_pt3(p,i,q,j) * wm_interm_2_so_left_pt3(j,i)
term(19) = term(19) + wm_interm_3_so_left_pt3(p,i,q,j) * wm_interm_4_so_left_pt3(i,j)
term(20) = term(20) + wm_interm_3_so_left_pt3(p,i,q,j) * wm_interm_5_so_left_pt3(i,j)
term(21) = term(21) + wm_interm_3_so_left_pt3(p,q,i,j) * wm_interm_4_so_left_pt3(i,j)
term(22) = term(22) + wm_interm_3_so_left_pt3(p,q,i,j) * wm_interm_5_so_left_pt3(i,j)
term(23) = term(23) + wm_interm_0_so_left_pt3(p,i,q,j) * wm_interm_7_so_left_pt3(j,i)
term(24) = term(24) + wm_interm_0_so_left_pt3(p,i,q,j) * wm_interm_8_so_left_pt3(j,i)
term(25) = term(25) + wm_interm_4_so_left_pt3(i,j) * wm_interm_9_so_left_pt3(p,q,i,j)
term(26) = term(26) + wm_interm_5_so_left_pt3(i,j) * wm_interm_9_so_left_pt3(p,q,i,j)
term(27) = term(27) + wm_interm_13_so_left_pt3(i,j) * wm_interm_14_so_left_pt3(p,i,q,j)
term(28) = term(28) + wm_interm_14_so_left_pt3(p,i,q,j) * wm_interm_15_so_left_pt3(i,j)
term(29) = term(29) + wm_interm_14_so_left_pt3(p,i,q,j) * wm_interm_16_so_left_pt3(i,j)
term(30) = term(30) + wm_interm_32_so_left_pt3(p,i,q,j) * wm_interm_7_so_left_pt3(j,i)
term(31) = term(31) + wm_interm_32_so_left_pt3(p,i,q,j) * wm_interm_8_so_left_pt3(j,i)
term(32) = term(32) + wm_interm_0_so_left_pt3(p,i,q,j) * wm_interm_34_so_left_pt3(j,i)
term(33) = term(33) + wm_interm_35_so_left_pt3(p,i,q,j) * wm_interm_4_so_left_pt3(i,j)
term(34) = term(34) + wm_interm_35_so_left_pt3(p,i,q,j) * wm_interm_5_so_left_pt3(i,j)
term(35) = term(35) + wm_interm_35_so_left_pt3(p,q,i,j) * wm_interm_4_so_left_pt3(i,j)
term(36) = term(36) + wm_interm_35_so_left_pt3(p,q,i,j) * wm_interm_5_so_left_pt3(i,j)
term(37) = term(37) + wm_interm_0_so_left_pt3(p,i,q,j) * wm_interm_36_so_left_pt3(j,i)
term(38) = term(38) + wm_interm_37_so_left_pt3(p,q,i,j) * wm_interm_4_so_left_pt3(i,j)
term(39) = term(39) + wm_interm_37_so_left_pt3(p,q,i,j) * wm_interm_5_so_left_pt3(i,j)
term(40) = term(40) + wm_interm_14_so_left_pt3(p,i,q,j) * wm_interm_38_so_left_pt3(i,j)
term(41) = term(41) + wm_interm_14_so_left_pt3(p,i,q,j) * wm_interm_39_so_left_pt3(i,j)
term(42) = term(42) + wm_interm_32_so_left_pt3(p,i,q,j) * wm_interm_34_so_left_pt3(j,i)
term(43) = term(43) + wm_interm_32_so_left_pt3(p,i,q,j) * wm_interm_36_so_left_pt3(j,i)
term(44) = term(44) + wm_interm_0_so_left_pt3(p,i,q,j) * wm_interm_64_so_left_pt3(j,i)
term(45) = term(45) + wm_interm_49_so_left_pt3(i,j) * wm_interm_65_so_left_pt3(p,i,q,j)
term(46) = term(46) + wm_interm_32_so_left_pt3(p,i,q,j) * wm_interm_64_so_left_pt3(j,i)
term(47) = term(47) + wm_interm_54_so_left_pt3(i,j) * wm_interm_68_so_left_pt3(p,i,q,j)
end do 
end do 

term(18) = term(18) * (-1.0d+0) 
term(19) = term(19) * (-1.0d+0) 
term(20) = term(20) * (2.0d+0) 
term(21) = term(21) * (2.0d+0) 
term(22) = term(22) * (-4.0d+0) 
term(23) = term(23) * (-1.0d+0) 
term(24) = term(24) * (2.0d+0) 
term(25) = term(25) * (-1.0d+0) 
term(26) = term(26) * (2.0d+0) 
term(27) = term(27) * (-1.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-1.0d+0) 
term(30) = term(30) * (-1.0d+0) 
term(31) = term(31) * (2.0d+0) 
term(32) = term(32) * (-4.0d+0) 
term(33) = term(33) * (-2.0d+0) 
term(34) = term(34) * (4.0d+0) 
term(35) = term(35) * (4.0d+0) 
term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * (4.0d+0) 
term(38) = term(38) * (-2.0d+0) 
term(39) = term(39) * (4.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * (4.0d+0) 
term(44) = term(44) * (2.0d+0) 
term(45) = term(45) * (2.0d+0) 
term(46) = term(46) * (2.0d+0) 
term(47) = term(47) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(48) = term(48) + r2p(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,j,k,i)
term(49) = term(49) + r2p(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,j,i)
term(50) = term(50) + r2p(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_12_so_left_pt3(b,i,j,k)
term(51) = term(51) + r2p(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,j,p,i) * wm_interm_12_so_left_pt3(a,i,q,k)
term(52) = term(52) + r2p(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_14_so_left_pt3(b,j,k,i)
term(53) = term(53) + r2p(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_14_so_left_pt3(b,k,j,i)
term(54) = term(54) + r2m(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,j,k,i)
term(55) = term(55) + r2m(vrdav_Rl, a,i,p,q) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,j,i)
term(56) = term(56) + r2m(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,j,k,i)
term(57) = term(57) + r2m(vrdav_Rl, a,q,p,i) * r2(vrdav_Rr, a,j,b,k) * wm_interm_0_so_left_pt3(b,k,j,i)
term(58) = term(58) + r2m(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_12_so_left_pt3(b,i,j,k)
term(59) = term(59) + r2m(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,j,p,i) * wm_interm_12_so_left_pt3(a,i,q,k)
term(60) = term(60) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_14_so_left_pt3(b,j,k,i)
term(61) = term(61) + r2m(vrdav_Rl, a,j,b,k) * t2(a,p,q,i) * wm_interm_14_so_left_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(48) = term(48) * (-1.0d+0) 
term(49) = term(49) * (2.0d+0) 
term(50) = term(50) * (-1.0d+0) 
term(51) = term(51) * (-1.0d+0) 
term(52) = term(52) * (-1.0d+0) 
term(53) = term(53) * (2.0d+0) 
term(54) = term(54) * (4.0d+0) 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * (8.0d+0) 
term(58) = term(58) * (-2.0d+0) 
term(59) = term(59) * (-2.0d+0) 
term(60) = term(60) * (-2.0d+0) 
term(61) = term(61) * (4.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
do b = nocc + 1, nactive 
term(62) = term(62) + r2p(vrdav_Rl, b,j,a,k) * t2(a,p,q,i) * wm_interm_14_so_left_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(62) = term(62) * (-1.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(63) = term(63) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_3_so_left_pt3(a,q,j,k)
term(64) = term(64) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_3_so_left_pt3(a,q,i,k)
term(65) = term(65) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_3_so_left_pt3(a,q,i,k)
term(66) = term(66) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_3_so_left_pt3(a,j,i,k)
term(67) = term(67) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_9_so_left_pt3(a,j,q,k)
term(68) = term(68) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_9_so_left_pt3(a,i,q,k)
term(69) = term(69) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_9_so_left_pt3(a,i,j,k)
term(70) = term(70) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_9_so_left_pt3(a,j,i,k)
term(71) = term(71) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_9_so_left_pt3(a,q,j,k)
term(72) = term(72) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_9_so_left_pt3(a,q,i,k)
term(73) = term(73) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_9_so_left_pt3(a,q,i,k)
term(74) = term(74) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_9_so_left_pt3(a,i,q,k)
term(75) = term(75) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_35_so_left_pt3(a,q,j,k)
term(76) = term(76) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_35_so_left_pt3(a,q,i,k)
term(77) = term(77) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_35_so_left_pt3(a,q,i,k)
term(78) = term(78) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_35_so_left_pt3(a,j,i,k)
term(79) = term(79) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_37_so_left_pt3(a,j,q,k)
term(80) = term(80) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_37_so_left_pt3(a,i,q,k)
term(81) = term(81) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_37_so_left_pt3(a,i,j,k)
term(82) = term(82) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,k) * wm_interm_37_so_left_pt3(a,j,i,k)
term(83) = term(83) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,i,k) * wm_interm_37_so_left_pt3(a,q,j,k)
term(84) = term(84) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,j,k) * wm_interm_37_so_left_pt3(a,q,i,k)
term(85) = term(85) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_37_so_left_pt3(a,q,i,k)
term(86) = term(86) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,j) * wm_interm_37_so_left_pt3(a,i,q,k)
end do 
end do 
end do 
end do 
end do 

term(63) = term(63) * (-1.0d+0) 
term(64) = term(64) * (2.0d+0) 
term(65) = term(65) * (-1.0d+0) 
term(66) = term(66) * (-1.0d+0) 
term(67) = term(67) * (-1.0d+0) 
term(68) = term(68) * (2.0d+0) 
term(69) = term(69) * (-1.0d+0) 
term(70) = term(70) * (2.0d+0) 
term(71) = term(71) * (2.0d+0) 
term(72) = term(72) * (-4.0d+0) 
term(73) = term(73) * (2.0d+0) 
term(74) = term(74) * (-1.0d+0) 
term(75) = term(75) * (-2.0d+0) 
term(76) = term(76) * (4.0d+0) 
term(77) = term(77) * (-2.0d+0) 
term(78) = term(78) * (-2.0d+0) 
term(79) = term(79) * (-2.0d+0) 
term(80) = term(80) * (4.0d+0) 
term(81) = term(81) * (-2.0d+0) 
term(82) = term(82) * (4.0d+0) 
term(83) = term(83) * (4.0d+0) 
term(84) = term(84) * (-8.0d+0) 
term(85) = term(85) * (4.0d+0) 
term(86) = term(86) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(87) = term(87) + wm_interm_17_so_left_pt3(a,q) * wm_interm_18_so_left_pt3(a,p)
term(88) = term(88) + wm_interm_17_so_left_pt3(a,q) * wm_interm_19_so_left_pt3(a,p)
term(89) = term(89) + wm_interm_18_so_left_pt3(a,p) * wm_interm_20_so_left_pt3(a,q)
term(90) = term(90) + wm_interm_19_so_left_pt3(a,p) * wm_interm_20_so_left_pt3(a,q)
term(91) = term(91) + wm_interm_18_so_left_pt3(a,p) * wm_interm_21_so_left_pt3(a,q)
term(92) = term(92) + wm_interm_19_so_left_pt3(a,p) * wm_interm_21_so_left_pt3(a,q)
term(93) = term(93) + wm_interm_10_so_left_pt3(a,q) * wm_interm_27_so_left_pt3(a,p)
term(94) = term(94) + wm_interm_10_so_left_pt3(a,q) * wm_interm_28_so_left_pt3(a,p)
term(95) = term(95) + wm_interm_10_so_left_pt3(a,q) * wm_interm_29_so_left_pt3(a,p)
term(96) = term(96) + wm_interm_11_so_left_pt3(a,q) * wm_interm_27_so_left_pt3(a,p)
term(97) = term(97) + wm_interm_11_so_left_pt3(a,q) * wm_interm_28_so_left_pt3(a,p)
term(98) = term(98) + wm_interm_11_so_left_pt3(a,q) * wm_interm_29_so_left_pt3(a,p)
term(99) = term(99) + wm_interm_18_so_left_pt3(a,p) * wm_interm_40_so_left_pt3(a,q)
term(100) = term(100) + wm_interm_19_so_left_pt3(a,p) * wm_interm_40_so_left_pt3(a,q)
term(101) = term(101) + wm_interm_18_so_left_pt3(a,p) * wm_interm_41_so_left_pt3(a,q)
term(102) = term(102) + wm_interm_19_so_left_pt3(a,p) * wm_interm_41_so_left_pt3(a,q)
term(103) = term(103) + wm_interm_10_so_left_pt3(a,q) * wm_interm_44_so_left_pt3(a,p)
term(104) = term(104) + wm_interm_10_so_left_pt3(a,q) * wm_interm_45_so_left_pt3(a,p)
term(105) = term(105) + wm_interm_11_so_left_pt3(a,q) * wm_interm_44_so_left_pt3(a,p)
term(106) = term(106) + wm_interm_11_so_left_pt3(a,q) * wm_interm_45_so_left_pt3(a,p)
term(107) = term(107) + wm_interm_53_so_left_pt3(p,a) * wm_interm_66_so_left_pt3(a,q)
term(108) = term(108) + wm_interm_10_so_left_pt3(a,q) * wm_interm_67_so_left_pt3(a,p)
term(109) = term(109) + wm_interm_11_so_left_pt3(a,q) * wm_interm_67_so_left_pt3(a,p)
end do 

term(87) = term(87) * (2.0d+0) 
term(88) = term(88) * (-4.0d+0) 
term(89) = term(89) * (-1.0d+0) 
term(90) = term(90) * (2.0d+0) 
term(91) = term(91) * (-1.0d+0) 
term(92) = term(92) * (2.0d+0) 
term(93) = term(93) * (2.0d+0) 
term(94) = term(94) * (-4.0d+0) 
term(95) = term(95) * (2.0d+0) 
term(96) = term(96) * (-1.0d+0) 
term(97) = term(97) * (2.0d+0) 
term(98) = term(98) * (-1.0d+0) 
term(99) = term(99) * (4.0d+0) 
term(100) = term(100) * (-8.0d+0) 
term(101) = term(101) * (-4.0d+0) 
term(102) = term(102) * (8.0d+0) 
term(103) = term(103) * (8.0d+0) 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * (-4.0d+0) 
term(106) = term(106) * (4.0d+0) 
term(107) = term(107) * (2.0d+0) 
term(108) = term(108) * (-4.0d+0) 
term(109) = term(109) * (2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(110) = term(110) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_3_so_left_pt3(a,q,j,k)
term(111) = term(111) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_3_so_left_pt3(a,j,q,k)
term(112) = term(112) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_9_so_left_pt3(a,q,j,k)
term(113) = term(113) + r2p(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_12_so_left_pt3(b,i,k,j)
term(114) = term(114) + r2p(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,k,p,i) * wm_interm_12_so_left_pt3(a,i,q,j)
term(115) = term(115) + r2p(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,k,p,i) * wm_interm_12_so_left_pt3(b,i,q,j)
term(116) = term(116) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_35_so_left_pt3(a,q,j,k)
term(117) = term(117) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_35_so_left_pt3(a,j,q,k)
term(118) = term(118) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,k,i) * wm_interm_37_so_left_pt3(a,q,j,k)
term(119) = term(119) + r2m(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_12_so_left_pt3(b,i,k,j)
term(120) = term(120) + r2m(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, b,k,p,i) * wm_interm_12_so_left_pt3(a,i,q,j)
term(121) = term(121) + r2m(vrdav_Rl, a,j,b,k) * r2(vrdav_Rr, a,k,p,i) * wm_interm_12_so_left_pt3(b,i,q,j)
end do 
end do 
end do 
end do 
end do 

term(110) = term(110) * (2.0d+0) 
term(111) = term(111) * (-1.0d+0) 
term(112) = term(112) * (-1.0d+0) 
term(113) = term(113) * (2.0d+0) 
term(114) = term(114) * (2.0d+0) 
term(115) = term(115) * (-1.0d+0) 
term(116) = term(116) * (4.0d+0) 
term(117) = term(117) * (-2.0d+0) 
term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (4.0d+0) 
term(120) = term(120) * (4.0d+0) 
term(121) = term(121) * (-2.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(122) = term(122) + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_10_so_left_pt3(b,j)
term(123) = term(123) + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_11_so_left_pt3(b,j)
term(124) = term(124) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_17_so_left_pt3(a,j)
term(125) = term(125) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_20_so_left_pt3(a,j)
term(126) = term(126) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_21_so_left_pt3(a,j)
term(127) = term(127) + r2p(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_22_so_left_pt3(b,j)
term(128) = term(128) + r2p(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_23_so_left_pt3(b,j)
term(129) = term(129) + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_10_so_left_pt3(b,j)
term(130) = term(130) + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_11_so_left_pt3(b,j)
term(131) = term(131) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_40_so_left_pt3(a,j)
term(132) = term(132) + r2(vrdav_Rr, a,i,b,j) * s2(b,p,q,i) * wm_interm_41_so_left_pt3(a,j)
term(133) = term(133) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_22_so_left_pt3(b,j)
term(134) = term(134) + r2m(vrdav_Rl, a,i,b,j) * t2(a,p,q,i) * wm_interm_23_so_left_pt3(b,j)
end do 
end do 
end do 
end do 

term(122) = term(122) * (-4.0d+0) 
term(123) = term(123) * (2.0d+0) 
term(124) = term(124) * (2.0d+0) 
term(125) = term(125) * (-1.0d+0) 
term(126) = term(126) * (-1.0d+0) 
term(127) = term(127) * (4.0d+0) 
term(128) = term(128) * (-2.0d+0) 
term(129) = term(129) * (-8.0d+0) 
term(130) = term(130) * (4.0d+0) 
term(131) = term(131) * (4.0d+0) 
term(132) = term(132) * (-4.0d+0) 
term(133) = term(133) * (8.0d+0) 
term(134) = term(134) * (-4.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(135) = term(135) + r2p(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_10_so_left_pt3(b,j)
term(136) = term(136) + r2p(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_11_so_left_pt3(b,j)
term(137) = term(137) + r2p(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_22_so_left_pt3(b,j)
term(138) = term(138) + r2p(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_23_so_left_pt3(b,j)
term(139) = term(139) + r2m(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_10_so_left_pt3(b,j)
term(140) = term(140) + r2m(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, a,q,p,i) * wm_interm_11_so_left_pt3(b,j)
term(141) = term(141) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_22_so_left_pt3(b,j)
term(142) = term(142) + r2m(vrdav_Rl, a,j,b,i) * t2(a,p,q,i) * wm_interm_23_so_left_pt3(b,j)
end do 
end do 
end do 
end do 

term(135) = term(135) * (2.0d+0) 
term(136) = term(136) * (-1.0d+0) 
term(137) = term(137) * (-2.0d+0) 
term(139) = term(139) * (8.0d+0) 
term(140) = term(140) * (-4.0d+0) 
term(141) = term(141) * (-8.0d+0) 
term(142) = term(142) * (4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(143) = term(143) + r2p(vrdav_Rl, b,i,a,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_10_so_left_pt3(b,j)
term(144) = term(144) + r2p(vrdav_Rl, b,i,a,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_11_so_left_pt3(b,j)
term(145) = term(145) + r2p(vrdav_Rl, b,i,a,j) * t2(a,p,q,i) * wm_interm_22_so_left_pt3(b,j)
term(146) = term(146) + r2p(vrdav_Rl, b,i,a,j) * t2(a,p,q,i) * wm_interm_23_so_left_pt3(b,j)
end do 
end do 
end do 
end do 

term(143) = term(143) * (2.0d+0) 
term(144) = term(144) * (-1.0d+0) 
term(145) = term(145) * (-2.0d+0) 

do i = 1, nocc 
term(147) = term(147) + wm_interm_13_so_left_pt3(i,q) * wm_interm_22_so_left_pt3(p,i)
term(148) = term(148) + wm_interm_16_so_left_pt3(i,q) * wm_interm_22_so_left_pt3(p,i)
term(149) = term(149) + wm_interm_15_so_left_pt3(i,q) * wm_interm_22_so_left_pt3(p,i)
term(150) = term(150) + wm_interm_13_so_left_pt3(i,q) * wm_interm_23_so_left_pt3(p,i)
term(151) = term(151) + wm_interm_16_so_left_pt3(i,q) * wm_interm_23_so_left_pt3(p,i)
term(152) = term(152) + wm_interm_15_so_left_pt3(i,q) * wm_interm_23_so_left_pt3(p,i)
term(153) = term(153) + wm_interm_22_so_left_pt3(p,i) * wm_interm_38_so_left_pt3(i,q)
term(154) = term(154) + wm_interm_22_so_left_pt3(p,i) * wm_interm_39_so_left_pt3(i,q)
term(155) = term(155) + wm_interm_23_so_left_pt3(p,i) * wm_interm_38_so_left_pt3(i,q)
term(156) = term(156) + wm_interm_23_so_left_pt3(p,i) * wm_interm_39_so_left_pt3(i,q)
term(157) = term(157) + wm_interm_49_so_left_pt3(q,i) * wm_interm_63_so_left_pt3(p,i)
term(158) = term(158) + wm_interm_49_so_left_pt3(q,i) * wm_interm_62_so_left_pt3(p,i)
end do 

term(147) = term(147) * (-2.0d+0) 
term(148) = term(148) * (-2.0d+0) 
term(149) = term(149) * (4.0d+0) 
term(152) = term(152) * (-2.0d+0) 
term(153) = term(153) * (-8.0d+0) 
term(154) = term(154) * (8.0d+0) 
term(155) = term(155) * (4.0d+0) 
term(156) = term(156) * (-4.0d+0) 
term(157) = term(157) * (-4.0d+0) 
term(158) = term(158) * (2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(159) = term(159) + wm_interm_2_so_left_pt3(i,j) * wm_interm_32_so_left_pt3(p,j,q,i)
end do 
end do 

term(159) = term(159) * (-1.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(160) = term(160) + r2p(vrdav_Rl, b,j,a,k) * r2(vrdav_Rr, a,q,p,i) * wm_interm_12_so_left_pt3(b,i,k,j)
end do 
end do 
end do 
end do 
end do 

term(160) = term(160) * (-1.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(161) = term(161) + r2p(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, b,k,p,i) * wm_interm_12_so_left_pt3(a,k,q,j)
term(162) = term(162) + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_12_so_left_pt3(a,k,q,j)
term(163) = term(163) + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_12_so_left_pt3(b,k,q,j)
term(164) = term(164) + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_12_so_left_pt3(a,k,j,q)
term(165) = term(165) + r2p(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_12_so_left_pt3(b,k,j,q)
term(166) = term(166) + r2m(vrdav_Rl, a,k,b,j) * r2(vrdav_Rr, a,q,p,i) * wm_interm_12_so_left_pt3(b,i,k,j)
term(167) = term(167) + r2m(vrdav_Rl, a,j,b,i) * r2(vrdav_Rr, b,k,p,i) * wm_interm_12_so_left_pt3(a,k,q,j)
term(168) = term(168) + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_12_so_left_pt3(a,k,q,j)
term(169) = term(169) + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_12_so_left_pt3(b,k,q,j)
term(170) = term(170) + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, b,k,p,i) * wm_interm_12_so_left_pt3(a,k,j,q)
term(171) = term(171) + r2m(vrdav_Rl, a,i,b,j) * r2(vrdav_Rr, a,k,p,i) * wm_interm_12_so_left_pt3(b,k,j,q)
term(172) = term(172) + r2m(vrdav_Rl, a,k,b,j) * t2(a,p,q,i) * wm_interm_14_so_left_pt3(b,k,j,i)
end do 
end do 
end do 
end do 
end do 

term(161) = term(161) * (-1.0d+0) 
term(162) = term(162) * (2.0d+0) 
term(163) = term(163) * (-1.0d+0) 
term(164) = term(164) * (-1.0d+0) 
term(165) = term(165) * (2.0d+0) 
term(166) = term(166) * (-2.0d+0) 
term(167) = term(167) * (-2.0d+0) 
term(168) = term(168) * (4.0d+0) 
term(169) = term(169) * (-2.0d+0) 
term(170) = term(170) * (-2.0d+0) 
term(171) = term(171) * (4.0d+0) 
term(172) = term(172) * (-2.0d+0) 


    calc_D_vo_wm_so_left_pt3 = zero
    do s = 0, 172
    calc_D_vo_wm_so_left_pt3 = calc_D_vo_wm_so_left_pt3 + term(s)
    end do

    end function calc_D_vo_wm_so_left_pt3
    
    function calc_D_vv_wm_so_left_pt3(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_so_left_pt3
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
    real(F64), dimension(0:67) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_0_so_left_pt3(q,i,j,k) * wm_interm_47_so_left_pt3(p,k,i,j)
end do 
end do 
end do 

term(0) = term(0) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
term(1) = term(1) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_52_so_left_pt3(p,j,k,i)
term(2) = term(2) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_52_so_left_pt3(p,k,j,i)
term(3) = term(3) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_51_so_left_pt3(p,k,j,i)
term(4) = term(4) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_59_so_left_pt3(p,j,k,i)
term(5) = term(5) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_59_so_left_pt3(p,k,j,i)
term(6) = term(6) + wm_interm_32_so_left_pt3(q,i,j,k) * wm_interm_58_so_left_pt3(p,k,j,i)
end do 
end do 
end do 

term(2) = term(2) * (-2.0d+0) 
term(4) = term(4) * (2.0d+0) 
term(5) = term(5) * (-4.0d+0) 
term(6) = term(6) * (2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(7) = term(7) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,i,k) * wm_interm_49_so_left_pt3(j,k)
term(8) = term(8) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,j,k) * wm_interm_49_so_left_pt3(i,k)
term(9) = term(9) + r2p(vrdav_Rl, a,j,p,k) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,i)
term(10) = term(10) + r2m(vrdav_Rl, a,j,p,k) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,i)
end do 
end do 
end do 
end do 

term(7) = term(7) * (2.0d+0) 
term(8) = term(8) * (-4.0d+0) 
term(10) = term(10) * (2.0d+0) 

do a = nocc + 1, nactive 
term(11) = term(11) + wm_interm_18_so_left_pt3(p,a) * wm_interm_48_so_left_pt3(q,a)
term(12) = term(12) + wm_interm_19_so_left_pt3(p,a) * wm_interm_48_so_left_pt3(q,a)
term(13) = term(13) + wm_interm_18_so_left_pt3(a,p) * wm_interm_48_so_left_pt3(a,q)
term(14) = term(14) + wm_interm_19_so_left_pt3(a,p) * wm_interm_48_so_left_pt3(a,q)
term(15) = term(15) + wm_interm_25_so_left_pt3(q,a) * wm_interm_53_so_left_pt3(p,a)
term(16) = term(16) + wm_interm_26_so_left_pt3(q,a) * wm_interm_53_so_left_pt3(p,a)
term(17) = term(17) + wm_interm_24_so_left_pt3(q,a) * wm_interm_53_so_left_pt3(p,a)
term(18) = term(18) + wm_interm_24_so_left_pt3(a,q) * wm_interm_53_so_left_pt3(a,p)
term(19) = term(19) + wm_interm_25_so_left_pt3(a,q) * wm_interm_53_so_left_pt3(a,p)
term(20) = term(20) + wm_interm_26_so_left_pt3(a,q) * wm_interm_53_so_left_pt3(a,p)
term(21) = term(21) + wm_interm_42_so_left_pt3(q,a) * wm_interm_53_so_left_pt3(p,a)
term(22) = term(22) + wm_interm_43_so_left_pt3(q,a) * wm_interm_53_so_left_pt3(p,a)
term(23) = term(23) + wm_interm_42_so_left_pt3(a,q) * wm_interm_53_so_left_pt3(a,p)
term(24) = term(24) + wm_interm_43_so_left_pt3(a,q) * wm_interm_53_so_left_pt3(a,p)
end do 

term(11) = term(11) * (2.0d+0) 
term(12) = term(12) * (-4.0d+0) 
term(13) = term(13) * (2.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(16) = term(16) * (-2.0d+0) 
term(20) = term(20) * (-2.0d+0) 
term(21) = term(21) * (4.0d+0) 
term(22) = term(22) * (-4.0d+0) 
term(23) = term(23) * (4.0d+0) 
term(24) = term(24) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(25) = term(25) + r2(vrdav_Rr, a,j,p,i) * s2(a,q,k,j) * wm_interm_49_so_left_pt3(i,k)
term(26) = term(26) + r2p(vrdav_Rl, a,i,p,j) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,k)
term(27) = term(27) + r2m(vrdav_Rl, a,i,p,j) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,k)
term(28) = term(28) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,j,i) * wm_interm_54_so_left_pt3(k,i)
term(29) = term(29) + r2m(vrdav_Rl, a,k,p,j) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,i)
end do 
end do 
end do 
end do 

term(25) = term(25) * (2.0d+0) 
term(27) = term(27) * (4.0d+0) 
term(28) = term(28) * (2.0d+0) 
term(29) = term(29) * (-4.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
do j = 1, nocc 
term(30) = term(30) + r2p(vrdav_Rl, p,j,a,i) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,k)
end do 
end do 
end do 
end do 


do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do k = 1, nocc 
term(31) = term(31) + r2p(vrdav_Rl, p,i,a,j) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,k)
end do 
end do 
end do 
end do 

term(31) = term(31) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(32) = term(32) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,j,i) * wm_interm_54_so_left_pt3(k,i)
term(33) = term(33) + r2p(vrdav_Rl, p,j,a,k) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,i)
end do 
end do 
end do 
end do 

term(33) = term(33) * (-2.0d+0) 

do i = 1, nocc 
term(34) = term(34) + wm_interm_11_so_left_pt3(q,i) * wm_interm_55_so_left_pt3(p,i)
term(35) = term(35) + wm_interm_11_so_left_pt3(q,i) * wm_interm_56_so_left_pt3(p,i)
term(36) = term(36) + wm_interm_11_so_left_pt3(q,i) * wm_interm_57_so_left_pt3(p,i)
term(37) = term(37) + wm_interm_10_so_left_pt3(q,i) * wm_interm_57_so_left_pt3(p,i)
term(38) = term(38) + wm_interm_10_so_left_pt3(q,i) * wm_interm_55_so_left_pt3(p,i)
term(39) = term(39) + wm_interm_10_so_left_pt3(q,i) * wm_interm_56_so_left_pt3(p,i)
term(40) = term(40) + wm_interm_11_so_left_pt3(q,i) * wm_interm_60_so_left_pt3(p,i)
term(41) = term(41) + wm_interm_11_so_left_pt3(q,i) * wm_interm_61_so_left_pt3(p,i)
term(42) = term(42) + wm_interm_10_so_left_pt3(q,i) * wm_interm_61_so_left_pt3(p,i)
term(43) = term(43) + wm_interm_10_so_left_pt3(q,i) * wm_interm_60_so_left_pt3(p,i)
end do 

term(34) = term(34) * (-2.0d+0) 
term(37) = term(37) * (-2.0d+0) 
term(38) = term(38) * (4.0d+0) 
term(39) = term(39) * (-2.0d+0) 
term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * (4.0d+0) 
term(42) = term(42) * (-8.0d+0) 
term(43) = term(43) * (8.0d+0) 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(44) = term(44) + r2m(vrdav_Rl, a,j,p,i) * t2(a,q,k,i) * wm_interm_54_so_left_pt3(j,k)
end do 
end do 
end do 
end do 

term(44) = term(44) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(45) = term(45) + s2(a,p,i,j) * t1(q,i) * wm_interm_50_so_left_pt3(a,j)
term(46) = term(46) + r2p(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_10_so_left_pt3(a,j)
term(47) = term(47) + r2p(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_11_so_left_pt3(a,j)
term(48) = term(48) + s1(p,i) * t2(a,q,i,j) * wm_interm_57_so_left_pt3(a,j)
term(49) = term(49) + s1(p,i) * t2(a,q,i,j) * wm_interm_55_so_left_pt3(a,j)
term(50) = term(50) + s1(p,i) * t2(a,q,i,j) * wm_interm_56_so_left_pt3(a,j)
term(51) = term(51) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_10_so_left_pt3(a,j)
term(52) = term(52) + r2m(vrdav_Rl, a,i,q,j) * r1(vrdav_Rr, p,i) * wm_interm_11_so_left_pt3(a,j)
term(53) = term(53) + s1(p,i) * t2(a,q,i,j) * wm_interm_61_so_left_pt3(a,j)
term(54) = term(54) + s1(p,i) * t2(a,q,i,j) * wm_interm_60_so_left_pt3(a,j)
end do 
end do 
end do 

term(45) = term(45) * (2.0d+0) 
term(46) = term(46) * (-2.0d+0) 
term(49) = term(49) * (-2.0d+0) 
term(51) = term(51) * (-8.0d+0) 
term(52) = term(52) * (4.0d+0) 
term(53) = term(53) * (4.0d+0) 
term(54) = term(54) * (-4.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(55) = term(55) + r2p(vrdav_Rl, q,i,a,j) * r1(vrdav_Rr, p,i) * wm_interm_10_so_left_pt3(a,j)
term(56) = term(56) + r2p(vrdav_Rl, q,i,a,j) * r1(vrdav_Rr, p,i) * wm_interm_11_so_left_pt3(a,j)
end do 
end do 
end do 

term(55) = term(55) * (4.0d+0) 
term(56) = term(56) * (-2.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(57) = term(57) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * wm_interm_10_so_left_pt3(a,j)
term(58) = term(58) + r2p(vrdav_Rl, q,j,a,i) * r1(vrdav_Rr, p,i) * wm_interm_11_so_left_pt3(a,j)
end do 
end do 
end do 

term(57) = term(57) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(59) = term(59) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_10_so_left_pt3(a,j)
term(60) = term(60) + r2m(vrdav_Rl, a,j,q,i) * r1(vrdav_Rr, p,i) * wm_interm_11_so_left_pt3(a,j)
end do 
end do 
end do 

term(59) = term(59) * (8.0d+0) 
term(60) = term(60) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(61) = term(61) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_4_so_left_pt3(i,j)
term(62) = term(62) + r1(vrdav_Rl, p,i) * t1(q,j) * wm_interm_5_so_left_pt3(i,j)
term(63) = term(63) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_13_so_left_pt3(i,j)
term(64) = term(64) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_15_so_left_pt3(i,j)
term(65) = term(65) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_16_so_left_pt3(i,j)
term(66) = term(66) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_38_so_left_pt3(i,j)
term(67) = term(67) + r1(vrdav_Rr, p,i) * s1(q,j) * wm_interm_39_so_left_pt3(i,j)
end do 
end do 

term(61) = term(61) * (2.0d+0) 
term(62) = term(62) * (-4.0d+0) 
term(64) = term(64) * (-2.0d+0) 
term(66) = term(66) * (4.0d+0) 
term(67) = term(67) * (-4.0d+0) 


    calc_D_vv_wm_so_left_pt3 = zero
    do s = 0, 67
    calc_D_vv_wm_so_left_pt3 = calc_D_vv_wm_so_left_pt3 + term(s)
    end do

    end function calc_D_vv_wm_so_left_pt3
    

end module density_exc_exc_functions_so_left_pt0123
