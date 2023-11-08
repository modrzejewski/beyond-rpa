module density_exc_exc_functions_triplet_pt012
 use ccsd_transformed_integrals
    use cc3_intermediates
    use arithmetic
    use s_gen
    use basis
    use eom_vectors

    implicit none
    !
    ! File generated automatically on 2016-09-15 15:31:48
    !
    real(F64) :: wm_interm_0_triplet_pt0 
real(F64) :: wm_interm_1_triplet_pt0 
real(F64) :: wm_interm_2_triplet_pt0 
real(F64) :: wm_interm_3_triplet_pt0 
real(F64) :: wm_interm_4_triplet_pt0 
real(F64) :: wm_interm_5_triplet_pt0 
real(F64) :: wm_interm_6_triplet_pt0 

   real(F64), dimension(:, :), allocatable :: wm_interm_0_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_3_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_4_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_6_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_7_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_8_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_9_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_10_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_11_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_13_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_14_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_16_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_20_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_21_triplet_pt1 
real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet_pt1 

    real(F64), dimension(:, :, :, :), allocatable :: wm_interm_0_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_1_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_2_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_3_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_4_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_5_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_6_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_7_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_8_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_9_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_10_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_11_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_12_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_13_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_14_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_15_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_16_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_17_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_18_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_19_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_20_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_21_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_22_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_23_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_24_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_25_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_26_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_27_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_28_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_29_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_30_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_31_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_32_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_33_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_34_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_35_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_36_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_37_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_38_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_39_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_40_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_41_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_42_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_43_triplet_pt2 
real(F64) :: wm_interm_44_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_45_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_46_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_47_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_48_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_49_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_50_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_51_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_52_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_53_triplet_pt2 
real(F64) :: wm_interm_54_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_55_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_56_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_57_triplet_pt2 
real(F64) :: wm_interm_58_triplet_pt2 
real(F64) :: wm_interm_59_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_60_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_61_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_62_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_63_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_64_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_65_triplet_pt2 
real(F64) :: wm_interm_66_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_67_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_68_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_69_triplet_pt2 
real(F64), dimension(:, :), allocatable :: wm_interm_70_triplet_pt2 
real(F64) :: wm_interm_71_triplet_pt2 
real(F64) :: wm_interm_72_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_73_triplet_pt2 
real(F64), dimension(:, :, :, :), allocatable :: wm_interm_74_triplet_pt2 

    contains
    
    subroutine wm_triplet_intermediates_ccsd_init_pt0(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    wm_interm_0_triplet_pt0 = zero 
wm_interm_1_triplet_pt0 = zero 
wm_interm_2_triplet_pt0 = zero 
wm_interm_3_triplet_pt0 = zero 
wm_interm_4_triplet_pt0 = zero 
wm_interm_5_triplet_pt0 = zero 
wm_interm_6_triplet_pt0 = zero 

    end subroutine wm_triplet_intermediates_ccsd_init_pt0
    
    subroutine wm_triplet_intermediates_ccsd_free_pt0
    
    end subroutine wm_triplet_intermediates_ccsd_free_pt0
    
    subroutine wm_triplet_intermediates_ccsd_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, b, i, j 

    sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_0_triplet_pt0 = wm_interm_0_triplet_pt0 + sum 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_1_triplet_pt0 = wm_interm_1_triplet_pt0 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_2_triplet_pt0 = wm_interm_2_triplet_pt0 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_3_triplet_pt0 = wm_interm_3_triplet_pt0 + sum 
sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_4_triplet_pt0 = wm_interm_4_triplet_pt0 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_5_triplet_pt0 = wm_interm_5_triplet_pt0 + sum 
sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_6_triplet_pt0 = wm_interm_6_triplet_pt0 + sum 


    end subroutine wm_triplet_intermediates_ccsd_pt0

    subroutine wm_triplet_intermediates_ccsd_init_pt1(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_1_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_2_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_3_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_4_triplet_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_5_triplet_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_7_triplet_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_8_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_9_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_10_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_11_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_12_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_13_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_15_triplet_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_16_triplet_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_17_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_18_triplet_pt1(nocc+1: nactive, 1: nocc))
allocate(wm_interm_19_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_20_triplet_pt1(1: nocc, 1: nocc))
allocate(wm_interm_21_triplet_pt1(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_22_triplet_pt1(nocc+1: nactive, nocc+1: nactive))
wm_interm_0_triplet_pt1 = zero 
wm_interm_1_triplet_pt1 = zero 
wm_interm_2_triplet_pt1 = zero 
wm_interm_3_triplet_pt1 = zero 
wm_interm_4_triplet_pt1 = zero 
wm_interm_5_triplet_pt1 = zero 
wm_interm_6_triplet_pt1 = zero 
wm_interm_7_triplet_pt1 = zero 
wm_interm_8_triplet_pt1 = zero 
wm_interm_9_triplet_pt1 = zero 
wm_interm_10_triplet_pt1 = zero 
wm_interm_11_triplet_pt1 = zero 
wm_interm_12_triplet_pt1 = zero 
wm_interm_13_triplet_pt1 = zero 
wm_interm_14_triplet_pt1 = zero 
wm_interm_15_triplet_pt1 = zero 
wm_interm_16_triplet_pt1 = zero 
wm_interm_17_triplet_pt1 = zero 
wm_interm_18_triplet_pt1 = zero 
wm_interm_19_triplet_pt1 = zero 
wm_interm_20_triplet_pt1 = zero 
wm_interm_21_triplet_pt1 = zero 
wm_interm_22_triplet_pt1 = zero 

    end subroutine wm_triplet_intermediates_ccsd_init_pt1
    
    subroutine wm_triplet_intermediates_ccsd_free_pt1
    deallocate(wm_interm_0_triplet_pt1)
deallocate(wm_interm_1_triplet_pt1)
deallocate(wm_interm_2_triplet_pt1)
deallocate(wm_interm_3_triplet_pt1)
deallocate(wm_interm_4_triplet_pt1)
deallocate(wm_interm_5_triplet_pt1)
deallocate(wm_interm_6_triplet_pt1)
deallocate(wm_interm_7_triplet_pt1)
deallocate(wm_interm_8_triplet_pt1)
deallocate(wm_interm_9_triplet_pt1)
deallocate(wm_interm_10_triplet_pt1)
deallocate(wm_interm_11_triplet_pt1)
deallocate(wm_interm_12_triplet_pt1)
deallocate(wm_interm_13_triplet_pt1)
deallocate(wm_interm_14_triplet_pt1)
deallocate(wm_interm_15_triplet_pt1)
deallocate(wm_interm_16_triplet_pt1)
deallocate(wm_interm_17_triplet_pt1)
deallocate(wm_interm_18_triplet_pt1)
deallocate(wm_interm_19_triplet_pt1)
deallocate(wm_interm_20_triplet_pt1)
deallocate(wm_interm_21_triplet_pt1)
deallocate(wm_interm_22_triplet_pt1)

    end subroutine wm_triplet_intermediates_ccsd_free_pt1
    
    subroutine wm_triplet_intermediates_ccsd_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    real(F64) :: sum
    integer :: a, i, j, b, c, k 

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
wm_interm_0_triplet_pt1(b, c) = wm_interm_0_triplet_pt1(b, c) + sum 
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
wm_interm_1_triplet_pt1(b, j) = wm_interm_1_triplet_pt1(b, j) + sum 
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
wm_interm_2_triplet_pt1(j, k) = wm_interm_2_triplet_pt1(j, k) + sum 
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
wm_interm_3_triplet_pt1(j, k) = wm_interm_3_triplet_pt1(j, k) + sum 
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
wm_interm_4_triplet_pt1(b, c) = wm_interm_4_triplet_pt1(b, c) + sum 
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
wm_interm_5_triplet_pt1(b, c) = wm_interm_5_triplet_pt1(b, c) + sum 
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
wm_interm_6_triplet_pt1(b, j) = wm_interm_6_triplet_pt1(b, j) + sum 
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
wm_interm_7_triplet_pt1(b, c) = wm_interm_7_triplet_pt1(b, c) + sum 
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
wm_interm_8_triplet_pt1(b, j) = wm_interm_8_triplet_pt1(b, j) + sum 
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
wm_interm_9_triplet_pt1(j, k) = wm_interm_9_triplet_pt1(j, k) + sum 
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
wm_interm_10_triplet_pt1(j, k) = wm_interm_10_triplet_pt1(j, k) + sum 
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
wm_interm_11_triplet_pt1(j, k) = wm_interm_11_triplet_pt1(j, k) + sum 
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
wm_interm_12_triplet_pt1(b, j) = wm_interm_12_triplet_pt1(b, j) + sum 
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
wm_interm_13_triplet_pt1(j, k) = wm_interm_13_triplet_pt1(j, k) + sum 
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
wm_interm_14_triplet_pt1(j, k) = wm_interm_14_triplet_pt1(j, k) + sum 
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
wm_interm_15_triplet_pt1(b, c) = wm_interm_15_triplet_pt1(b, c) + sum 
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
wm_interm_16_triplet_pt1(b, c) = wm_interm_16_triplet_pt1(b, c) + sum 
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
wm_interm_17_triplet_pt1(b, j) = wm_interm_17_triplet_pt1(b, j) + sum 
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
wm_interm_18_triplet_pt1(b, j) = wm_interm_18_triplet_pt1(b, j) + sum 
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
wm_interm_19_triplet_pt1(j, k) = wm_interm_19_triplet_pt1(j, k) + sum 
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
wm_interm_20_triplet_pt1(j, k) = wm_interm_20_triplet_pt1(j, k) + sum 
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
wm_interm_21_triplet_pt1(b, c) = wm_interm_21_triplet_pt1(b, c) + sum 
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
wm_interm_22_triplet_pt1(b, c) = wm_interm_22_triplet_pt1(b, c) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_ccsd_pt1

    subroutine wm_triplet_intermediates_ccsd_init_pt2(nocc, nactive)
    integer, intent(in) :: nocc
    integer, intent(in) :: nactive
    allocate(wm_interm_0_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_1_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_2_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_3_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_4_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_5_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_6_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_7_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_8_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_9_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_10_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_11_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_12_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_13_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_14_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_15_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_16_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_17_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_18_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_19_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_20_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_21_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_22_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_23_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_24_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_25_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_26_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_27_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_28_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_29_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_30_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_31_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_32_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_33_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_34_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_35_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_36_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_37_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_38_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_39_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_40_triplet_pt2(1: nocc, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_41_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_42_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_43_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_45_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_46_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_47_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_48_triplet_pt2(nocc+1: nactive, nocc+1: nactive))
allocate(wm_interm_49_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_50_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_51_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_52_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_53_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_55_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_56_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_57_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_60_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_61_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_62_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_63_triplet_pt2(nocc+1: nactive, 1: nocc))
allocate(wm_interm_64_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_65_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_67_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_68_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_69_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_70_triplet_pt2(1: nocc, 1: nocc))
allocate(wm_interm_73_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
allocate(wm_interm_74_triplet_pt2(nocc+1: nactive, 1: nocc, 1: nocc, 1: nocc))
wm_interm_0_triplet_pt2 = zero 
wm_interm_1_triplet_pt2 = zero 
wm_interm_2_triplet_pt2 = zero 
wm_interm_3_triplet_pt2 = zero 
wm_interm_4_triplet_pt2 = zero 
wm_interm_5_triplet_pt2 = zero 
wm_interm_6_triplet_pt2 = zero 
wm_interm_7_triplet_pt2 = zero 
wm_interm_8_triplet_pt2 = zero 
wm_interm_9_triplet_pt2 = zero 
wm_interm_10_triplet_pt2 = zero 
wm_interm_11_triplet_pt2 = zero 
wm_interm_12_triplet_pt2 = zero 
wm_interm_13_triplet_pt2 = zero 
wm_interm_14_triplet_pt2 = zero 
wm_interm_15_triplet_pt2 = zero 
wm_interm_16_triplet_pt2 = zero 
wm_interm_17_triplet_pt2 = zero 
wm_interm_18_triplet_pt2 = zero 
wm_interm_19_triplet_pt2 = zero 
wm_interm_20_triplet_pt2 = zero 
wm_interm_21_triplet_pt2 = zero 
wm_interm_22_triplet_pt2 = zero 
wm_interm_23_triplet_pt2 = zero 
wm_interm_24_triplet_pt2 = zero 
wm_interm_25_triplet_pt2 = zero 
wm_interm_26_triplet_pt2 = zero 
wm_interm_27_triplet_pt2 = zero 
wm_interm_28_triplet_pt2 = zero 
wm_interm_29_triplet_pt2 = zero 
wm_interm_30_triplet_pt2 = zero 
wm_interm_31_triplet_pt2 = zero 
wm_interm_32_triplet_pt2 = zero 
wm_interm_33_triplet_pt2 = zero 
wm_interm_34_triplet_pt2 = zero 
wm_interm_35_triplet_pt2 = zero 
wm_interm_36_triplet_pt2 = zero 
wm_interm_37_triplet_pt2 = zero 
wm_interm_38_triplet_pt2 = zero 
wm_interm_39_triplet_pt2 = zero 
wm_interm_40_triplet_pt2 = zero 
wm_interm_41_triplet_pt2 = zero 
wm_interm_42_triplet_pt2 = zero 
wm_interm_43_triplet_pt2 = zero 
wm_interm_44_triplet_pt2 = zero 
wm_interm_45_triplet_pt2 = zero 
wm_interm_46_triplet_pt2 = zero 
wm_interm_47_triplet_pt2 = zero 
wm_interm_48_triplet_pt2 = zero 
wm_interm_49_triplet_pt2 = zero 
wm_interm_50_triplet_pt2 = zero 
wm_interm_51_triplet_pt2 = zero 
wm_interm_52_triplet_pt2 = zero 
wm_interm_53_triplet_pt2 = zero 
wm_interm_54_triplet_pt2 = zero 
wm_interm_55_triplet_pt2 = zero 
wm_interm_56_triplet_pt2 = zero 
wm_interm_57_triplet_pt2 = zero 
wm_interm_58_triplet_pt2 = zero 
wm_interm_59_triplet_pt2 = zero 
wm_interm_60_triplet_pt2 = zero 
wm_interm_61_triplet_pt2 = zero 
wm_interm_62_triplet_pt2 = zero 
wm_interm_63_triplet_pt2 = zero 
wm_interm_64_triplet_pt2 = zero 
wm_interm_65_triplet_pt2 = zero 
wm_interm_66_triplet_pt2 = zero 
wm_interm_67_triplet_pt2 = zero 
wm_interm_68_triplet_pt2 = zero 
wm_interm_69_triplet_pt2 = zero 
wm_interm_70_triplet_pt2 = zero 
wm_interm_71_triplet_pt2 = zero 
wm_interm_72_triplet_pt2 = zero 
wm_interm_73_triplet_pt2 = zero 
wm_interm_74_triplet_pt2 = zero 

    end subroutine wm_triplet_intermediates_ccsd_init_pt2
    
    subroutine wm_triplet_intermediates_ccsd_free_pt2
    deallocate(wm_interm_0_triplet_pt2)
deallocate(wm_interm_1_triplet_pt2)
deallocate(wm_interm_2_triplet_pt2)
deallocate(wm_interm_3_triplet_pt2)
deallocate(wm_interm_4_triplet_pt2)
deallocate(wm_interm_5_triplet_pt2)
deallocate(wm_interm_6_triplet_pt2)
deallocate(wm_interm_7_triplet_pt2)
deallocate(wm_interm_8_triplet_pt2)
deallocate(wm_interm_9_triplet_pt2)
deallocate(wm_interm_10_triplet_pt2)
deallocate(wm_interm_11_triplet_pt2)
deallocate(wm_interm_12_triplet_pt2)
deallocate(wm_interm_13_triplet_pt2)
deallocate(wm_interm_14_triplet_pt2)
deallocate(wm_interm_15_triplet_pt2)
deallocate(wm_interm_16_triplet_pt2)
deallocate(wm_interm_17_triplet_pt2)
deallocate(wm_interm_18_triplet_pt2)
deallocate(wm_interm_19_triplet_pt2)
deallocate(wm_interm_20_triplet_pt2)
deallocate(wm_interm_21_triplet_pt2)
deallocate(wm_interm_22_triplet_pt2)
deallocate(wm_interm_23_triplet_pt2)
deallocate(wm_interm_24_triplet_pt2)
deallocate(wm_interm_25_triplet_pt2)
deallocate(wm_interm_26_triplet_pt2)
deallocate(wm_interm_27_triplet_pt2)
deallocate(wm_interm_28_triplet_pt2)
deallocate(wm_interm_29_triplet_pt2)
deallocate(wm_interm_30_triplet_pt2)
deallocate(wm_interm_31_triplet_pt2)
deallocate(wm_interm_32_triplet_pt2)
deallocate(wm_interm_33_triplet_pt2)
deallocate(wm_interm_34_triplet_pt2)
deallocate(wm_interm_35_triplet_pt2)
deallocate(wm_interm_36_triplet_pt2)
deallocate(wm_interm_37_triplet_pt2)
deallocate(wm_interm_38_triplet_pt2)
deallocate(wm_interm_39_triplet_pt2)
deallocate(wm_interm_40_triplet_pt2)
deallocate(wm_interm_41_triplet_pt2)
deallocate(wm_interm_42_triplet_pt2)
deallocate(wm_interm_43_triplet_pt2)
deallocate(wm_interm_45_triplet_pt2)
deallocate(wm_interm_46_triplet_pt2)
deallocate(wm_interm_47_triplet_pt2)
deallocate(wm_interm_48_triplet_pt2)
deallocate(wm_interm_49_triplet_pt2)
deallocate(wm_interm_50_triplet_pt2)
deallocate(wm_interm_51_triplet_pt2)
deallocate(wm_interm_52_triplet_pt2)
deallocate(wm_interm_53_triplet_pt2)
deallocate(wm_interm_55_triplet_pt2)
deallocate(wm_interm_56_triplet_pt2)
deallocate(wm_interm_57_triplet_pt2)
deallocate(wm_interm_60_triplet_pt2)
deallocate(wm_interm_61_triplet_pt2)
deallocate(wm_interm_62_triplet_pt2)
deallocate(wm_interm_63_triplet_pt2)
deallocate(wm_interm_64_triplet_pt2)
deallocate(wm_interm_65_triplet_pt2)
deallocate(wm_interm_67_triplet_pt2)
deallocate(wm_interm_68_triplet_pt2)
deallocate(wm_interm_69_triplet_pt2)
deallocate(wm_interm_70_triplet_pt2)
deallocate(wm_interm_73_triplet_pt2)
deallocate(wm_interm_74_triplet_pt2)

    end subroutine wm_triplet_intermediates_ccsd_free_pt2
    
    subroutine wm_triplet_intermediates_ccsd_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr)
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
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
wm_interm_0_triplet_pt2(b, i, j, k) = wm_interm_0_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_1_triplet_pt2(j, k) = wm_interm_1_triplet_pt2(j, k) + sum 
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
wm_interm_2_triplet_pt2(j, k) = wm_interm_2_triplet_pt2(j, k) + sum 
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
wm_interm_3_triplet_pt2(b, j) = wm_interm_3_triplet_pt2(b, j) + sum 
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
wm_interm_4_triplet_pt2(b, c) = wm_interm_4_triplet_pt2(b, c) + sum 
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
wm_interm_5_triplet_pt2(b, c) = wm_interm_5_triplet_pt2(b, c) + sum 
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
wm_interm_6_triplet_pt2(b, i, j, k) = wm_interm_6_triplet_pt2(b, i, j, k) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, b,j,a,i)
end do 
end do 
wm_interm_7_triplet_pt2(b, j) = wm_interm_7_triplet_pt2(b, j) + sum 
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
wm_interm_8_triplet_pt2(i, j, k, l) = wm_interm_8_triplet_pt2(i, j, k, l) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * t2(a,b,j,k)
end do 
wm_interm_9_triplet_pt2(b, i, j, k) = wm_interm_9_triplet_pt2(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rr, b,i,a,j) * s2(a,c,j,i)
end do 
end do 
end do 
wm_interm_10_triplet_pt2(b, c) = wm_interm_10_triplet_pt2(b, c) + sum 
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
wm_interm_11_triplet_pt2(b, c) = wm_interm_11_triplet_pt2(b, c) + sum 
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
wm_interm_12_triplet_pt2(j, k) = wm_interm_12_triplet_pt2(j, k) + sum 
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
wm_interm_13_triplet_pt2(j, k) = wm_interm_13_triplet_pt2(j, k) + sum 
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
wm_interm_14_triplet_pt2(i, j, k, l) = wm_interm_14_triplet_pt2(i, j, k, l) + sum 
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
sum = sum + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
wm_interm_15_triplet_pt2(b, j) = wm_interm_15_triplet_pt2(b, j) + sum 
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
wm_interm_16_triplet_pt2(b, j) = wm_interm_16_triplet_pt2(b, j) + sum 
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
wm_interm_17_triplet_pt2(b, c) = wm_interm_17_triplet_pt2(b, c) + sum 
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
wm_interm_18_triplet_pt2(b, c) = wm_interm_18_triplet_pt2(b, c) + sum 
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
wm_interm_19_triplet_pt2(b, c) = wm_interm_19_triplet_pt2(b, c) + sum 
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
wm_interm_20_triplet_pt2(j, k) = wm_interm_20_triplet_pt2(j, k) + sum 
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
wm_interm_21_triplet_pt2(j, k) = wm_interm_21_triplet_pt2(j, k) + sum 
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
wm_interm_22_triplet_pt2(j, k) = wm_interm_22_triplet_pt2(j, k) + sum 
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
wm_interm_23_triplet_pt2(i, j, k, l) = wm_interm_23_triplet_pt2(i, j, k, l) + sum 
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
sum = sum + r2p(vrdav_Rl, b,j,a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_24_triplet_pt2(b, j) = wm_interm_24_triplet_pt2(b, j) + sum 
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
wm_interm_25_triplet_pt2(b, i, j, k) = wm_interm_25_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_26_triplet_pt2(b, i, j, k) = wm_interm_26_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_27_triplet_pt2(b, j) = wm_interm_27_triplet_pt2(b, j) + sum 
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
wm_interm_28_triplet_pt2(b, c) = wm_interm_28_triplet_pt2(b, c) + sum 
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
wm_interm_29_triplet_pt2(b, c) = wm_interm_29_triplet_pt2(b, c) + sum 
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
wm_interm_30_triplet_pt2(j, k) = wm_interm_30_triplet_pt2(j, k) + sum 
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
wm_interm_31_triplet_pt2(i, j, k, l) = wm_interm_31_triplet_pt2(i, j, k, l) + sum 
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
wm_interm_32_triplet_pt2(j, k) = wm_interm_32_triplet_pt2(j, k) + sum 
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
wm_interm_33_triplet_pt2(b, j) = wm_interm_33_triplet_pt2(b, j) + sum 
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
wm_interm_34_triplet_pt2(b, i, j, k) = wm_interm_34_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_35_triplet_pt2(b, j) = wm_interm_35_triplet_pt2(b, j) + sum 
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
wm_interm_36_triplet_pt2(b, j, i, k) = wm_interm_36_triplet_pt2(b, j, i, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * t2(a,c,j,i)
end do 
end do 
end do 
wm_interm_37_triplet_pt2(b, c) = wm_interm_37_triplet_pt2(b, c) + sum 
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
wm_interm_38_triplet_pt2(b, c) = wm_interm_38_triplet_pt2(b, c) + sum 
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
wm_interm_39_triplet_pt2(j, k) = wm_interm_39_triplet_pt2(j, k) + sum 
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
wm_interm_40_triplet_pt2(i, j, k, l) = wm_interm_40_triplet_pt2(i, j, k, l) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * t2(a,b,k,i)
end do 
end do 
end do 
wm_interm_41_triplet_pt2(j, k) = wm_interm_41_triplet_pt2(j, k) + sum 
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
wm_interm_42_triplet_pt2(i, j) = wm_interm_42_triplet_pt2(i, j) + sum 
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
wm_interm_43_triplet_pt2(a, b) = wm_interm_43_triplet_pt2(a, b) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r1(vrdav_Rl, a,i) * r1(vrdav_Rr, a,i)
end do 
end do 
wm_interm_44_triplet_pt2 = wm_interm_44_triplet_pt2 + sum 
!$omp parallel private(i, a, b, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
sum = zero 
do i = 1, nocc 
sum = sum + r1(vrdav_Rr, a,i) * s1(b,i)
end do 
wm_interm_45_triplet_pt2(a, b) = wm_interm_45_triplet_pt2(a, b) + sum 
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
wm_interm_46_triplet_pt2(i, j) = wm_interm_46_triplet_pt2(i, j) + sum 
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
wm_interm_47_triplet_pt2(i, j) = wm_interm_47_triplet_pt2(i, j) + sum 
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
wm_interm_48_triplet_pt2(a, b) = wm_interm_48_triplet_pt2(a, b) + sum 
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
wm_interm_49_triplet_pt2(b, j) = wm_interm_49_triplet_pt2(b, j) + sum 
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
wm_interm_50_triplet_pt2(b, j) = wm_interm_50_triplet_pt2(b, j) + sum 
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
wm_interm_51_triplet_pt2(b, j) = wm_interm_51_triplet_pt2(b, j) + sum 
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
wm_interm_52_triplet_pt2(b, j) = wm_interm_52_triplet_pt2(b, j) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_53_triplet_pt2(j, k) = wm_interm_53_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_54_triplet_pt2 = wm_interm_54_triplet_pt2 + sum 
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
wm_interm_55_triplet_pt2(b, i, j, k) = wm_interm_55_triplet_pt2(b, i, j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_56_triplet_pt2(j, k) = wm_interm_56_triplet_pt2(j, k) + sum 
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
sum = sum + r2p(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_57_triplet_pt2(j, k) = wm_interm_57_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_58_triplet_pt2 = wm_interm_58_triplet_pt2 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2p(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_59_triplet_pt2 = wm_interm_59_triplet_pt2 + sum 
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
wm_interm_60_triplet_pt2(b, j, i, k) = wm_interm_60_triplet_pt2(b, j, i, k) + sum 
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
wm_interm_61_triplet_pt2(b, i, j, k) = wm_interm_61_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_62_triplet_pt2(b, j) = wm_interm_62_triplet_pt2(b, j) + sum 
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
wm_interm_63_triplet_pt2(b, j) = wm_interm_63_triplet_pt2(b, j) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_64_triplet_pt2(j, k) = wm_interm_64_triplet_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_65_triplet_pt2(j, k) = wm_interm_65_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2p(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_66_triplet_pt2 = wm_interm_66_triplet_pt2 + sum 
!$omp parallel private(a, b, i, j, k, sum)& 
!$omp default(shared)
!$omp do collapse(2)
do j = 1, nocc 
do k = 1, nocc 
sum = zero 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_67_triplet_pt2(j, k) = wm_interm_67_triplet_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,j,b,i) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_68_triplet_pt2(j, k) = wm_interm_68_triplet_pt2(j, k) + sum 
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
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,k,b,i)
end do 
end do 
end do 
wm_interm_69_triplet_pt2(j, k) = wm_interm_69_triplet_pt2(j, k) + sum 
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
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,k)
end do 
end do 
end do 
wm_interm_70_triplet_pt2(j, k) = wm_interm_70_triplet_pt2(j, k) + sum 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 

sum = zero 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,j,b,i)
end do 
end do 
end do 
end do 
wm_interm_71_triplet_pt2 = wm_interm_71_triplet_pt2 + sum 
sum = zero 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
sum = sum + r2m(vrdav_Rl, a,i,b,j) * r2m(vrdav_Rr, a,i,b,j)
end do 
end do 
end do 
end do 
wm_interm_72_triplet_pt2 = wm_interm_72_triplet_pt2 + sum 
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
wm_interm_73_triplet_pt2(b, i, j, k) = wm_interm_73_triplet_pt2(b, i, j, k) + sum 
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
wm_interm_74_triplet_pt2(b, j, i, k) = wm_interm_74_triplet_pt2(b, j, i, k) + sum 
end do 
end do 
end do 
end do 
!$omp end do nowait 
!$omp end parallel 



    end subroutine wm_triplet_intermediates_ccsd_pt2
    
    function calc_D_oo_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, b, a, j 
    real(F64), dimension(0:16) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(0) = term(0) + r2p(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
term(1) = term(1) + r2p(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
term(2) = term(2) + r2p(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
term(3) = term(3) + r2m(vrdav_Rl, a,p,b,i) * r2p(vrdav_Rr, a,q,b,i)
term(4) = term(4) + r2m(vrdav_Rl, a,i,b,p) * r2p(vrdav_Rr, a,q,b,i)
term(5) = term(5) + r2m(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,i,b,q)
term(6) = term(6) + r2m(vrdav_Rl, a,p,b,i) * r2m(vrdav_Rr, a,q,b,i)
term(7) = term(7) + r2m(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,q,b,i)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 4.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * 4.0d+0 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * 4.0d+0 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(8) = term(8) + r2m(vrdav_Rl, a,i,b,p) * r2m(vrdav_Rr, a,i,b,q)
end do 
end do 
end do 

term(8) = term(8) * (-4.0d+0) 

term(9) = term(9) + wm_interm_0_triplet_pt0
term(10) = term(10) + wm_interm_1_triplet_pt0
term(11) = term(11) + wm_interm_2_triplet_pt0
term(12) = term(12) + wm_interm_3_triplet_pt0
term(13) = term(13) + wm_interm_4_triplet_pt0
term(14) = term(14) + wm_interm_5_triplet_pt0
term(15) = term(15) + wm_interm_6_triplet_pt0

term(9) = term(9) * 4.0d+0 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * 4.0d+0 
term(12) = term(12) * 8.0d+0 
term(13) = term(13) * (-8.0d+0) 
term(14) = term(14) * 8.0d+0 
term(15) = term(15) * 4.0d+0 

do a = nocc + 1, nactive 
term(16) = term(16) + r1(vrdav_Rl, a,p) * r1(vrdav_Rr, a,q)
end do 

term(16) = term(16) * (-2.0d+0) 


    calc_D_oo_wm_triplet_pt0 = zero
    do s = 0, 16
    calc_D_oo_wm_triplet_pt0 = calc_D_oo_wm_triplet_pt0 + term(s)
    end do

    end function calc_D_oo_wm_triplet_pt0
    
    function calc_D_ov_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_ov_wm_triplet_pt0 = zero
    do s = 0, 1
    calc_D_ov_wm_triplet_pt0 = calc_D_ov_wm_triplet_pt0 + term(s)
    end do

    end function calc_D_ov_wm_triplet_pt0
    
    function calc_D_vo_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, b, j 
    real(F64), dimension(0:5) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,a,i)
term(1) = term(1) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,q,p,i)
term(2) = term(2) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, a,i,p,q)
term(3) = term(3) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, a,i)
term(4) = term(4) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, a,i)
term(5) = term(5) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, a,i)
end do 
end do 

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * 4.0d+0 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-4.0d+0) 
term(5) = term(5) * 4.0d+0 


    calc_D_vo_wm_triplet_pt0 = zero
    do s = 0, 5
    calc_D_vo_wm_triplet_pt0 = calc_D_vo_wm_triplet_pt0 + term(s)
    end do

    end function calc_D_vo_wm_triplet_pt0
    
    function calc_D_vv_wm_triplet_pt0(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt0
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, a, i, b 
    real(F64), dimension(0:9) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(0) = term(0) + r2p(vrdav_Rl, p,i,a,j) * r2p(vrdav_Rr, q,i,a,j)
term(1) = term(1) + r2p(vrdav_Rl, p,i,a,j) * r2m(vrdav_Rr, a,i,q,j)
term(2) = term(2) + r2m(vrdav_Rl, a,i,p,j) * r2p(vrdav_Rr, q,i,a,j)
end do 
end do 
end do 

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-4.0d+0) 
term(2) = term(2) * (-4.0d+0) 

do i = 1, nocc 
term(3) = term(3) + r1(vrdav_Rl, p,i) * r1(vrdav_Rr, q,i)
end do 

term(3) = term(3) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(4) = term(4) + r2p(vrdav_Rl, p,i,a,j) * r2m(vrdav_Rr, a,j,q,i)
term(5) = term(5) + r2m(vrdav_Rl, a,j,p,i) * r2p(vrdav_Rr, q,i,a,j)
term(6) = term(6) + r2m(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,i,q,j)
term(7) = term(7) + r2m(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,j,q,i)
term(8) = term(8) + r2m(vrdav_Rl, a,i,p,j) * r2m(vrdav_Rr, a,i,q,j)
end do 
end do 
end do 

term(4) = term(4) * 4.0d+0 
term(5) = term(5) * 4.0d+0 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(9) = term(9) + r2m(vrdav_Rl, a,j,p,i) * r2m(vrdav_Rr, a,j,q,i)
end do 
end do 
end do 

term(9) = term(9) * 4.0d+0 


    calc_D_vv_wm_triplet_pt0 = zero
    do s = 0, 9
    calc_D_vv_wm_triplet_pt0 = calc_D_vv_wm_triplet_pt0 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt0
    

    
    
    function calc_D_oo_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_oo_wm_triplet_pt1 = zero
    do s = 0, 1
    calc_D_oo_wm_triplet_pt1 = calc_D_oo_wm_triplet_pt1 + term(s)
    end do

    end function calc_D_oo_wm_triplet_pt1
    
    function calc_D_ov_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:7) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do a = nocc + 1, nactive 
term(0) = term(0) + r1(vrdav_Rl, a,p) * wm_interm_0_triplet_pt1(a, q)
term(1) = term(1) + r1(vrdav_Rl, a,p) * wm_interm_4_triplet_pt1(a, q)
term(2) = term(2) + r1(vrdav_Rl, a,p) * wm_interm_5_triplet_pt1(a, q)
term(3) = term(3) + r1(vrdav_Rl, a,p) * wm_interm_7_triplet_pt1(a, q)
term(4) = term(4) + r1(vrdav_Rr, a,p) * wm_interm_15_triplet_pt1(a, q)
term(5) = term(5) + r1(vrdav_Rr, a,p) * wm_interm_16_triplet_pt1(a, q)
term(6) = term(6) + r1(vrdav_Rr, a,p) * wm_interm_21_triplet_pt1(a, q)
term(7) = term(7) + r1(vrdav_Rr, a,p) * wm_interm_22_triplet_pt1(a, q)
end do 

term(0) = term(0) * 2.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * 3.0d+0 
term(5) = -term(5) 
term(6) = term(6) * (-4.0d+0) 
term(7) = term(7) * 4.0d+0 


    calc_D_ov_wm_triplet_pt1 = zero
    do s = 0, 7
    calc_D_ov_wm_triplet_pt1 = calc_D_ov_wm_triplet_pt1 + term(s)
    end do

    end function calc_D_ov_wm_triplet_pt1
    
    function calc_D_vo_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , i, a, b, j 
    real(F64), dimension(0:20) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do i = 1, nocc 
term(0) = term(0) + r1(vrdav_Rl, p,i) * wm_interm_2_triplet_pt1(i, q)
term(1) = term(1) + r1(vrdav_Rl, p,i) * wm_interm_3_triplet_pt1(i, q)
term(2) = term(2) + r1(vrdav_Rl, p,i) * wm_interm_9_triplet_pt1(i, q)
term(3) = term(3) + r1(vrdav_Rl, p,i) * wm_interm_10_triplet_pt1(i, q)
term(4) = term(4) + r1(vrdav_Rl, p,i) * wm_interm_11_triplet_pt1(i, q)
term(5) = term(5) + r1(vrdav_Rr, p,i) * wm_interm_13_triplet_pt1(i, q)
term(6) = term(6) + r1(vrdav_Rr, p,i) * wm_interm_14_triplet_pt1(i, q)
term(7) = term(7) + r1(vrdav_Rr, p,i) * wm_interm_19_triplet_pt1(i, q)
term(8) = term(8) + r1(vrdav_Rr, p,i) * wm_interm_20_triplet_pt1(i, q)
end do 

term(0) = -term(0) 
term(1) = term(1) * 3.0d+0 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * (-2.0d+0) 
term(4) = term(4) * 4.0d+0 
term(5) = -term(5) 
term(6) = term(6) * 3.0d+0 
term(7) = term(7) * (-4.0d+0) 
term(8) = term(8) * 4.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(9) = term(9) + s2(a,p,i,q) * wm_interm_1_triplet_pt1(a, i)
term(10) = term(10) + s2(a,p,q,i) * wm_interm_1_triplet_pt1(a, i)
term(11) = term(11) + s2(a,p,i,q) * wm_interm_6_triplet_pt1(a, i)
term(12) = term(12) + s2(a,p,i,q) * wm_interm_8_triplet_pt1(a, i)
term(13) = term(13) + s2(a,p,q,i) * wm_interm_6_triplet_pt1(a, i)
term(14) = term(14) + s2(a,p,q,i) * wm_interm_8_triplet_pt1(a, i)
term(15) = term(15) + t2(a,p,q,i) * wm_interm_12_triplet_pt1(a, i)
term(16) = term(16) + t2(a,p,i,q) * wm_interm_12_triplet_pt1(a, i)
term(17) = term(17) + t2(a,p,q,i) * wm_interm_17_triplet_pt1(a, i)
term(18) = term(18) + t2(a,p,q,i) * wm_interm_18_triplet_pt1(a, i)
term(19) = term(19) + t2(a,p,i,q) * wm_interm_17_triplet_pt1(a, i)
term(20) = term(20) + t2(a,p,i,q) * wm_interm_18_triplet_pt1(a, i)
end do 
end do 

term(9) = term(9) * (-8.0d+0) 
term(10) = term(10) * 4.0d+0 
term(11) = term(11) * 8.0d+0 
term(12) = term(12) * (-8.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * 4.0d+0 
term(15) = term(15) * 4.0d+0 
term(16) = term(16) * (-8.0d+0) 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * 8.0d+0 
term(20) = term(20) * (-8.0d+0) 


    calc_D_vo_wm_triplet_pt1 = zero
    do s = 0, 20
    calc_D_vo_wm_triplet_pt1 = calc_D_vo_wm_triplet_pt1 + term(s)
    end do

    end function calc_D_vo_wm_triplet_pt1
    
    function calc_D_vv_wm_triplet_pt1(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt1
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , a, i, b, j 
    real(F64), dimension(0:1) :: term 
    term = 0.d+0 

    term = 0.d+0 
    
    calc_D_vv_wm_triplet_pt1 = zero
    do s = 0, 1
    calc_D_vv_wm_triplet_pt1 = calc_D_vv_wm_triplet_pt1 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt1
    

    
    
    function calc_D_oo_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_oo_wm_triplet_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , j, i, a, b 
    real(F64), dimension(0:39) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(0) = term(0) + wm_interm_25_triplet_pt2(a, p, i, j) * wm_interm_9_triplet_pt2(a, q, i, j)
term(1) = term(1) + wm_interm_25_triplet_pt2(a, p, i, j) * wm_interm_9_triplet_pt2(a, q, j, i)
term(2) = term(2) + wm_interm_25_triplet_pt2(a, i, j, p) * wm_interm_9_triplet_pt2(a, i, j, q)
term(3) = term(3) + wm_interm_25_triplet_pt2(a, i, p, j) * wm_interm_9_triplet_pt2(a, i, q, j)
term(4) = term(4) + wm_interm_25_triplet_pt2(a, i, j, p) * wm_interm_9_triplet_pt2(a, i, q, j)
term(5) = term(5) + wm_interm_25_triplet_pt2(a, i, p, j) * wm_interm_9_triplet_pt2(a, i, j, q)
end do 
end do 
end do 

term(0) = term(0) * 4.0d+0 
term(1) = term(1) * (-2.0d+0) 
term(2) = term(2) * 4.0d+0 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * (-2.0d+0) 

term(6) = term(6) + wm_interm_2_triplet_pt2(p, q) * wm_interm_44_triplet_pt2
term(7) = term(7) + wm_interm_1_triplet_pt2(p, q) * wm_interm_44_triplet_pt2

term(6) = term(6) * (-8.0d+0) 
term(7) = term(7) * 4.0d+0 

do i = 1, nocc 
term(8) = term(8) + wm_interm_1_triplet_pt2(q, i) * wm_interm_42_triplet_pt2(i, p)
term(9) = term(9) + wm_interm_2_triplet_pt2(q, i) * wm_interm_42_triplet_pt2(i, p)
term(10) = term(10) + wm_interm_1_triplet_pt2(i, q) * wm_interm_42_triplet_pt2(p, i)
term(11) = term(11) + wm_interm_2_triplet_pt2(i, q) * wm_interm_42_triplet_pt2(p, i)
end do 

term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * 4.0d+0 
term(10) = term(10) * (-2.0d+0) 
term(11) = term(11) * 4.0d+0 

do a = nocc + 1, nactive 
term(12) = term(12) + wm_interm_27_triplet_pt2(a, p) * wm_interm_3_triplet_pt2(a, q)
term(13) = term(13) + r1(vrdav_Rl, a,q) * wm_interm_49_triplet_pt2(a, p)
term(14) = term(14) + r1(vrdav_Rl, a,q) * wm_interm_50_triplet_pt2(a, p)
term(15) = term(15) + r1(vrdav_Rl, a,q) * wm_interm_51_triplet_pt2(a, p)
term(16) = term(16) + s1(a,q) * wm_interm_7_triplet_pt2(a, p)
term(17) = term(17) + s1(a,q) * wm_interm_15_triplet_pt2(a, p)
term(18) = term(18) + s1(a,q) * wm_interm_16_triplet_pt2(a, p)
term(19) = term(19) + r1(vrdav_Rr, a,p) * wm_interm_52_triplet_pt2(a, q)
term(20) = term(20) + t1(a,q) * wm_interm_24_triplet_pt2(a, p)
term(21) = term(21) + r1(vrdav_Rr, a,p) * wm_interm_62_triplet_pt2(a, q)
term(22) = term(22) + r1(vrdav_Rr, a,p) * wm_interm_63_triplet_pt2(a, q)
term(23) = term(23) + t1(a,q) * wm_interm_33_triplet_pt2(a, p)
term(24) = term(24) + t1(a,q) * wm_interm_35_triplet_pt2(a, p)
end do 

term(12) = term(12) * (-2.0d+0) 
term(13) = term(13) * (-4.0d+0) 
term(14) = term(14) * (-4.0d+0) 
term(15) = term(15) * 4.0d+0 
term(16) = term(16) * 4.0d+0 
term(17) = term(17) * (-4.0d+0) 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-4.0d+0) 
term(20) = term(20) * 4.0d+0 
term(21) = term(21) * 4.0d+0 
term(22) = term(22) * (-4.0d+0) 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(25) = term(25) + wm_interm_1_triplet_pt2(i, j) * wm_interm_42_triplet_pt2(j, i)
term(26) = term(26) + wm_interm_2_triplet_pt2(i, j) * wm_interm_42_triplet_pt2(j, i)
end do 
end do 

term(25) = term(25) * 4.0d+0 
term(26) = term(26) * (-8.0d+0) 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(27) = term(27) + wm_interm_25_triplet_pt2(a, p, i, q) * wm_interm_3_triplet_pt2(a, i)
term(28) = term(28) + wm_interm_27_triplet_pt2(a, i) * wm_interm_3_triplet_pt2(a, i)
term(29) = term(29) + wm_interm_27_triplet_pt2(a, i) * wm_interm_9_triplet_pt2(a, p, i, q)
term(30) = term(30) + r1(vrdav_Rl, a,i) * wm_interm_49_triplet_pt2(a, i)
term(31) = term(31) + r1(vrdav_Rl, a,i) * wm_interm_50_triplet_pt2(a, i)
term(32) = term(32) + r1(vrdav_Rl, a,i) * wm_interm_51_triplet_pt2(a, i)
term(33) = term(33) + r1(vrdav_Rr, a,i) * wm_interm_52_triplet_pt2(a, i)
term(34) = term(34) + r1(vrdav_Rr, a,i) * wm_interm_62_triplet_pt2(a, i)
term(35) = term(35) + r1(vrdav_Rr, a,i) * wm_interm_63_triplet_pt2(a, i)
end do 
end do 

term(27) = term(27) * (-2.0d+0) 
term(28) = term(28) * 4.0d+0 
term(29) = term(29) * (-2.0d+0) 
term(30) = term(30) * 8.0d+0 
term(31) = term(31) * 8.0d+0 
term(32) = term(32) * (-8.0d+0) 
term(33) = term(33) * (-8.0d+0) 
term(34) = term(34) * (-8.0d+0) 
term(35) = term(35) * 8.0d+0 

do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(36) = term(36) + wm_interm_43_triplet_pt2(a, b) * wm_interm_4_triplet_pt2(b, a)
term(37) = term(37) + wm_interm_43_triplet_pt2(a, b) * wm_interm_5_triplet_pt2(b, a)
end do 
end do 

term(36) = term(36) * (-8.0d+0) 
term(37) = term(37) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
term(38) = term(38) + wm_interm_42_triplet_pt2(i, j) * wm_interm_8_triplet_pt2(j, p, i, q)
term(39) = term(39) + wm_interm_42_triplet_pt2(i, j) * wm_interm_8_triplet_pt2(p, j, i, q)
end do 
end do 

term(38) = term(38) * 4.0d+0 
term(39) = term(39) * (-2.0d+0) 


    calc_D_oo_wm_triplet_pt2 = zero
    do s = 0, 39
    calc_D_oo_wm_triplet_pt2 = calc_D_oo_wm_triplet_pt2 + term(s)
    end do

    end function calc_D_oo_wm_triplet_pt2
    
    function calc_D_ov_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_ov_wm_triplet_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:78) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_14_triplet_pt2(i, p, j, k) * wm_interm_9_triplet_pt2(q, i, j, k)
term(1) = term(1) + wm_interm_14_triplet_pt2(i, p, j, k) * wm_interm_9_triplet_pt2(q, i, k, j)
term(2) = term(2) + wm_interm_23_triplet_pt2(p, i, j, k) * wm_interm_9_triplet_pt2(q, i, j, k)
term(3) = term(3) + wm_interm_23_triplet_pt2(i, p, j, k) * wm_interm_9_triplet_pt2(q, i, j, k)
term(4) = term(4) + wm_interm_23_triplet_pt2(i, p, j, k) * wm_interm_9_triplet_pt2(q, i, k, j)
term(5) = term(5) + wm_interm_26_triplet_pt2(q, i, j, k) * wm_interm_8_triplet_pt2(k, p, i, j)
term(6) = term(6) + wm_interm_26_triplet_pt2(q, i, j, k) * wm_interm_8_triplet_pt2(p, k, i, j)
term(7) = term(7) + wm_interm_36_triplet_pt2(q, i, j, k) * wm_interm_8_triplet_pt2(k, p, i, j)
term(8) = term(8) + wm_interm_34_triplet_pt2(q, i, j, k) * wm_interm_8_triplet_pt2(p, k, i, j)
term(9) = term(9) + wm_interm_34_triplet_pt2(q, i, j, k) * wm_interm_8_triplet_pt2(k, p, i, j)
end do 
end do 
end do 

term(0) = term(0) * 3.0d+0 
term(1) = -term(1) 
term(2) = term(2) * (-2.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * (-2.0d+0) 
term(5) = term(5) * 3.0d+0 
term(6) = -term(6) 
term(7) = term(7) * (-2.0d+0) 
term(8) = term(8) * (-2.0d+0) 
term(9) = term(9) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(10) = term(10) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,j) * wm_interm_25_triplet_pt2(b, i, p, k)
term(11) = term(11) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,j,k) * wm_interm_25_triplet_pt2(b, i, p, k)
term(12) = term(12) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,j,k) * wm_interm_25_triplet_pt2(b, i, k, p)
end do 
end do 
end do 
end do 
end do 

term(10) = term(10) * 4.0d+0 
term(11) = term(11) * (-8.0d+0) 
term(12) = term(12) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(13) = term(13) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,j) * wm_interm_25_triplet_pt2(b, i, p, k)
term(14) = term(14) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_25_triplet_pt2(b, i, p, k)
term(15) = term(15) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,j,k) * wm_interm_25_triplet_pt2(b, i, k, p)
end do 
end do 
end do 
end do 
end do 

term(13) = term(13) * (-2.0d+0) 
term(14) = term(14) * 4.0d+0 
term(15) = term(15) * (-2.0d+0) 

do a = nocc + 1, nactive 
term(16) = term(16) + wm_interm_4_triplet_pt2(a, q) * wm_interm_7_triplet_pt2(a, p)
term(17) = term(17) + wm_interm_5_triplet_pt2(a, q) * wm_interm_7_triplet_pt2(a, p)
term(18) = term(18) + wm_interm_15_triplet_pt2(a, p) * wm_interm_4_triplet_pt2(a, q)
term(19) = term(19) + wm_interm_16_triplet_pt2(a, p) * wm_interm_4_triplet_pt2(a, q)
term(20) = term(20) + wm_interm_15_triplet_pt2(a, p) * wm_interm_5_triplet_pt2(a, q)
term(21) = term(21) + wm_interm_16_triplet_pt2(a, p) * wm_interm_5_triplet_pt2(a, q)
term(22) = term(22) + wm_interm_27_triplet_pt2(a, p) * wm_interm_28_triplet_pt2(q, a)
term(23) = term(23) + wm_interm_27_triplet_pt2(a, p) * wm_interm_29_triplet_pt2(q, a)
term(24) = term(24) + wm_interm_27_triplet_pt2(a, p) * wm_interm_37_triplet_pt2(q, a)
term(25) = term(25) + wm_interm_27_triplet_pt2(a, p) * wm_interm_38_triplet_pt2(q, a)
term(26) = term(26) + r1(vrdav_Rl, a,p) * wm_interm_45_triplet_pt2(a, q)
term(27) = term(27) + r1(vrdav_Rr, a,p) * wm_interm_48_triplet_pt2(a, q)
end do 

term(16) = term(16) * (-8.0d+0) 
term(17) = term(17) * 4.0d+0 
term(18) = term(18) * 8.0d+0 
term(19) = term(19) * (-8.0d+0) 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * 4.0d+0 
term(22) = term(22) * (-3.0d+0) 
term(24) = term(24) * (-4.0d+0) 
term(25) = term(25) * 4.0d+0 
term(26) = term(26) * 2.0d+0 
term(27) = term(27) * 2.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(28) = term(28) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,j) * wm_interm_25_triplet_pt2(b, i, k, p)
end do 
end do 
end do 
end do 
end do 

term(28) = term(28) * 4.0d+0 

do i = 1, nocc 
term(29) = term(29) + wm_interm_12_triplet_pt2(p, i) * wm_interm_3_triplet_pt2(q, i)
term(30) = term(30) + wm_interm_13_triplet_pt2(p, i) * wm_interm_3_triplet_pt2(q, i)
term(31) = term(31) + wm_interm_20_triplet_pt2(p, i) * wm_interm_3_triplet_pt2(q, i)
term(32) = term(32) + wm_interm_21_triplet_pt2(p, i) * wm_interm_3_triplet_pt2(q, i)
term(33) = term(33) + wm_interm_22_triplet_pt2(p, i) * wm_interm_3_triplet_pt2(q, i)
term(34) = term(34) + wm_interm_1_triplet_pt2(p, i) * wm_interm_24_triplet_pt2(q, i)
term(35) = term(35) + wm_interm_24_triplet_pt2(q, i) * wm_interm_2_triplet_pt2(p, i)
term(36) = term(36) + wm_interm_1_triplet_pt2(p, i) * wm_interm_33_triplet_pt2(q, i)
term(37) = term(37) + wm_interm_2_triplet_pt2(p, i) * wm_interm_33_triplet_pt2(q, i)
term(38) = term(38) + wm_interm_1_triplet_pt2(p, i) * wm_interm_35_triplet_pt2(q, i)
term(39) = term(39) + wm_interm_2_triplet_pt2(p, i) * wm_interm_35_triplet_pt2(q, i)
term(40) = term(40) + s1(q,i) * wm_interm_53_triplet_pt2(p, i)
term(41) = term(41) + s1(q,i) * wm_interm_56_triplet_pt2(p, i)
term(42) = term(42) + s1(q,i) * wm_interm_57_triplet_pt2(p, i)
term(43) = term(43) + t1(q,i) * wm_interm_53_triplet_pt2(i, p)
term(44) = term(44) + t1(q,i) * wm_interm_56_triplet_pt2(i, p)
term(45) = term(45) + t1(q,i) * wm_interm_57_triplet_pt2(i, p)
term(46) = term(46) + s1(q,i) * wm_interm_64_triplet_pt2(p, i)
term(47) = term(47) + s1(q,i) * wm_interm_65_triplet_pt2(p, i)
term(48) = term(48) + s1(q,i) * wm_interm_67_triplet_pt2(p, i)
term(49) = term(49) + s1(q,i) * wm_interm_68_triplet_pt2(p, i)
term(50) = term(50) + s1(q,i) * wm_interm_69_triplet_pt2(p, i)
term(51) = term(51) + s1(q,i) * wm_interm_70_triplet_pt2(p, i)
term(52) = term(52) + t1(q,i) * wm_interm_64_triplet_pt2(i, p)
term(53) = term(53) + t1(q,i) * wm_interm_65_triplet_pt2(i, p)
term(54) = term(54) + t1(q,i) * wm_interm_67_triplet_pt2(i, p)
term(55) = term(55) + t1(q,i) * wm_interm_68_triplet_pt2(i, p)
term(56) = term(56) + t1(q,i) * wm_interm_69_triplet_pt2(i, p)
term(57) = term(57) + t1(q,i) * wm_interm_70_triplet_pt2(i, p)
end do 

term(29) = -term(29) 
term(30) = term(30) * 3.0d+0 
term(31) = term(31) * (-2.0d+0) 
term(32) = term(32) * (-2.0d+0) 
term(33) = term(33) * 4.0d+0 
term(34) = term(34) * 4.0d+0 
term(35) = term(35) * (-8.0d+0) 
term(36) = term(36) * (-4.0d+0) 
term(37) = term(37) * 8.0d+0 
term(38) = term(38) * 4.0d+0 
term(39) = term(39) * (-8.0d+0) 
term(40) = term(40) * 4.0d+0 
term(41) = term(41) * 4.0d+0 
term(42) = term(42) * (-4.0d+0) 
term(43) = term(43) * 4.0d+0 
term(44) = term(44) * 4.0d+0 
term(45) = term(45) * (-4.0d+0) 
term(46) = term(46) * (-4.0d+0) 
term(47) = term(47) * 4.0d+0 
term(48) = term(48) * (-4.0d+0) 
term(49) = term(49) * 4.0d+0 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * 4.0d+0 
term(52) = term(52) * (-4.0d+0) 
term(53) = term(53) * 4.0d+0 
term(54) = term(54) * (-4.0d+0) 
term(55) = term(55) * 4.0d+0 
term(56) = term(56) * (-4.0d+0) 
term(57) = term(57) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(58) = term(58) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_0_triplet_pt2(a, k, p, j)
term(59) = term(59) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_0_triplet_pt2(a, k, p, j)
term(60) = term(60) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_6_triplet_pt2(a, k, p, j)
term(61) = term(61) + s2(a,b,i,j) * t2(b,q,k,i) * wm_interm_6_triplet_pt2(a, k, j, p)
term(62) = term(62) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_6_triplet_pt2(a, k, p, j)
term(63) = term(63) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_25_triplet_pt2(b, i, k, p)
term(64) = term(64) + r2m(vrdav_Rl, a,i,q,j) * t2(a,b,k,i) * wm_interm_25_triplet_pt2(b, j, k, p)
end do 
end do 
end do 
end do 
end do 

term(58) = term(58) * (-4.0d+0) 
term(59) = term(59) * 4.0d+0 
term(60) = term(60) * (-4.0d+0) 
term(61) = term(61) * 4.0d+0 
term(62) = term(62) * 4.0d+0 
term(63) = term(63) * (-2.0d+0) 
term(64) = term(64) * (-2.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(65) = term(65) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_0_triplet_pt2(a, i, p, j)
term(66) = term(66) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_6_triplet_pt2(a, i, p, j)
term(67) = term(67) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,j) * wm_interm_25_triplet_pt2(b, i, p, k)
end do 
end do 
end do 
end do 
end do 

term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * (-8.0d+0) 
term(67) = term(67) * 4.0d+0 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(68) = term(68) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_0_triplet_pt2(a, i, p, k)
term(69) = term(69) + s2(a,b,j,k) * t2(b,q,k,i) * wm_interm_6_triplet_pt2(a, i, j, p)
term(70) = term(70) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_6_triplet_pt2(a, i, k, p)
term(71) = term(71) + s2(a,b,j,k) * t2(b,q,j,i) * wm_interm_6_triplet_pt2(a, i, p, k)
term(72) = term(72) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_25_triplet_pt2(b, i, p, k)
term(73) = term(73) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,j,k) * wm_interm_25_triplet_pt2(b, i, k, p)
term(74) = term(74) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,k,i) * wm_interm_25_triplet_pt2(b, j, p, k)
term(75) = term(75) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_25_triplet_pt2(b, j, p, k)
term(76) = term(76) + r2m(vrdav_Rl, a,j,q,i) * t2(a,b,i,k) * wm_interm_25_triplet_pt2(b, j, k, p)
end do 
end do 
end do 
end do 
end do 

term(68) = term(68) * 4.0d+0 
term(69) = term(69) * 8.0d+0 
term(70) = term(70) * (-4.0d+0) 
term(71) = term(71) * 4.0d+0 
term(72) = term(72) * (-8.0d+0) 
term(73) = term(73) * 4.0d+0 
term(74) = term(74) * (-2.0d+0) 
term(75) = term(75) * 4.0d+0 
term(76) = term(76) * (-2.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(77) = term(77) + s2(a,b,j,i) * t2(b,q,k,i) * wm_interm_6_triplet_pt2(a, k, j, p)
end do 
end do 
end do 
end do 
end do 

term(77) = term(77) * (-4.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(78) = term(78) + r2p(vrdav_Rl, q,i,a,j) * t2(a,b,k,j) * wm_interm_25_triplet_pt2(b, i, k, p)
end do 
end do 
end do 
end do 
end do 

term(78) = term(78) * (-4.0d+0) 


    calc_D_ov_wm_triplet_pt2 = zero
    do s = 0, 78
    calc_D_ov_wm_triplet_pt2 = calc_D_ov_wm_triplet_pt2 + term(s)
    end do

    end function calc_D_ov_wm_triplet_pt2
    
    function calc_D_vo_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vo_wm_triplet_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:203) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_0_triplet_pt2(p, i, j, k) * wm_interm_8_triplet_pt2(k, j, i, q)
term(1) = term(1) + wm_interm_0_triplet_pt2(p, i, j, k) * wm_interm_8_triplet_pt2(j, k, i, q)
term(2) = term(2) + wm_interm_6_triplet_pt2(p, i, j, k) * wm_interm_8_triplet_pt2(k, j, i, q)
term(3) = term(3) + wm_interm_6_triplet_pt2(p, i, j, k) * wm_interm_8_triplet_pt2(j, k, i, q)
term(4) = term(4) + wm_interm_25_triplet_pt2(p, i, j, k) * wm_interm_31_triplet_pt2(i, q, j, k)
term(5) = term(5) + wm_interm_25_triplet_pt2(p, i, j, k) * wm_interm_31_triplet_pt2(i, q, k, j)
term(6) = term(6) + wm_interm_25_triplet_pt2(p, i, j, k) * wm_interm_40_triplet_pt2(q, i, j, k)
term(7) = term(7) + wm_interm_25_triplet_pt2(p, i, j, k) * wm_interm_40_triplet_pt2(i, q, j, k)
term(8) = term(8) + wm_interm_25_triplet_pt2(p, i, j, k) * wm_interm_40_triplet_pt2(i, q, k, j)
end do 
end do 
end do 

term(0) = term(0) * (-2.0d+0) 
term(1) = term(1) * 2.0d+0 
term(2) = term(2) * (-4.0d+0) 
term(3) = term(3) * 4.0d+0 
term(4) = term(4) * 3.0d+0 
term(5) = -term(5) 
term(6) = term(6) * (-2.0d+0) 
term(7) = term(7) * 4.0d+0 
term(8) = term(8) * (-2.0d+0) 

do i = 1, nocc 
do j = 1, nocc 
term(9) = term(9) + wm_interm_1_triplet_pt2(i, j) * wm_interm_6_triplet_pt2(p, j, q, i)
term(10) = term(10) + wm_interm_2_triplet_pt2(i, j) * wm_interm_6_triplet_pt2(p, j, q, i)
term(11) = term(11) + wm_interm_1_triplet_pt2(i, j) * wm_interm_6_triplet_pt2(p, j, i, q)
term(12) = term(12) + wm_interm_2_triplet_pt2(i, j) * wm_interm_6_triplet_pt2(p, j, i, q)
term(13) = term(13) + wm_interm_1_triplet_pt2(i, j) * wm_interm_26_triplet_pt2(p, j, q, i)
term(14) = term(14) + wm_interm_1_triplet_pt2(i, j) * wm_interm_34_triplet_pt2(p, q, j, i)
term(15) = term(15) + wm_interm_2_triplet_pt2(i, j) * wm_interm_34_triplet_pt2(p, q, j, i)
term(16) = term(16) + wm_interm_1_triplet_pt2(i, j) * wm_interm_36_triplet_pt2(p, j, q, i)
term(17) = term(17) + wm_interm_1_triplet_pt2(i, j) * wm_interm_34_triplet_pt2(p, j, q, i)
term(18) = term(18) + wm_interm_2_triplet_pt2(i, j) * wm_interm_36_triplet_pt2(p, j, q, i)
term(19) = term(19) + wm_interm_2_triplet_pt2(i, j) * wm_interm_34_triplet_pt2(p, j, q, i)
end do 
end do 

term(9) = term(9) * (-4.0d+0) 
term(10) = term(10) * 8.0d+0 
term(11) = term(11) * 4.0d+0 
term(12) = term(12) * (-8.0d+0) 
term(13) = term(13) * 4.0d+0 
term(14) = term(14) * (-2.0d+0) 
term(15) = term(15) * 4.0d+0 
term(16) = term(16) * (-2.0d+0) 
term(17) = term(17) * 4.0d+0 
term(18) = term(18) * 4.0d+0 
term(19) = term(19) * (-8.0d+0) 

do j = 1, nocc 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(20) = term(20) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_55_triplet_pt2(a, i, j, q)
term(21) = term(21) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_60_triplet_pt2(a, i, j, q)
term(22) = term(22) + r2p(vrdav_Rl, p,i,a,j) * wm_interm_60_triplet_pt2(a, j, i, q)
term(23) = term(23) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_61_triplet_pt2(a, i, j, q)
term(24) = term(24) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_73_triplet_pt2(a, i, j, q)
term(25) = term(25) + r2p(vrdav_Rr, p,i,a,j) * wm_interm_74_triplet_pt2(a, i, j, q)
end do 
end do 
end do 

term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * (-4.0d+0) 
term(24) = term(24) * 4.0d+0 
term(25) = term(25) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(26) = term(26) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_61_triplet_pt2(a, i, j, q)
term(27) = term(27) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_61_triplet_pt2(a, j, i, q)
term(28) = term(28) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_55_triplet_pt2(a, i, j, q)
term(29) = term(29) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_60_triplet_pt2(a, j, i, q)
term(30) = term(30) + r2m(vrdav_Rl, a,i,p,j) * wm_interm_60_triplet_pt2(a, i, j, q)
term(31) = term(31) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_73_triplet_pt2(a, i, j, q)
term(32) = term(32) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_73_triplet_pt2(a, j, i, q)
term(33) = term(33) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_74_triplet_pt2(a, j, i, q)
term(34) = term(34) + r2m(vrdav_Rr, a,i,p,j) * wm_interm_74_triplet_pt2(a, i, j, q)
end do 
end do 
end do 

term(26) = term(26) * 4.0d+0 
term(27) = term(27) * (-4.0d+0) 
term(28) = term(28) * 4.0d+0 
term(29) = term(29) * (-4.0d+0) 
term(30) = term(30) * 4.0d+0 
term(31) = term(31) * (-4.0d+0) 
term(32) = term(32) * 4.0d+0 
term(33) = term(33) * (-4.0d+0) 
term(34) = term(34) * 4.0d+0 

do i = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(35) = term(35) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_55_triplet_pt2(a, i, j, q)
term(36) = term(36) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_60_triplet_pt2(a, i, j, q)
term(37) = term(37) + r2m(vrdav_Rl, a,j,p,i) * wm_interm_60_triplet_pt2(a, j, i, q)
end do 
end do 
end do 

term(35) = term(35) * (-4.0d+0) 
term(36) = term(36) * (-4.0d+0) 
term(37) = term(37) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do j = 1, nocc 
term(38) = term(38) + r2p(vrdav_Rr, p,q,a,i) * s2(a,b,j,i) * wm_interm_3_triplet_pt2(b, j)
term(39) = term(39) + r2p(vrdav_Rl, p,q,a,i) * t2(a,b,j,i) * wm_interm_27_triplet_pt2(b, j)
end do 
end do 
end do 
end do 

term(38) = term(38) * 4.0d+0 
term(39) = term(39) * 4.0d+0 

do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(40) = term(40) + r2m(vrdav_Rr, a,q,p,i) * s2(a,b,j,i) * wm_interm_3_triplet_pt2(b, j)
term(41) = term(41) + r2m(vrdav_Rr, a,i,p,q) * s2(a,b,j,i) * wm_interm_3_triplet_pt2(b, j)
term(42) = term(42) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_7_triplet_pt2(b, j)
term(43) = term(43) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_15_triplet_pt2(b, j)
term(44) = term(44) + s2(a,b,j,i) * t2(a,p,q,i) * wm_interm_16_triplet_pt2(b, j)
term(45) = term(45) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_24_triplet_pt2(b, j)
term(46) = term(46) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_24_triplet_pt2(b, j)
term(47) = term(47) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_35_triplet_pt2(b, j)
term(48) = term(48) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_35_triplet_pt2(b, j)
term(49) = term(49) + s2(a,p,q,i) * t2(a,b,j,i) * wm_interm_33_triplet_pt2(b, j)
term(50) = term(50) + r2m(vrdav_Rl, a,q,p,i) * t2(a,b,j,i) * wm_interm_27_triplet_pt2(b, j)
term(51) = term(51) + r2m(vrdav_Rl, a,i,p,q) * t2(a,b,j,i) * wm_interm_27_triplet_pt2(b, j)
term(52) = term(52) + s2(a,p,i,q) * t2(a,b,j,i) * wm_interm_33_triplet_pt2(b, j)
end do 
end do 
end do 
end do 

term(40) = term(40) * (-4.0d+0) 
term(41) = term(41) * 4.0d+0 
term(42) = term(42) * 4.0d+0 
term(43) = term(43) * (-4.0d+0) 
term(44) = term(44) * 4.0d+0 
term(45) = term(45) * 4.0d+0 
term(46) = term(46) * (-8.0d+0) 
term(47) = term(47) * 4.0d+0 
term(48) = term(48) * (-8.0d+0) 
term(49) = term(49) * (-4.0d+0) 
term(50) = term(50) * (-4.0d+0) 
term(51) = term(51) * 4.0d+0 
term(52) = term(52) * 8.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
term(53) = term(53) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_49_triplet_pt2(a, i)
term(54) = term(54) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_50_triplet_pt2(a, i)
term(55) = term(55) + r2p(vrdav_Rl, p,q,a,i) * wm_interm_51_triplet_pt2(a, i)
term(56) = term(56) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_52_triplet_pt2(a, i)
term(57) = term(57) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_52_triplet_pt2(a, i)
term(58) = term(58) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_52_triplet_pt2(a, i)
term(59) = term(59) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_49_triplet_pt2(a, i)
term(60) = term(60) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_49_triplet_pt2(a, i)
term(61) = term(61) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_50_triplet_pt2(a, i)
term(62) = term(62) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_50_triplet_pt2(a, i)
term(63) = term(63) + r2m(vrdav_Rl, a,q,p,i) * wm_interm_51_triplet_pt2(a, i)
term(64) = term(64) + r2m(vrdav_Rl, a,i,p,q) * wm_interm_51_triplet_pt2(a, i)
term(65) = term(65) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_62_triplet_pt2(a, i)
term(66) = term(66) + r2p(vrdav_Rr, p,q,a,i) * wm_interm_63_triplet_pt2(a, i)
term(67) = term(67) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_62_triplet_pt2(a, i)
term(68) = term(68) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_62_triplet_pt2(a, i)
term(69) = term(69) + r2m(vrdav_Rr, a,q,p,i) * wm_interm_63_triplet_pt2(a, i)
term(70) = term(70) + r2m(vrdav_Rr, a,i,p,q) * wm_interm_63_triplet_pt2(a, i)
end do 
end do 

term(53) = term(53) * 8.0d+0 
term(54) = term(54) * 8.0d+0 
term(55) = term(55) * (-8.0d+0) 
term(56) = term(56) * (-8.0d+0) 
term(57) = term(57) * 8.0d+0 
term(58) = term(58) * (-8.0d+0) 
term(59) = term(59) * (-8.0d+0) 
term(60) = term(60) * 8.0d+0 
term(61) = term(61) * (-8.0d+0) 
term(62) = term(62) * 8.0d+0 
term(63) = term(63) * 8.0d+0 
term(64) = term(64) * (-8.0d+0) 
term(65) = term(65) * (-8.0d+0) 
term(66) = term(66) * 8.0d+0 
term(67) = term(67) * 8.0d+0 
term(68) = term(68) * (-8.0d+0) 
term(69) = term(69) * (-8.0d+0) 
term(70) = term(70) * 8.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(71) = term(71) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_7_triplet_pt2(b, j)
term(72) = term(72) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_15_triplet_pt2(b, j)
term(73) = term(73) + s2(a,b,i,j) * t2(a,p,q,i) * wm_interm_16_triplet_pt2(b, j)
term(74) = term(74) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_24_triplet_pt2(b, j)
term(75) = term(75) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_35_triplet_pt2(b, j)
term(76) = term(76) + s2(a,p,q,i) * t2(a,b,i,j) * wm_interm_33_triplet_pt2(b, j)
end do 
end do 
end do 
end do 

term(71) = term(71) * (-8.0d+0) 
term(72) = term(72) * 8.0d+0 
term(73) = term(73) * (-8.0d+0) 
term(74) = term(74) * (-8.0d+0) 
term(75) = term(75) * (-8.0d+0) 
term(76) = term(76) * 8.0d+0 

do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(77) = term(77) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_7_triplet_pt2(a, i)
term(78) = term(78) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_7_triplet_pt2(a, j)
term(79) = term(79) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_15_triplet_pt2(a, i)
term(80) = term(80) + s2(a,b,i,j) * t2(b,p,j,q) * wm_interm_16_triplet_pt2(a, i)
term(81) = term(81) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_15_triplet_pt2(a, j)
term(82) = term(82) + s2(a,b,i,j) * t2(b,p,i,q) * wm_interm_16_triplet_pt2(a, j)
term(83) = term(83) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_24_triplet_pt2(b, j)
term(84) = term(84) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_35_triplet_pt2(b, j)
term(85) = term(85) + s2(a,p,i,q) * t2(a,b,i,j) * wm_interm_33_triplet_pt2(b, j)
end do 
end do 
end do 
end do 

term(77) = term(77) * 16.0d+0 
term(78) = term(78) * (-8.0d+0) 
term(79) = term(79) * (-16.0d+0) 
term(80) = term(80) * 16.0d+0 
term(81) = term(81) * 8.0d+0 
term(82) = term(82) * (-8.0d+0) 
term(83) = term(83) * 16.0d+0 
term(84) = term(84) * 16.0d+0 
term(85) = term(85) * (-16.0d+0) 

do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(86) = term(86) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_4_triplet_pt2(b, a)
term(87) = term(87) + r1(vrdav_Rl, a,i) * r2p(vrdav_Rr, p,q,b,i) * wm_interm_5_triplet_pt2(b, a)
term(88) = term(88) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_28_triplet_pt2(a, b)
term(89) = term(89) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_29_triplet_pt2(a, b)
term(90) = term(90) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_37_triplet_pt2(a, b)
term(91) = term(91) + r1(vrdav_Rr, a,i) * s2(b,p,q,i) * wm_interm_38_triplet_pt2(a, b)
end do 
end do 
end do 

term(86) = term(86) * (-8.0d+0) 
term(87) = term(87) * 4.0d+0 
term(88) = term(88) * 3.0d+0 
term(89) = -term(89) 
term(90) = term(90) * (-4.0d+0) 
term(91) = term(91) * 4.0d+0 

do i = 1, nocc 
do a = nocc + 1, nactive 
do b = nocc + 1, nactive 
term(92) = term(92) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_4_triplet_pt2(b, a)
term(93) = term(93) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,q,p,i) * wm_interm_5_triplet_pt2(b, a)
term(94) = term(94) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_10_triplet_pt2(b, a)
term(95) = term(95) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_11_triplet_pt2(b, a)
term(96) = term(96) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_17_triplet_pt2(b, a)
term(97) = term(97) + r1(vrdav_Rl, b,i) * t2(a,p,q,i) * wm_interm_18_triplet_pt2(b, a)
term(98) = term(98) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, b,i) * wm_interm_4_triplet_pt2(b, a)
term(99) = term(99) + r2p(vrdav_Rl, p,q,a,i) * r1(vrdav_Rr, b,i) * wm_interm_5_triplet_pt2(b, a)
term(100) = term(100) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_4_triplet_pt2(b, a)
term(101) = term(101) + r2m(vrdav_Rl, a,q,p,i) * r1(vrdav_Rr, b,i) * wm_interm_5_triplet_pt2(b, a)
end do 
end do 
end do 

term(92) = term(92) * 8.0d+0 
term(93) = term(93) * (-4.0d+0) 
term(94) = term(94) * 2.0d+0 
term(95) = term(95) * (-2.0d+0) 
term(96) = term(96) * (-4.0d+0) 
term(97) = term(97) * 4.0d+0 
term(98) = term(98) * (-8.0d+0) 
term(99) = term(99) * 4.0d+0 
term(100) = term(100) * 8.0d+0 
term(101) = term(101) * (-4.0d+0) 

do a = nocc + 1, nactive 
do i = 1, nocc 
do b = nocc + 1, nactive 
term(102) = term(102) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_4_triplet_pt2(b, a)
term(103) = term(103) + r1(vrdav_Rl, a,i) * r2m(vrdav_Rr, b,i,p,q) * wm_interm_5_triplet_pt2(b, a)
term(104) = term(104) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_4_triplet_pt2(b, a)
term(105) = term(105) + r2m(vrdav_Rl, a,i,p,q) * r1(vrdav_Rr, b,i) * wm_interm_5_triplet_pt2(b, a)
end do 
end do 
end do 

term(102) = term(102) * (-8.0d+0) 
term(103) = term(103) * 4.0d+0 
term(104) = term(104) * (-8.0d+0) 
term(105) = term(105) * 4.0d+0 

do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(106) = term(106) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_10_triplet_pt2(a, b)
term(107) = term(107) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_11_triplet_pt2(a, b)
term(108) = term(108) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_17_triplet_pt2(a, b)
term(109) = term(109) + r1(vrdav_Rl, a,i) * t2(b,p,i,q) * wm_interm_18_triplet_pt2(a, b)
term(110) = term(110) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_28_triplet_pt2(a, b)
term(111) = term(111) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_29_triplet_pt2(a, b)
term(112) = term(112) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_37_triplet_pt2(a, b)
term(113) = term(113) + r1(vrdav_Rr, a,i) * s2(b,p,i,q) * wm_interm_38_triplet_pt2(a, b)
end do 
end do 
end do 

term(106) = term(106) * (-4.0d+0) 
term(107) = term(107) * 4.0d+0 
term(108) = term(108) * 8.0d+0 
term(109) = term(109) * (-8.0d+0) 
term(110) = term(110) * (-6.0d+0) 
term(111) = term(111) * 2.0d+0 
term(112) = term(112) * 8.0d+0 
term(113) = term(113) * (-8.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
term(114) = term(114) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,j) * wm_interm_9_triplet_pt2(b, i, k, q)
end do 
end do 
end do 
end do 
end do 

term(114) = term(114) * (-4.0d+0) 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
do i = 1, nocc 
term(115) = term(115) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,j,k) * wm_interm_9_triplet_pt2(b, i, k, q)
term(116) = term(116) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,j,k) * wm_interm_9_triplet_pt2(b, i, q, k)
term(117) = term(117) + r2p(vrdav_Rr, p,i,a,j) * s2(a,b,k,j) * wm_interm_9_triplet_pt2(b, i, q, k)
end do 
end do 
end do 
end do 
end do 

term(115) = term(115) * 4.0d+0 
term(116) = term(116) * (-8.0d+0) 
term(117) = term(117) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(118) = term(118) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,i) * wm_interm_9_triplet_pt2(b, j, k, q)
term(119) = term(119) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_9_triplet_pt2(b, i, k, q)
term(120) = term(120) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_26_triplet_pt2(b, k, q, j)
term(121) = term(121) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_26_triplet_pt2(b, k, q, i)
term(122) = term(122) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_34_triplet_pt2(b, q, k, j)
term(123) = term(123) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_34_triplet_pt2(b, q, k, i)
term(124) = term(124) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_36_triplet_pt2(b, k, q, j)
term(125) = term(125) + s2(a,p,j,i) * t2(a,b,k,i) * wm_interm_34_triplet_pt2(b, k, q, j)
term(126) = term(126) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_34_triplet_pt2(b, k, q, i)
term(127) = term(127) + s2(a,p,j,i) * t2(a,b,k,j) * wm_interm_36_triplet_pt2(b, q, k, i)
end do 
end do 
end do 
end do 
end do 

term(118) = term(118) * (-2.0d+0) 
term(119) = term(119) * (-2.0d+0) 
term(120) = term(120) * 4.0d+0 
term(121) = term(121) * (-4.0d+0) 
term(122) = term(122) * (-2.0d+0) 
term(123) = term(123) * 4.0d+0 
term(124) = term(124) * (-2.0d+0) 
term(125) = term(125) * 4.0d+0 
term(126) = term(126) * (-2.0d+0) 
term(127) = term(127) * (-2.0d+0) 

do j = 1, nocc 
do b = nocc + 1, nactive 
do k = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(128) = term(128) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,j) * wm_interm_9_triplet_pt2(b, i, k, q)
end do 
end do 
end do 
end do 
end do 

term(128) = term(128) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do b = nocc + 1, nactive 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(129) = term(129) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_9_triplet_pt2(b, i, k, q)
term(130) = term(130) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,j,k) * wm_interm_9_triplet_pt2(b, i, q, k)
term(131) = term(131) + r2m(vrdav_Rr, a,i,p,j) * s2(a,b,k,j) * wm_interm_9_triplet_pt2(b, i, q, k)
end do 
end do 
end do 
end do 
end do 

term(129) = term(129) * (-2.0d+0) 
term(130) = term(130) * 4.0d+0 
term(131) = term(131) * (-2.0d+0) 

do k = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(132) = term(132) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_9_triplet_pt2(b, j, k, q)
term(133) = term(133) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_9_triplet_pt2(b, i, k, q)
term(134) = term(134) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,i,k) * wm_interm_9_triplet_pt2(b, j, q, k)
term(135) = term(135) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,j,k) * wm_interm_9_triplet_pt2(b, i, q, k)
term(136) = term(136) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,i) * wm_interm_9_triplet_pt2(b, j, q, k)
term(137) = term(137) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_26_triplet_pt2(b, k, q, i)
term(138) = term(138) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_34_triplet_pt2(b, q, k, i)
term(139) = term(139) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_34_triplet_pt2(b, k, q, i)
term(140) = term(140) + s2(a,p,j,i) * t2(a,b,j,k) * wm_interm_36_triplet_pt2(b, q, k, i)
end do 
end do 
end do 
end do 
end do 

term(132) = term(132) * (-2.0d+0) 
term(133) = term(133) * 4.0d+0 
term(134) = term(134) * 4.0d+0 
term(135) = term(135) * (-8.0d+0) 
term(136) = term(136) * (-2.0d+0) 
term(137) = term(137) * 8.0d+0 
term(138) = term(138) * (-8.0d+0) 
term(139) = term(139) * 4.0d+0 
term(140) = term(140) * 4.0d+0 

do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
do b = nocc + 1, nactive 
do a = nocc + 1, nactive 
term(141) = term(141) + r2m(vrdav_Rr, a,j,p,i) * s2(a,b,k,j) * wm_interm_9_triplet_pt2(b, i, q, k)
term(142) = term(142) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_26_triplet_pt2(b, k, q, j)
term(143) = term(143) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_34_triplet_pt2(b, q, k, j)
term(144) = term(144) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_34_triplet_pt2(b, k, q, j)
term(145) = term(145) + s2(a,p,j,i) * t2(a,b,i,k) * wm_interm_36_triplet_pt2(b, q, k, j)
end do 
end do 
end do 
end do 
end do 

term(141) = term(141) * 4.0d+0 
term(142) = term(142) * (-4.0d+0) 
term(143) = term(143) * 4.0d+0 
term(144) = term(144) * (-2.0d+0) 
term(145) = term(145) * (-2.0d+0) 

term(146) = term(146) + s1(p,q) * wm_interm_44_triplet_pt2
term(147) = term(147) + t1(p,q) * wm_interm_44_triplet_pt2
term(148) = term(148) + s1(p,q) * wm_interm_54_triplet_pt2
term(149) = term(149) + s1(p,q) * wm_interm_58_triplet_pt2
term(150) = term(150) + s1(p,q) * wm_interm_59_triplet_pt2
term(151) = term(151) + t1(p,q) * wm_interm_54_triplet_pt2
term(152) = term(152) + t1(p,q) * wm_interm_58_triplet_pt2
term(153) = term(153) + t1(p,q) * wm_interm_59_triplet_pt2
term(154) = term(154) + s1(p,q) * wm_interm_66_triplet_pt2
term(155) = term(155) + s1(p,q) * wm_interm_71_triplet_pt2
term(156) = term(156) + s1(p,q) * wm_interm_72_triplet_pt2
term(157) = term(157) + t1(p,q) * wm_interm_66_triplet_pt2
term(158) = term(158) + t1(p,q) * wm_interm_71_triplet_pt2
term(159) = term(159) + t1(p,q) * wm_interm_72_triplet_pt2

term(146) = term(146) * (-4.0d+0) 
term(147) = term(147) * (-4.0d+0) 
term(148) = term(148) * (-4.0d+0) 
term(149) = term(149) * 4.0d+0 
term(150) = term(150) * (-4.0d+0) 
term(151) = term(151) * (-4.0d+0) 
term(152) = term(152) * 4.0d+0 
term(153) = term(153) * (-4.0d+0) 
term(154) = term(154) * (-8.0d+0) 
term(155) = term(155) * 8.0d+0 
term(156) = term(156) * (-8.0d+0) 
term(157) = term(157) * (-8.0d+0) 
term(158) = term(158) * 8.0d+0 
term(159) = term(159) * (-8.0d+0) 

do i = 1, nocc 
term(160) = term(160) + wm_interm_1_triplet_pt2(i, q) * wm_interm_7_triplet_pt2(p, i)
term(161) = term(161) + wm_interm_2_triplet_pt2(i, q) * wm_interm_7_triplet_pt2(p, i)
term(162) = term(162) + wm_interm_15_triplet_pt2(p, i) * wm_interm_1_triplet_pt2(i, q)
term(163) = term(163) + wm_interm_16_triplet_pt2(p, i) * wm_interm_1_triplet_pt2(i, q)
term(164) = term(164) + wm_interm_15_triplet_pt2(p, i) * wm_interm_2_triplet_pt2(i, q)
term(165) = term(165) + wm_interm_16_triplet_pt2(p, i) * wm_interm_2_triplet_pt2(i, q)
term(166) = term(166) + wm_interm_27_triplet_pt2(p, i) * wm_interm_30_triplet_pt2(q, i)
term(167) = term(167) + wm_interm_27_triplet_pt2(p, i) * wm_interm_32_triplet_pt2(q, i)
term(168) = term(168) + wm_interm_27_triplet_pt2(p, i) * wm_interm_39_triplet_pt2(q, i)
term(169) = term(169) + wm_interm_27_triplet_pt2(p, i) * wm_interm_41_triplet_pt2(q, i)
term(170) = term(170) + r1(vrdav_Rl, p,i) * wm_interm_46_triplet_pt2(i, q)
term(171) = term(171) + r1(vrdav_Rr, p,i) * wm_interm_47_triplet_pt2(i, q)
end do 

term(160) = term(160) * 4.0d+0 
term(161) = term(161) * (-8.0d+0) 
term(162) = term(162) * (-4.0d+0) 
term(163) = term(163) * 4.0d+0 
term(164) = term(164) * 8.0d+0 
term(165) = term(165) * (-8.0d+0) 
term(166) = term(166) * (-3.0d+0) 
term(168) = term(168) * (-4.0d+0) 
term(169) = term(169) * 4.0d+0 
term(170) = term(170) * 2.0d+0 
term(171) = term(171) * 2.0d+0 

do a = nocc + 1, nactive 
term(172) = term(172) + wm_interm_10_triplet_pt2(p, a) * wm_interm_3_triplet_pt2(a, q)
term(173) = term(173) + wm_interm_11_triplet_pt2(p, a) * wm_interm_3_triplet_pt2(a, q)
term(174) = term(174) + wm_interm_19_triplet_pt2(p, a) * wm_interm_3_triplet_pt2(a, q)
term(175) = term(175) + wm_interm_17_triplet_pt2(p, a) * wm_interm_3_triplet_pt2(a, q)
term(176) = term(176) + wm_interm_18_triplet_pt2(p, a) * wm_interm_3_triplet_pt2(a, q)
term(177) = term(177) + wm_interm_24_triplet_pt2(a, q) * wm_interm_4_triplet_pt2(p, a)
term(178) = term(178) + wm_interm_24_triplet_pt2(a, q) * wm_interm_5_triplet_pt2(p, a)
term(179) = term(179) + wm_interm_33_triplet_pt2(a, q) * wm_interm_4_triplet_pt2(p, a)
term(180) = term(180) + wm_interm_33_triplet_pt2(a, q) * wm_interm_5_triplet_pt2(p, a)
term(181) = term(181) + wm_interm_35_triplet_pt2(a, q) * wm_interm_4_triplet_pt2(p, a)
term(182) = term(182) + wm_interm_35_triplet_pt2(a, q) * wm_interm_5_triplet_pt2(p, a)
end do 

term(172) = term(172) * 2.0d+0 
term(173) = term(173) * (-2.0d+0) 
term(174) = term(174) * (-2.0d+0) 
term(175) = term(175) * (-2.0d+0) 
term(176) = term(176) * 4.0d+0 
term(177) = term(177) * (-8.0d+0) 
term(178) = term(178) * 4.0d+0 
term(179) = term(179) * 8.0d+0 
term(180) = term(180) * (-4.0d+0) 
term(181) = term(181) * (-8.0d+0) 
term(182) = term(182) * 4.0d+0 

do j = 1, nocc 
do i = 1, nocc 
term(183) = term(183) + wm_interm_0_triplet_pt2(p, i, q, j) * wm_interm_1_triplet_pt2(j, i)
term(184) = term(184) + wm_interm_0_triplet_pt2(p, i, q, j) * wm_interm_2_triplet_pt2(j, i)
term(185) = term(185) + wm_interm_12_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, q, j)
term(186) = term(186) + wm_interm_13_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, q, j)
term(187) = term(187) + wm_interm_12_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, j, q)
term(188) = term(188) + wm_interm_13_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, j, q)
term(189) = term(189) + wm_interm_20_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, q, j)
term(190) = term(190) + wm_interm_21_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, q, j)
term(191) = term(191) + wm_interm_22_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, q, j)
term(192) = term(192) + wm_interm_20_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, j, q)
term(193) = term(193) + wm_interm_21_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, j, q)
term(194) = term(194) + wm_interm_22_triplet_pt2(i, j) * wm_interm_9_triplet_pt2(p, i, j, q)
term(195) = term(195) + wm_interm_26_triplet_pt2(p, i, q, j) * wm_interm_2_triplet_pt2(j, i)
term(196) = term(196) + wm_interm_25_triplet_pt2(p, i, j, q) * wm_interm_30_triplet_pt2(i, j)
term(197) = term(197) + wm_interm_25_triplet_pt2(p, i, q, j) * wm_interm_30_triplet_pt2(i, j)
term(198) = term(198) + wm_interm_25_triplet_pt2(p, i, j, q) * wm_interm_32_triplet_pt2(i, j)
term(199) = term(199) + wm_interm_25_triplet_pt2(p, i, q, j) * wm_interm_32_triplet_pt2(i, j)
term(200) = term(200) + wm_interm_25_triplet_pt2(p, i, j, q) * wm_interm_39_triplet_pt2(i, j)
term(201) = term(201) + wm_interm_25_triplet_pt2(p, i, q, j) * wm_interm_39_triplet_pt2(i, j)
term(202) = term(202) + wm_interm_25_triplet_pt2(p, i, j, q) * wm_interm_41_triplet_pt2(i, j)
term(203) = term(203) + wm_interm_25_triplet_pt2(p, i, q, j) * wm_interm_41_triplet_pt2(i, j)
end do 
end do 

term(183) = term(183) * (-4.0d+0) 
term(184) = term(184) * 8.0d+0 
term(185) = -term(185) 
term(186) = term(186) * 3.0d+0 
term(187) = term(187) * 2.0d+0 
term(188) = term(188) * (-6.0d+0) 
term(189) = term(189) * (-2.0d+0) 
term(190) = term(190) * (-2.0d+0) 
term(191) = term(191) * 4.0d+0 
term(192) = term(192) * 4.0d+0 
term(193) = term(193) * 4.0d+0 
term(194) = term(194) * (-8.0d+0) 
term(195) = term(195) * (-8.0d+0) 
term(196) = term(196) * (-6.0d+0) 
term(197) = term(197) * 3.0d+0 
term(198) = term(198) * 2.0d+0 
term(199) = -term(199) 
term(200) = term(200) * 8.0d+0 
term(201) = term(201) * (-4.0d+0) 
term(202) = term(202) * (-8.0d+0) 
term(203) = term(203) * 4.0d+0 


    calc_D_vo_wm_triplet_pt2 = zero
    do s = 0, 203
    calc_D_vo_wm_triplet_pt2 = calc_D_vo_wm_triplet_pt2 + term(s)
    end do

    end function calc_D_vo_wm_triplet_pt2
    
    function calc_D_vv_wm_triplet_pt2(t2, t1, s2, s1, nocc, nactive, vrdav_Rl, vrdav_Rr, k1, k2, p, q) 
    real(F64) :: calc_D_vv_wm_triplet_pt2
    integer, intent(in) :: nocc, nactive
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: t2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: t1 
    real(F64), dimension(nocc+1:nactive,nocc+1:nactive,nocc,nocc), intent(in) :: s2 
    real(F64), dimension(nocc+1:nactive,nocc), intent(in)                  :: s1 
    double precision, dimension(:), intent(in) :: vrdav_Rl
    double precision, dimension(:), intent(in) :: vrdav_Rr
    integer, intent(in) :: k1, k2
    integer, intent(in) :: p, q
    integer :: s , k, j, i, a, b 
    real(F64), dimension(0:28) :: term 
    term = 0.d+0 

    term = 0.d+0 
    do k = 1, nocc 
do j = 1, nocc 
do i = 1, nocc 
term(0) = term(0) + wm_interm_25_triplet_pt2(p, i, j, k) * wm_interm_9_triplet_pt2(q, i, j, k)
term(1) = term(1) + wm_interm_25_triplet_pt2(p, i, j, k) * wm_interm_9_triplet_pt2(q, i, k, j)
end do 
end do 
end do 

term(0) = term(0) * (-4.0d+0) 
term(1) = term(1) * 2.0d+0 

do j = 1, nocc 
do i = 1, nocc 
do k = 1, nocc 
do a = nocc + 1, nactive 
term(2) = term(2) + s2(a,p,k,j) * t2(a,q,j,i) * wm_interm_42_triplet_pt2(i, k)
term(3) = term(3) + s2(a,p,k,j) * t2(a,q,k,i) * wm_interm_42_triplet_pt2(i, j)
term(4) = term(4) + s2(a,p,i,j) * t2(a,q,k,i) * wm_interm_42_triplet_pt2(k, j)
end do 
end do 
end do 
end do 

term(2) = term(2) * 2.0d+0 
term(3) = term(3) * (-4.0d+0) 
term(4) = term(4) * 2.0d+0 

do i = 1, nocc 
do k = 1, nocc 
do j = 1, nocc 
do a = nocc + 1, nactive 
term(5) = term(5) + s2(a,p,j,i) * t2(a,q,k,i) * wm_interm_42_triplet_pt2(k, j)
end do 
end do 
end do 
end do 

term(5) = term(5) * (-4.0d+0) 

term(6) = term(6) + wm_interm_44_triplet_pt2 * wm_interm_4_triplet_pt2(p, q)
term(7) = term(7) + wm_interm_44_triplet_pt2 * wm_interm_5_triplet_pt2(p, q)

term(6) = term(6) * 8.0d+0 
term(7) = term(7) * (-4.0d+0) 

do a = nocc + 1, nactive 
term(8) = term(8) + wm_interm_43_triplet_pt2(a, p) * wm_interm_4_triplet_pt2(q, a)
term(9) = term(9) + wm_interm_43_triplet_pt2(a, p) * wm_interm_5_triplet_pt2(q, a)
term(10) = term(10) + wm_interm_43_triplet_pt2(p, a) * wm_interm_4_triplet_pt2(a, q)
term(11) = term(11) + wm_interm_43_triplet_pt2(p, a) * wm_interm_5_triplet_pt2(a, q)
end do 

term(8) = term(8) * (-4.0d+0) 
term(9) = term(9) * 2.0d+0 
term(10) = term(10) * (-4.0d+0) 
term(11) = term(11) * 2.0d+0 

do i = 1, nocc 
term(12) = term(12) + wm_interm_27_triplet_pt2(p, i) * wm_interm_3_triplet_pt2(q, i)
term(13) = term(13) + r1(vrdav_Rl, q,i) * wm_interm_49_triplet_pt2(p, i)
term(14) = term(14) + r1(vrdav_Rl, q,i) * wm_interm_50_triplet_pt2(p, i)
term(15) = term(15) + r1(vrdav_Rl, q,i) * wm_interm_51_triplet_pt2(p, i)
term(16) = term(16) + s1(q,i) * wm_interm_7_triplet_pt2(p, i)
term(17) = term(17) + s1(q,i) * wm_interm_15_triplet_pt2(p, i)
term(18) = term(18) + s1(q,i) * wm_interm_16_triplet_pt2(p, i)
term(19) = term(19) + r1(vrdav_Rr, p,i) * wm_interm_52_triplet_pt2(q, i)
term(20) = term(20) + t1(q,i) * wm_interm_24_triplet_pt2(p, i)
term(21) = term(21) + r1(vrdav_Rr, p,i) * wm_interm_62_triplet_pt2(q, i)
term(22) = term(22) + r1(vrdav_Rr, p,i) * wm_interm_63_triplet_pt2(q, i)
term(23) = term(23) + t1(q,i) * wm_interm_33_triplet_pt2(p, i)
term(24) = term(24) + t1(q,i) * wm_interm_35_triplet_pt2(p, i)
end do 

term(12) = term(12) * 2.0d+0 
term(13) = term(13) * 4.0d+0 
term(14) = term(14) * 4.0d+0 
term(15) = term(15) * (-4.0d+0) 
term(16) = term(16) * (-4.0d+0) 
term(17) = term(17) * 4.0d+0 
term(18) = term(18) * (-4.0d+0) 
term(19) = term(19) * 4.0d+0 
term(20) = term(20) * (-4.0d+0) 
term(21) = term(21) * (-4.0d+0) 
term(22) = term(22) * 4.0d+0 
term(23) = term(23) * 4.0d+0 
term(24) = term(24) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
term(25) = term(25) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_1_triplet_pt2(i, j)
term(26) = term(26) + r1(vrdav_Rl, q,j) * r1(vrdav_Rr, p,i) * wm_interm_2_triplet_pt2(i, j)
end do 
end do 

term(25) = term(25) * 2.0d+0 
term(26) = term(26) * (-4.0d+0) 

do j = 1, nocc 
do i = 1, nocc 
do a = nocc + 1, nactive 
term(27) = term(27) + r1(vrdav_Rr, p,i) * s2(a,q,i,j) * wm_interm_3_triplet_pt2(a, j)
term(28) = term(28) + r1(vrdav_Rl, p,i) * t2(a,q,i,j) * wm_interm_27_triplet_pt2(a, j)
end do 
end do 
end do 

term(27) = term(27) * 2.0d+0 
term(28) = term(28) * 2.0d+0 


    calc_D_vv_wm_triplet_pt2 = zero
    do s = 0, 28
    calc_D_vv_wm_triplet_pt2 = calc_D_vv_wm_triplet_pt2 + term(s)
    end do

    end function calc_D_vv_wm_triplet_pt2
    

    
    
end module density_exc_exc_functions_triplet_pt012
